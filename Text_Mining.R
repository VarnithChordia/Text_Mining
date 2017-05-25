
main.page <- read_html(x = "http://www.presidency.ucsb.edu/sou.php")

urls <- main.page %>% # feed `main.page` to the next step
  html_nodes(".ver12 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

# Get link text
links <- main.page %>% # feed `main.page` to the next step
  html_nodes(".ver12 a") %>% # get the CSS nodes
  html_text() # extract the link text

sotu <- data.frame(links = links, urls = urls, stringsAsFactors = FALSE)
head(sotu)

sotu <- subset(x = sotu, links %in% 1947:2015)

republicans <- c(1953:1960, 1970:1974, 1974:1977, 1981:1988, 1989:1992, 2001:2008)

for(i in seq(nrow(sotu))) {
  text <- read_html(sotu$urls[i]) %>% 
    html_nodes(".displaytext") %>% 
    html_text() 
  
  # Find the political party of this link
  party <- ifelse(test = sotu$links[i] %in% republicans,
                  yes = "republican", no = "democrat")
  
  # Create the file name
  filename <- paste0("D:/Project_Work_V1", party, "-", sotu$links[i], ".txt")
  sink(file = filename) %>% # open file to write 
    cat(text)  # write the file
  sink() # close the file
}

View(sotu)


read.corpus <- function(directory, pattern = "", to.lower = TRUE) {
  corpus <- DirSource(directory = directory, pattern = pattern) %>%
    VCorpus # Read files and create `VCorpus` object
  if(to.lower == TRUE) 
    corpus <- # Lowercase text
      tm_map(corpus,content_transformer(tolower))
  return(corpus)
}

rep.corpus.raw <- read.corpus(directory = "D:/Project-Work/", pattern = "republican")

dem.corpus.raw <- read.corpus(directory = "D:/Project-Work/", pattern = "democrat")


corp<-function(x){
  x<-tm_map(x,removePunctuation) #remove punctuations like '.',';'
  x<-tm_map(x,removeNumbers)
  x<-tm_map(x,tolower)#to turn uppercase to lowercase
  x<-tm_map(x,removeWords,stopwords("english"))#remove englishstopwords
  x<-tm_map(x,stripWhitespace)
  x <- tm_map(x, PlainTextDocument)
}

rep.corpus.raw<-corp(rep.corpus.raw)
dem.corpus.raw<-corp(dem.corpus.raw)



create.wordlist <- function(corpus,create_df=FALSE) {
  wordlist <- corpus %>%
    DocumentTermMatrix() %>% 
    as.matrix() %>% 
    colSums()
  if(create_df == TRUE) 
    wordlist <- wordlist %>%
      data.frame(words = names(.), freq = ., row.names = NULL)
  return(wordlist)
}

rep.wordlist<-create.wordlist(rep.corpus.raw,create_df=TRUE)

dec.wordlist<-create.wordlist(dem.corpus.raw,create_df=TRUE)

nrow(rep.wordlist)

sort.rep.wordlist<-rep.wordlist[order(rep.wordlist$freq,decreasing =TRUE),]

sort.dec.wordlist<-dec.wordlist[order(dec.wordlist$freq,decreasing =TRUE),]


head(sort.rep.wordlist)

head(sort.dec.wordlist)


p<-ggplot(subset(sort.rep.wordlist,sort.rep.wordlist$freq>500),aes(words,freq))
p<-p+geom_bar(stat ="identity")
p



library(wordcloud)


wordcloud(words = rep.wordlist$words, freq = rep.wordlist$freq, max.words = 30)




wordcloud(words = dec.wordlist$words, freq = dec.wordlist$freq, max.words = 30)



#Create Corpus

repub.wordlist <- create.wordlist(corpus = rep.corpus.raw)
dem.wordlist <- create.wordlist(corpus = dem.corpus.raw)

# Calculate Relative Frequency Ratio #
# Words in dec.wordlist, not in rep.wordlist

repub.wordlist[setdiff(names(dem.wordlist), names(repub.wordlist))] <- 0 # Fill missing words

# Words in wordlist.a, not in dem.wordlist

dem.wordlist[setdiff(names(repub.wordlist), names(dem.wordlist))] <- 0 # Fill missing words

vocabulary <- repub.wordlist %>% names # get complete vocabulary listing

a.ratios <- rep.wordlist/sum(repub.wordlist) # ratios for wordlist a
b.ratios <- dem.wordlist/sum(dem.wordlist) # ratios for wordlist b

# wordlist.a ratio/ wordlist.b ratio (with +1 smoothing)

rfr <- log((a.ratios[vocabulary] + 1) / (b.ratios[vocabulary] + 1))

# Create a `data.frame`
rfr <- rfr %>%
  data.frame(Words = names(.), 
             Scores = ., 
             row.names = NULL, 
             stringsAsFactors = FALSE) 

head(rfr)





rep.dem_df <- rbind(head(rfr, 25), tail(rfr, 25))
rep.dem_df$Party <- ifelse(test = (rep.dem_df$Scores > 0), 
                           yes = "Republicans", 
                           no = "Democrats")
library(ggplot2)
ggplot(rep.dem_df, aes(x = reorder(Words, Scores), y = Scores, color = Party)) + 
  geom_point() +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Terms") + ylab("Relative Frequency Ratio (log)") + ggtitle("Top 50 most indicative terms") +
  theme(legend.position = "bottom")





findAssocs(repub.wordlist,"will",corlimit=0.98)

remove(main.page)

setwd("D:/Project-Work")

require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('Word_Usage_US_Presidents.rmd', 'test.md') # creates md file
markdownToHTML('test.md', 'test.html') # creates html file
browseURL(paste('file://', file.path(getwd(),'test.html'), sep='')) # open file in browse

