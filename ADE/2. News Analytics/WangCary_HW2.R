library(tm)
library(topicmodels)
library(rvest)
library(stringr)

# Step 1:
# Import data
setwd("C://Users/Cary Wang/Downloads")
article_training = read.csv("NewsArticles.csv")

# Transform content into corpus and clean text
corp.original = VCorpus(VectorSource(article_training$content))
corp = tm_map(corp.original, removePunctuation)
corp = tm_map(corp, removeNumbers)
corp = tm_map(corp, content_transformer(removeWords), stopwords("english") ,lazy=TRUE)  
corp = tm_map(corp, content_transformer(tolower),lazy=TRUE)
corp = tm_map(corp, content_transformer(removeWords), c("said") ,lazy=TRUE)
corp = tm_map(corp, content_transformer(stemDocument), lazy=TRUE) 
corp = tm_map(corp, stripWhitespace)

# Convert corp into a DTM
dtm = DocumentTermMatrix(corp)
dtms = removeSparseTerms(dtm, .995)

# Select the first 2000 documents and run LDA model
trunc_dtm = dtms[1:2000,]
lda_model = LDA(trunc_dtm, 10,method="Gibbs")
topics = topics(lda_model)

# View terms and rename topic names
terms = as.matrix(terms(lda_model,10))
colnames(terms) = c("Business","Technology","News","Politics", "Crime","Personal Finance","Health Insurance","Finance", "Markets","Real Estate")
View(terms)

# Step 2: Scrape CNBC home page
homepage = read_html("http://www.cnbc.com/us-news/")
urls = html_nodes(homepage,".headline a")
urls = html_attr(urls, "href")
urls = urls[!is.na(urls)]

# First testing on one article
urls
newspage = read_html("http://www.cnbc.com/2016/11/25/black-friday-sales-stats.html")
article_text = html_nodes(newspage, "#article_body li , .inline-player+ .group p")
article_text = html_text(article_text)

# Create a function that strips whitespace and collapses string vectors into one
textCleaner = function(text){
  text = paste(text,collapse='')
  text = tolower(text)
  text = gsub("\\s+"," ",text)
}

# Now run function on all articles and store in a new list
texts = list()

for(i in 1:23){
  text = read_html(paste('http://www.cnbc.com',urls[i],sep=''))
  text = html_nodes(text, "p")
  text = html_text(text)
  text = textCleaner(text)
  texts = rbind(texts,text)
  }

# Step 3: Classify news articles
# Clean up text
dic = Terms(dtms)
corp.foo = VCorpus(VectorSource(texts))
corp.df = tm_map(corp.foo, removePunctuation)
corp.df = tm_map(corp.df, removeNumbers)
corp.df = tm_map(corp.df, content_transformer(stemDocument), lazy = T)
corp.df = tm_map(corp.df, stripWhitespace)

# Specify this dictionary when creating the dtm for the new articles, which will limit the dtm it creates to only the words that also appeared in the archive.  In the example below, 'ldaOut' would be the name assigned to the topic model you created in Step 1.
new_dtm = DocumentTermMatrix(corp.df, control=list(dictionary = dic))
new_dtm = new_dtm[rowSums(as.matrix(new_dtm))!=0,]
topic_probabilities = posterior(lda_model, new_dtm)

# Assign column names to probabilities
probabilities = as.data.frame(topic_probabilities$topics)
names(probabilities) = colnames(terms)
probabilities

# Assign the max probability column to each story
tests = list()
for (i in 1:23){
  test = probabilities[i,which(probabilities[i,]==max(probabilities[i,]), arr.ind=T)]
  test[,1] = NULL
  tests = append(test,tests)
}

# Create a table matching topics with text
tests = rev(tests)
tab = cbind(tests,texts)
colnames(tab) = c("Probability", "Content")
View(tab)
