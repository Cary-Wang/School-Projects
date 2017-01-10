library(ggplot2)
library(plyr)
library(tm)
library(wordcloud)

yelp_data = read.csv("C://Users/Cary Wang/Downloads/YelpReviews_20K.csv")

# 1. Summary Statistics
# a) Histogram
ggplot(yelp_data, aes(x=stars))+geom_histogram()+ggtitle("Count of Restaurants by Star Rating")

# b) Average restaurant review count
biz_counted = count(yelp_data, c("business_id"))
average_biz_reviews = mean(biz_counted$freq)
average_biz_reviews

#c) Average reviews per user
user_counted = count(yelp_data,c("user_id"))
average_user_reviews = mean(user_counted$freq)

#d) Reviews for lunch
GoodForLunch = yelp_data[which(as.logical(yelp_data$GoodforLunch)==TRUE),]
NotGoodForLunch = yelp_data[which(as.logical(yelp_data$GoodforLunch)==FALSE),]
lunch_reviews_counted = count(GoodForLunch,"business_id")
non_lunch_reviews_counted = count(NotGoodForLunch,"business_id")
average_lunch_reviews = mean(lunch_reviews_counted$freq)
average_nonlunch_reviews = mean(non_lunch_reviews_counted$freq)

#e) Number of stars

average_lunch_rating=mean(GoodForLunch$stars)
average_nonlunch_rating=mean(NotGoodForLunch$stars)

# 2. Exploratory Text Analysis
#a) Convert to VCorpus

corp.original = VCorpus(VectorSource(yelp_data$text))

#b) Remove stopwords and whitespace

corp = tm_map(corp.original,removePunctuation)
corp = tm_map(corp,content_transformer(removeWords),stopwords("english"),lazy=TRUE)
corp = tm_map(corp,stripWhitespace)
corp = tm_map(corp,content_transformer(removeWords),c("the"),lazy=TRUE)
corp = tm_map(corp,content_transformer(tolower),lazy=TRUE)

writeLines(as.character(corp[[1]]))

#c) Create a document term matrix

dtm = DocumentTermMatrix(corp)
dtms = removeSparseTerms(dtm,.995)
m = as.matrix(dtms)
attri = colSums(m)
terms = rowSums(m)
ord = order(attri,decreasing = T)
freq.term = attri[ord[1:400]]
head(freq.term,15)

#d) Create wordcloud
wordcloud(names(freq.term),freq.term,max.words=100, colors = brewer.pal(6, "Set1"))

# 3. Text Analytics and Prediction
# Find unique words
dim(dtms)

# Generate list of words with most predictive power
dtmss = removeSparseTerms(dtms,.990)
dtmss = as.matrix(dtmss)
corr = cor(as.logical(yelp_data$GoodforLunch),dtmss)
names(corr) = colnames(dtmss)
sel <- order(abs(corr),decreasing=T)<=200
subset <- colnames(corr)[sel]

sapply(corr,class)


pos_terms <- corr[corr>0]
top_pos <- sort(pos_terms,decreasing=T)[1:20]

neg_terms <- corr[corr<0]
top_neg <-sort(neg_terms)[1:20]

# Create a wordcloud 
wordcloud(c(names(top_pos), names(top_neg)), 
          c(top_pos, abs(top_neg)), 
          random.order=FALSE, max.words=40, scale = c(2, .3), 
          colors = c(rep("darkgreen",20), rep("darkred", 20)), ordered.colors = TRUE)


# Split into training and test data
training = foo[c(1:16113),]
testing = foo[c(16114:20141),]

foo = as.data.frame(cbind(lunch = as.logical(yelp_data$GoodforLunch), dtmss[,subset]))
training_model = glm(lunch~.,data=training, family=binomial)
summary(training_model)
coef= coef(training_model)[-1]

# Split up into positive and negative terms
pos_subset<- coef[coef>0]
top_positive<- sort(pos_subset,decreasing=T)[1:15]
neg_subset <- coef[coef<0]
top_negative <-sort(neg_subset)[1:15]

wordcloud(c(names(top_positive), names(top_negative)), 
          c(top_positive, abs(top_negative)), scale = c(3, .2), 
          random.order=TRUE, max.words=30, 
          colors = c(rep("darkgreen",15),rep("darkred",15)), ordered.colors=T)


training$predict_value = predict(training_model,type = "response")
training$gfl_predicted = training$predict_val > 0.5
mean(training$gfl_predicted == training$lunch)

testing$predict_val = predict(training_model, newdata = testing[,c(2:201)], type = "response")
testing$predict_val

# Test against the same threshold as training model
testing$gfl_predicted = testing$predict_val > 0.5
mean(testing$gfl_predicted == testing$lunch)

training

