  # Objective 1  

  setwd("C:/Users/Cary Wang/Downloads/Names/Names")
  babynames = NULL
  years = c(1880:2014)
  for(x in years){
    temp=read.csv(paste("yob",toString(x),".txt",sep=""),header=FALSE)
    temp[["year"]] <- x
    babynames <- rbind(babynames,temp)}
  colnames(babynames) <- c("Name","Gender","Instances","Year")
  
  # Objective 2
  
  myname <- babynames[which(babynames$Name=="Cary"&babynames$Gender=="M"),]
  plot(myname$Year,myname$Instances,main='Frequency of Cary',xlab='Year',ylab='Instance Count')

  # Objective 3
  
  girlnames <- subset(babynames, babynames$Gender=="F")
  girlnames <- girlnames[,-(2:3),drop=FALSE]
  counts <- table(girlnames$Year)
  barplot(counts,main="Unique Girl Names Over Time",ylab="Number of Unique Names", xlab="Year")
  
  # Objective 4
  
  toptennames <- function(year,gender){
    ttn <- babynames[which(babynames$Gender==gender&babynames$Year==year),]
    top_names = ttn[order(ttn$Instances,decreasing=TRUE),]
    return(head(top_names,10))
  }
  
  print(toptennames(2014,"F"))
  
  # Objective 5
  
  male <- babynames[which(babynames$Gender=="M" & babynames$Year==2014),]
  male <- cbind(male[order(male$Instances, decreasing=TRUE),], c(1:nrow(male)))
  
  female <- babynames[which(babynames$Gender=="F"  & babynames$Year==2014),]
  female <- cbind(female[order(female$Instances, decreasing=TRUE),], c(1:nrow(female)))
  
  names(male)[5] <- "rank"
  names(female)[5] <- "rank"
  
  mf <- merge(male, female, by=c("Name", "Year"), suffixes=c(".m", ".f"))
  mf <- mf[order(pmax(mf$rank.m, mf$rank.f)),]
  head(as.data.frame(unique(mf[, "Name"])))
  