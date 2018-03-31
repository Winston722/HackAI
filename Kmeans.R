# Hack.AI
ted <- read.csv("C:\\Users\\eaber\\Documents\\Programming\\Git repos\\HackAI\\ted_main.csv")
head(ted)
names(ted)
#didn't need all the qualitative (yet, later for NLP)
ted_n <- ted[,c(1, 3:7, 9:10, 13, 15, 17)]
names(ted_n)
summary(ted_n)

# successful goal:  if count(event) < 70 event = "other"
ted_n$event <- as.character(ted_n$event)
ted_n$event <- ifelse(ted_n$event %in% c("TED2014", "TED2009", "TED2013",
                                         "TED2016", "TED2015", "TED2011"), ted_n$event, "Other")

# tried to visualize for clusters
library(ggplot2)
ggplot(ted_n, aes(views, comments, color = event)) + geom_point() # try color = event or occupation

# moving on with k means
set.seed(20)
ted_k <- kmeans(ted_n[,c(1, 11)], 3, nstart = 100) # 1=comments 11=views
# plot
ted_k$cluster <- as.factor(ted_k$cluster)
ggplot(ted_n, aes(log(comments), log(views), color = ted_k$cluster)) + geom_point()


# end

# failed goal:  if count(event) < 70 event = "other"
# wrong solution 1: 
ted_s <- lapply(ted_n$event, function(X){
  if(X %in% c("TED2014", "TED2009")){
    return(X)
  } else {
    return("Other")
  }
})
# wrong solution 2: 
for(i in 1:length(ted_n)){
  if(ted_n[i]$event != "TED2014"){
    ted_n$event[i] = "Other"
  }
}
