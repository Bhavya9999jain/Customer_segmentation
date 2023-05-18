###GETTING THE DATA

data <- read.csv(file.choose(("Mall_Customers.csv")))
str(data)

head(data)

summary(data)

na.omit(data)


hist(data$Age,
    col="green",
    main="Histogram to Show Count of Age Class",
    xlab="Age",
    ylab="Frequency",
    xlim = c(15,80),
    ylim = c(0, 40),
    labels=TRUE)

hist(data$Annual.Income..k..,
  col="#660033",
  main="Histogram for Annual Income",
  xlab="Annual Income Class",
  ylab="Frequency",
  labels=TRUE)

hist(data$Spending.Score..1.100.,
    main="HistoGram for Spending Score",
    xlab="Spending Score Class",
    ylab="Frequency",
    col="#6600cc",
    labels=TRUE)

boxplot(data$Age,
       col="#ff0066",
       main="Boxplot for Descriptive Analysis of Age")

plot(density(data$Annual.Income..k..),
    col="yellow",
    main="Density Plot for Annual Income",
    xlab="Annual Income Class",
    ylab="Density")
polygon(density(data$Annual.Income..k..), col="#ccff66")

library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
    type="b", pch = 19, frame = FALSE, 
    xlab="Number of clusters K",
    ylab="Total intra-clusters sum of squares")

##K-MEANS CLUSTERING

#STEP 1 - Determining the optimal no of clusters
##Here I'm using the gap statistics method.
library(cluster)

set.seed(125)
stat_gap <- clusGap(data[,3:5], FUN = kmeans, nstart = 25,
            K.max = 10, B = 50)
#plot(stat_gap)
#Dividing into 6 clusters
k6<-kmeans(data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

clusplot(data, k6$cluster, color=TRUE, shade=TRUE, labels=0,lines=0, )


pcclust=prcomp(data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

library("ggplot2")
set.seed(1)
ggplot(data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
              breaks=c("1", "2", "3", "4", "5","6"),
              labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


