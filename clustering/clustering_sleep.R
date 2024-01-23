# more options with other packages
install.packages("factoextra")
install.packages("flexclust")
install.packages("fpc")
install.packages("clustertend")
install.packages("cluster")
install.packages("ClusterR")
install.packages("stats")
install.packages("hopkins")
install.packages("gridExtra")
library(gridExtra)
library(factoextra)
library(flexclust)
library(fpc)
### library(clustertend)
library(hopkins)
library(cluster)
library(ClusterR)
library(stats)

loadedNamespaces()

Sys.setlocale("LC_ALL","English")
Sys.setenv(LANGUAGE='en')

#reading data
health<-read.csv("Sleep_health_and_lifestyle_dataset.csv")
health

#chcking data structure
str(health)

#data summary
summary(health)

#checking data for null values
health[!complete.cases(health), ]

is.na(health)

#converting chr to factor
health$Gender<-as.factor(health$Gender)
health$Occupation<-as.factor(health$Occupation)
health$BMI.Category<-as.factor(health$BMI.Category)
health$Blood.Pressure <-as.factor(health$Blood.Pressure)
health$Sleep.Disorder  <-as.factor(health$Sleep.Disorder)

#converting factor to numeric 
health$Gender<-as.numeric(health$Gender)
health$Occupation<-as.numeric(health$Occupation)
health$BMI.Category<-as.numeric(health$BMI.Category)
health$Blood.Pressure <-as.numeric(health$Blood.Pressure)
health$Sleep.Disorder  <-as.numeric(health$Sleep.Disorder)

write.csv(health, file = 'health_num.csv')

health<-read.csv('health_num.csv')

#plot columns
hist(health$Stress.Level)
hist(health$Sleep.Duration)
hist(health$Quality.of.Sleep)


for (i in colnames(health)) {
  hist(health[,i], main = paste("Plot of ", i))
}



#correlation matrix 
library(corrplot)

health_matrix <- data.matrix(health, rownames.force = NA)
M <- cor(health_matrix)
corrplot(M, method = "number", number.cex = 0.75, order="hclust")


#scaling values
health_scaled<-scale(health)


##clustering

a <- fviz_nbclust(health_scaled, FUNcluster = kmeans, method = "silhouette") + theme_classic() 
b <- fviz_nbclust(health_scaled, FUNcluster = cluster::pam, method = "silhouette") + theme_classic() 
c <- fviz_nbclust(health_scaled, FUNcluster = cluster::clara, method = "silhouette") + theme_classic() 
d <- fviz_nbclust(health_scaled, FUNcluster = hcut, method = "silhouette") + theme_classic() 
e <- fviz_nbclust(health_scaled, FUNcluster = cluster::fanny, method = "silhouette") + theme_classic() 
grid.arrange(a, b, c, d, e, ncol=2)
a
b
c
d
e


health_clusters_kmeans<- kmeans(health_scaled, 4)

health_clusters_kmeans$size
health_clusters_kmeans$cluster
health$cluster<-health_clusters_kmeans$cluster




health_clusters_hcut<- hcut(health_scaled, 4)
fviz_dend(health_clusters_hcut)



# dissimilarity matrix
d <- dist(health_scaled, method = "euclidean")

# complete linkage
hc1 <- hclust(d, method = "complete" )

# dendrogram
plot(hc1, cex = 0.6, hang = -1)

library(factoextra)

# general characteristics of clusters
aggregate(health, Stress.Level ~ cluster, mean)

aggregate(health, Quality.of.Sleep ~ cluster, mean)

aggregate(health, Sleep.Duration ~ cluster, mean)


