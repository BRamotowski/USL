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

health_int=health[,c(1,3,4,5,6,7,8,11,12)]
str(health_int)

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


str(health)
health


#correlation matrix 
library(corrplot)

health_matrix <- data.matrix(health, rownames.force = NA)
M <- cor(health_matrix)
corrplot(M, method = "number", number.cex = 0.75, order="hclust")


#plot columns


for (i in colnames(health)) {
  plot(health[,i], main = paste("Plot of ", i))
}
str(health)


#scaling values
health_scaled<-scale(health)

a <- fviz_nbclust(health_scaled, FUNcluster = kmeans, method = "silhouette") + theme_classic() 
b <- fviz_nbclust(health_scaled, FUNcluster = cluster::pam, method = "silhouette") + theme_classic() 
c <- fviz_nbclust(health_scaled, FUNcluster = cluster::clara, method = "silhouette") + theme_classic() 
d <- fviz_nbclust(health_scaled, FUNcluster = hcut, method = "silhouette") + theme_classic() 
e <- fviz_nbclust(health_scaled, FUNcluster = cluster::fanny, method = "silhouette") + theme_classic() 
grid.arrange(a, b, c, d, e, ncol=2)
a


library(factoextra)



# optimal number of clusters - elbow
opt<-Optimal_Clusters_KMeans(health_scaled, max_clusters=10, plot_clusters = TRUE)

get_clust_tendency(health_scaled, 2, graph=TRUE, gradient=list(low="blue",  high="white"), seed=1234)
# optimal number of clusters - silhouette
opt<-Optimal_Clusters_KMeans(health_scaled, max_clusters=10, plot_clusters=TRUE, criterion="silhouette")
