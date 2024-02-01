library(corrplot) 

health<-read.csv("Sleep_health_and_lifestyle_dataset.csv")
health<-health[,c(2:13)]

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

health.cor<-cor(health, method="pearson") 
print(health.cor, digits=2)
corrplot(health.cor, order ="alphabet", tl.cex=0.6)
corrplot(health.cor,method = "number", order ="alphabet", tl.cex=0.6,number.cex = 0.6)



dist.health<-dist(t(health)) # as a main input we need distance between units
as.matrix(dist.health)[1:10, 1:10] # let’s see the distance matrix
mds1<-cmdscale(dist.health, k=2) #k - the maximum dimension of the space
summary(mds1)	# we get coordinates of new points

plot(mds1,xlim=c(-12000,-10000))
head(mds1)

# k-means
install.packages("factoextra")
library(factoextra)

# for features
#km.f<-eclust(t(CIA), "kmeans", hc_metric="euclidean",k=5) # does not work due to NAs
km.f<-eclust(t(CIA.new), "kmeans", hc_metric="euclidean",k=5)

fviz_cluster(km.f, main="kmeans / Euclidean", ylim=c(-15, 5), xlim=c(-15, 55), labelsize=10, repel=TRUE)
fviz_silhouette(km.f)