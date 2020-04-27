cd <- read.csv(file.choose())
View(cd)
library(Amelia)
missmap(cd)#FOR CHECKING ANY MISSING OR NA VALUE IN DATASET
table(cd$X)#FOR CHECKING ANY REPETITION OF CITY NAME
cd1 <- cd[,-1]#1st COLUMN IS CATEGORICAL VARIABLE SO REMOVING IT
View(cd1)
summary(cd1)
cd2 <- scale(cd1)#TO NORMALIZE THE DATA
cd2 <- as.data.frame(cd2)
library(tidyverse)
g1 <- cd2 %>% ggplot()+geom_boxplot(aes(cd2$Murder),color="black",fill="green")+coord_flip()
g2 <- cd2 %>% ggplot()+geom_histogram(aes(cd2$Murder),color="black",fill="green")
a1 <-  cd2 %>% ggplot()+geom_boxplot(aes(cd2$Assault),color="black",fill="blue")+coord_flip()
a2 <-  cd2 %>% ggplot()+geom_histogram(aes(cd2$Assault),color="black",fill="blue")
u1 <- cd2 %>% ggplot()+geom_boxplot(aes(cd2$UrbanPop),color="black",fill="red")+coord_flip()
u2 <-  cd2 %>% ggplot()+geom_histogram(aes(cd2$UrbanPop),color="black",fill="red")
r1 <- cd2 %>% ggplot()+geom_boxplot(aes(cd2$Rape),color="black",fill="yellow")+coord_flip()
r2 <-  cd2 %>% ggplot()+geom_histogram(aes(cd2$Rape),color="black",fill="yellow")
install.packages("gridExtra")
library(gridExtra)
grid.arrange(g1,g2,a1,a2,u1,u2,r1,r2,nrow=4)
#FROM THE BOX PLOT AND THE HISTOGRAM OF cd2$Rape WE FIND THAT IT HAS OUTLIERS IN THE Rape data
d1 <- dist(cd2,method = "euclidean")
dl <- hclust(d1,method="complete")
plot(dl)
plot(dl,hang = -1)
cluster1 <- cutree(dl,k=4)
rect.hclust(dl,k=4,border="red")
c1 <- as.matrix(cluster1)
c2 <- data.frame(cluster1,cd)
c2
library(kselection)
(k <- kselection(cd2,k_threshold=0.9,max_centers=10))#giving 2 cluster which is not proper
nm <- (ncol(cd2)-1)*sum(cd2,apply(cd2,2,var))
for (i in 1:10) {nm[i]=sum(kmeans(cd2,i)$withinss)}
plot(1:10,nm,type = "b",xlab = "No. of clusters",ylab = "within group sum of square")#So we get from the graph that we have 4 cluster
library(animation)
km <- kmeans.ani(cd2,4)
km$cluster
text(cd2,rownames(cd2))
library(cluster)
k1 <-clara(cd2,4) 
clusplot(k1)
cluster2 <- as.matrix(km$cluster)
cluster2
k2 <- cbind(cluster2,cd)
View(k2)
write.csv(c2,"crime data result.csv")

getwd()
