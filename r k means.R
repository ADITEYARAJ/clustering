m <-  read.csv(file.choose())
ml <- read.csv(file.choose())
View(m)
ml$Gender <- as.numeric(ml$Gender)
ml$Gender
table(ml$Gender)
#1 for female and 2 for males.so 112 females and 88 males
d <- dist(ml,"euclidean")
dl <- hclust(d,"complete")
plot(dl)
#we see from plot that we need to use k means clustering
k <- scale(ml)
k <- as.data.frame(k)
View(k)
library(ggplot2)
ggplot(k,aes(x=k$Gender))+geom_histogram(bandwidth=0.25,fill="blue",color="black")
ggplot(k,aes(k$Age))+geom_histogram(bandwidth=0.5,fill="red",color="black")
ggplot(k,aes(k$Age))+geom_boxplot(bandwidth=0.5,fill="green",color="black")+scale_x_sqrt(k$Age)
ggplot(k,aes(k$Annual.Income..k..))+geom_histogram(bandwidth=0.5,fill="orange",color="black")
ggplot(k,aes(k$Annual.Income..k..))+geom_boxplot(bandwidth=0.5,fill="orange",color="black")#it is almost normal data with 1 outlier
ggplot(k,aes(k$Spending.Score..1.100.))+geom_histogram(bandwidth=0.9,fill="yellow",color="black")
ggplot(k,aes(k$Spending.Score..1.100.))+geom_boxplot(bandwidth=1,fill="yellow",color="black")#it is almost normal data
kk <- kselection(ml,k_threshold=0.9,max_centers = 12)
kk
View(k)
#we see from the boxplot that column 3 and 4 are influencing the most so i will consider the most
k <- k[,c(4,5)]
mn <- (ncol(k)-1)*(sum(apply(ml,2,var)))
mn
for (i in 1:15) {mn[i] <- sum(kmeans(ml,i)$withinss)}
plot(1:15,mn,type ="b")
#so from graph we get cluster as 5
library(animation)
plot(k)
kr <- kmeans.ani(k,5)
text(k,rownames(k))
library(cluster)
x <- clara(k,5)
clusplot(x)
xl <- pam(k,2)
clusplot(k)
text(k,rownames(k))
kr
res <- cbind(m,kr)
View(res)
res1 <- res[,c(6,1:5,7,8)]
View(res1)
write.csv(res1,"res1.csv")
getwd()
