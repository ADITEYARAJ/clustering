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
ggplot(k,aes(x=k$
              ))+geom_histogram(bandwidth=0.25,fill="blue",color="black")
kk <- kselection(ml,k_threshold=0.9,max_centers = 12)
kk
View(k)
mn <- (ncol(k)-1)*(sum(apply(ml,2,var)))
mn
for (i in 1:10) {mn[i] <- sum(kmeans(ml,i)$withinss)}
plot(1:10,mn,type ="b")
#so from graph we get cluster as 2
library(cluster)
x <- clara(ml,4)
clusplot(x)
xl <- pam(ml,2)
clusplot(xl)
text(xl,rownames(ml))
final=sapply(1:15,function(x){kmeans(ml,x)$tot.withinss})
plot(1:15,final,main="Elbow for K",xlab="K",ylab="Total Withinss",frame=FALSE,type="b")
