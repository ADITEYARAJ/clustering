cc <- read.csv(file.choose())
View(cc)
library(Amelia)
missmap(cc)
summary(cc)
library(Hmisc)
library(dummies)
cc$MINIMUM_PAYMENTS <- with(cc,impute(cc$MINIMUM_PAYMENTS,mean))
summary(cc)                           
cc$CREDIT_LIMIT <- with(cc,impute(cc$CREDIT_LIMIT,mean))                           
summary(cc)
View(cc)
cc1 <- cc[,-1]
cc2 <- scale(cc1)
View(cc2)
install.packages("DataExplorer")
library(DataExplorer)
plot_histogram(cc2)
plot_missing(cc)
summary(cc)
library(kselection)
km <- kselection(cc2,k_threshold = 0.85,max_centers = 10)
km
wss <- sum(nrow(cc2)-1)*sum(apply(cc2,2,var))
wss
for (i in 1:20) {wss[i]=sum(kmeans(cc2,centers = i)$withinss)}
plot(1:20,wss,type = "b")
#It is clear from scree plot that there most be 5 cluster
library(animation)
plot(cc2)
k <- kmeans(cc2,centers = 5)
k$centers
library(cluster)
k1 <- clara(cc2,5)
clusplot(k1)
?c_plot()
cc2$cluster <- k$cluster
cc2$cluster
install.packages("reshape")
library(reshape)
cc2$cluster
c_plot <-melt(k,id.var="cluster")
View(k)
library(dplyr)
library(ggforce)
c_plot$cluster <- as.factor(cc2$cluster)
c_plot %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1) +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 1) +
  labs(x = NULL, y = NULL) +
  theme_minimal()