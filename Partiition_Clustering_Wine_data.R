#libraries
library(readxl)
library(ggplot2)
library(factoextra)
library(tidyverse)
library(dplyr)
library(cluster)
library (dataset)
library(fpc) #remember to install first this package!
library(NbClust)

#Setting the directory
setwd("C:/Users/ehima/OneDrive/Desktop/Year2/Semester2/Machine Learning/CourseWork")
getwd()

#reading the file
dataset <-read_excel("C:/Users/ehima/OneDrive/Desktop/Year2/Semester2/Machine Learning/CourseWork/Whitewine_v6.xlsx")
  write.csv(dataset,"Whitewine.csv",row.names = FALSE) #running only once
Mydata<-read.csv("Whitewine.csv")
y<-Mydata$quality
# Remove the last column from the dataframe

Mydata <- Mydata[, -ncol(Mydata)]
head(Mydata)

#boxplot outlier detection
# Only density and quality has no ouliers
print(length(boxplot.stats(Mydata$fixed.acidity)$out)+length(boxplot.stats(Mydata$volatile.acidity)$out)+length(boxplot.stats(Mydata$citric.acid)$out)+
        length(boxplot.stats(Mydata$residual.sugar)$out)+length(boxplot.stats(Mydata$chlorides)$out)+length(boxplot.stats(Mydata$free.sulfur.dioxide)$out)+
        length(boxplot.stats(Mydata$total.sulfur.dioxide)$out)+length(boxplot.stats(Mydata$density)$out)+length(boxplot.stats(Mydata$pH)$out)+
        length(boxplot.stats(Mydata$sulphates)$out)+length(boxplot.stats(Mydata$alcohol)$out)+length(boxplot.stats(Mydata$quality)$out)
  
)


sapply(Mydata[ , c('fixed.acidity', 'volatile.acidity', 'citric.acid','residual.sugar','chlorides',
                 'free.sulfur.dioxide','total.sulfur.dioxide','density','pH','sulphates','alcohol')],
       IQR)
outlierdetection1<-function(x){
  Q3 <- quantile(x, 0.75)
  Q1 <- quantile(x, 0.25)
  IQR <- Q3 - Q1
  result<-(Q1-(2.05*IQR))
  return(result)
  
}
outlierdetection2<-function(x){
  Q3 <- quantile(x, 0.75)
  Q1 <- quantile(x, 0.25)
  IQR <- Q3 - Q1
  result<-(Q3+(3.2*IQR))
  return(result)
  
}
lower<-apply(Mydata,2,outlierdetection1)
upper<-apply(Mydata,2,outlierdetection2)

#After getting the lower and upper bound
#check total outlier
for (col in colnames(Mydata)) {
  total_lower <- sum(Mydata[[col]] < lower[col])
  total_upper <- sum(Mydata[[col]] > upper[col])
  cat("Column:", col, "\n")
  cat("Total values lower than lower bound:", total_lower, "\n")
  cat("Total values greater than upper bound:", total_upper, "\n\n")
}
#Removal of outlier
# Remove outliers from the dataset
cleaned_data <- Mydata

for (col in colnames(Mydata)) {
  outliers_lower <- Mydata[[col]] < lower[col]
  outliers_upper <- Mydata[[col]] > upper[col]
  
  # Replace outliers with NA
  cleaned_data[[col]][outliers_lower | outliers_upper] <- NA
}

# Remove rows with any NA values
cleaned_data <- na.omit(cleaned_data)

# Print the number of rows removed
rows_removed <- nrow(Mydata) - nrow(cleaned_data)
cat("Number of rows removed due to outliers:", rows_removed, "\n")


write.csv(cleaned_data,"cleaned_data.csv",row.names = FALSE) 


#Scaling
num_cols <- ncol(cleaned_data)
df_NormZ<-as.data.frame(scale(cleaned_data))
df_NormZ
str(df_NormZ)
############
#Before PCA#
#NBCLUST
#1>>NbClust=2
set.seed(22)
clusterNo1=NbClust(df_NormZ, min.nc=2,max.nc=5,method="kmeans",index="all")
clusterNo1

############
#Elbow Method===2
#Using 2 to 6 given Nblcust answers
k<-2:15
set.seed(29)
WSS<-sapply(k,function(k){kmeans(df_NormZ,centers=k)$tot.withinss})
plot(k, WSS, type="b", xlab= "Number of k", ylab="Within sum of squares")

######Average Silhouette Method===2
k_result3<-fviz_nbclust(df_NormZ, kmeans, method = 'silhouette')
k_result3

#######Gap Statistics===3
set.seed(30)
fviz_nbclust(df_NormZ, kmeans, method = 'gap_stat')#

##Majority for k==2,2,2,2
#K means before PCA
correct_k<-2
incorrect_k<-5
k_kmeans<-kmeans(df_NormZ, centers = correct_k,nstart=10)
##visualization 1
fviz_cluster(k_kmeans, data = df_NormZ)
##visualizaton 2
fviz_cluster(k_kmeans, data = df_NormZ, ellipse.type = "euclid",star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
####
#In formations to Print
###
#The center,BSS,WSS,TSS-Total Sum of Square,Clustered Result
center=k_kmeans$centers
center
BSS=k_kmeans$betweenss
BSS
WSS=k_kmeans$withinss
WSS
TSS=k_kmeans$totss
TSS
clustered_result=k_kmeans#This is the clustered result
clustered_result
ksize=k_kmeans$size
ksize
####
y <- data.frame(quality = y)
y <- y[1:2599, ]
table(k_kmeans$cluster,y)
#1d#######
#Silhoutte is an evaluation metrics,it measure how well an conservation was clustered
#and it estimate an average distance between the cluster
sil <- silhouette(k_kmeans$cluster, dist(df_NormZ))
fviz_silhouette(sil)
average_silhouette_score <- round(mean(sil[, "sil_width"]),3)######
plotcluster(df_NormZ, k_kmeans$cluster)
#????? I need to Fix 1 data that is Negatively influencing the Silhouette??
## The negative
neg_sil_index <- which(sil[, 'sil_width'] < 0)
df_NormZ[neg_sil_index,]
nb = NbClust(df_NormZ, method = "complete")
##########
#PCA Implementation#
#Using Pr Comp beacuse it quicker
#:we scale the variables to have standard deviation one.{using :TRUE}
#The output from prcomp contains a number of useful quantities.
The_pca<- prcomp(df_NormZ)
summary(The_pca)
##Checking With PCA is relevant
The_pca$rotation
head(The_pca$x)
##So i can deduce PCA(1-7) 
#>1)Add pca exceeds the 86% mark for the proportion of variance benchmark
#>2)Higher cumulative variance offering a more comprehensive representation of data
final_pca<-as.data.frame(-The_pca$x[, c(1:7)])#Excluding the 8 PCA
head(final_pca)


########
#Using the Automated tool again to re-estimate my k
#NBCLUST 2
#1>>NbClust//=2
set.seed(32)
clusterNo1_2=NbClust(final_pca, min.nc=2,max.nc=15,method="kmeans",index="all")
clusterNo1_2
##Given majority of K2=2(2 is the best)

#Testing K result
k2<-2:15
#Elbow Method===2
set.seed(35)
WSS2<-sapply(k2,function(k2){kmeans(final_pca,centers=k2)$tot.withinss})
plot(k2, WSS2, type="b", xlab= "Number of k", ylab="Within sum of squares")
fviz_nbclust(final_pca, kmeans, method = 'wss')
######Average Silhouette after pca===2
k_result3_2<-fviz_nbclust(final_pca, kmeans, method = 'silhouette')
k_result3_2
#######Gap Statistics  after pca===2
set.seed(51)
fviz_nbclust(final_pca, kmeans, method = 'gap_stat')#
####################
#K after after PCA
correct_k2<-2
incorrect_k2<-5
k_kmeans2<-kmeans(final_pca, centers = correct_k2,nstart=10)

######
##visualization 
set.seed(44)
fviz_cluster(k_kmeans2, data = final_pca)
##visualization 2
fviz_cluster(k_kmeans2, data = final_pca, ellipse.type = "euclid",star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
####
fviz_cluster(k_kmeans2, 
             data = final_pca, 
             geom = "point", 
             repel = TRUE, 
             ggtheme = theme_minimal()) + 
  theme(legend.position = "right")


#In formations to Print
###
#The center,BSS,WSS,TSS-Total Sum of Square,Clustered Result
center2=k_kmeans2$centers
center2
BSS2=k_kmeans2$betweenss
BSS2
WSS2=k_kmeans2$withinss
WSS2
TSS2=k_kmeans2$totss
TSS2
clustered_result2=k_kmeans2#This is the clustered result
clustered_result2
ksize2=k_kmeans2$size
ksize2
#Silhouette 2#
sil2 <- silhouette(k_kmeans2$cluster, dist(final_pca))
fviz_silhouette(sil2)




#Calinski-Harabasz Index#==2 Is the best kmeans
set.seed(55)
index_values <- numeric(9)
for (k in 2:15) {
  model <- kmeans(final_pca, k, iter.max = 50)
  stats <- cluster.stats(final_pca, model$cluster)
  index_values[k - 1] <- stats$ch
}
###########
#plotting
plot(2:15, index_values, type = "b", pch = 19, xlab = "Number of Clusters", 
     ylab = "Calinski-Harabasz Index", main = "Calinski-Harabasz Index vs Number of Clusters")

optimal_k <- which.max(diff(index_values)) + 1
abline(v = optimal_k, col = "red", lty = 2)
text(optimal_k, max(index_values), labels = paste("Optimal k =", optimal_k), pos = 3, col = "red")

