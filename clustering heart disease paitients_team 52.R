#read dataset
Heartdiseas <- read.table("Heartdiseas.txt", header = TRUE, sep = ",")

#view dataset
View(Heartdiseas)

#store dataset in two variables
A <- Heartdiseas
AS <- Heartdiseas

# Evidence that the data should be scaled?
summary(AS)

# Remove id
A = A[ , !(names(A) %in% c("id"))]
AS = AS[ , !(names(AS) %in% c("id"))]

# Scaling data and saving as a data frame
scaled = scale(AS)

# What does data look like now?
summary(scaled)

#view dataset after remove id
View(AS)

#give duplicated rows
AS[duplicated(AS),]

#give number of duplicates
sum(duplicated(AS))

#Find Location of Missing Values
which(is.na(AS$column_name))

#Count Total Missing Values
sum(is.na(AS$column_name))

#check if there is null
is.null(AS)

#visualizing outliers
boxplot(A[,c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope')])
boxplot(scaled)$out

#########################################################
#detect and remove outliers from dataset
outliers <- function(AS) { 
  
  Q1 <- quantile(AS, probs=.25) 
  Q3 <- quantile(AS, probs=.75) 
  iqr = Q3-Q1 

  upper_limit = Q3 + (iqr*1.5) 
  lower_limit = Q1 - (iqr*1.5) 
  
  AS > upper_limit | AS < lower_limit 
} 

remove_outliers <- function(AS, cols = names(d)) { 
  for (col in cols) { 
    AS <- AS[!outliers(AS[[col]]),] 
  } 
  AS
} 

AS <- remove_outliers(AS, c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope'))

# Create a boxplot of the dataset after removing outliers
boxplot(AS)$out

#find other outliers then remove them
AS <- remove_outliers(AS, c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope'))

# Create a boxplot after cleaning dataset from all outliers
boxplot(AS)$out

#########################################################
# create a histogram for all columns after cleaning dataset from all outliers
hist(AS$age)
hist(AS$sex)
hist(AS$cp)
hist(AS$trestbps)
hist(AS$chol)
hist(AS$fbs)
hist(AS$restecg)
hist(AS$thalach)
hist(AS$exang)
hist(AS$oldpeak)
hist(AS$slope)

#########################################################
#create scater plot
install.packages("car")
library("car")
scatterplot(age ~  sex , data =AS )

#test for correlation between 2 columns
cor.test(AS$age,AS$sex)

#########################################################
#kmeans by using 3 centroid
zz <- kmeans (AS, 3)
zz

#every centroid have a number of item
tap <- table(zz$cluster)
tap

## Create a boxplot of kmeans
k<- boxplot(zz)$stats

# Create a Plot of  the clusters 
q<-plot(AS, col = zz$cluster)
#x<-plot(AS$age, AS$chol, col = zz$cluster)

# Create  their centers
z<-points(zz$centers, col = 1:3, pch = 8, cex=2)

#To know the appropriate number of clusters
arr=numeric(16)  
for(i in 1:16) arr[i]=sum(kmeans(AS,i) $withinss)
plot(1:16,arr,type='b',xlab='number of cluster',ylab='within grp sum square')

#########################################################
# Finding distance matrix
distance_mat <- dist(AS, method = 'euclidean')
distance_mat

# Fitting Hierarchical clustering Model for single method
Hierar_cl_single <- hclust(distance_mat, method = "single")
Hierar_cl_single

# Plotting dendrogram
plot(Hierar_cl_single)
rect.hclust(Hierar_cl_single, k = 3, border = 2:5)


# Fitting Hierarchical clustering Model for complete method
Hierar_cl_complete <- hclust(distance_mat, method = "complete")
Hierar_cl_complete

# Plotting dendrogram
plot(Hierar_cl_complete)
rect.hclust(Hierar_cl_complete, k = 3, border = 2:5)


# Fitting Hierarchical clustering Model for average method
Hierar_cl_average <- hclust(distance_mat, method = "average")
Hierar_cl_average

# Plotting dendrogram
plot(Hierar_cl_average)
rect.hclust(Hierar_cl_average, k = 3, border = 2:5)



#########################################################
#fitting DBscan
install.packages("dbscan")
library(dbscan)

db = dbscan(AS, 0.45, 5)
db

hullplot(AS, db$cluster)
hullplot(AS, db$cluster + 1L:2L)
pairs(AS, col = db$cluster + 1L)
pairs(AS, col = db$cluster + 1L:2L)



