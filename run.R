############################
# CSC 424 PROJECT          #
############################
# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
  if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
  dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)

## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
  result <- paste(x1,x2,sep="")
  return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
  if (x %in% rownames(installed.packages())) { 
    print(concat("package already installed: ", x))
  }
  else { 
    install.packages(x) 
  }
  library(x, character.only=TRUE)
}

# get the data
# check to see if data is already downloaded
# if not, download it and save it to the data directory
csv_file <- concat(DATA_DIR, '/turkiye-student-evaluation_generic.csv')
if (file.exists(csv_file)) {
  data <- read.csv(csv_file)
} else {
  URL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00262/turkiye-student-evaluation_generic.csv"
  current_date <- date()
  data <- read.csv(URL)
  write.csv(data, file=csv_file)
}


# how many are n/a?
sum(is.na(data))
head(which(is.na(data)))
# how many are NULL?
sum(is.null(data))
# how many are blank?
length(which(data == ""))
str(data)

################################
# Linear Discriminant Analysis #
################################
# need to scale the Likert-type data from Q1-Q28; data[,7:34]
# in order to use it for anything useful
x <- data[, 7:34]
data[, 7:34] <- scale(x, center=TRUE, scale=TRUE)
y <- as.factor(data[,5]) # this is the class variable
data <- data.frame(y, data[,7:34])
head(data)
str(data)
lda <- lda(formula=data$y ~ ., data=data)
ld_coefs <- lda$scaling
write.table(round(ld_coefs,3), file=concat(OUTPUT_DIR,'/lda coefficients.csv'), sep=",")
# Box's M: It performs the Box's M-test for homogeneity of covariance matrices 
# obtained from multivariate normal data according to one classification factor. 
# The test is based on the chi-square approximation.
source('BoxMTest.R')
BoxMTest(data[,2:29],cl=as.factor(data[,1]))
# covariance matrices are different, so don't use the data for prediction

# make predictions for plotting
plda <- predict(lda)

# Confusion Matrix:
confusion_matrix <- table(plda$class, data$y)
write.table(confusion_matrix, 
            file=concat(OUTPUT_DIR,'/confusion matrix.csv'), sep=",")
# estimate the percentage of faculty rankings that will be mis-classified
round(1 - diag(prop.table(confusion_matrix)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix))), 4)
# cross-validation with leave-one-out
lda_cv <- lda(formula=data$y ~ ., data=data, CV=TRUE)
# Confusion Matrix:
confusion_matrix_cv <- table(lda_cv$class, data$y)
write.table(confusion_matrix_cv, 
            file=concat(OUTPUT_DIR,'/confusion matrix leave one out.csv'), sep=",")
# estimate the percentage that will be mis-classified
round(1 - diag(prop.table(confusion_matrix_cv)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix_cv))), 4)

# plot the LDA projection
prop.lda = lda$svd^2/sum(lda$svd^2)
lda_data <- data.frame(y = data$y, lda = plda$x)
lda_data <- lda_data[,1:3] # drop unnecessary LD's
load_package("ggplot2")
load_package("scales")
png(concat(IMAGES_DIR,'/lda plot.png'), 
    width = 1024, height = 1024)
ggplot(lda_data) + 
  geom_point(aes(lda.LD1, lda.LD2, col = y, shape = y), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep="")) +
  ggtitle("LDA Projection") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off()

###############################
# K-Means Clustering Analysis #
###############################
# Determine number of clusters based on weighted sum of squares
wss <- (nrow(data)-1)*sum(apply(data[,2:29],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data[,2:29], centers=i)$withinss)
png(concat(IMAGES_DIR,'/wss to pick number of clusters.png'), 
    width = 1024, height = 1024)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
dev.off()
# K-Means Cluster Analysis for k=5 on original data
fit5 <- kmeans(data[,2:29], 5) # 5 cluster solution

# cluster plot with ellipses
load_package("cluster")
png(concat(IMAGES_DIR,'/cluster plot1.png'), 
    width = 1024, height = 1024)
clusplot(data[,2:29], 
         fit5$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot with k=5")
dev.off()

# Centroid Plot against 1st 2 discriminant functions
load_package("fpc")
png(concat(IMAGES_DIR,'/cluster plot2.png'), 
    width = 1024, height = 1024)
plotcluster(data[,2:29], 
            method="dc",
            fit5$cluster,
            main="Cluster Plot with k=5")
dev.off()

# Colors represent clusters and numbers represent y's
clvecd <- as.integer(data$y)
png(concat(IMAGES_DIR,'/cluster plot3.png'), 
    width = 1024, height = 1024)
plotcluster(x=data[,2:29], 
            clvecd=fit5$cluster,
            method="dc",
            clnum=clvecd,
            main="Cluster Plot with k=5")
dev.off()
# Colors represent y's and numbers represent clusters
png(concat(IMAGES_DIR,'/cluster plot4.png'), 
    width = 1024, height = 1024)
plotcluster(x=data[,2:29], 
            clvecd=clvecd,
            method="dc",
            clnum=fit5$cluster,
            main="Cluster Plot with k=5")
dev.off()

####################################
# Hierarchical Clustering Analysis #
####################################
d <- dist(data[,2:29], method="euclidean")
# Option "ward.D2" implements Ward's (1963) clustering criterion 
# (Murtagh and Legendre 2014). With the latter, the dissimilarities 
# are squared before cluster updating.
fit <- hclust(d, method="ward.D2")
load_package("sparcl")
png(concat(IMAGES_DIR,'/dendrogram.png'), 
    width = 1024, height = 512)
ColorDendrogram(fit, 
                y = fit5$cluster, 
                main = "Hierarchical Clustering", 
                xlab = "Euclidean Distance",
                sub = "with Ward D2 Clustering",
                branchlength = 50)
# draw red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
dev.off()