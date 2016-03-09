################################################################################
# CSC 424 PROJECT
# Turkiye Student Evaluation Data Set
# http://archive.ics.uci.edu/ml/datasets/Turkiye+Student+Evaluation
# Abstract: This data set contains a total 5820 evaluation scores provided by 
# students from Gazi University in Ankara (Turkey). There is a total of 28 course 
# specific questions and additional 5 attributes.
# Source:
# Ernest Fokoue 
# Center for Quality and Applied Statistics 
# Rochester Institute of Technology 
# 98 Lomb Memorial Drive 
# Rochester, NY 14623, USA 
# eMaÄ±l: epfeqa '@' rit.edu 
# 
# Necla Gunduz 
# Department of Statistics 
# Faculty of Science, Gazi University 
# Teknikokullar,06500 Ankara, Turkey 
# eMail: ngunduz '@' gazi.edu.tr 
# gunduznecla '@' yahoo.com
#
# Citation:
# Gunduz, G. & Fokoue, E. (2013). UCI Machine Learning Repository [[Web Link]]. 
#   Irvine, CA: University of California, School of Information and 
#   Computer Science. 
#
# Attribute Information:
# instr: Instructor's identifier; values taken from {1,2,3} 
# class: Course code (descriptor); values taken from {1-13} 
# repeat: Number of times the student is taking this course; values taken from {0,1,2,3,...} 
# attendance: Code of the level of attendance; values from {0, 1, 2, 3, 4} 
# difficulty: Level of difficulty of the course as perceived by the student; values taken from {1,2,3,4,5} 
# Q1: The semester course content, teaching method and evaluation system were provided at the start. 
# Q2: The course aims and objectives were clearly stated at the beginning of the period. 
# Q3: The course was worth the amount of credit assigned to it. 
# Q4: The course was taught according to the syllabus announced on the first day of class. 
# Q5:	The class discussions, homework assignments, applications and studies were satisfactory. 
# Q6: The textbook and other courses resources were sufficient and up to date.	
# Q7: The course allowed field work, applications, laboratory, discussion and other studies. 
# Q8: The quizzes, assignments, projects and exams contributed to helping the learning.	
# Q9: I greatly enjoyed the class and was eager to actively participate during the lectures. 
# Q10: My initial expectations about the course were met at the end of the period or year. 
# Q11: The course was relevant and beneficial to my professional development. 
# Q12: The course helped me look at life and the world with a new perspective. 
# Q13: The Instructor's knowledge was relevant and up to date. 
# Q14: The Instructor came prepared for classes. 
# Q15: The Instructor taught in accordance with the announced lesson plan. 
# Q16: The Instructor was committed to the course and was understandable. 
# Q17: The Instructor arrived on time for classes. 
# Q18: The Instructor has a smooth and easy to follow delivery/speech. 
# Q19: The Instructor made effective use of class hours. 
# Q20: The Instructor explained the course and was eager to be helpful to students. 
# Q21: The Instructor demonstrated a positive approach to students. 
# Q22: The Instructor was open and respectful of the views of students about the course. 
# Q23: The Instructor encouraged participation in the course. 
# Q24: The Instructor gave relevant homework assignments/projects, and helped/guided students. 
# Q25: The Instructor responded to questions about the course inside and outside of the course. 
# Q26: The Instructor's evaluation system (midterm and final questions, projects, assignments, etc.) effectively measured the course objectives. 
# Q27: The Instructor provided solutions to exams and discussed them with students. 
# Q28: The Instructor treated all students in a right and objective manner. 
# 
# Q1-Q28 are all Likert-type, meaning that the values are taken from {1,2,3,4,5}
################################################################################

# setup
# clear the environment
rm(list = grep("^current_date", ls(), value = TRUE, invert = TRUE))

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
csv_file <- concat(DATA_DIR, '/turkiye-student-evaluation_R_Specific.csv')
if (file.exists(csv_file)) {
  data <- read.csv(csv_file)
} else {
  URL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00262/turkiye-student-evaluation_R_Specific.csv"
  current_date <- date()
  download.file(url=URL, 
                destfile=csv_file, 
                quiet=TRUE,
                mode = "wb")
  data <- read.csv(csv_file)
}


# how many are n/a?
sum(is.na(data))
head(which(is.na(data)))
# how many are NULL?
sum(is.null(data))
# how many are blank?
length(which(data == ""))
str(data)
length(which(!complete.cases(data)))

# need to scale the Likert-type data from Q1-Q28
# in order to use it for anything useful
x <- data[, 6:33]
data[, 6:33] <- scale(x, 
                      center=FALSE, 
                      scale=apply(x,2, sd, na.rm=TRUE))
rm(x)
# class variable is difficulty, on a scale of 1 to 5
data <- data.frame(difficulty=data[,5], data[,6:33])
data$difficulty <- as.factor(data$difficulty)
head(data)
str(data)

################################
# Principal Component Analysis #
################################
# ***CAN IT BE USED?***
pca <- prcomp(data[,2:29])
summary(pca)
# between four and five PC's make-up 90% of the cumulative variance

# variance charts
# from http://rstudio-pubs-static.s3.amazonaws.com/27823_dbc155ba66444eae9eb0a6bacb36824f.html
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  par(mfrow=c(2,2))
  plot(x.pvar,
       xlab="Principal Component", 
       ylab="Variance", 
       ylim=c(0,1), 
       type='b',
       main="Proportion of Variance Explained")
  plot(cumsum(x.pvar),
       xlab="Principal Component", 
       ylab="Variance", 
       ylim=c(0,1), type='b',
       main="Cumulative Proportion of Variance Explained")
  abline(h=0.9,col="red")
  cor_eigs <- eigen(cor(data[,2:29]))
  plot(cor_eigs$values, 
       xlab="Principal Component",
       ylab="Eigenvalues",  
       type="b",
       main="Eigenvalues")
  abline(h=1,col="red")
  par(mfrow=c(1,1))
}
png(concat(IMAGES_DIR,'/variance graphs.png'), 
    width = 1024, height = 1024)
pcaCharts(pca)
dev.off()
# scree plot of eigenvalues suggests two PC's should work
rm(pca_data)
pca_data <- data.frame(difficulty=as.factor(data[,1]), PC1=pca$x[,1], PC2=pca$x[,2])

################################
# Linear Discriminant Analysis #
################################
# test for homogeneity of covariance matrices
log(det(x=cov(as.matrix(pca_data[pca_data[,1]=="1",2:3]))))
log(det(x=cov(as.matrix(pca_data[pca_data[,1]=="2",2:3]))))
log(det(x=cov(as.matrix(pca_data[pca_data[,1]=="3",2:3]))))
log(det(x=cov(as.matrix(pca_data[pca_data[,1]=="4",2:3]))))
log(det(x=cov(as.matrix(pca_data[pca_data[,1]=="5",2:3]))))
source("BoxMTest.R")
BoxMTest(pca_data[2:3],pca_data$difficulty)

# compute d-dimesnional mean vectors
load_package("DiscriMiner")
t(groupMeans(variables=pca_data[,2:3], group=pca_data[,1]))
t(groupStds(variables=pca_data[,2:3], group=pca_data[,1]))
# test correlations
cor.test(data[,2],data[,3])

# compute d-dimensional scatter matrices
betweenCov(variables=data[,2:3], group=data[,1], div_by_n = FALSE)
withinCov(variables=data[,2:3], group=data[,1])
(Sb <- betweenSS(variables=data[,2:3], group=data[,1]))
(Sw <- withinSS(variables=data[,2:3], group=data[,1]))

# calculate eigenvalues and eigenvectors
(eigenvalues <- eigen(solve(Sw) %*% Sb)$values)

# Select eigenvalues
plot(eigenvalues, type="b")

# transform samples onto new subspace provided by LDA
load_package("MASS")
lda <- lda(formula=pca_data$difficulty ~ ., data=pca_data)
ld_coefs <- lda$scaling
write.table(round(ld_coefs,3), file=concat(OUTPUT_DIR,'/lda coefficients.csv'), sep=",")

# make predictions
plda <- predict(lda)

# how well do the histograms separate?
png(concat(IMAGES_DIR,'/LD1 Histogram.png'), 
    width = 1024, height = 1024)
ldahist(data=plda$x[,1], g=pca_data$difficulty)
dev.off()
png(concat(IMAGES_DIR,'/LD2 Histogram.png'), 
    width = 1024, height = 1024)
ldahist(data=plda$x[,2], g=pca_data$difficulty)
dev.off()

# how many of each class?
load_package('dplyr')
grouped_by_job <- group_by(pca_data[,2:3],pca_data[,1])
summarise(grouped_by_job, n=n())

# Confusion Matrix:
confusion_matrix <- table(plda$class, pca_data$difficulty)
write.table(confusion_matrix, 
            file=concat(OUTPUT_DIR,'/confusion matrix.csv'), sep=",")
# estimate the percentage of faculty rankings that will be mis-classified
round(1 - diag(prop.table(confusion_matrix)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix))), 4)
# cross-validation with leave-one-out
lda_cv <- lda(formula=pca_data$difficulty ~ ., data=pca_data, CV=TRUE)
# Confusion Matrix:
confusion_matrix_cv <- table(lda_cv$class, pca_data$difficulty)
write.table(confusion_matrix_cv, 
            file=concat(OUTPUT_DIR,'/confusion matrix leave one out.csv'), sep=",")
# estimate the percentage that will be mis-classified
round(1 - diag(prop.table(confusion_matrix_cv)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix_cv))), 4)

# Calculate Wilk's Lambda
# It also gives us the group centroids
load_package('rrcov')
Wilks.test(plda$x,pca_data$difficulty)

# plot the LDA projection
prop.lda = lda$svd^2/sum(lda$svd^2)
lda_data <- data.frame(y = pca_data$difficulty, lda = plda$x)
lda_data <- lda_data[,1:3] # drop unnecessary LD's
class <- lda_data$y
x <- plda$x[,1]
y <- plda$x[,2]
df <- data.frame(class, x, y)
centroids <- aggregate(cbind(x,y)~class,df,mean)
prop.lda = lda$svd^2/sum(lda$svd^2)
load_package("ggplot2")
load_package("scales")
png(concat(IMAGES_DIR,'/lda plot.png'), 
    width = 1024, height = 1024)
ggplot(df,aes(x,y,color=factor(class),shape=factor(class))) +
  geom_point(size=2.5) + 
  geom_point(data=centroids,size=5) +
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep="")) +
  ggtitle("LDA Projection Data") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off()

# territorial map
load_package("klaR")
partimat(class ~ y + x,
         data=pca_data[,2:3],
         method="lda",
         main="Data Partitioned by Class")

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
clvecd <- as.integer(data[,1])
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


####################################
# Support Vector Machines          #
####################################
## split data into a train and test set
index <- 1:nrow(data)
set.seed(12345789)
testindex <- sample(index, trunc(length(index)/3))
testset <- data[testindex,]
trainset <- data[-testindex,]

## svm
load_package("e1071")
svm.model <- svm(difficulty ~ ., data=trainset, cost=100, gamma=1)
svm.pred <- predict(svm.model, testset[,-1])
## rpart
load_package("rpart")
rpart.model <- rpart(difficulty ~ ., data=trainset)
rpart.pred <- predict(rpart.model, testset[,-1], type="class")
## compute svm confusion matrix
table(pred = svm.pred, true = testset[,1])
## compute rpart confusion matrix
table(pred = rpart.pred, true = testset[,1])
