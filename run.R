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
# nb.repeat: Number of times the student is taking this course; values taken from 
#         {0,1,2,3,...} 
# attendance: Code of the level of attendance; values from {0, 1, 2, 3, 4} 
# difficulty: Level of difficulty of the course as perceived by the student; 
#             values taken from {1,2,3,4,5} 
# Q1: The semester course content, teaching method and evaluation system were 
#     provided at the start. 
# Q2: The course aims and objectives were clearly stated at the beginning of 
#     the period. 
# Q3: The course was worth the amount of credit assigned to it. 
# Q4: The course was taught according to the syllabus announced on the first 
#     day of class. 
# Q5:	The class discussions, homework assignments, applications and studies 
#     were satisfactory. 
# Q6: The textbook and other courses resources were sufficient and up to date.	
# Q7: The course allowed field work, applications, laboratory, discussion and 
#     other studies. 
# Q8: The quizzes, assignments, projects and exams contributed to helping the 
#     learning.	
# Q9: I greatly enjoyed the class and was eager to actively participate during 
#     the lectures. 
# Q10: My initial expectations about the course were met at the end of the 
#      period or year. 
# Q11: The course was relevant and beneficial to my professional development. 
# Q12: The course helped me look at life and the world with a new perspective. 
# Q13: The Instructor's knowledge was relevant and up to date. 
# Q14: The Instructor came prepared for classes. 
# Q15: The Instructor taught in accordance with the announced lesson plan. 
# Q16: The Instructor was committed to the course and was understandable. 
# Q17: The Instructor arrived on time for classes. 
# Q18: The Instructor has a smooth and easy to follow delivery/speech. 
# Q19: The Instructor made effective use of class hours. 
# Q20: The Instructor explained the course and was eager to be helpful to 
#      students. 
# Q21: The Instructor demonstrated a positive approach to students. 
# Q22: The Instructor was open and respectful of the views of students about 
#      the course. 
# Q23: The Instructor encouraged participation in the course. 
# Q24: The Instructor gave relevant homework assignments/projects, and 
#      helped/guided students. 
# Q25: The Instructor responded to questions about the course inside and 
#      outside of the course. 
# Q26: The Instructor's evaluation system (midterm and final questions, 
#      projects, assignments, etc.) effectively measured the course objectives. 
# Q27: The Instructor provided solutions to exams and discussed them with 
#      students. 
# Q28: The Instructor treated all students in a right and objective manner. 
# 
# Q1-Q28 are all Likert-type, meaning that the values are taken from {1,2,3,4,5}
# Therefore, I will convert the ordinal scale to a dichotomous variable and use 
# logistic regression to assess the impact of other variables on an ordinal 
# scale variable, difficulty.
################################################################################

# setup
# clear the environment
rm(list = grep("^current_date", ls(), value = TRUE, invert = TRUE))

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

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

## load required packages
load_package('dplyr')
load_package('clusterSim')
load_package('corrplot')
load_package('MASS')
load_package('Hmisc')
load_package("cluster")
load_package("fpc")
load_package("sparcl")
load_package('GGally')
load_package('CCA')

## get the data
# check to see if data is already downloaded
# if not, download it and save it to the data directory
csv_file <- concat(DATA_DIR, '/turkiye-student-evaluation_R_Specific.csv')
if (file.exists(csv_file)) {
  D <- read.csv(csv_file)
} else {
  URL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00262/turkiye-student-evaluation_R_Specific.csv"
  current_date <- date()
  download.file(url=URL, 
                destfile=csv_file, 
                quiet=TRUE,
                mode = "wb")
  D <- read.csv(csv_file)
}
rm(csv_file)

# how many are n/a?
sum(is.na(D))
head(which(is.na(D)))
sum(is.null(D))
length(which(D == ""))
length(which(!complete.cases(D)))
str(D)

# group data by instructor
p <- length(D[,1])
grouped <- group_by(D, as.factor(D$instr))
grouped_summary <- data.frame(summarise(grouped, n=n()))
grouped_summary$pct <- grouped_summary$n / p
print(grouped_summary)
rm(p)
rm(grouped_summary)
png(concat(IMAGES_DIR,'/hist - instr.png'), 
    width = 1024, height = 1024)
hist(as.numeric(D$instr), breaks=10, col="red", xlab="ID", main="Instructor Histogram") 
dev.off()

# group data by difficulty
p <- length(D[,1])
grouped <- group_by(D, as.factor(D$difficulty))
grouped_summary <- data.frame(summarise(grouped, n=n()))
grouped_summary$pct <- grouped_summary$n / p
print(grouped_summary)
rm(p)
rm(grouped_summary)
png(concat(IMAGES_DIR,'/hist - difficulty.png'), 
    width = 1024, height = 1024)
hist(D$difficulty, breaks=10, col="red", xlab="Score", main="Difficulty Histogram") 
dev.off()

# difficulty, cross-tabbed with instructor
ft <- function(var) {
  ftable(xtabs(~D$instr + var, data=D))
}
lapply(D[,2:5], ft)
rm(ft)

# group data by class
p <- length(D[,1])
grouped <- group_by(D, as.factor(D$class))
grouped_summary <- data.frame(summarise(grouped, n=n()))
grouped_summary$pct <- grouped_summary$n / p
print(grouped_summary)
rm(p)
rm(grouped)
rm(grouped_summary)
png(concat(IMAGES_DIR,'/hist - class.png'), 
    width = 1024, height = 1024)
hist(as.numeric(D$class), breaks=10, col="red", xlab="ID", main="Class Histogram") 
dev.off()

# group data by attendance
p <- length(D[,1])
grouped <- group_by(D, as.factor(D$attendance))
grouped_summary <- data.frame(summarise(grouped, n=n()))
grouped_summary$pct <- grouped_summary$n / p
print(grouped_summary)
rm(p)
rm(grouped)
rm(grouped_summary)
png(concat(IMAGES_DIR,'/hist - attendance.png'), 
    width = 1024, height = 1024)
hist(as.numeric(D$attendance), breaks=10, col="red", xlab="ID", main="Attendance Histogram") 
dev.off()
hist(as.numeric(D$attendance), breaks=10, col="red", xlab="ID", main="Attendance Histogram")

# difficulty, cross-tabbed with attendance
ft <- function(var) {
  ftable(xtabs(~D$attendance + var, data=D))
}
lapply(D[,2:5], ft)
rm(ft)

# difficulty, cross-tabbed with class
ft <- function(var) {
  ftable(xtabs(~D$class + var, data=D))
}
lapply(D[,2:5], ft)
rm(ft)
write.csv(ftable(xtabs(~D$class + D$difficulty, data=D)), file=concat(OUTPUT_DIR, '/difficulty by class.csv'))

# instructor, cross-tabbed with class
write.csv(ftable(xtabs(~D$class + D$instr, data=D)), file=concat(OUTPUT_DIR, '/instructor by class.csv'))

# each question, cross-tabbed with difficulty
ft <- function(var) {
  ftable(xtabs(~D$difficulty + var, data=D))
}
lapply(D[,6:33], ft)
rm(ft)

## Use clusterSim package to normalize the question data
# store the data in D_norm
source('ConvertAndBackup.R')
str(D_norm)

## before performing PCA, run step-wise selection based on AIC
full_glm_data <- D_norm[,-c(1,2,3,5)]
load(concat(OUTPUT_DIR, '/difficulty_norm.rda'))
load(concat(OUTPUT_DIR, '/attendance_norm.rda'))
difficulty_norm <- Q1_norm
full_glm_data$attendance_norm <- attendance_norm
rm(Q1_norm)
full_glm <- glm(difficulty_norm ~ ., data=full_glm_data)
summary(full_glm)
base_glm <- glm(difficulty_norm ~ 1)
summary(base_glm)
step <- step(object=full_glm, 
             scope=list(lower=formula(base_glm),
                        upper=formula(full_glm)),
             direction="both",trace=0)
step$formula
step$anova

## build a reduced polyr model based on the selected variables
#############################################################
# Ordered Logistic Regression using reduced model variables #
#############################################################
load(concat(OUTPUT_DIR, '/attendance_norm.rda'))
m_data <- data.frame(D_norm$D.difficulty,
                     attendance_norm,
                     D_norm$Q1_norm,
                     D_norm$Q3_norm,
                     D_norm$Q5_norm,
                     D_norm$Q8_norm,
                     D_norm$Q9_norm,
                     D_norm$Q11_norm,
                     D_norm$Q16_norm,
                     D_norm$Q17_norm,
                     D_norm$Q18_norm,
                     D_norm$Q22_norm,
                     D_norm$Q24_norm,
                     D_norm$Q26_norm)
m <- polr(as.factor(m_data$D_norm.D.difficulty) ~ ., 
          method='logistic',
          data=m_data, 
          Hess=TRUE)
summary(m)
## chi-square test for goodness of fit suggests its not a good fit
1-pchisq(deviance(m),df.residual(m))
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail=FALSE) * 2
(ctable <- cbind(ctable, "p vlaue" = p))
(ci <- exp(MASS:::confint.polr(m)))
# none of the varaibles' CI crosses 0, so the parameter estimate is  
# statistically significant
## odds ratios
exp(coef(m))
## OR and CI
exp(cbind(OR = coef(m), ci))
rm(ci)
rm(ctable)

## make predictions
predicted <- predict(m)
table(predicted)

## Confusion Matrix:
(confusion_matrix <- table(predicted, D[,5]))
# estimate the percentage of difficulties that will be classified correctly
round(diag(prop.table(confusion_matrix)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix))), 4)

# graphical interpretation
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)))
}
(s <- with(m_data, summary(as.numeric(D_norm.D.difficulty) ~ 
                             attendance_norm + 
                             D_norm$Q1_norm + 
                             D_norm$Q3_norm + 
                             D_norm$Q5_norm + 
                             D_norm$Q8_norm + 
                             D_norm$Q9_norm + 
                             D_norm$Q11_norm + 
                             D_norm$Q16_norm + 
                             D_norm$Q17_norm + 
                             D_norm$Q18_norm + 
                             D_norm$Q22_norm + 
                             D_norm$Q24_norm + 
                             D_norm$Q26_norm, fun=sf)))

# compare the GLM
glm(I(as.numeric(m_data$D_norm.D.difficulty) >= 2) ~ attendance_norm, family = 'binomial', data=m_data)
glm(I(as.numeric(m_data$D_norm.D.difficulty) >= 3) ~ attendance_norm, family = 'binomial', data=m_data)
glm(I(as.numeric(m_data$D_norm.D.difficulty) >= 4) ~ attendance_norm, family = 'binomial', data=m_data)
glm(I(as.numeric(m_data$D_norm.D.difficulty) >= 5) ~ attendance_norm, family = 'binomial', data=m_data)

s[,6] <- s[,6] - s[,3]
s[,5] <- s[,3] - s[,3]
s

png(concat(IMAGES_DIR,'/distances.png'), 
    width = 1024, height = 1024)
plot(s, which=1:5, pch=1:5, xlab='logit', main = ' ', xlim=range(s[,5:6]))
dev.off()

################################
# Principal Component Analysis #
################################
## are the variables correlated?
# look at correlations
write.csv(cor(m_data[,-1]), file=concat(OUTPUT_DIR, '/correlations.csv'))
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                           "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
                           "cyan", "#007FFF", "blue", "#00007F"))
wb <- c("white", "black")
png(concat(IMAGES_DIR,'/correlations.png'), 
    width = 1024, height = 1024)
corrplot(cor(m_data[,-1]), order="hclust", addrect = 3, col=col4(10))
dev.off()

pca <- prcomp(m_data[,-c(1,2)])
summary(pca)
# the PC's are dominated by the attendance variable
# 2 PC's make up 85% of the variance

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
  cor_eigs <- eigen(cor(D_norm[,6:33]))
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
pca_data <- data.frame(difficulty=as.factor(D_norm$D.difficulty), PC1=pca$x[,1], PC2=pca$x[,2])

##########################################
# Ordered Logistic Regression using PC's #
##########################################
m <- polr(pca_data$difficulty ~ ., data=pca_data, Hess=TRUE)
summary(m)

## chi-square test for goodness of fit suggests its not a good fit
1-pchisq(deviance(m),df.residual(m))

(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail=FALSE) * 2
(ctable <- cbind(ctable, "p vlaue" = p))
(ci <- confint(m))
# neither of the PC's CI crosses 0, so the parameter estimate is statistically 
# significant
confint.default(m)
## odds ratios
exp(coef(m))
## OR and CI
exp(cbind(OR = coef(m), ci))

# make predictions
predicted <- predict(m)

# graphical interpretation
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)))
}
(s <- with(pca_data, summary(as.numeric(difficulty) ~ PC1 + PC2, fun=sf)))

# compare the GLM
glm(I(as.numeric(difficulty) >= 2) ~ PC1, family = 'binomial', data=pca_data)
glm(I(as.numeric(difficulty) >= 3) ~ PC1, family = 'binomial', data=pca_data)
glm(I(as.numeric(difficulty) >= 4) ~ PC1, family = 'binomial', data=pca_data)
glm(I(as.numeric(difficulty) >= 5) ~ PC1, family = 'binomial', data=pca_data)

s[,6] <- s[,6] - s[,3]
s[,5] <- s[,3] - s[,3]
s
plot(s, which=1:5, pch=1:5, xlab='logit', main = ' ', xlim=range(s[,5:6]))

###############################
# K-Means Clustering Analysis #
###############################
# Determine number of clusters based on weighted sum of squares
wss <- (nrow(D_norm)-1)*sum(apply(D_norm[,6:33],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(D_norm[,6:33], centers=i)$withinss)
png(concat(IMAGES_DIR,'/wss to pick number of clusters.png'), 
    width = 1024, height = 1024)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
dev.off()
# K-Means Cluster Analysis for k=5
fit5 <- kmeans(D_norm[,6:33], 5) # 5 cluster solution

# cluster plot with ellipses
png(concat(IMAGES_DIR,'/cluster plot1.png'), 
    width = 1024, height = 1024)
clusplot(D_norm[,6:33], 
         fit5$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot with k=5")
dev.off()

# Centroid Plot against 1st 2 discriminant functions
png(concat(IMAGES_DIR,'/cluster plot2.png'), 
    width = 1024, height = 1024)
plotcluster(D_norm[,6:33], 
            method="dc",
            fit5$cluster,
            main="Cluster Plot with k=5")
dev.off()

# Colors represent clusters and numbers represent y's
clvecd <- as.integer(D_norm$D.difficulty)
png(concat(IMAGES_DIR,'/cluster plot3.png'), 
    width = 1024, height = 1024)
plotcluster(x=D_norm[,6:33], 
            clvecd=fit5$cluster,
            method="dc",
            clnum=clvecd,
            main="Cluster Plot with k=5")
dev.off()
# Colors represent y's and numbers represent clusters
png(concat(IMAGES_DIR,'/cluster plot4.png'), 
    width = 1024, height = 1024)
plotcluster(x=D_norm[,6:33], 
            clvecd=clvecd,
            method="dc",
            clnum=fit5$cluster,
            main="Cluster Plot with k=5")
dev.off()

####################################
# Hierarchical Clustering Analysis #
####################################
d <- dist(D_norm[,6:33], method="euclidean")
# Option "ward.D2" implements Ward's (1963) clustering criterion 
# (Murtagh and Legendre 2014). With the latter, the dissimilarities 
# are squared before cluster updating.
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=5)

png(concat(IMAGES_DIR,'/dendrogram - difficulty.png'), 
    width = 1024, height = 512)
ColorDendrogram(fit, 
                y = D$difficulty, 
                main = "Hierarchical Clustering (Colors are Difficulty Score)", 
                xlab = "Euclidean Distance",
                sub = "with Ward D2 Clustering",
                branchlength = 50)
# draw red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
dev.off()

png(concat(IMAGES_DIR,'/dendrogram - inst.png'), 
    width = 1024, height = 512)
ColorDendrogram(fit, 
                y = D$instr, 
                main = "Hierarchical Clustering (Colors are Instructor)", 
                xlab = "Euclidean Distance",
                sub = "with Ward D2 Clustering",
                branchlength = 50)
# draw red borders around the 3 clusters 
rect.hclust(fit, k=3, border="red")
dev.off()

png(concat(IMAGES_DIR,'/dendrogram - attendance.png'), 
    width = 1024, height = 512)
ColorDendrogram(fit, 
                y = D$attendance, 
                main = "Hierarchical Clustering (Colors are Attendance)", 
                xlab = "Euclidean Distance",
                sub = "with Ward D2 Clustering",
                branchlength = 50)
# draw red borders around the 3 clusters 
rect.hclust(fit, k=5, border="red")
dev.off()

png(concat(IMAGES_DIR,'/dendrogram - Q14.png'), 
    width = 1024, height = 512)
ColorDendrogram(fit, 
                y = D$Q14, 
                main = "Hierarchical Clustering (Colors are Q14)", 
                xlab = "Euclidean Distance",
                sub = "with Ward D2 Clustering",
                branchlength = 50)
# draw red borders around the 3 clusters 
rect.hclust(fit, k=5, border="red")
dev.off()

#######
# PAM #
#######
p <- pam(x=D_norm[,6:33], diss=FALSE, k=5)
summary(p)
plot(p)

##########
# DBSCAN #
##########
dbs <- dbscan(D_norm[,6:33], eps=0.5)

###############
# Model-Based #
###############
load_package('mclust')
mc <- Mclust(D_norm[,6:33])
mc2 <- MclustDA(D_norm[,6:33], D_norm$D.difficulty)


par(mfrow=c(2,2))
plot(pca_data$PC1, pca_data$PC2, col=groups, xlab="PC1", ylab="PC2", main="Hierarchical Clustering")
plot(pca_data$PC1, pca_data$PC2, col=fit5$cluster, xlab="PC1", ylab="PC2", main="K-Means Clustering")
plot(pca_data$PC1, pca_data$PC2, col=p$clustering, xlab="PC1", ylab="PC2", main="PAM Clustering")
plot(pca_data$PC1, pca_data$PC2, col=mc$classification, xlab="PC1", ylab="PC2", main="Model-Based Clustering")
par(mfrow=c(1,1))


#######
# CCA #
#######
D_norm$attendance_norm <- D$attendance
questions <- D_norm[,6:33]
behavior <- D_norm[,3:5]
str(behavior)

corrs <- matcor(behavior, questions)
img.matcor(corrs, type=3)
cc1 <- cc(behavior, questions)
# Cannonical Correlations
cc1$cor
# Raw Canonical Coefficients
write.csv(cc1$xcoef, concat(OUTPUT_DIR, '/cc1$xcoef.csv'))
write.csv(cc1$ycoef, concat(OUTPUT_DIR, '/cc1$ycoef.csv'))
# plot the canonical correlations of the first 3 dimensions
plot(cc1$cor, xlab = "Dimension", 
     ylab = "Canonical Correlations",
     ylim = c(0,1),
     type="b")
# plot the values and units on the first 2 canonical variates
plt.cc(cc1, var.label=TRUE)
cc1$scores
cc2 <- comput(behavior, questions, cc1)
# Canonical Loadings
cc2$corr.X.xscores
cc2$corr.Y.xscores
cc2$corr.X.yscores
cc2$corr.Y.yscores
# Test the null hypothesis that the canonical correlations are all equal to zero,
# that the second and third canonical correlations are equal to zero, and that 
# the third canonical correlation is equal to zero. 
ev <- (1 - cc1$cor^2)
n <- dim(behavior)[1]
p <- length(behavior)
q <- length(questions)
k <- min(p,q)
m <- n - 3/2 - (p + q) / 2
w <- rev(cumprod(rev(ev)))
# initialize
d1 <- d2 <- f <- vector("numeric", k)
for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2- 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}
pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))
# calculate the standardized canonical coefficients diagonal matrix of sd's
(s1 <- diag(sqrt(diag(cov(behavior)))))
(s2 <- diag(sqrt(diag(cov(questions)))))
write.csv(s1 %*% cc1$xcoef, file=concat(OUTPUT_DIR, '/std canonical coefficients - behavior.csv'))
write.csv(s2 %*% cc1$ycoef, file=concat(OUTPUT_DIR, '/std canonical coefficients - questions.csv'))


