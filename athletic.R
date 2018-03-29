# Our objective is to figure out multivariate outliers in the given data set. 
# Besides, we will carry out a principle components analysis using both the
# covariance and correlation matrices, then choose the appropriate principal
# components, respectively. At the same time, we will conduct a MANOVA analysis 
# to compare the means across 6 different continents.

athletics <- read.csv(".../athletic record data.csv")
athletics[,5:9] <- 60 * athletics[,5:9]

head(athletics)
pairs(athletics[,2:9])

####################### 1. ourliers #################################
# method I
library(mvoutlier)
res <-chisq.plot(log(athletics[,2:9]))
res$outliers # these are the potential outliers

# method II
library(mvnormtest)
C <- t(athletics[2:9])
mshapiro.test(C)

library(chemometrics)
md.1 <- Moutlier(athletics[,2:9], quantile = 0.99, plot = T)
inde <- which(md.1$md > md.1$cutoff)
inde

####################### 2. principle component analysis ######################
####################### covariance matrix
covariace <- round(cov(athletics[,2:9]), 2)
covariace
pca_cov <- prcomp(athletics[,2:9]) # default: scale = FALSE
pca_cov
screeplot(pca_cov, type = "lines")
summary(pca_cov)
cumsum(pca_cov$sdev^2)/sum(pca_cov$sdev^2)
# The first component account for 99.5% of the variance.

# linear combination for the first principal component
lc_pc1 <- pca_cov$rotation[,1] 
lc_pc1

# compute the first principal component score for each country   
predict(pca_cov)[,1]

####################### correlation matrix
correlation <- round(cor(athletics[,2:9]), 2)
correlation
pca_corr <- prcomp(athletics[,2:9], scale = T) # default: scale = FALSE
pca_corr
screeplot(pca_corr, type = "lines")
summary(pca_corr)
cumsum(pca_corr$sdev^2)/sum(pca_corr$sdev^2)
# The first three components account for 95.3% of the variance.

pairs(pca_corr$x[,c(1:3)],main="Principal Component Score Plot by Correlation Matrix") 

# linear combination for the first three principal components
lc_pc2 <- pca_corr$rotation[,1:3] 
lc_pc2

# compute the first principal component score for each country   
pc_socre <- predict(pca_corr)[,1]
athletics_pc <- cbind(athletics, pc_socre)
rank <- athletics_pc[order(pc_socre, decreasing = T), c(1,11)]
rank


####################### 3. comparison of means between groups (MANOVA) ######################
library(car)
# specify the linear relationship in MANOVA
fit.lm <- lm(cbind(X100m, X200m, X400m, X800m, X1500m, X5000m, X10000m, Marathon)~Continent , data = athletics)
# Run the Manova
fit.manova <- Manova(fit.lm)
summary(fit.manova)
