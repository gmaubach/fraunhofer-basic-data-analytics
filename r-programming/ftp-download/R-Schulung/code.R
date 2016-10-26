#install.packages('Package.name' dependencies=T)
require(ggplot2)
require(GGally)
require(lattice)
require(caret)
require(cluster)
require(C50)
require(gmodels)





## Creating a data toy example

# Create a scaling funtion that returns values between 0 and 100(%)
scale.fun <- function(vals) {return(abs( vals / max(vals) * 100 ))}
# Assume three different quality levels
quality.levels <- factor(c(1,2,3), labels=c("low", "mid", "hi"))
# Create a vector with 100 entries for each level
qualities <- rep(quality.levels, 100)
table(qualities)

# Model relationship between price & customer satisfaction for different quality levels
dat <- data.frame(satisfaction=NA, price=NA, quality=qualities)

dat$price[dat$quality=="low"] <- 50:149
dat$price[dat$quality=="mid"] <- 150:249
dat$price[dat$quality=="hi"] <- 250:349

dat$satisfaction[dat$quality=="low"] <- scale.fun(0.1 * 1:100 + rnorm(100))
dat$satisfaction[dat$quality=="mid"] <- scale.fun(0.75 * 1:100 + rnorm(100))
dat$satisfaction[dat$quality=="hi"] <- scale.fun(runif(100))

## A basic plot example

# Create a histogram, suppress default label for x-axis
hist(dat$satisfaction, main="Distribution of people\'s satisfaction", xlab="")
# One can add things to the opened plot

## A simple lattice example

# Create a scatterplot for each quality level
xyplot(satisfaction ~ price | quality, data = dat)

## A simple ggplot2 example (1/2)

# Plot price vs. satisfaction in a colored scatterplot (Note: British spelling!)
ggplot(dat, aes(x=price, y=satisfaction, colour=quality)) + geom_point() + geom_density2d()

# Now a boxplot of the data (with a slight change of the command)
ggplot(dat, aes(x=price, y=satisfaction, colour=quality)) + geom_boxplot() 


# Overlaying two plots
ggplot(dat, aes(x=price, y=satisfaction, colour=quality)) + geom_point() + geom_density2d(alpha=0.3)

## Loading the data

# Read in the credit data from a CSV-file
raw.data <- read.csv("credit.csv", header=T, sep=",", stringsAsFactors=F)

# How many rows and columns? ("dimensions" of the data frame)
dim(raw.data)

## An initial inspection of the data

# The str-function provides a quick overview
str(raw.data)

## Treatment of missing values

dim(raw.data)
# Check for missing values  
any(is.na(raw.data))

# Account for empty strings
raw.data[raw.data == ""] <- NA

# Drop rows with NaN values
na.raw.inds <- which(apply(is.na(raw.data), 1, any))
length(na.raw.inds)
raw.data <- raw.data[-c(na.raw.inds),]
dim(raw.data)

## Inspecting a subset of the data

# Show a sample from the data
raw.data[c(1,5,8,353,601),]

## Replacements of symbolic true/false values
colnames(raw.data)

# We already know that the Class-column contains + and - signs
unique(raw.data$Class)
# However, what else?
head(raw.data, n=1)

# Replace +/- values with 1/0
raw.data[raw.data$Class == "+", "Class"] <- 1.0 
raw.data[raw.data$Class == "-", "Class"] <- 0.0

# Replace t/f values with 1/0
raw.data[raw.data == "t"] <- 1.0
raw.data[raw.data == "f"] <- 0.0

## Transforming categorical to numerical data columns

# (Already) numeric columns
to.keep <- c("Class", "A2", "A3", "A8", "A9", "A10", "A11","A12","A14","A15")
# Categorical columns
to.split <- c("A1", "A4", "A5", "A6", "A7", "A13")

# Separate numeric from categorical data
numeric.data <- raw.data[, to.keep]
dim(numeric.data)

## Introduce separate columns for categorical values

# Append the former categorical attributes as one-hot-encoded index attributes
ext.raw.data <- raw.data[,to.keep]
for (j in to.split) {
  fac <- as.factor(raw.data[,j])
  for (categ in levels(fac)) {
    tmp.vec <- fac == categ
    tmp.vec <- as.numeric(tmp.vec)
    ext.raw.data <- cbind(ext.raw.data, tmp.vec)
    colnames(ext.raw.data)[ncol(ext.raw.data)] <- sprintf("%s(%s)", j, categ)
  }
}
print(paste(dim(raw.data), dim(ext.raw.data), sep= " vs. "))

## Export data to csv for inspection in an external editor

write.table(raw.data, "binarized_credit_data.csv", col.names = T, row.names=F, quote=F, sep=",")

## Count non-zero entries

data <- ext.raw.data
# Check if all columns are numeric
all(sapply(data, is.numeric))

# Need to convert each column of the matrix
data <- as.data.frame(apply(data, 2, as.numeric))
all(sapply(data, is.numeric))

table(data[,"Class"])
# Count the non-zero entries of each attribute for each class
agg <- aggregate(subset(data, select = -Class), by=list(Class = data[,"Class"]),
                 function(x) {length(x[x >0])})
agg[,c(1:10)]

## Printing some discriminative attributes

# Mark interesting attributes with high presence and strong discriminativity
agg <- subset(agg, select=-Class)
ratios <- sapply(agg, function(x) {x[2] / x[1]})
nonzeros <- colSums(agg)
inds.decreasing <- order(nonzeros, decreasing=T)

# Prepare a print-out ...
print(sprintf("% -8s % -8s (% -s)", "Attribute", "Count", "Ratio"))
print("--------------------------")

# Do the print-out ...
for (i in inds.decreasing[1:15]) { # print out top 15 elements
  count <- nonzeros[i]
  ratio <- ratios[i]
  if (count > 10) {
    if (ratio > 2 || ratio < 0.5) {
      print(sprintf("% -8s % -8s  (%.2f)  <-- discriminates the classes well)", 
                    names(ratios)[i], count, ratio))
    } else {
      print(sprintf("% -8s % -8s  (%.2f)", names(ratios)[i], count, ratio))
    }
  }
}

## Selecting discriminative attributes by Pearsons's correlation

# Compute Pearson's correlation between attributes and the class labels
test <- cor(subset(data, select=-Class), as.numeric(data[,"Class"]))
sort(test, decreasing = T)[1:10]
top5 <- rownames(test)[order(test, decreasing = T)][1:5]
top5

## Visualizing with a scatterplot matrix (1/5)

selection <- which(apply(data, 2, function(x) {!(all(is.element(x, c(0,1))))}))

# Builtin-function 'pairs'
pairs(data[,selection], main = "Scatterplots for credit attributes", 
      pch = 21, bg = c("red", "green3")[data$Class + 1])

# To beautify the labeling of the plots, we should convert the class into a factor
data[,"Class"] <- factor(data[,"Class"], levels = c("0","1"), labels=c("denial", "approval"))

# define a function to render the diagonal of the scatter plot matrix in a nicer way
multi_colored_kde <- function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(mapping = aes_string(color="Class"), fill=NA)
}

# ggpairs-function from package GGally 
ggpairs(data, columns=selection, title= "Scatterplot matrix", 
        mapping=ggplot2::aes_string(color="Class"),
        upper="blank",
        diag  = list(continuous=multi_colored_kde),
        lower = list(continuous=wrap("points", alpha=0.2)),
        axisLabels= "none")

## Pre-filtering of attributes (needed for some methods)

# Separate the class from the rest of the data
classes <- data[,"Class"]
data <- subset(data, select=-Class)

# Remove columns with little variation
data <- data[,-c(nearZeroVar(data))]

# Center and scale columns (necessary for some ML-methods)
data.orig <- data
data <- scale(data)

## Compute a PCA 
pca <- prcomp(data)
pca4plot <- as.data.frame(pca$x)
pca4plot[,"Class"] <- classes
ggplot(pca4plot, aes(x=PC1, y=PC2, colour=Class)) + geom_point()

# Scatterplot matrix of a PCA
ggpairs(pca4plot, columns= c(1,2,3,4), color="Class", title= "Scatterplot matrix", 
        mapping=ggplot2::aes_string(color="Class"),
        upper="blank",
        diag  = list(continuous=multi_colored_kde),
        lower = list(continuous=wrap("points", alpha=0.2)),
        axisLabels= "none")

# Computing kmeans-clusters

# Since k-means starts with a randomized initialization, we fix a random seed
# for reproducibility of the experiments
set.seed(10) 

# Assuming 2 cluster centers
clustIDs <- kmeans(data, 2)$cluster
pca4plot[,"Cluster"] <- as.factor(clustIDs)
ggplot(pca4plot, aes(x=PC1, y=PC2, colour=Class, shape=Cluster)) + geom_point(size=4, alpha=0.75)

# Assuming 3 cluster centers
clustIDs <- kmeans(data, 3)$cluster
pca4plot[,"Cluster"] <- as.factor(clustIDs)
ggplot(pca4plot, aes(x=PC1, y=PC2, colour=Class, shape=Cluster)) + geom_point(size=4, alpha=0.75)

## Use another form of visualizing
clusplot(data, clustIDs, color=TRUE, shade=TRUE, labels=0, lines=0, 
         main = "Clusters in PCA space", xlab="PC1", ylab="PC2")

## Learning a decision tree model
set.seed(51)
train.sample <- sample(nrow(data), floor(0.75 * 653))
str(train.sample)

d.train <- data.orig[train.sample,]
d.test <- data.orig[-train.sample,]

cl.train <- classes[train.sample]
cl.test <- classes[-train.sample]

prop.table(table(classes))
prop.table(table(cl.train))
prop.table(table(cl.test))

credit.model <- C5.0(d.train, cl.train) 

summary(credit.model)

plot(credit.model)

cred.preds <- predict(credit.model, d.test)

CrossTable(cl.test, cred.preds, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r =F, dnn = c("actual approval","predicted approval"))

## Visualizing the decision tree
plot(credit.model)


## Using a support vector machine

library(kernlab)

dat.train.df <- as.data.frame(d.train)
dat.train.df$Class <- cl.train

svm.classifier <- ksvm(Class ~ ., data=dat.train.df, kernel = "vanilladot")
svm.classifier

svm.preds <- predict(svm.classifier, d.test)
head(svm.preds)

table(svm.preds, cl.test)

svm.preds <- predict(svm.classifier, d.test)
CrossTable(cl.test, svm.preds, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r =F, dnn = c("actual approval","predicted approval"))


# working with code-snippets
source('read_in_credit_data.R')
source('preprocess_credit_data.R')
source('scatter_plot_matrix.R')

