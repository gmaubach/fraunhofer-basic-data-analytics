# Account for empty strings
data[data == ""] <- NA

# Drop rows with NaN values
na.raw.inds <- which(apply(is.na(data), 1, any))
data <- data[-c(na.raw.inds),]

# Replace +/- values with 1/0
data[data$Class == "+", "Class"] <- 1.0 
data[data$Class == "-", "Class"] <- 0.0

# Replace t/f values with 1/0
data[data == "t"] <- 1.0
data[data == "f"] <- 0.0

# Transforming categorical to numerical data columns
# (Already) numeric columns
to.keep <- c("Class", "A2", "A3", "A8", "A9", "A10", "A11","A12","A14","A15")
# Categorical columns
to.split <- c("A1", "A4", "A5", "A6", "A7", "A13")

# Separate numeric from categorical data
numeric.data <- data[, to.keep]

# Introduce separate columns for categorical values
# Append the former categorical attributes as one-hot-encoded index attributes
ext.data <- data[,to.keep]
for (j in to.split) {
  fac <- as.factor(data[,j])
  for (categ in levels(fac)) {
    tmp.vec <- fac == categ
    tmp.vec <- as.numeric(tmp.vec)
    ext.data <- cbind(ext.data, tmp.vec)
    colnames(ext.data)[ncol(ext.data)] <- sprintf("%s(%s)", j, categ)
  }
}

data <- ext.data