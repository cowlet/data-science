# Read in the relabelled best features
basedir <- "/Users/vic/Projects/bearings/bearing_IMS/1st_test/"
data <- read.table(file=paste0(basedir, "../all_bearings_relabelled.csv"), sep=",", header=TRUE)

# Split into train and test sets, preserving percentage across states
train.pc <- 0.7
train <- vector()
for (state in unique(data$State))
{
  all.samples <- data[data$State==state,]
  len <- length(all.samples[,1])
  rownums <- sample(len, len*train.pc, replace=FALSE)
  train <- c(train, as.integer(row.names(all.samples)[rownums]))
}

# Write to file for future use
write.table(train, file=paste0(basedir, "../train.rows.csv"), sep=",")

# Compare the balance of classes
table(data$State)
table(data[train,"State"])

