basedir <- "../bearing_IMS/1st_test/"

## As with feature_correlation, add the bearing status labels to previous features

b1 <- read.table(file=paste0(basedir, "../b1_all.csv"), sep=",", header=TRUE)
b2 <- read.table(file=paste0(basedir, "../b2_all.csv"), sep=",", header=TRUE)
b3 <- read.table(file=paste0(basedir, "../b3_all.csv"), sep=",", header=TRUE)
b4 <- read.table(file=paste0(basedir, "../b4_all.csv"), sep=",", header=TRUE)

# Bearing state classes (determined by eye)
b1.labels <- c(rep("early", times=150), rep("unknown", times=450), rep("normal", times=899), rep("suspect", times=600), rep("failure.b1", times=57))
b2.labels <- c(rep("early", times=499), rep("normal", times=1500), rep("suspect", times=120), rep("failure.b2", times=37))
b3.labels <- c(rep("early", times=499), rep("normal", times=1290), rep("suspect", times=330), rep("failure.inner", times=37))
b4.labels <- c(rep("early", times=199), rep("normal", times=800), rep("suspect", times=435), rep("failure.roller", times=405), rep("stage2", times=317))

b1 <- cbind(b1, State=b1.labels)
b2 <- cbind(b2, State=b2.labels)
b3 <- cbind(b3, State=b3.labels)
b4 <- cbind(b4, State=b4.labels)


## Now, work with only the minimal set of features

# I found offline analysis to give the following minimal set
best <- c("Min.x", "Median.x", "Max.x", "Mean.x", "Skew.x", "Kurt.x", "FTF.x", "BPFI.x", "BPFO.x", "BSF.x", "F2.x", "F3.x", "F4.x", "F5.x", "Min.y", "Max.y", "Skew.y", "Kurt.y", "FTF.y", "BPFI.y", "BPFO.y", "BSF.y", "F2.y", "F3.y", "F4.y", "F5.y", "Qu.1.x", "VHF.pow.x", "Qu.1.y", "Median.y", "HF.pow.y")

# Select all rows from all bearings, only the best feature cols plus State, and bind on bearing
data <- rbind(
  cbind(bearing="b1", (b1[,c("State", best)])),
  cbind(bearing="b2", (b2[,c("State", best)])),
  cbind(bearing="b3", (b3[,c("State", best)])),
  cbind(bearing="b4", (b4[,c("State", best)]))
)

write.table(data, file=paste0(basedir, "../all_bearings.csv"), sep=",", row.names=FALSE)


library("entropy")
# MI = H(x) + H(y) - H(x, y)
H.x <- entropy(table(data$State))
mi <- apply(data[, -c(1, 2)], 2, function(col) { H.x + entropy(table(col)) - entropy(table(data$State, col))})

# Now select the correct size of FV by testing different classifiers
library("rpart")
library("caret")

train.rows <- seq(1, length(data$State), by=4) # every fourth index
sorted <- names(sort(mi, decreasing=TRUE))
accuracies <- vector()

for (i in 2:length(sorted))
{
	form <- paste0("data$State ~ data$", Reduce(function (r, name) { paste(r, paste0("data$", name), sep=" + ") }, sorted[1:i]))
	model <- rpart(as.formula(form), data, subset=train.rows)
	pred <- predict(model, data, type="class") # contains train and test set predictions	
	cm <- confusionMatrix(pred[-train.rows], data[-train.rows, 2]) # only the non-training rows
	# pull out the answer
	accuracies <- c(accuracies, cm["overall"][[1]]["Accuracy"][[1]])
}

plot(accuracies, xlab="Number of features", ylab="Accuracy")

# 14 features looks best. save those plus bearing and state labels
write.table(data[c("bearing", "State", sorted[1:14])], file=paste0(basedir, "../all_bearings_best_fv.csv"), sep=",", row.names=FALSE)


