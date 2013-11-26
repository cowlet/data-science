basedir <- "../bearing_IMS/1st_test/"

# Run more_features.R first, so we have data frames b1, b2, b3, b4 in file
b1 <- read.table(file=paste0(basedir, "../b1_all.csv"), sep=",", header=TRUE)
b2 <- read.table(file=paste0(basedir, "../b2_all.csv"), sep=",", header=TRUE)
b3 <- read.table(file=paste0(basedir, "../b3_all.csv"), sep=",", header=TRUE)
b4 <- read.table(file=paste0(basedir, "../b4_all.csv"), sep=",", header=TRUE)

### Add an extra column for classes of bearing state

# Bearing state classes (determined by eye)
b1.labels <- c(rep("early", times=150), rep("unknown", times=450), rep("normal", times=899), rep("suspect", times=600), rep("failure.b1", times=57))
b2.labels <- c(rep("early", times=499), rep("normal", times=1500), rep("suspect", times=120), rep("failure.b2", times=37))
b3.labels <- c(rep("early", times=499), rep("normal", times=1290), rep("suspect", times=330), rep("failure.inner", times=37))
b4.labels <- c(rep("early", times=199), rep("normal", times=800), rep("suspect", times=435), rep("failure.roller", times=405), rep("stage2", times=317))

b1 <- cbind(b1, State=b1.labels)
b2 <- cbind(b2, State=b2.labels)
b3 <- cbind(b3, State=b3.labels)
b4 <- cbind(b4, State=b4.labels)

# Now make one large "normal" dataset
# Ignore columns 1 (timestamp), 16 and 39 (F1.x and F1.y), and 48 (state), which are not useful for correlation
norm <- rbind(
  cbind((b1[(b1$State == "normal"),-c(1, 16, 39, 48)]), bearing="b1"),
  cbind((b2[(b2$State == "normal"),-c(1, 16, 39, 48)]), bearing="b2"),
  cbind((b3[(b3$State == "normal"),-c(1, 16, 39, 48)]), bearing="b3"),
  cbind((b4[(b4$State == "normal"),-c(1, 16, 39, 48)]), bearing="b4")
)

# Export
write.table(norm, file=paste0(basedir, "../normal_bearings.csv"), sep=",", row.names=FALSE)



# Find high (absolute) correlations (ignoring non-numeric bearing name col)
cor <- cov2cor(cov(norm[, -45]))
alikes <- apply(cor, 2, function(col) { names(Filter(function (val) { val > 0.9 }, sort.int(abs(col), decreasing=TRUE))) } )

cat(str(alikes))
# As an offline exercise, minimise the set of features by removing those with high correlations

