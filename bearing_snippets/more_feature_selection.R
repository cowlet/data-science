basedir <- "../bearing_IMS/1st_test/"

# Run more_features.R first, so we have data frames b1, b2, b3, b4 in file
b1 <- read.table(file=paste0(basedir, "../b1_all.csv"), sep=",", header=TRUE)
b2 <- read.table(file=paste0(basedir, "../b2_all.csv"), sep=",", header=TRUE)
b3 <- read.table(file=paste0(basedir, "../b3_all.csv"), sep=",", header=TRUE)
b4 <- read.table(file=paste0(basedir, "../b4_all.csv"), sep=",", header=TRUE)

### Add extra columns for classes of bearing state, and time to failure (TTF)

# Bearing state classes (determined by eye)
b1.labels <- c(rep("early", times=150), rep("unknown", times=450), rep("normal", times=899), rep("suspect", times=600), rep("failure.b1", times=57))
b2.labels <- c(rep("early", times=499), rep("normal", times=1500), rep("suspect", times=120), rep("failure.b2", times=37))
b3.labels <- c(rep("early", times=499), rep("normal", times=1290), rep("suspect", times=330), rep("failure.inner", times=37))
b4.labels <- c(rep("early", times=199), rep("normal", times=800), rep("suspect", times=435), rep("failure.roller", times=405), rep("stage2", times=317))

b1 <- cbind(b1, State=b1.labels)
b2 <- cbind(b2, State=b2.labels)
b3 <- cbind(b3, State=b3.labels)
b4 <- cbind(b4, State=b4.labels)

# Time to failure (using same failure points as above)
# b1 fails on sample 150+450+899+600 + 1 = 2100
b1 <- cbind(b1, TTF=(as.POSIXct(b1[,1]) - as.POSIXct(b1[2100,1]))) # fails on row 2100
b2 <- cbind(b2, TTF=(as.POSIXct(b2[,1]) - as.POSIXct(b2[2120,1]))) # fails on row 2120
b3 <- cbind(b3, TTF=(as.POSIXct(b3[,1]) - as.POSIXct(b3[2120,1]))) # fails on row 2120
b4 <- cbind(b4, TTF=(as.POSIXct(b4[,1]) - as.POSIXct(b4[1435,1]))) # fails on row 1435


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



# Find high (absolute) correlations (ignoring non-numeric bearing name and TTF cols)
cor <- cov2cor(cov(norm[, -c(45, 46)]))
alikes <- apply(cor, 2, function(col) { names(Filter(function (val) { val > 0.9 }, sort.int(abs(col), decreasing=TRUE))) } )

cat(str(alikes))
# As an offline exercise, minimise the set of features by removing those with high correlations

# I found this to give the following minimal set
best <- c("Min.x", "Median.x", "Max.x", "Mean.x", "Skew.x", "Kurt.x", "FTF.x", "BPFI.x", "BPFO.x", "BSF.x", "F2.x", "F3.x", "F4.x", "F5.x", "Min.y", "Max.y", "Skew.y", "Kurt.y", "FTF.y", "BPFI.y", "BPFO.y", "BSF.y", "F2.y", "F3.y", "F4.y", "F5.y", "Qu.1.x", "VHF.pow.x", "Qu.1.y", "Median.y", "HF.pow.y")

info <- c("State", "TTF")

# Select all rows from all bearings, only the best feature cols plus State, TTF, and bind on bearing
data <- rbind(
  cbind(bearing="b1", (b1[,c(info, best)])),
  cbind(bearing="b2", (b2[,c(info, best)])),
  cbind(bearing="b3", (b3[,c(info, best)])),
  cbind(bearing="b4", (b4[,c(info, best)]))
)

write.table(data, file=paste0(basedir, "../all_bearings.csv"), sep=",", row.names=FALSE)


# Graph key features against each other to reveal patterns

# mark the state of the bearing with a point type
pts <- do.call(rbind, Map(function(s)
  {
  	ifelse (s=="early", "e", 
  	  ifelse(s == "normal", "n", 
  	    ifelse(s == "suspect", "s", 
  	      ifelse(s == "failure.b1", "1", 
  	        ifelse(s == "failure.b2", "2", 
  	          ifelse(s == "failure.inner", "i", 
  	            ifelse(s == "failure.roller", "r",
  	              ifelse(s == "stage2", "x", "u"))))))))
  }, data$State))
  
# mark the bearing number with a colour
colours <- do.call(rbind, Map(function(b) 
  { 
  	switch(b, b1 = "dodgerblue2", b2 = "darkorchid2", b3 = "coral2", b4 = "chartreuse")
  }, data$bearing))

# plot!
plot(data$Skew.x ~ data$Kurt.x, col=colours, pch=pts)
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(1, 1, 1, 1))

# Instead of data$Skew.x and data$Kurt.x, swap in other features of interest

