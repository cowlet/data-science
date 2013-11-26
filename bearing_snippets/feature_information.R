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

info <- "State"

# Select all rows from all bearings, only the best feature cols plus State, and bind on bearing
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

