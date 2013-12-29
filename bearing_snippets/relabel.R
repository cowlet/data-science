# Read in best features
basedir <- "/Users/vic/Projects/bearings/bearing_IMS/1st_test/"
data <- read.table(file=paste0(basedir, "../all_bearings_best_fv.csv"), sep=",", header=TRUE)

# Read in the best kmeans model. Straight after running kmeans.R the 
# filename will be "kmeans.obj", but this is the best model I found.
load(paste0(basedir, "../../models/best-kmeans-12-0.612.obj"))

## Adjust the class labels as a result of k-means
# Cluster 2 should be labelled "suspect"
data[best.kmeans$cluster==2,2] <- "suspect"

# Cluster 3 should be labelled "normal"
data[best.kmeans$cluster==3,2] <- "normal"

# Cluster 9 datapoints labelled "early" should be "normal"
data[((best.kmeans$cluster==9)&(data$State=="early")),2] <- "normal"

# b1 failure looks like a rolling element failure
data[data$State=="failure.b1", 2] <- "failure.roller"
data[((best.kmeans$cluster==11)&(data$State=="unknown")),2] <- "failure.roller"
data[((best.kmeans$cluster==11)&(data$State=="normal")),2] <- "suspect"
data[((best.kmeans$cluster==10)&(data$State=="unknown")),2] <- "early"

# Cluster 6 should all be "normal"
data[best.kmeans$cluster==6,2] <- "normal"

## Now plot to check the result
# use the same colours for states as before
cols <- do.call(rbind, Map(function(s)
  {
    if (s=="early") "green" 
    else if (s == "normal") "blue" 
    else if (s == "suspect") "darkgoldenrod" 
    else if (s == "stage2") "salmon"
    else if (s == "unknown") "black"
    else "red"
  }, data$State))

# plot each bearing changing state
par(mfrow=c(2,2))
for (i in 1:4)
{
  s <- (i-1)*2156 + 1 # 2156 datapoints per bearing
  e <- i*2156
  plot(best.kmeans$cluster[s:e], col=cols[s:e], ylim=c(1,k), main=paste0("Bearing ", i), ylab="Cluster")
}

# Now save
write.table(data, file=paste0(basedir, "../all_bearings_relabelled.csv"), sep=",", row.names=FALSE)

