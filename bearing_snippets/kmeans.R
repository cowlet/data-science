calculate.confusion <- function(states, clusters)
{
  # generate a confusion matrix of cols C versus states S
  d <- data.frame(state = states, cluster = clusters)
  td <- as.data.frame(table(d))
  # convert from raw counts to percentage of each label
  pc <- matrix(ncol=max(clusters),nrow=0) # k cols
  for (i in 1:9) # 9 labels
  {
    total <- sum(td[td$state==td$state[i],3])
    pc <- rbind(pc, td[td$state==td$state[i],3]/total)
  }
  rownames(pc) <- td[1:9,1]
  return(pc)
}

assign.cluster.labels <- function(cm, k)
{
  # take the cluster label from the highest percentage in that column
  cluster.labels <- list()
  for (i in 1:k)
  {
    cluster.labels <- rbind(cluster.labels, row.names(cm)[match(max(cm[,i]), cm[,i])])
  }

  # this may still miss some labels, so make sure all labels are included
  for (l in rownames(cm)) 
  { 
    if (!(l %in% cluster.labels)) 
    { 
      cluster.number <- match(max(cm[l,]), cm[l,])
      cluster.labels[[cluster.number]] <- c(cluster.labels[[cluster.number]], l)
    } 
  }
  return(cluster.labels)
}

calculate.accuracy <- function(states, clabels)
{
  # For each means$cluster c, if cluster.labels[[c]] contains data$State, it's correct
  matching <- Map(function(state, labels) { state %in% labels }, states, clabels)
  tf <- unlist(matching, use.names=FALSE)
  return (sum(tf)/length(tf))
}



# Read in best features
basedir <- "/Users/vic/Projects/bearings/bearing_IMS/1st_test/"
data <- read.table(file=paste0(basedir, "../all_bearings_best_fv.csv"), sep=",", header=TRUE)

# Test multiple k values
results <- matrix(ncol=2, nrow=0)
models <- list()

for (k in 6:18)
{
  # Don't cluster columns for bearing or State  
  means <- kmeans(data[,-c(1, 2)], k)
  
  # generate a confusion matrix of cols C versus states S
  conf.mat <- calculate.confusion(data$State, means$cluster)
  cluster.labels <- assign.cluster.labels(conf.mat, k)

  # Now calculate accuracy, using states and groups of labels for each cluster
  accuracy <- calculate.accuracy(data$State, cluster.labels[means$cluster])
  results <- rbind(results, c(k, accuracy))
  models[[(length(models)+1)]] <- means
}


# Can run the loop above multiple times to get different random starts
# Then pick out the most accurate
best.row <- match(max(results[,2]), results[,2])
best.kmeans <- models[[best.row]]
save(best.kmeans, file=paste0(basedir, "../../models/kmeans.obj"))


# Now visualise
k <- length(best.kmeans$size)
conf.mat <- calculate.confusion(data$State, best.kmeans$cluster)
cluster.labels <- assign.cluster.labels(conf.mat, k)
acc <- calculate.accuracy(data$State, cluster.labels[best.kmeans$cluster])
cat("For", k, "means with accuracy", acc, ", labels are assigned as:\n")
cat(str(cluster.labels))

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

# plot the data with cluster centres superimposed
par(mfrow=c(2,3))
plot(Kurt.x ~ Skew.x, data=data, col=cols)
points(Kurt.x ~ Skew.x, data=best.kmeans$centers, pch=10)
plot(FTF.x ~ BPFI.x, data=data, col=cols)
points(FTF.x ~ BPFI.x, data=best.kmeans$centers, pch=10)
plot(BPFO.x ~ BSF.x, data=data, col=cols)
points(BPFO.x~ BSF.x, data=best.kmeans$centers, pch=10)
plot(FTF.y ~ BPFI.y, data=data, col=cols)
points(FTF.y ~ BPFI.y, data=best.kmeans$centers, pch=10)
plot(BPFO.y ~ BSF.y, data=data, col=cols)
points(BPFO.y ~ BSF.y, data=best.kmeans$centers, pch=10)
plot(VHF.pow.x ~ HF.pow.y, data=data, col=cols)
points(VHF.pow.x ~ HF.pow.y, data=best.kmeans$centers, pch=10)

