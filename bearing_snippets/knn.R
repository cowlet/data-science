library("kknn")

# Calculate accuracy weighted by counts per class
weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

# Read in the relabelled best features
basedir <- "/Users/vic/Projects/bearings/bearing_IMS/1st_test/"
data <- read.table(file=paste0(basedir, "../all_bearings_relabelled.csv"), sep=",", header=TRUE)

# Read in training set row numbers
train <- read.table(file=paste0(basedir, "../train.rows.csv"), sep=",")[,1]

kernels <- c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal")

results <- matrix(ncol=5, nrow=0)
models <- list()

for (k in c(1, 3, 5, 10, 15, 20, 35, 50))
{
  for (kern in 1:length(kernels))
  {
    cat("Run for k", k, "and kernel", kernels[kern], "\n")  

    model <- kknn(State ~ ., train=data[train,-1], test=data[-train,-1], kernel=kernels[kern], k=k)
    wacc <- weighted.acc(model$fitted.values, data[-train,2])
    pacc <- sum(model$fitted.values==data[-train,2])/length(model$fitted.values)

    results <- rbind(results, c(k, kern, wacc, pacc, 1))
    models[[(length(models)+1)]] <- model
  }
}

# Visualise accuracies
pts <- do.call(rbind, Map(function(kern)
  {
    if (kern==1) "r" # rectangular
    else if (kern == 2) "t" # triangular
    else if (kern == 3) "e" 
    else if (kern == 4) "b"
    else if (kern == 5) "w"
    else if (kern == 6) "c"
    else if (kern == 7) "i"
    else if (kern == 8) "g"
    else if (kern == 9) "a"
    else if (kern == 10) "o"
    else "x"
  }, results[,2]))


plot(results[,3] ~ results[,1], col="blue", pch=pts, ylim=c(0.4,0.71), ylab="Accuracy", xlab="k values", main="k Nearest Neighbour")
points(results[,4] ~ results[,1], col="red", pch=pts)
legend("bottomleft", kernels, pch=c("r", "t", "e", "b", "w", "c", "i", "g", "a", "o"))
legend("bottom", c("Unweighted accuracy", "Weighted accuracy"), col=c("red", "blue"), pch=15)

# Save everything
save(results, file=paste0(basedir, "../../models/knn.results.obj"))
save(models, file=paste0(basedir, "../../models/knn.models.obj"))

write.table(results, file=paste0(basedir, "../../models/knn.results.csv"), sep=",")

best.row <- match(max(results[,3]), results[,3])
best.knn <- models[[best.row]]
save(best.knn, file=paste0(basedir, "../../models/best.knn.obj"))

# Confusion matrix
table(data[-train,2], best.knn$fitted.values)

