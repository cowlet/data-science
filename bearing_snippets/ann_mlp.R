library("nnet")

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

# Read in training set
train <- read.table(file=paste0(basedir, "../train.rows.csv"), sep=",")

# Set up class weights to penalise the minority classes more
cw1 <- rep(1, 7) # all equal
cw2 <- c(10, 100, 100, 10, 1, 10, 1) # 1/order of count

freqs <- as.data.frame(table(data$State))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(data[,1])/as.integer(s[2])})) # 1/weight

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("early", "failure.b2", "failure.inner", "failure.roller", "normal", "stage2", "suspect")

# Also normalise the data for comparison
normed <- cbind(data[,1:2], as.data.frame(lapply(data[,-c(1,2)], function(col) { col / max(abs(col)) })))

results <- matrix(ncol=6, nrow=0)
models <- list()

# Run three iterations of each
for (i in 1:3)
{
  for (c in 1:length(class.weights[,1]))
  {
    data.weights <- do.call(rbind, Map(function(s)
      {
        class.weights[c,s]
      }, data$State))

    for (h in 2:30)
    {
      cat("Run", i, "for c", c, "and h", h, "\n")
      # With range
      ann <- nnet(State ~ ., data=data[train,-1], weights=data.weights[train], size=h, decay=5e-4, rang=(1/max(data[,-c(1,2)])), maxit=200)
      pred <- predict(ann, data[,-1], type="class")
      tacc <- weighted.acc(pred[train], data[train,2])
      wacc <- weighted.acc(pred[-train], data[-train,2])
      pacc <- sum(pred[-train]==data[-train,2])/length(pred[-train])

      results <- rbind(results, c(h, tacc, wacc, pacc, c, 1))
      models[[(length(models)+1)]] <- ann

      # With normalised data (no need for range now)
      ann <- nnet(State ~ ., data=normed[train,-1], weights=data.weights[train], size=h, decay=5e-4, maxit=200)
      pred <- predict(ann, normed[,-1], type="class")
      tacc <- weighted.acc(pred[train], normed[train,2])
      wacc <- weighted.acc(pred[-train], normed[-train,2])
      pacc <- sum(pred[-train]==normed[-train,2])/length(pred[-train])

      results <- rbind(results, c(h, tacc, wacc, pacc, c, 2))
      models[[(length(models)+1)]] <- ann

      # With neither range nor normalisation
      ann <- nnet(State ~ ., data=data[train,-1], weights=data.weights[train], size=h, decay=5e-4, maxit=200)
      pred <- predict(ann, data[,-1], type="class")
      tacc <- weighted.acc(pred[train], data[train,2])
      wacc <- weighted.acc(pred[-train], data[-train,2])
      pacc <- sum(pred[-train]==data[-train,2])/length(pred[-train])

      results <- rbind(results, c(h, tacc, wacc, pacc, c, 3))
      models[[(length(models)+1)]] <- ann

    }
  }
}

# Visualise results
cols <- do.call(rbind, Map(function(c)
  {
    if (c==1) "green" 
    else if (c == 2) "blue" 
    else if (c == 3) "red" 
    else "black"
  }, results[,5]))

pts <- do.call(rbind, Map(function(v)
  {
    if (v==1) "r" # range
    else if (v == 2) "n" # normalised input
    else if (v == 3) "p" # 
    else "x"
  }, results[,6]))


plot(results[,3] ~ results[,1], ylim=c(0,1), col=cols, pch=pts, xlab="Hidden neurons", ylab="Weighted accuracy")


# Save everything
save(results, file=paste0(basedir, "../ann.results.obj"))
save(models, file=paste0(basedir, "../ann.models.obj"))

write.table(results, file=paste0(basedir, "../ann.results.csv"), sep=",")

best.row <- match(max(results[,3]), results[,3])
best.ann <- models[[best.row]]
save(best.ann, file=paste0(basedir, "../best.ann.obj"))

