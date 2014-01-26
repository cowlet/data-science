library("rpart")

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

# Set up class weights to penalise the minority classes more
cw1 <- rep(1, 7) # all equal
cw2 <- c(10, 100, 100, 10, 1, 10, 1) # 1/order of count

freqs <- as.data.frame(table(data$State))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(data[,1])/as.integer(s[2])})) # 1/weight

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("early", "failure.b2", "failure.inner", "failure.roller", "normal", "stage2", "suspect")

results <- matrix(ncol=4, nrow=0)
models <- list()

for (c in 1:length(class.weights[,1]))
{
  data.weights <- do.call(rbind, Map(function(s)
    {
      class.weights[c,s]
    }, data$State))
  
  cat("Run for c", c, "\n")

  model <- rpart(State ~ ., data=data[train,-1], weights=data.weights[train], method="class")
  pred <- predict(model, data[,-1], type="class") 

  tacc <- weighted.acc(pred[train], data[train,2])
  wacc <- weighted.acc(pred[-train], data[-train,2])
  pacc <- sum(pred[-train]==data[-train,2])/length(pred[-train])

  results <- rbind(results, c(tacc, wacc, pacc, c))
  models[[(length(models)+1)]] <- model
}


# Visualise the best tree
best.row <- match(max(results[,2]), results[,2])
best.rpart <- models[[best.row]]

plot(best.rpart, compress=TRUE)
text(best.rpart)

# And the confusion matrix
pred <- predict(best.rpart, data[,-1], type="class")
table(data[-train,2], pred[-train])

# Save everything
save(results, file=paste0(basedir, "../../models/rpart.results.obj"))
save(models, file=paste0(basedir, "../../models/rpart.models.obj"))

write.table(results, file=paste0(basedir, "../../models/rpart.results.csv"), sep=",")

save(best.rpart, file=paste0(basedir, "../../models/best.rpart.obj"))

