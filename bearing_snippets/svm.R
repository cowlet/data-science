library("e1071")

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

cw4 <- c(10, 1, 1, 10, 100, 10, 100) # order of count

class.weights <- rbind(cw1, cw2, cw3[,2], cw4)
colnames(class.weights) <- c("early", "failure.b2", "failure.inner", "failure.roller", "normal", "stage2", "suspect")

results <- matrix(ncol=5, nrow=0)
models <- list()

for (c in 1:length(class.weights[,1]))
{
  for (g in seq(-6, -1, by = 1))
  {
    for (cost in 0:3)
    {
      cat("Run for weights", c, ", g", 10^g, "and c", 10^cost, "\n")  

      # Data are scaled internally in svm, so no need to normalise
      model <- svm(State ~ ., data=data[train,-1], class.weights=class.weights[c,], gamma=10^g, cost=10^cost)
      pred <- predict(model, data[,-1], type="class")
      
      wacc <- weighted.acc(pred[-train], data[-train,2])
      pacc <- sum(pred[-train]==data[-train,2])/length(pred[-train])

      results <- rbind(results, c(10^g, 10^cost, c, wacc, pacc))
      models[[(length(models)+1)]] <- model
    }  
  }
}


# Save for now
save(results, file=paste0(basedir, "../../models/svm.results.obj"))
save(models, file=paste0(basedir, "../../models/svm.models.obj"))

write.table(results, file=paste0(basedir, "../../models/svm.results.csv"), sep=",")

best.row <- match(max(results[,4]), results[,4])
best.svm <- models[[best.row]]
save(best.svm, file=paste0(basedir, "../../models/best.svm.obj"))

# Visualise
library("RColorBrewer")
pal <- brewer.pal(10, "RdYlGn")

cols <- do.call(rbind, Map(function(a)
  {
    if (a>0.88) pal[1]
    else if (a > 0.86) pal[2]
    else if (a > 0.84) pal[3]
    else if (a > 0.82) pal[4]
    else if (a > 0.8) pal[5]
    else if (a > 0.7) pal[6] 
    else if (a > 0.6) pal[7]
    else if (a > 0.5) pal[8]
    else if (a > 0.3) pal[9]
    else pal[10]
  }, results[,4]))


plot(results[,2] ~ results[,1], log="xy", col=cols, pch=15, xlab="Gamma", ylab="Cost", main="Accuracy map of SVMs")
labs <- c("Above 88%", "86 to 88%", "84 to 86%", "82 to 84%", "80 to 82%", "70 to 80%", "60 to 70%", "50 to 60%", "30 to 50%", "Below 30%")
legend("topleft", labs, col=pal, pch=15)


