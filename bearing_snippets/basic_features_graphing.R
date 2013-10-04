# Snippets to graph the output of the basic feature extraction
# Either run this whole script to plot onto one page,
# or paste these paragraphs into the R console for individual plots.

basedir <- "../bearing_IMS/1st_test/"
b1 <- read.table(file=paste0(basedir, "../b1.csv"), sep=",", header=FALSE)
b2 <- read.table(file=paste0(basedir, "../b2.csv"), sep=",", header=FALSE)
b3 <- read.table(file=paste0(basedir, "../b3.csv"), sep=",", header=FALSE)
b4 <- read.table(file=paste0(basedir, "../b4.csv"), sep=",", header=FALSE)


par(mfrow=c(5,2))

# x axis components
plot(b1[, 2], pch=4, col="dodgerblue2", ylab="Frequency", main="1st strongest x", ylim=c(0, 2)) 
points(b2[, 2], pch=3, col="darkorchid2")
points(b4[, 2], pch=1, col="chartreuse")
points(b3[, 2], pch=20, col="coral2")

legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1[, 7], pch=4, col="dodgerblue2", ylab="Frequency", main="1st strongest y", ylim=c(0, 2)) 
points(b2[, 7], pch=3, col="darkorchid2")
points(b4[, 7], pch=1, col="chartreuse")
points(b3[, 7], pch=20, col="coral2")

legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))


plot(b1[, 3], pch=4, col="dodgerblue2", ylab="Frequency", main="2nd strongest x", ylim=c(0,10000))
points(b2[, 3], pch=3, col="darkorchid2")
points(b4[, 3], pch=1, col="chartreuse")
points(b3[, 3], pch=20, col="coral2")

plot(b1[, 8], pch=4, col="dodgerblue2", ylab="Frequency", main="2nd strongest y", ylim=c(0,10000))
points(b2[, 8], pch=3, col="darkorchid2")
points(b4[, 8], pch=1, col="chartreuse")
points(b3[, 8], pch=20, col="coral2")

plot(b1[, 4], pch=4, col="dodgerblue2", ylab="Frequency", main="3rd strongest x", ylim=c(0,10000))
points(b2[, 4], pch=3, col="darkorchid2")
points(b4[, 4], pch=1, col="chartreuse")
points(b3[, 4], pch=20, col="coral2")

plot(b1[, 9], pch=4, col="dodgerblue2", ylab="Frequency", main="3rd strongest y", ylim=c(0,10000))
points(b2[, 9], pch=3, col="darkorchid2")
points(b4[, 9], pch=1, col="chartreuse")
points(b3[, 9], pch=20, col="coral2")

plot(b1[, 5], pch=4, col="dodgerblue2", ylab="Frequency", main="4th strongest x", ylim=c(0,10000))
points(b2[, 5], pch=3, col="darkorchid2")
points(b4[, 5], pch=1, col="chartreuse")
points(b3[, 5], pch=20, col="coral2")

plot(b1[, 10], pch=4, col="dodgerblue2", ylab="Frequency", main="4th strongest y", ylim=c(0,10000))
points(b2[, 10], pch=3, col="darkorchid2")
points(b4[, 10], pch=1, col="chartreuse")
points(b3[, 10], pch=20, col="coral2")

plot(b1[, 6], pch=4, col="dodgerblue2", ylab="Frequency", main="5th strongest x", ylim=c(0,10000))
points(b2[, 6], pch=3, col="darkorchid2")
points(b4[, 6], pch=1, col="chartreuse")
points(b3[, 6], pch=20, col="coral2")

plot(b1[, 11], pch=4, col="dodgerblue2", ylab="Frequency", main="5th strongest y", ylim=c(0,10000))
points(b2[, 11], pch=3, col="darkorchid2")
points(b4[, 11], pch=1, col="chartreuse")
points(b3[, 11], pch=20, col="coral2")

