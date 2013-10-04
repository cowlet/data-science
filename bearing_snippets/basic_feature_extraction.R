basedir <- "../bearing_IMS/1st_test/"

fft.profile <- function (dataset, n)
{
	fft.data <- fft(dataset)
	# Ignore the 2nd half, which are complex conjugates of the 1st half, 
	# and calculate the Mod (magnitude of each complex number)
	amplitude <- Mod(fft.data[1:(length(fft.data)/2)])
	# Calculate the frequencies
	frequencies <- seq(0, 10000, length.out=length(fft.data)/2)

	sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
	top <- sorted$ix[1:n] # indexes of the largest n components
	return (frequencies[top]) # convert indexes to frequencies
}


# How many FFT components should we grab as features?
n <- 5

# Set up storage for bearing-grouped data
b1 <- matrix(nrow=0, ncol=(2*n+1))
b2 <- matrix(nrow=0, ncol=(2*n+1))
b3 <- matrix(nrow=0, ncol=(2*n+1))
b4 <- matrix(nrow=0, ncol=(2*n+1))

for (filename in list.files(basedir))
{
	cat("Processing file ", filename, "\n")
	
	timestamp <- as.character(strptime(filename, format="%Y.%m.%d.%H.%M.%S"))
	
	data <- read.table(paste0(basedir, filename), header=FALSE, sep="\t")
	colnames(data) <- c("b1.x", "b1.y", "b2.x", "b2.y", "b3.x", "b3.y", "b4.x", "b4.y")
	
	# Bind the new rows to the bearing matrices
    b1 <- rbind(b1, c(timestamp, fft.profile(data$b1.x, n), fft.profile(data$b1.y, n)))
    b2 <- rbind(b2, c(timestamp, fft.profile(data$b2.x, n), fft.profile(data$b2.y, n)))
    b3 <- rbind(b3, c(timestamp, fft.profile(data$b3.x, n), fft.profile(data$b3.y, n)))
    b4 <- rbind(b4, c(timestamp, fft.profile(data$b4.x, n), fft.profile(data$b4.y, n)))

}

write.table(b1, file=paste0(basedir, "../b1.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(b2, file=paste0(basedir, "../b2.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(b3, file=paste0(basedir, "../b3.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(b4, file=paste0(basedir, "../b4.csv"), sep=",", row.names=FALSE, col.names=FALSE)



