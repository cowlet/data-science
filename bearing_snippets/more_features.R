basedir <- "../bearing_IMS/1st_test/"

library(e1071)

# Helper functions
fft.spectrum <- function (d)
{
	fft.data <- fft(d)
	# Ignore the 2nd half, which are complex conjugates of the 1st half, 
	# and calculate the Mod (magnitude of each complex number)
	return (Mod(fft.data[1:(length(fft.data)/2)]))
}

freq2index <- function(freq)
{
	step <- 10000/10240 # 10kHz over 10240 bins
	return (floor(freq/step))
}

# Bearing data
Bd <- 0.331 # ball diameter, in inches
Pd <- 2.815 # pitch diameter, in inches
Nb <- 16 # number of rolling elements
a <- 15.17*pi/180 # contact angle, in radians
s <- 2000/60 # rotational frequency, in Hz

ratio <- Bd/Pd * cos(a)
ftf <- s/2 * (1 - ratio)
bpfi <- Nb/2 * s * (1 + ratio)
bpfo <- Nb/2 * s * (1 - ratio)
bsf <- Pd/Bd * s/2 * (1 - ratio**2)


all.features <- function(d)
{
	# Statistical features
	features <- c(quantile(d, names=FALSE), mean(d), sd(d), skewness(d), kurtosis(d))
	
	# RMS
	features <- append(features, sqrt(mean(d**2)))
	
	# Key frequencies
	fft.amps <- fft.spectrum(d)
	
	features <- append(features, fft.amps[freq2index(ftf)])
	features <- append(features, fft.amps[freq2index(bpfi)])
	features <- append(features, fft.amps[freq2index(bpfo)])
	features <- append(features, fft.amps[freq2index(bsf)])
	
	# Strongest frequencies
	n <- 5
	frequencies <- seq(0, 10000, length.out=length(fft.amps))
	sorted <- sort.int(fft.amps, decreasing=TRUE, index.return=TRUE)
	top.ind <- sorted$ix[1:n] # indexes of the largest n components
	features <- append(features, frequencies[top.ind]) # convert indexes to frequencies

	# Power in frequency bands
	vhf <- freq2index(6000):length(fft.amps)    # 6kHz plus
	hf <- freq2index(2600):(freq2index(6000)-1) # 2.6kHz to 6kHz
	mf <- freq2index(1250):(freq2index(2600)-1) # 1.25kHz to 2.6kHz
	lf <- 0:(freq2index(1250)-1)                # forcing frequency band

	powers <- c(sum(fft.amps[vhf]), sum(fft.amps[hf]), sum(fft.amps[mf]), sum(fft.amps[lf]))
	features <- append(features, powers)
	
	return(features)
}




# Set up storage for bearing-grouped data
b1m <- matrix(nrow=0, ncol=(2*23))
b2m <- matrix(nrow=0, ncol=(2*23))
b3m <- matrix(nrow=0, ncol=(2*23))
b4m <- matrix(nrow=0, ncol=(2*23))
# and for timestamps
timestamp <- vector()

for (filename in list.files(basedir))
{
	cat("Processing file ", filename, "\n")
	
	ts <- as.character(strptime(filename, format="%Y.%m.%d.%H.%M.%S"))
	
	data <- read.table(paste0(basedir, filename), header=FALSE, sep="\t")
	colnames(data) <- c("b1.x", "b1.y", "b2.x", "b2.y", "b3.x", "b3.y", "b4.x", "b4.y")
	
	# Bind the new rows to the bearing matrices
    b1m <- rbind(b1m, c(all.features(data$b1.x), all.features(data$b1.y)))
    b2m <- rbind(b2m, c(all.features(data$b2.x), all.features(data$b2.y)))
    b3m <- rbind(b3m, c(all.features(data$b3.x), all.features(data$b3.y)))
    b4m <- rbind(b4m, c(all.features(data$b4.x), all.features(data$b4.y)))
    
    timestamp <- c(timestamp, ts)

}

cnames <- c("Min.x", "Qu.1.x", "Median.x", "Qu.3.x", "Max.x", "Mean.x", "SD.x", "Skew.x", "Kurt.x", "RMS.x", "FTF.x", "BPFI.x", "BPFO.x", "BSF.x", "F1.x", "F2.x", "F3.x", "F4.x", "F5.x", "VHF.pow.x", "HF.pow.x", "MF.pow.x", "LF.pow.x", "Min.y", "Qu.1.y", "Median.y", "Qu.3.y", "Max.y", "Mean.y", "SD.y", "Skew.y", "Kurt.y", "RMS.y", "FTF.y", "BPFI.y", "BPFO.y", "BSF.y", "F1.y", "F2.y", "F3.y", "F4.y", "F5.y", "VHF.pow.y", "HF.pow.y", "MF.pow.y", "LF.pow.y")
colnames(b1m) <- cnames
colnames(b2m) <- cnames
colnames(b3m) <- cnames
colnames(b4m) <- cnames
b1 <- data.frame(timestamp, b1m)
b2 <- data.frame(timestamp, b2m)
b3 <- data.frame(timestamp, b3m)
b4 <- data.frame(timestamp, b4m)

write.table(b1, file=paste0(basedir, "../b1_all.csv"), sep=",", row.names=FALSE)
write.table(b2, file=paste0(basedir, "../b2_all.csv"), sep=",", row.names=FALSE)
write.table(b3, file=paste0(basedir, "../b3_all.csv"), sep=",", row.names=FALSE)
write.table(b4, file=paste0(basedir, "../b4_all.csv"), sep=",", row.names=FALSE)


