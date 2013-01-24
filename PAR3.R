# 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[,11], main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate")

#2
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])
par(mfrow=c(3,1))
r = range(range(outcome[,11], na.rm=TRUE), range(outcome[,17], na.rm=TRUE), range(outcome[,23], na.rm=TRUE))
hist(outcome[,11], main="Heart Attack", xlab="30-day Death Rate", xlim=r)
hist(outcome[,17], main="Heart Failure", xlab="30-day Death Rate", xlim=r)
hist(outcome[,23], main="Heart Pneumonia", xlab="30-day Death Rate", xlim=r)

#2.4
par(mfrow=c(3,1))
hist(outcome[,11], main="Heart Attack", xlab="30-day Death Rate", xlim=r, probability=TRUE)
lines(density(outcome[,11], na.rm=TRUE, adjust=2))
hist(outcome[,17], main="Heart Failure", xlab="30-day Death Rate", xlim=r, probability=TRUE)
lines(density(outcome[,17], na.rm=TRUE, adjust=2))
hist(outcome[,23], main="Heart Pneumonia", xlab="30-day Death Rate", xlim=r, probability=TRUE)
lines(density(outcome[,23], na.rm=TRUE, adjust=2))

#3