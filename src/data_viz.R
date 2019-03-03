library(tidyverse)
library(ggfortify)
library(ggplot2)
library(Rtsne)

#################
# Data cleaning #
#################

data = readRDS("WV6_Data.rds")

# keep interesting variables
# V9: importance in life: religion
# V41: Would not like to have as neighbors: People of a different religion
# V79: Tradition is important to this person; to follow the customs handed down by one’s religion or family
# V106: How much you trust: People of another religion
# V150: Meaning of religion: To follow religious norms and ceremonies vs To do good to other people
# V151: Meaning of religion: To make sense of life after death vs To make sense of life in this world
# V153: Whenever science and religion conflict, religion is always right
# V154: The only acceptable religion is my religion
# V156: People who belong to different religions are probably just as moral as those who belong to mine
# V144: Religious denomination
voi <- c("V9", "V41", "V79", "V106", "V150", "V151", "V153", "V154", "V156", "V144")

data <- data[voi]

# get rid of missing values
data <- data[complete.cases(data),]
missing <- c()
has.neg <- apply(data, 1, function(row) any(row < 0))
data <- data[-which(has.neg), ]

# Group religions into bins
data <- data[data$V144 < 100, ]

# keep top 10 religions and group the rest into other 
top_ten <- sort(rownames(as.matrix(sort(table(data$V144), decreasing=TRUE)[1:10])))
other <- lapply(data$V144, function(row) !(row %in% top_ten))

# change the religion codes
data[which(!(data$V144 %in% top_ten)), which(colnames(data)=="V144")] <- 9
data[data$V144 == 12, which(colnames(data)=="V144")] <- 1
data[data$V144 == 31, which(colnames(data)=="V144")] <- 2
data[data$V144 == 49, which(colnames(data)=="V144")] <- 3
data[data$V144 == 52, which(colnames(data)=="V144")] <- 4
data[data$V144 == 53, which(colnames(data)=="V144")] <- 5
data[data$V144 == 62, which(colnames(data)=="V144")] <- 9
data[data$V144 == 64, which(colnames(data)=="V144")] <- 7
data[data$V144 == 75, which(colnames(data)=="V144")] <- 8

# remove religious denomination
data_without <- data[, 1:9]
no_dup_data <- unique(data)
no_dup_data_without <- unique(as.matrix(no_dup_data[1:9]))

#######
# EDA #
#######
pca <- prcomp(data_without) 
autoplot(pca, data=data, colour="V144")

tsne <- Rtsne(no_dup_data_without,pca=FALSE,perplexity=30)
plot(tsne$Y, col=no_dup_data$V144, asp=1)



