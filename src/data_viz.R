library(tidyverse)
library(ggfortify)
library(ggplot2)
library(Rtsne)
library(wesanderson)
library(FactoMineR)
library(factoextra)
library(stargazer)
library(AER)

#################
# Data cleaning #
#################

all_data = readRDS("WV6_Data.rds")

# keep interesting variables
# V9: importance in life: religion
# V41: Would not like to have as neighbors: People of a different religion
# V70: It is important to this person to think up new ideas and be creative; to do things one’s own way
# V73: It is important to this person to have a good time; to “spoil” oneself
# V78: Looking after the environment is important to this person; to care for nature and save life resources
# V79: Tradition is important to this person; to follow the customs handed down by one’s religion or family
# V106: How much you trust: People of another religion
# V150: Meaning of religion: To follow religious norms and ceremonies vs To do good to other people
# V151: Meaning of religion: To make sense of life after death vs To make sense of life in this world
# V152: How important is God in your life
# V153: Whenever science and religion conflict, religion is always right
# V154: The only acceptable religion is my religion
# V156: People who belong to different religions are probably just as moral as those who belong to mine
# V211: How proud of nationality

# V144: Religious denomination
# V242: Age
# V240: Sex
# V248: Highest level of education attained
# V96: Income equality
# V98: Government responsibility
# V99: Competition good or harmful
# V239: Scale of incomes
voi <- c("V9", "V41", "V79", "V106", "V150", "V151", "V156", "V153", "V154", "V152", "V70", "V211", "V73", "V78",
         "V144", "V242", "V240", "V248")

new_voi <- c("V9", "V41", "V70", "V73", "V78", "V79", "V106", "V150", "V151", "V153", "V154", "V156", "V152", "V211",
             "V144", "V242", "V240", "V248", "V96", "V98", "V99", "V239")

# data <- all_data[voi]
data <- all_data[new_voi]

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
data[which(!(data$V144 %in% top_ten)), which(colnames(data)=="V144")] <- 5
data[data$V144 == 12, which(colnames(data)=="V144")] <- 1
data[data$V144 == 31, which(colnames(data)=="V144")] <- 2
data[data$V144 == 49, which(colnames(data)=="V144")] <- 3
data[data$V144 == 52, which(colnames(data)=="V144")] <- 4
data[data$V144 == 53, which(colnames(data)=="V144")] <- 5
data[data$V144 == 62, which(colnames(data)=="V144")] <- 9
data[data$V144 == 64, which(colnames(data)=="V144")] <- 7
data[data$V144 == 75, which(colnames(data)=="V144")] <- 8

# scale "neither of the above" responses
data[, which(colnames(data)=="V150")] <- data[, which(colnames(data)=="V150")] + 1
data[data$V150 == 4, which(colnames(data)=="V150")] <- 1
data[data$V150 == 5, which(colnames(data)=="V150")] <- 4

data[, which(colnames(data)=="V151")] <- data[, which(colnames(data)=="V151")] + 1
data[data$V151 == 4, which(colnames(data)=="V151")] <- 1
data[data$V151 == 5, which(colnames(data)=="V151")] <- 4

data <- data[!(data$V211 == 5), ]

# remove religious denomination
data_without <- data[, 1:14]

#######
# EDA #
#######

### t-SNE ###
tsne_new <- Rtsne(data, check_duplicates=FALSE,pca=FALSE,perplexity=30)
tsne_new_without <- Rtsne(data_without[1:13], check_duplicates=FALSE,pca=FALSE,perplexity=30)
tsne_new_without <- Rtsne(data_without[c(1:2, 5:13)], check_duplicates=FALSE,pca=FALSE,perplexity=30)
tsne_new_without <- Rtsne(data_without[c(1:2, 5:8, 10:13)], check_duplicates=FALSE,pca=FALSE,perplexity=30)

# ggplot stuff
ggplot(data, aes(x=tsne_new$Y[,1], y=tsne_new$Y[,2], color=factor(data$V144))) + geom_point() +
  scale_color_manual(name="Religious denomination",
                     labels=c("None","Buddhist","Hindu","Muslim","Orthodox","Other","Armenian Apostolic Church","Protestant",
                              "Roman Catholic","Sunni"),
                     values=c(wes_palette("BottleRocket2"), wes_palette("Royal2")))

ggplot(data_without, aes(x=tsne_new_without$Y[,1], y=tsne_new_without$Y[,2], color=factor(data$V144))) + geom_point() +
  scale_color_manual(name="Religious denomination",
                     labels=c("None","Buddhist","Hindu","Muslim","Orthodox","Other","Armenian Apostolic Church","Protestant",
                              "Roman Catholic","Sunni"),
                     values=c(wes_palette("BottleRocket2"), wes_palette("Royal2"))) +
  ggtitle("t-SNE visualization of our variables of interest") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Dimension 1") + ylab("Dimension 2")

### PCA ###
# no preprocessing
pca <- prcomp(data_without) 
autoplot(pca, data=data, colour="V144")

# all ordinal variables converted to dummies
pca_dummy <- prcomp(dummy_data_without) 
autoplot(pca_dummy, data=dummy_data, colour="V144")

### MCA ###
x <- as.data.frame(data_without)
i=0
while(i < ncol(x)){
  i=i+1  
  x[,i] = as.factor(x[,i])
}
mca_res <- MCA(x, ncp = 2, graph=TRUE, method="Burt")
ind <- mca_res$ind
  
fviz_screeplot(mca_res, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_var(mca_res, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# inner feelings regression
model1 <- lm(ind$coord[, 1] ~ V144 + V242 + V240 + V248, data=data)

# V144: Religious denomination
# V242: Age
# V240: Sex
# V248: Highest level of education attained
# V96: Income equality
# V98: Government responsibility
# V99: Competition good or harmful
# V239: Scale of incomes
# outer feelings regression
model2 <- ivreg(ind$coord[, 2] ~ V144 +  V96 + V98 + V99 | .-V96 + V239, data=data)

# summary
stargazer(model1, model2, type="latex")

