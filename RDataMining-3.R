
seg.raw <- read.csv("http://goo.gl/qw303p")
head(seg.raw)
nrow(seg.raw)
ncol(seg.raw)
seg.df  <- seg.raw[ , -7]     # remove the known segment assignments
summary(seg.df)

write.csv(seg.raw, file="E:/RDataMining/segdata.csv")

############################################################
### Read the data
############################################################

seg = read.csv("E:/RDataMining/segdata.csv", header=T)
nrow(seg)
ncol(seg)

seg.raw = seg
seg.df  <- seg.raw[ , -7]     # remove the known segment assignments
summary(seg.df)

# Examine group differences

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}
seg.summ(seg.df, seg.raw$Segment)

head(seg,3)
############################################
# Clustering
############################################
library(cluster)
seg.dist <- daisy(seg.df) # daisy works with mixed data types
as.matrix(seg.dist)[1:3, 1:3] # distances of first 4 observations

seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)

plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]]) # first tree below 0.5
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[2]]) # 2nd tree below 0.5

seg.df[c(128, 137), ] # similar
seg.df[c(128, 173), ] # less similar

cor(cophenetic(seg.hc), seg.dist) # cophenetic correlation coefficient

plot(seg.hc)
rect.hclust(seg.hc, k=4, border="blue")
#######################################################
# getting membership from hclust
#######################################################
seg.hc.segment <- cutree(seg.hc, k=4) # membership vector for 4 groups
table(seg.hc.segment)

seg.summ(seg.df, seg.hc.segment)

plot(jitter(as.numeric(seg.df$gender)) ~
       jitter(as.numeric(seg.df$subscribe)),
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))

########################################
# train and test data
########################################


set.seed(92118)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.raw[ train.cases, ]
sub.df.test  <- seg.raw[-train.cases, ]
summary(sub.df.train)

library(cluster)
clusplot(sub.df.train[, -6], sub.df.train$subscribe, color=TRUE, 
         shade=TRUE, labels=4, lines=0, main="Status, training data")

##############################################
# random forest
##############################################

install.packages("randomForest")
library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000))

set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000, 
                        sampsize=c(25, 25)) )   # balanced classes

sub.rf.sub <- predict(sub.rf, sub.df.test, predict.all=TRUE)
sub.ind.p  <- apply(sub.rf.sub$individual, 1, 
                    function(x) prop.table(table(x)))["subYes", ]
summary(sub.ind.p)

plot(sub.ind.p, xlab="Holdout respondent", ylab="Likelihood")

table(targeted=sub.ind.p >= 0.5, sub.df.test$subscribe)
chisq.test(table(sub.ind.p>=0.5, sub.df.test$subscribe))



