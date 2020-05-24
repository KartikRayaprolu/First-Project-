mission_1 <- read.csv(file.choose(), header = T)
# View(mission_2)
mission_1 <- mission_1[,-c(1,4,5,7,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26,28,
                       31,33,34,36,37,38,39,40,41,47,48,49,50,51,52,53,54,57,61,
                       63,64)]
colnames(mission_1) <- c("agegr18","agels65","male","unmarried","cad.tvd",
                       "bmi","pulseN","bph","bpl","rrN",
                       "diabetes1","hbAd","hbChild","ur","cret","bcr","billed",
                       "concession","stay","icu","ward","implant","pol")
summary(mission_1)
str(mission_1)
mission_1$pol <- as.factor(mission_1$pol)
str(mission_1)
table(mission_1$pol)
summary(mission_1)

#install.packages("caTools")
library(caTools)
set.seed(009)
split_1 <- sample.split(mission_1, SplitRatio = 0.7)
train_1 <- subset(mission_1, split_1 == T)
test_1 <- subset(mission_1, split_1 == F)
length(test_1)
table(train_1$pol)
prop.table(table(train_1$pol))
summary(train_1)

#Scaling
train_1[-23] <- scale(train_1[-23])
test_1[-23] <- scale(test_1[-23])

# Model
logit_model1 <- glm(pol ~., data = train_1, family = "binomial")
logit_model1
summary(logit_model1)

fitted.results1 <- predict(logit_model1, test_1, type = "response")
fitted.results1

fitted.results10 <- ifelse(fitted.results1 > 0.6, 1, 0)
fitted.results10


table(test_1$pol, fitted.results10)

misClassError_1 <- mean(fitted.results10 != test_1$pol)
misClassError_1
print(paste("Accuracy = ", 1 - misClassError_1))

# install.packages("caret")
library(caret)
cm_1 <- confusionMatrix(table(test_1$pol,fitted.results10))
cm_1

# ROC-AUC Curve
# install.packages("pROC")
library(pROC)
par(pty = "s")

# ROC-AUC Curve
# Without Threshold
roc.info1 <- roc(test_1$pol, fitted.results1, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "purple", lwd = 2, print.auc = T, main = "ROC model general data")

#With Threshold
roc.info10 <- roc(test_1$pol, fitted.results10, plot = T, legacy.axes = T, percent = T,
                  xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                  col = "purple", lwd = 2, print.auc = T, main = "ROC model general data")
