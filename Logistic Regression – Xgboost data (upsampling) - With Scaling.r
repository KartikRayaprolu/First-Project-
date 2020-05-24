mission_2 <- read.csv(file.choose(), header = T)
# View(mission_3)
mission_2 <- mission_2[,-c(1,4,5,7,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26,28,
                       31,33,34,36,37,38,39,40,41,47,48,49,50,51,52,53,54,57,61,
                       63,64)]
colnames(mission_2) <- c("agegr18","agels65","male","unmarried","cad.tvd",
                       "bmi","pulseN","bph","bpl","rrN",
                       "diabetes1","hbAd","hbChild","ur","cret","bcr","billed",
                       "concession","stay","icu","ward","implant","pol")
summary(mission_2)
str(mission_2)
mission_2$pol <- as.factor(mission_2$pol)
str(mission_2)
table(mission_2$pol)
summary(mission_2)

#install.packages("caTools")
library(caTools)
set.seed(009)
split_2 <- sample.split(mission_2, SplitRatio = 0.7)
train_2 <- subset(mission_2, split_2 == T)
test_2 <- subset(mission_2, split_2 == F)
length(test_2)
table(train_2$pol)

#Scaling
train_2[-23] <- scale(train_2[-23])
test_2[-23] <- scale(test_2[-23])

# SMOTE sampling
# install.packages("DMwR")
library(DMwR)

over2 <- SMOTE(pol ~., data = train_2, perc.over = 100)
over2
table(over2$pol)

# Model
logit_model2 <- glm(pol ~., data = over2, family = "binomial")
logit_model2
summary(logit_model2)

fitted.results2 <- predict(logit_model2, test_2, type = "response")
fitted.results2

fitted.results20 <- ifelse(fitted.results2 > 0.4, 1, 0)
fitted.results20

table(test_2$pol, fitted.results20)

misClassError_2 <- mean(fitted.results20 != test_2$pol)
misClassError_2
print(paste("Accuracy = ", 1 - misClassError_2))

# install.packages("caret")
library(caret)
cm_2 <- confusionMatrix(table(test_2$pol,fitted.results20))
cm_2

# ROC-AUC Curve
# install.packages("pROC")
library(pROC)
par(pty = "s")

# ROC-AUC Curve
# Without Threshold
roc.info2 <- roc(test_2$pol, fitted.results2, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "red", lwd = 2, print.auc = T, main = "ROC model up and down sampling")
#With Threshold
roc.info20 <- roc(test_2$pol, fitted.results20, plot = T, legacy.axes = T, percent = T,
                  xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                  col = "red", lwd = 2, print.auc = T, main = "ROC model up and down sampling")

