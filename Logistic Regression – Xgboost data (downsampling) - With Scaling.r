mission_3 <- read.csv(file.choose(), header = T)
# View(mission_4)
mission_3 <- mission_3[,-c(1,4,5,7,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26,28,
                       31,33,34,36,37,38,39,40,41,47,48,49,50,51,52,53,54,57,61,
                       63,64)]
colnames(mission_3) <- c("agegr18","agels65","male","unmarried","cad.tvd",
                       "bmi","pulseN","bph","bpl","rrN",
                       "diabetes1","hbAd","hbChild","ur","cret","bcr","billed",
                       "concession","stay","icu","ward","implant","pol")
summary(mission_3)
str(mission_3)
mission_3$pol <- as.factor(mission_3$pol)
str(mission_3)
table(mission_3$pol)
summary(mission_3)

#install.packages("caTools")
library(caTools)
set.seed(009)
split_3 <- sample.split(mission_3, SplitRatio = 0.7)
train_3 <- subset(mission_3, split_3 == T)
test_3 <- subset(mission_3, split_3 == F)
length(test_3)
table(train_3$pol)

#Scaling
train_3[-23] <- scale(train_3[-23])
test_3[-23] <- scale(test_3[-23])

# Undersampling sampling
# install.packages("ROSE")
library(ROSE)

over3 <- ovun.sample(pol ~., data = train_3, method = "under", N = 124)$data
over3
table(over3$pol)

# Model 4
logit_model3 <- glm(pol ~., data = over3, family = "binomial")
logit_model3
summary(logit_model3)

fitted.results3 <- predict(logit_model3, test_3, type = "response")
fitted.results3

fitted.results30 <- ifelse(fitted.results3 > 0.5, 1, 0)
fitted.results30

table(test_3$pol, fitted.results30)

misClassError_3 <- mean(fitted.results30 != test_3$pol)
misClassError_3
print(paste("Accuracy = ", 1 - misClassError_3))

library(caret)
cm_3 <- confusionMatrix(table(test_3$pol,fitted.results30))
cm_3

# ROC-AUC Curve
# install.packages("pROC")
library(pROC)
par(pty = "s")

# Withou Threshold
roc.info3 <- roc(test_3$pol, fitted.results3, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "blue", lwd = 2, print.auc = T, main = "ROC model down sampling")

# With Threshold
roc.info30 <- roc(test_3$pol, fitted.results30, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "blue", lwd = 2, print.auc = T, main = "ROC model down sampling")
