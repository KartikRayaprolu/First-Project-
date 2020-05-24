mission_4 <- read.csv(file.choose(), header = T)
# View(mission_5)
mission_4 <- mission_4[,-c(1,4,5,7,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26,28,
                       31,33,34,36,37,38,39,40,41,47,48,49,50,51,52,53,54,57,61,
                       63,64)]
colnames(mission_4) <- c("agegr18","agels65","male","unmarried","cad.tvd",
                       "bmi","pulseN","bph","bpl","rrN",
                       "diabetes1","hbAd","hbChild","ur","cret","bcr","billed",
                       "concession","stay","icu","ward","implant","pol")
summary(mission_4)
str(mission_4)
mission_4$pol <- as.factor(mission_4$pol)
str(mission_4)
table(mission_4$pol)
summary(mission_4)

#install.packages("caTools")
library(caTools)
set.seed(009)
split_4 <- sample.split(mission_4, SplitRatio = 0.7)
train_4 <- subset(mission_4, split_4 == T)
test_4 <- subset(mission_4, split_4 == F)
length(test_4)
table(train_4$pol)
prop.table(table(train_4$pol))

# Up and Down sampling
# install.packages("ROSE")
library(ROSE)

over4 <- ovun.sample(pol ~ ., data = train_4, method = "both")$data
over4
table(over4$pol)
prop.table(table(over4$pol))

# over4_sampled <- barplot(prop.table(table(over4$pol)),
#                          col = rainbow,
#                          ylim = c(0,0.7),
#                          main = "Class Distribution")

# Model 5
logit_model4 <- glm(pol ~., data = over4, family = "binomial")
logit_model4
summary(logit_model4)

fitted.results4 <- predict(logit_model4, test_4, type = "response")
fitted.results4

fitted.results40 <- ifelse(fitted.results4 > 0.5, 1, 0)
fitted.results40

table(test_4$pol, fitted.results40)

misClassError_4 <- mean(fitted.results40 != test_4$pol)
misClassError_4
print(paste("Accuracy = ", 1 - misClassError_4))

library(caret)
cm_4 <- confusionMatrix(table(test_4$pol,fitted.results40))
cm_4

# ROC-AUC Curve
# install.packages("pROC")
library(pROC)
par(pty = "s")

# Without Threshold
roc.info4 <- roc(test_4$pol, fitted.results4, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "black", lwd = 2, print.auc = T, main = "ROC model upsampling")

# With Threshold
roc.info40 <- roc(test_4$pol, fitted.results40, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "black", lwd = 2, print.auc = T, main = "ROC model upsampling")

