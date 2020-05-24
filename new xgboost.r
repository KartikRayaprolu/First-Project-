install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("Matrix")
library(Matrix)
install.packages("xgboost")
library(xgboost)

mission <- read.csv(file.choose(), header = T)
View(mission)
mission <- mission[,-c(3,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,
                       26,28,31,32,33,34,36,37,39,40,41,42,43,44,46,47,52,53,
                       54,55,57,58,61,66,67,68)]
colnames(mission)
colnames(mission) <- c("age","gend","ms","kcc","bmi","pulse","bph","bpl","rr",
                       "pmh","hb","ur","cret","bcr","moa","admsn","billed",
                       "concession","stay","icu","ward","implant","pol")
colnames(mission)
class(mission)
lapply(mission, class)


#install.packages("caTools")
library(caTools)
set.seed(001)
split <- sample.split(mission, SplitRatio = 0.7)
train <- subset(mission, split == T)
test <- subset(mission, split == F)

# One-hot coding
trainm <- sparse.model.matrix(pol ~.-1, data = train)
head(trainm)
View(trainm)
train_label <- train[,"pol"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(pol ~.-1,data = test)
test_label <- test[,"pol"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# parameters
nc <- length(unique(train_label))
nc
xgb_params <- list("objective" = "multi:softprob", "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

# eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 99,
                       watchlist = watchlist, eta = 0.05, max.depth = 3,
                       gamma = 0, subsample = 1, colsample_bytree = 1,
                       missing = NA, set.seed(100))

# Training & test error
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = "blue")
lines(e$iter, e$test_mlogloss, col = "red")
min(e$test_mlogloss)
e[e$test_mlogloss == 0.424872,]

# Feature Importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_matrix)
p
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(.,'last')-1)
head(pred)
table(Prediction = pred$max_prob, Actual = pred$label)
