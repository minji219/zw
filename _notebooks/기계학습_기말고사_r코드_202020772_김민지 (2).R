library(MASS) #MASS package에 내장돼있음
library(caret)
library(pROC)
library(randomForest)
Boston
# Boston 데이터셋 불러오기
data(Boston)

# Boston 데이터셋 불러오기
data(Boston)
Boston

# crim1 변수 정의
Boston$crim1 <- ifelse(Boston$crim >= 0.25, 1, 0)

# crim1 변수 확인
head(Boston$crim1)
Boston$crim1


# 데이터셋 나누기 (훈련:평가 = 5:5)
set.seed(123)
trainIndex <- createDataPartition(Boston$crim1, p = 0.5, list = FALSE)
train_data <- Boston[trainIndex, ]
test_data <- Boston[-trainIndex, ]

# 모델 훈련: 로지스틱 회귀
logistic_model <- glm(crim1 ~ lstat + nox + age, data = train_data, family = "binomial")
logistic_probs <- predict(logistic_model, newdata = test_data, type = "response")

# crim1 변수를 factor로 변환
train_data$crim1 <- as.factor(train_data$crim1)

# QDA 모델 훈련
qda_model <- qda(crim1 ~ lstat + nox + age, data = train_data)

# 예측
qda_pred <- predict(qda_model, newdata = test_data, type = "posterior")
qda_probs <- qda_pred$posterior[, 2]


# 모델 훈련: Random Forest
rf_model <- randomForest(factor(crim1) ~ lstat + nox + age, data = train_data, ntree = 100)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[,2]

# AUC 계산 및 비교
logistic_auc <- roc(test_data$crim1, logistic_probs)
qda_auc <- roc(test_data$crim1, qda_probs)
rf_auc <- roc(test_data$crim1, rf_probs)

# 결과 출력
print(paste("Logistic Regression AUC:", round(auc(logistic_auc), 4)))
print(paste("QDA AUC:", round(auc(qda_auc), 4)))
print(paste("Random Forest AUC:", round(auc(rf_auc), 4)))

#로지스틱의 auc는 0.9279이고 qda의 auc는 0.912이다. 
#랜덤포레스트의 auc는 0.9499로 랜덤포레스트, 로지스틱, qda순으로 좋다고 할 수 있다. 

