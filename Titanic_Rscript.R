

train_titanic<-read.csv("train.csv", stringsAsFactors=FALSE)

train_titanic$Survived<-as.factor(train_titanic$Survived)

train_titanic$Pclass<-as.factor(train_titanic$Pclass)
train_titanic$Sex<-as.factor(train_titanic$Sex)
train_titanic$Parch<-as.factor(train_titanic$Parch)
train_titanic$Embarked<-as.factor(train_titanic$Embarked)


summary(train_titanic)
typeof(train_titanic$Name)


summary(train_titanic)
is.na(train_titanic)

train_titanic$Age[is.na(train_titanic$Age)] = mean(train_titanic$Age, na.rm = TRUE)
length(train_titanic)


test_titanic<-read.csv("test.csv", stringsAsFactors=FALSE)

test_titanic$Survived<-as.factor(test_titanic$Survived)

test_titanic$Pclass<-as.factor(test_titanic$Pclass)
test_titanic$Sex<-as.factor(test_titanic$Sex)
test_titanic$Parch<-as.factor(test_titanic$Parch)
test_titanic$Embarked<-as.factor(test_titanic$Embarked)


summary(test_titanic)
typeof(test_titanic$Name)


summary(test_titanic)
is.na(test_titanic)

test_titanic$Age[is.na(test_titanic$Age)] = mean(test_titanic$Age, na.rm = TRUE)


set.seed(123)

train_sample <- sample(1:891, size=600)
train <- train_titanic[train_sample,]
test <- train_titanic[-train_sample,]



train.lm <- glm(Survived ~ . -Name - PassengerId - Ticket - Embarked - Cabin, data=train, family="binomial")
summary(train.lm)

train.lm2 <- glm(Survived ~ . - PassengerId - Embarked - Fare, data=train[,!colnames(train) %in% c("Name","Ticket","Cabin","Parch")], family="binomial")
summary(train.lm2)


probs<-as.vector(predict(train.lm2,newdata=test, type="response"))
preds <- rep(0,291)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,test$Survived)

acc = (154 + 80) / nrow(test)
#0.8041237


train.lmf <- glm(Survived ~ . - PassengerId - Embarked - Fare, data=train_titanic[,!colnames(train_titanic) %in% c("Name","Ticket","Cabin","Parch")], family="binomial")
summary(train.lmf)


probs2<-as.vector(predict(train.lmf,newdata=test_titanic, type="response"))
preds2 <- rep(0,418)  # Initialize prediction vector
preds2[probs2>0.5] <- 1 # p>0.5 -> 1



test_titanic$Survived = as.numeric(probs2 >= 0.5)
table(test_titanic$Survived)
# 0   1 
# 256 162

predictions = data.frame(test_titanic[c("PassengerId","Survived")])
write.csv(file = "titanic_preds_sj8em", x = predictions)

######################


