
#第三章2020.03.24#
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), 
							labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
normalize <- function(x)
	{
		return ((x - min(x)) / (max(x) - min(x)))
	}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
								cl = wbcd_train_labels, k = 21)
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
							cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

#第四章2020.03.22#
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumber)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_test, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0,5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

findFreqTerms(sms_dtm_train, 5)
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

convert_counts <- function(x)
	{
		x <- ifelse(x > 0, 1, 0)
		x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
		return(x)
	}
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq = FALSE, 
				prop.t = FALSE, dnn = c("predicted", "actual"))

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type, prop.chisq = FALSE,
			 prop.t = FALSE, prop.r = FALSE, dnn = c("predicted", "actual"))
				
#第五章2020.03.22#
credit <- read.csv("credit.csv")
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

set.seed(12345)
credit$default <- factor(credit$default)
credit_rand <- credit[order(runif(1000)), ]
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, 
				prop.r = FALSE, dnn = c("actual default", "predicted default"))
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, 
				prop.c = FALSE, prop.r = FALSE, dnn = c("actual default","predicted default"))
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, 
				prop.c = FALSE, prop.r = FALSE, dnn = c("actual default", "predicted default"))

mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)
mushrooms$veil_type <- NULL
table(mushrooms$type)
library(RWeka)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R
summary(mushroom_1R)
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip

#第六章2020.03.21#
launch <- read.csv("challenger.csv")
reg <- function(y, x)
	{
		x <- as.matrix(x)
		x <- cbind(Intercept = 1, x)
		solve(t(x) %*% x) %*% t(x) %*% y
	}
str(launch)
reg(y = launch$distress_ct, x = launch[3:5])

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age", "bmi", "children")])
pairs(insurance[c("age", "bmi", "children", "charges")])
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model
summary(ins_model)

insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
charges <- insurance$bmi30 + insurance$smoker + insurance$bmi30:insurance$smoker
ins_model2 <- lm(charges ~ age +insurance$age2 + 
							children + bmi + sex + bmi30*smoker + region, 
	data = insurance)
summary(ins_model2)

wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)
MAE <- function(actual, predicted)
	{
		mean(abs(actual - predicted))
	}
MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)
mean_abserror(5.87, wine_test$quality)

library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)

#第七章2020.03.18#
concrete <- read.csv("concrete.csv")
str(concrete)
normalize <- function(x)
{
	return((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
library(neuralnet)
concrete_model <- neuralnet(strength ~ cement + slag + ash + 
								water + superplastic + coarseagg + fineagg + age, 
								data = concrete_train)
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

concrete_model2 <- neuralnet(strength ~ cement + slag + ash
									+ water + superplastic + coarseagg + fineagg + age, 
									data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

letters <- read.csv("letterdata.csv")
str(letters)
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

#第八章2020.03.16#
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:5])
image(sample(groceries, 100))

apriori(groceries)
groceryrules <- apriori(groceries, parameter = 
							list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules
summary(groceryrules)
inspect(groceryrules[1:3])
inspect(sort(groceryrules, by = "lift")[1:5])
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)
fruitrules <- subset(groceryrules, items %pin% "fruit")
inspect(fruitrules)

write(groceryrules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

#第九章2020.03.16#
teens <- read.csv("snsdata.csv")
str(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) {mean(x, na.rm = TRUE)})
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

interests <- teens[5: 40]
interests_z <- as.data.frame(lapply(interests, scale))
teen_clusters <- kmeans(interests_z, 5)
teen_clusters$size
teen_clusters$centers

#第十章2020.03.14#来自第四章
sms_results <- read.csv("sms_results.csv")
head(sms_results)

head(subset(sms_results, actual_type != predict_type))

table(sms_results$actual_type, sms_results$predict_type)

library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

library(vcd)
Kappa(sms_results$actual_type, sms_results$predict_type)

library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam, 
										labels = sms_results$actual_type)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500], ]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

is_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

folds <- createFolds(credit$default, k = 10)
str(folds)

credit01_train <- credit[folds$Fold01, ]
credit01_test <- credit[-folds$Fold01, ]

library(caret)
library(C50)
library(irr)
set.seed(123)
folds <- createFolds(credit$default, k = 10)
cv_results <- lapply(folds, function(x)
				{
					credit_train <- credit[x, ]
					credit_test <- credit[-x, ]
					credit_model <- C5.0(default ~ ., data = credit_train)
					credit_pred <- predict(credit_model, credit_test)
					credit_actual <- credit_test$default
					kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
					return(kappa)
				})
str(cv_results)
mean(unlist(cv_results))

#第十一章2020.03.16#有问题
library(ggplot2)
library(lattice)
library(caret)
set.seed(300)
credit <- read.csv("credit.csv")
m <- train(default ~ ., data = credit, method = "C5.0")
str(m)

p = predict(m, credit)
table(p, credit$default)

head(predict(m, credit))
head(predict(m, credit, type = "prob"))

ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid <- expand.grid(.model = "tree", .trials = 
							c(1, 5, 10, 15, 20, 25, 30, 35), .winnow = "FALSE")

set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa",
			trControl = ctrl, tuneGrid = grid)
m

library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag", trControl = ctrl)

str(svmBag)
svmBag$fit
bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, aggregate = svmBag$aggregate)
set.seed(300)
svmbag <- train(default ~ ., data = credit, "bag", trControl = ctrl, bagControl = bagctrl)
svmbag

library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

library(caret)
ctrl < trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf", 
				metric = "kappa", trControl = ctrl, tuneGrid = grid_rf)
grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40), 
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa", 
					trControl = ctrl, tuneGrid = grid_c50)
m_rf
m_c50



