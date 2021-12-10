library(corrplot)
library(dplyr)
library(magrittr)

#readind the data
data <- read.csv("promotion_tr_ppa (1).csv",stringsAsFactors = TRUE)
summary(data)

#changing to factors
data$is_promoted <- factor(data$is_promoted)

#find mean department1 and impute in missing values
data <-data %>%
  mutate(previous_year_rating= ifelse(is.na(previous_year_rating) & length_of_service==1,
                              data %>% filter(length_of_service==1) %>% summarise(mean(previous_year_rating,na.rm=TRUE)),
                                 previous_year_rating))
data$previous_year_rating <- round(as.numeric(data$previous_year_rating),0)

#correlation
data$is_promoted <- as.numeric(data$is_promoted)
cr<-cor(data[c("is_promoted","no_of_trainings","age","previous_year_rating","length_of_service","KPIs_met..80.","awards_won.","avg_training_score")])

corrplot(cr,method = "number")

#splitting the data
library(caTools)
split<- sample.split(data$is_promoted,SplitRatio = 0.7)
train <- subset(data,split=='TRUE')
test1 <- subset(data,split=='FALSE')
View(train)
View(test)

#logistic regression
model <- glm (is_promoted ~ department,data = train)
summary(model)
model1 <- glm (is_promoted ~ department+education,data = train)
summary(model1)
model2 <- glm (is_promoted ~ department+education+gender,data = train)
summary(model2)
model3 <- glm (is_promoted ~ department+education+recruitment_channel,data = train)
summary(model3)
model4 <- glm (is_promoted ~ department+education+recruitment_channel+previous_year_rating,data = train)
summary(model4)
model5 <- glm (is_promoted ~ department+education+recruitment_channel+previous_year_rating+KPIs_met..80.,data = train)
summary(model5)
model6 <- glm (is_promoted ~ department+education+recruitment_channel+previous_year_rating+KPIs_met..80.+awards_won.,data = train)
summary(model6)
model7 <- glm (is_promoted ~ department+education+recruitment_channel+previous_year_rating+KPIs_met..80.+awards_won.+avg_training_score,data = train)
summary(model7)
model8 <- glm (is_promoted ~ department+education+recruitment_channel+previous_year_rating+KPIs_met..80.+awards_won.+avg_training_score+length_of_service,data = train)
summary(model8)
model9 <- glm(is_promoted ~ previous_year_rating+KPIs_met..80.+awards_won.+avg_training_score+no_of_trainings,data = train)
summary(model9)


#test logi reg

res <- predict(model9,test1,type="response")
head(res)
table(Actualvalue=test1$is_promoted, Predictedvalue= res>0.4)


#ROCR
ROCRpred <- prediction(res,data$is_promoted)
ROCRpref <- performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.2,by=0.3))

#prediction
promotion_ts <- read.csv("promotion_ts_N.csv")
Pred <- predict(model7, newdata = promotion_ts, type= "response")
promotion_ts$LR_is_promoted <- Pred
promotion_ts$LR_is_promoted <- ifelse(promotion_ts$LR_is_promoted >0.3,1,0)
table(promotion_ts$LR_is_promoted)
##__________________DECISION TREES___________________##
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(caret)

promoDT <- rpart(is_promoted ~department+education+recruitment_channel+previous_year_rating+KPIs_met..80.+awards_won.+avg_training_score+no_of_trainings , data = train,parms = list(split='information'))
fancyRpartPlot(promoDT)

pred<- predict(promoDT,newdata = test1,method = "class")
pred

test$is_promoted <- factor(test$is_promoted)
pred$is_promoted <- ifelse(train$is_promoted >1,1,0)
confusionMatrix(data =pred,reference =test$is_promoted,positive = '1')






