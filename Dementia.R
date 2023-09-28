##Loading libraries
library(dplyr)
library(rpart)
library(rpart.plot)
library(pROC)


##Importing data set
Dementia<-read.csv("dataset ICT583 2023.csv")
head(Dementia)
glimpse(Dementia)
##Changing integers to factor variables
Dementia$Sex<-as.factor(Dementia$Gender)
Dementia$MRT_status<-as.factor(Dementia$Marital)
Dementia$ED_level<-as.factor(Dementia$Education_ID)
Dementia$Financial<-as.factor(Dementia$Financial_status)
Dementia$MMSE_class_binary<-factor(Dementia$MMSE_class_binary)

##Data cleaning and transformation.
###Missing values

getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (cols in colnames(Dementia)) {
  if (cols %in% names(Dementia[,sapply(Dementia, is.numeric)])) {
    Dementia<-Dementia%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
    
  }
  else {
    
    Dementia<-Dementia%>%mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
    
  }
}


Dementia<-Dementia %>% mutate(BMI=Body_Weight/(Body_Height/100)^2)
Dementia<-Dementia %>% mutate(Marital=ifelse(Marital_status_ID==2, 1,0))
attach(Dementia)

##Descriptive statistics
#Frequency tables
table(Dementia$Gender)
table(Dementia$MRT_status)
table(Dementia$ED_level)
table(Dementia$MMSE_class_binary)
table(Dementia$Financial)
#Summary statistics
Dementia %>% summarise(Average_age=mean(Age),Average_bmi=mean(BMI),Average_GDS=mean(GDS),Average_MNA=mean(MNAa_total),Average_height=mean(Body_Height,Average_weight=mean(Body_Weight)))

#Data partitioning

ind <- sample(2, nrow(Dementia), replace = T, prob = c(0.8, 0.2))
train <- Dementia[ind == 1,]
test <- Dementia[ind == 2,]
##Logistic regression model. 
DMT_logit<-glm(MMSE_class_binary~Age+Sex+ED_level+GDS+Independent_or_depend_on_family+MNAa_total+BMI, data = train,family = "binomial")
summary(DMT_logit)
##Prediction
Predictited_prob<-predict(DMT_logit, test,type = "response")
Class_prediction<-ifelse(Predictited_prob>0.5, 1,0)
head(Class_prediction)

##Performance metrics
##Model accuracy
mean(Class_prediction==test$MMSE_class_binary)

##ROC curve
ROCL<-roc(test$MMSE_class_binary,Predictited_prob)
plot(ROCL,col="blue")
auc(ROCL)
##Decision tree model
Tree_model<-rpart(MMSE_class_binary~Age+Sex+ED_level+GDS+Independent_or_depend_on_family+MNAa_total+BMI+MRT_status, data = train,cp=0.02)
rpart.plot(Tree_model)
##Model predicition
pr<-predict(Tree_model,
                     newdata = test,
                     type = 'class')
pr<-as.numeric(pr)
##Performance Metrics
##Confusion matrix
CM<-table(test$MMSE_class_binary,pr)
CM
##ROC curve
ROCT<-roc(test$MMSE_class_binary,pr)
plot(ROCT,col="pink")
auc(ROCT)




