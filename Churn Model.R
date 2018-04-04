logon=read.csv(file="log_on.csv")

library(lubridate)
library(dplyr)
library(ggplot2)
library(sqldf)
library(corrplot)
library(glmnet)

str(logon)


head(logon,10)

head(finaldata,2)
###merge:
x=finaldata%>%
  select(member_id,activity_play_id,activity_timestamp_start,month)



x$activity_play_id=as.character(x$activity_play_id)
logon$activity_play_id=as.character(logon$activity_play_id)
str(logon)
y=merge(logon, x, by.x ="activity_play_id",by.y="activity_play_id",
        all.x = TRUE)

head(logon_new)

head(y,5)
logon_new=y%>%
  select(1,2,3,13,12,5,6,7,8,9,10)


#logon_new$recency_logon=as.numeric(logon_new$recency_logon)

logon_new$recency_logon=as.character(logon_new$recency_logon)
write.csv(logon_new, file="log_on_new2.csv", row.names = FALSE)

logon_new$recency_logon

may=logon_new%>%
  filter(month==5)%>%
  filter(recency_logon != "NULL")

may$recency_logon=as.character(may$recency_logon)


may_recency=may%>%
  select(recency_logon)%>%
  group_by(recency_logon)%>%
  mutate(n_count=n())%>%
  arrange(as.numeric(recency_logon))%>%
  distinct(n_count)%>%
  mutate(percent=round(100*n_count/2874,2))


may_recency$cum_percent=cumsum(may_recency$percent)
may_recency

 ggplot(data=may_recency, aes(x=as.numeric(recency_logon), y=percent)) +
  geom_bar(stat="identity", fill=cum_percent>0.95)+xlab("recency_logon")+ylab("percent")+
   ggtitle("Distribution of Number of Dyays between Log In")+
   geom_text(aes(cum_percent)) 
 
 ggplot(data=may_recency, aes(x=as.numeric(recency_logon), y=cum_percent)) +
   geom_bar(stat="identity")+xlab("recency_logon")+ylab("percent")+
   ggtitle("Cumulative Percentage of Number of Dyays between Log In")+
   geom_text(aes(cum_percent)) 

###sample data from may to aug, to reduce bias rather than since march.
 
 
 may_to_aug=logon_new%>%
   filter(month %in% c(5,6,7,8))%>%
   filter(recency_logon != "NULL")
 
 may2aug_recency=may%>%
   select(recency_logon)%>%
   group_by(recency_logon)%>%
   mutate(n_count=n())%>%
   arrange(as.numeric(recency_logon))%>%
   distinct(n_count)%>%
   mutate(percent=round(100*n_count/29424,2))
 
 may2aug_recency$cum_percent=cumsum(may_recency$percent)
 
 View(may2aug_recency)
 write.csv(may2aug_recency, file="churn_threshold.csv", row.names = FALSE)

 ####seperate churners an non_churnders
 ##if a user has been inactive for 23 days, then consider him a churners
 # at the significant level of 95%.
 
 churners=logon_new%>%
   filter(recency_logon != "NULL")%>%
   filter(recency_logon >23)
 
 churners_member_id=churners%>%
   select(member_id.x)
  
churners_member_id=unique(churners_member_id$member_id.x)
str(churners_member_id)

### there are 6077 people are churners 
churners_member_id=data.frame(churners_member_id)
churners_member_id$churners=churners_member_id$churners_member_id


logon_label=merge(logon_new, churners_member_id, 
                   by.x ="member_id.x", by.y="churners_member_id",
           all.x=TRUE )

str(logon_label[,11])
View(logon_label)

#logon_label$churn=ifelse(logon_label$churners=="NA", 1,0)
str(logon_label$churn)
summary(logon_label$churn)

logon_label=logon_label%>%
  mutate(churn1=ifelse(member_id.x %in% churners_member_id$churners_member_id,
                       1,0))
summary(logon_label1$churn1)

summary(finaldata)

###cohort in sept: filter active users in september
#aggregated features of them in past 23 day window.

sep_users=logon_label%>%
  filter(month==9)

##active_user
user_id=sep_users%>%
  select(member_id.x)

user_id=unique(user_id$member_id.x)
user_id=data.frame(user_id)

###
active_user=read.csv(file="churn_raw.csv")
churn_final=active_user%>%
  mutate(churn=ifelse(member_id %in% churners_member_id$churners_member_id,
                       1,0))

View(churn_final)


write.csv(churn_final, file = "churn_model.csv")

head(churn_final)

churn_final=read.csv(file="churn_model.csv")

##correlation
head(churn_final)
churn_corr=data.frame(churn_final[,c(7,8,9,10,11,12)])
corrplot(churn_corr, method="circle",type="upper")

####logistic regression
set.seed(1)

head(churn_final)
data1=data.frame(churn_final[,c(18,7:17)])
str(data1)
data1$avg_score_23days=as.numeric(data1$avg_score_23days)

data1$churn=as.numeric(data1$churn)

test = sample(1:nrow(data1), nrow(data1)*0.2)
test.data=data1[test,]
train.data=data1[-test,]
head(test)

logistic_model = glm( churn ~ ., data = train.data, family = "binomial")
summary(logistic_model)

logistic_probs = predict(logistic_model, test.data, type = "response")

###roc
library(ROCR)

#ROCRpred<-prediction(pred,obs)
#plot(performance(ROCRpred, measure = 'tpr', x.measure = 'fpr'))

ROCpred=prediction(logistic_probs, test.data$churn)
rocrPERF=performance(ROCpred,'tpr','fpr')
plot(rocrPERF,colorize = T, text.adj=c(-0.2,1.7))

###set threshold 
cutoff=data.frame(cut=rocrPERF@alpha.values[[1]], 
                  fpr=rocrPERF@x.values[[1]], 
                  tpr=rocrPERF@y.values[[1]])

cutoff[cutoff$tpr>0.80 & cutoff$fpr<0.5,]
###cutoff 0.61



logistic_pred_y = rep(0, length(logistic_probs))
logistic_pred_y[logistic_probs >= 0.59] = 1

accuracy(logistic_probs,test.data$churn)

View(data1)


