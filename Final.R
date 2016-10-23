

setwd("/Users/ankur/Documents/Competitions/OCT")
getwd()

library(sqldf)
library(lubridate)
library(dplyr)
library(xgboost)
library(DataCombine)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(flexclust)
library(rattle)

train = read.csv("train.csv")
test = read.csv("test.csv")
first = read.csv("First_Health_Camp_Attended.csv")
second = read.csv("Second_Health_Camp_Attended.csv")
third = read.csv("Third_Health_Camp_Attended.csv")
camp = read.csv("Health_Camp_Detail.csv")
patient = read.csv("Patient_Profile.csv")

train$Registration_Date = as.Date(train$Registration_Date, format = "%d-%b-%y")
test$Registration_Date = as.Date(test$Registration_Date, format = "%d-%b-%y")
camp$Camp_Start_Date = as.Date(camp$Camp_Start_Date, format = "%d-%b-%y")
camp$Camp_End_Date = as.Date(camp$Camp_End_Date, format = "%d-%b-%y")
patient$First_Interaction = as.Date(patient$First_Interaction , format = "%d-%b-%y")

train$T_F = 1
test$T_F = 0

tt = rbind(train,test)
tt = merge(tt,camp)
tt$Registration_Date[is.na(tt$Registration_Date)] = tt$Camp_Start_Date

tt$camp_dur = as.integer(tt$Camp_End_Date - tt$Camp_Start_Date)
tt$Reg_lag = ifelse(tt$Registration_Date < tt$Camp_Start_Date ,tt$Camp_Start_Date - tt$Registration_Date ,0  )
tt$Reg_lead_SD = ifelse(tt$Registration_Date > tt$Camp_Start_Date ,tt$Registration_Date - tt$Camp_Start_Date ,0  )
tt$Reg_lag_ED = ifelse(tt$Camp_End_Date > tt$Registration_Date, tt$Camp_End_Date - tt$Registration_Date,0)

tt_flags =sqldf("select Patient_ID,Health_Camp_ID, 
                (case when Category1  = 'First' then 1 else 0 end) as Format1,
                (case when Category1  = 'Second' then 1 else 0  end) as Format2,
                (case when Category1 = 'Third' then 1 else 0  end) as Format3,
                (case when Category2 = 'A' then 1 else 0  end  ) as CatA,
                (case when Category2 = 'B' then 1 else 0  end  ) as CatB,
                (case when Category2 = 'C' then 1 else 0  end  ) as CatC,
                (case when Category2 = 'D' then 1 else 0  end  ) as CatD,
                (case when Category2 = 'E' then 1 else 0  end  ) as CatE,
                (case when Category2 = 'F' then 1 else 0  end  ) as CatF,
                (case when Category2 = 'G' then 1 else 0  end  ) as CatG,
                (case when Category3 = 1 then 1 else 0  end  ) as Cat1,
                (case when Category3 = 2 then 1 else 0  end  ) as Cat2
                from tt as a  ")


tt = tt %>% arrange(Patient_ID,Category1,Camp_Start_Date) %>% group_by(Patient_ID,Category1) %>% mutate(lead_SD_f = lead(Camp_Start_Date)) 
tt$lead_SD_f[which(is.na(tt$lead_SD_f))] = as.Date("1900-01-01")
tt$Next_SD_dif_f = ifelse(tt$lead_SD_f == as.Date("1900-01-01") , 0, tt$lead_SD_f - tt$Camp_Start_Date )
tt$OLAP_f = ifelse(tt$camp_dur > tt$Next_SD_dif_f & tt$Next_SD_dif_f > 0 ,1,0 )

tt = tt %>% arrange(Camp_Start_Date) %>% group_by(Patient_ID) %>% mutate(lead_SD_o = lead(Camp_Start_Date)) 
tt$lead_SD_o[which(is.na(tt$lead_SD_o))] = as.Date("1900-01-01")
tt$Next_SD_dif_o = ifelse(tt$lead_SD_o == as.Date("1900-01-01") , 0, tt$lead_SD_o - tt$Camp_Start_Date )
tt$OLAP_o = ifelse(tt$camp_dur > tt$Next_SD_dif_o & tt$Next_SD_dif_o > 0 ,1,0 )

tt = tt %>% arrange(Camp_Start_Date) %>% group_by(Patient_ID) %>% mutate(lead_SD_o = lead(Camp_Start_Date)) 
tt$lead_SD_o[which(is.na(tt$lead_SD_o))] = as.Date("1900-01-01")
tt$Next_SD_dif_o = ifelse(tt$lead_SD_o == as.Date("1900-01-01") , 0, tt$lead_SD_o - tt$Camp_Start_Date )
tt$OLAP_o = ifelse(tt$camp_dur > tt$Next_SD_dif_o & tt$Next_SD_dif_o > 0 ,1,0 )
tt$reg_diff_SD = as.integer(tt$Camp_Start_Date - tt$Registration_Date)


tt = tt %>% arrange(Patient_ID,Category1,Registration_Date)  %>% group_by(Patient_ID,Category1) %>% mutate(lead = lead(Registration_Date), lag = lag(Registration_Date)) 
tt$lead[is.na(tt$lead)] = as.Date("2007-02-09")
tt$lag[is.na(tt$lag)] = as.Date("2003-10-19")
tt$lead_f = as.integer(tt$lead - tt$Registration_Date)
tt$lag_f = as.integer(tt$Registration_Date - tt$lag)


tt = tt %>% arrange(Patient_ID, Registration_Date) %>% group_by(Patient_ID) %>% mutate(lead = lead(Registration_Date), lag = lag(Registration_Date)) 
tt$lead[is.na(tt$lead)] = as.Date("2007-02-09")
tt$lag[is.na(tt$lag)] = as.Date("2003-10-19")
tt$lead_o = as.integer(tt$lead - tt$Registration_Date)
tt$lag_o = as.integer(tt$Registration_Date - tt$lag)

tt = tt %>% arrange(Patient_ID, Registration_Date) %>% group_by(Patient_ID) %>% mutate(lead = lead(Registration_Date), lag = lag(Registration_Date)) 
tt$lead[is.na(tt$lead)] = as.Date("2007-02-09")
tt$lag[is.na(tt$lag)] = as.Date("2003-10-19")
tt$lead_o = as.integer(tt$lead - tt$Registration_Date)
tt$lag_o = as.integer(tt$Registration_Date - tt$lag)

tt2 = tt %>% arrange(Registration_Date) %>% group_by(Patient_ID,Category1) %>%  summarise(max_visit_f = max(Registration_Date),
                                                                                          min_visit_f = min(Registration_Date),
                                                                                          num_visit_f = n())
tt2$diff = as.integer(tt2$max_visit_f - tt2$min_visit_f + 1)
tt3 = tt %>% arrange(Registration_Date) %>% group_by(Patient_ID) %>%  summarise(max_visit_o = max(Registration_Date),
                                                                                min_visit_o = min(Registration_Date),
                                                                                num_visit_o = n())
tt3$diff = as.integer(tt3$max_visit_o - tt3$min_visit_o + 1)

#Recency
tt$Rec1 = as.integer(as.Date("2007-02-09") - tt$Registration_Date ) 
tt4_f = sqldf("select Patient_ID,Category1 , min(Rec1) as recency_f from tt group by Patient_ID,Category1")
tt4_o = sqldf("select Patient_ID , min(Rec1) as recency_o from tt group by Patient_ID")


tt5 = sqldf("select Patient_ID , 
            count(case when Category1  = 'First' then Health_Camp_ID end) as F1_reg,
            count(case when Category1  = 'Second' then Health_Camp_ID end) as F2_reg,
            count(case when Category1 = 'Third' then Health_Camp_ID end) as F3_reg
            from tt group by Patient_ID ")

#patient

sum(is.na(patient$Income))

patient$mbr_crt_dt =  as.integer(as.Date("2007-02-06") - patient$First_Interaction )
patient$age_imp = ifelse(patient$Age == 'None' , -1, patient$Age)
patient$inc_imp = ifelse(patient$Income == 'None' , -1, patient$Income)
patient$edu_imp = ifelse(patient$Education_Score == 'None' , -1, patient$Education_Score)
patient$city_imp = as.factor(ifelse(patient$City_Type == 'None' , 'U', patient$City_Type))
patient$emp_cat_imp = as.factor(ifelse(patient$Employer_Category == 'None' , 'U', patient$Employer_Category))
patient$age_imp_f = ifelse(patient$Age == 'None' , 0, 1 )
patient$inc_imp_f = ifelse(patient$Income == 'None' , 0, 1)
patient$edu_imp_f = ifelse(patient$Education_Score == 'None' , 0, 1)
patient$city_imp_f = ifelse(patient$City_Type == 'None' , 0,1)
patient$emp_cat_imp_f = ifelse(patient$Employer_Category == 'None' , 0 , 1)
patient$info_flag = (patient$age_imp_f + patient$inc_imp_f + patient$edu_imp_f +patient$city_imp_f +patient$emp_cat_imp_f)


str(patient)

patient1   = sqldf("select Patient_ID,Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared,mbr_crt_dt,age_imp,
                  info_flag,age_imp_f,inc_imp_f,edu_imp_f,city_imp_f,emp_cat_imp_f,
                   inc_imp, edu_imp,city_imp,emp_cat_imp,max(Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared) as social from patient")


#dep
first1 = sqldf("select Patient_ID , Health_Camp_ID, 1 as Dep from first ")
second1 = sqldf("select Patient_ID , Health_Camp_ID, 2 as Dep from second ")
third1 = sqldf("select Patient_ID , Health_Camp_ID, 3 as Dep from third")

consol = rbind(first1,second1,third1)

#final table

str(tt_flags)

data_con = sqldf("select a.Patient_ID, a.Health_Camp_ID , a.Registration_Date,Var1,Var2,Var3, Var4,Var5, T_F,a.Category1,Category2,
                 Category3,camp_dur,reg_diff_SD ,Reg_lag,Reg_lead_SD,Reg_lag_ED,Next_SD_dif_f,OLAP_f,Next_SD_dif_o,OLAP_o,lead_f,lag_f,
                 lead_o, lag_o, a.Rec1 ,b.num_visit_f , b.diff as diff_f,c.num_visit_o,c.diff as diff_o ,
                 case when c.num_visit_o - b.num_visit_f = 0 then 0 else 1 end as multi_for_flag,
                 Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared,mbr_crt_dt,age_imp,social,inc_imp,edu_imp,city_imp,emp_cat_imp,
                 info_flag,age_imp_f,inc_imp_f,edu_imp_f,city_imp_f,emp_cat_imp_f,
                 Format1,Format2,Format3,CatA,CatB,CatC,CatD,CatE,CatF,CatG,Cat1,Cat2,g.recency_f,h.recency_o , 
                 i.F1_reg, i.F2_reg,i.F3_reg , e.Dep 
                 from tt as a left join tt2 as b on a.Patient_ID = b.Patient_ID and a.Category1 = b.Category1
                 left join tt3 as c on a.Patient_ID = c.Patient_ID
                 left join patient1 as d on  a.Patient_ID = d.Patient_ID
                 left join consol as e on  a.Patient_ID = e.Patient_ID and a.Health_Camp_ID = e.Health_Camp_ID
                 left join tt_flags as f on a.Patient_ID = f.Patient_ID and a.Health_Camp_ID = f.Health_Camp_ID
                 left join tt4_f as g on a.Patient_ID = g.Patient_ID and a.Category1 = g.Category1
                 left join tt4_o as h on a.Patient_ID = h.Patient_ID 
                 left join tt5 as i on a.Patient_ID = i.Patient_ID ")


data_con$Dep[which(is.na(data_con$Dep))] = 0
data_con$dependent = ifelse(data_con$Dep > 0 ,1,0)

train_con = filter(data_con, T_F == 1 )
test_con = filter(data_con, T_F == 0)

test_con$dependent = NULL
test_con$Dep = NULL

rp = train_con[,c(4:8,13:65,67)]

set.seed(1234)
fita <- rpart(dependent ~ .
              , data = rp , method="class" , control=rpart.control(minsplit=2, cp=0.001))

fancyRpartPlot(fita)

train_con$num_visit_f5 = ifelse(train_con$num_visit_f  < 5.5 , 1,0)
test_con$num_visit_f5 = ifelse(test_con$num_visit_f  < 5.5 , 1,0)

feature.names <- colnames(train_con[,c(1,2,4:8,13:65,68)])
cat("Feature Names\n")
feature.names

nrow(train_con)
h<-sample(nrow(train_con),20)

dval<-xgb.DMatrix(data=data.matrix(train_con[h,feature.names]),label=(train_con$dependent [h]))
dtrain<-xgb.DMatrix(data=data.matrix(train_con[-h,feature.names]),label=(train_con$dependent[-h] ))

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 7, #changed from default of 8
                subsample           = .99, # 0.7
                colsample_bytree    = 0.99 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1000, # 1800, 
                    verbose             = 0,  #1
                    #early.stop.round    = 150,
                    watchlist           = watchlist
                    #maximize            = FALSE
)


pred <- predict(clf, data.matrix(test_con[,feature.names]))
submission <- data.frame(Patient_ID =test_con$Patient_ID,Health_Camp_ID = test_con$Health_Camp_ID, Outcome = pred)
write.csv(submission , "submission2.csv",row.names = FALSE)

importance_matrix <- xgb.importance(feature.names, model = clf)
#submission = rbind(submission1,submission2,submission3)

#individual models

#first

train_con1 = filter(train_con , Category1 == 'First' )
test_con1 = filter(test_con , Category1 == 'First' )


feature.names1 <- colnames(train_con1[,c(1,2,4:8,13:65)])
cat("Feature Names\n")
feature.names1

nrow(train_con1)
h<-sample(nrow(train_con1),100)

dval1<-xgb.DMatrix(data=data.matrix(train_con1[h,feature.names1]),label=(train_con1$dependent [h]))
dtrain1<-xgb.DMatrix(data=data.matrix(train_con1[-h,feature.names1]),label=(train_con1$dependent[-h] ))

watchlist<-list(val=dval1,train=dtrain1)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 6, #changed from default of 8
                subsample           = 0.99, # 0.7
                colsample_bytree    = 0.99 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf1 <- xgb.train(   params              = param, 
                     data                = dtrain1, 
                     nrounds             = 1000, # 1800, 
                     verbose             = 0,  #1
                     #early.stop.round    = 150,
                     watchlist           = watchlist
                     #maximize            = FALSE
)


pred1 <- predict(clf1, data.matrix(test_con1[,feature.names1]))
submission_1 <- data.frame(Patient_ID =test_con1$Patient_ID,Health_Camp_ID = test_con1$Health_Camp_ID, Outcome = pred1)
write.csv(submission_1 , "submission_1.csv",row.names = FALSE)


submission_all_1 = merge(x = submission, y = submission_1, 
                         by.x = c("Patient_ID","Health_Camp_ID") , 
                         by.y = c("Patient_ID","Health_Camp_ID") , 
                         all.x = TRUE)

submission_all_1$Outcome.y[which(is.na(submission_all_1$Outcome.y))] = submission_all_1$Outcome.x
submission_all_1$Outcome = submission_all_1$Outcome.y
submission_all_1$Outcome = (submission_all_1$Outcome.x + submission_all_1$Outcome.y)/2

submission_all_1 = submission_all_1[,c(1,2,5)]
write.csv(submission_all_1 , "submission_all_1.csv",row.names = FALSE)

#second

train_con2 = filter(train_con , Category1 == 'Second' )
test_con2 = filter(test_con , Category1 == 'Second' )


feature.names2 <- colnames(train_con2[,c(4:8,13:65)])
cat("Feature Names\n")
feature.names2

nrow(train_con2)
h<-sample(nrow(train_con2),100)

dval2<-xgb.DMatrix(data=data.matrix(train_con2[h,feature.names2]),label=(train_con2$dependent [h]))
dtrain2<-xgb.DMatrix(data=data.matrix(train_con2[-h,feature.names2]),label=(train_con2$dependent[-h] ))

watchlist<-list(val=dval2,train=dtrain2)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 6, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf2 <- xgb.train(   params              = param, 
                     data                = dtrain2, 
                     nrounds             = 250, # 1800, 
                     verbose             = 0,  #1
                     #early.stop.round    = 150,
                     watchlist           = watchlist
                     #maximize            = FALSE
)


pred2 <- predict(clf2, data.matrix(test_con2[,feature.names2]))
submission_2 <- data.frame(Patient_ID =test_con2$Patient_ID,Health_Camp_ID = test_con2$Health_Camp_ID, Outcome = pred2)










#third

train_con3 = filter(train_con , Category1 == 'Third' )
test_con3 = filter(test_con , Category1 == 'Third' )


feature.names3 <- colnames(train_con3[,c(4:8,13:65)])
cat("Feature Names\n")
feature.names3

nrow(train_con3)
h<-sample(nrow(train_con3),100)

dval3<-xgb.DMatrix(data=data.matrix(train_con3[h,feature.names3]),label=(train_con3$dependent [h]))
dtrain3<-xgb.DMatrix(data=data.matrix(train_con3[-h,feature.names3]),label=(train_con3$dependent[-h] ))

watchlist<-list(val=dval3,train=dtrain3)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 6, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf3 <- xgb.train(   params              = param, 
                     data                = dtrain3, 
                     nrounds             = 250, # 1800, 
                     verbose             = 0,  #1
                     #early.stop.round    = 150,
                     watchlist           = watchlist
                     #maximize            = FALSE
)


pred3 <- predict(clf3, data.matrix(test_con3[,feature.names3]))
submission_3 <- data.frame(Patient_ID =test_con3$Patient_ID,Health_Camp_ID = test_con3$Health_Camp_ID, Outcome = pred3)

submission_c = rbind(submission_1,submission_2,submission_3)
write.csv(submission_c , "submission_c.csv",row.names = FALSE)

sdf = sqldf("select a.Patient_ID ,a.Health_Camp_ID , a.Outcome as O1, b.Outcome as O2
            from submission as a inner join submission_c as b
            on a.Patient_ID = b.Patient_ID and a.Health_Camp_ID = b.Health_Camp_ID")

write.csv(sdf , "sdf.csv",row.names = FALSE)
