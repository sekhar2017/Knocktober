
setwd("/Users/ankur/Documents/Competitions/OCT")
getwd()

library(sqldf)
library(lubridate)
library(dplyr)
library(xgboost)
library(DataCombine)

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


train1 = train %>% arrange(Registration_Date) %>% group_by(Patient_ID) %>% mutate(lead = lead(Registration_Date), lag = lag(Registration_Date))

str(train)
#train

patient$Inter_rec = as.integer(as.Date("2007-02-06") - patient$First_Interaction )
train_int_mb2 = filter(patient,  patient$Inter_rec  > 1095 &  patient$Inter_rec  <= 1460)
train_int_mb1 = filter(patient,  patient$Inter_rec  > 730 &  patient$Inter_rec  <= 1095)
train_int_mb = filter(patient,  patient$Inter_rec  > 365 &  patient$Inter_rec  <= 730)
test_int_mb = filter(patient,  patient$Inter_rec  <= 365 )

train_int_mb2$Recent = train_int_mb2$Inter_rec - 1095
train_int_mb1$Recent = train_int_mb1$Inter_rec - 730
train_int_mb$Recent = train_int_mb$Inter_rec - 366
test_int_mb$Recent = test_int_mb$Inter_rec - 0

train_int_mb2   = sqldf("select Patient_ID,Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared,Recent,
                     max(Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared) as social from train_int_mb2")

train_int_mb1   = sqldf("select Patient_ID,Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared,Recent,
                     max(Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared) as social from train_int_mb1")


train_int_mb   = sqldf("select Patient_ID,Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared,Recent,
                     max(Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared) as social from train_int_mb")

test_int_mb   = sqldf("select Patient_ID,Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared,Recent,
                     max(Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared) as social from test_int_mb")


train_hc = merge(train,camp)
test_hc = merge(test,camp)
tt_hc = rbind(train_hc,test_hc)

train_hc02 = filter(tt_hc,  tt_hc$Registration_Date  < as.Date('2004-02-07') & tt_hc$Registration_Date  > as.Date('2003-02-07') )
train_hc01 = filter(tt_hc,  tt_hc$Registration_Date  < as.Date('2005-02-07') & tt_hc$Registration_Date  > as.Date('2004-02-07') )
train_hc0 = filter(tt_hc,  tt_hc$Registration_Date  < as.Date('2006-02-07') & tt_hc$Registration_Date  > as.Date('2005-02-07') )
test_hc0 = filter(tt_hc,  tt_hc$Registration_Date  >= as.Date('2006-02-07'))

train_hc20 = sqldf("select Patient_ID , count(Health_Camp_ID) as num_reg, avg(Var1) as v1, 
               avg(Var2) as v2, max(Var3) as v3,
                   avg(Var4) as v4,avg(Var5) as v5, 
                   count(case when Category1  = 'First' then Health_Camp_ID end) as Format1,
                   count(case when Category1  = 'Second' then Health_Camp_ID end) as Format2,
                   count(case when Category1 = 'Third' then Health_Camp_ID end) as Format3,
                   count(case when Category2 = 'A' then Health_Camp_ID end  ) as CatA,
                   count(case when Category2 = 'B' then Health_Camp_ID end  ) as CatB,
                   count(case when Category2 = 'C' then Health_Camp_ID end  ) as CatC,
                   count(case when Category2 = 'D' then Health_Camp_ID end  ) as CatD,
                   count(case when Category2 = 'E' then Health_Camp_ID end  ) as CatE,
                   count(case when Category2 = 'F' then Health_Camp_ID end  ) as CatF,
                   count(case when Category2 = 'G' then Health_Camp_ID end  ) as CatG,
                   count(case when Category3 = 1 then Health_Camp_ID end  ) as Cat1,
                   count(case when Category3 = 2 then Health_Camp_ID end  ) as Cat2
                   from train_hc02 where Patient_ID in (select Patient_ID from train_int_mb2)
                   group by Patient_ID
                   ")

train_hc10 = sqldf("select Patient_ID , count(Health_Camp_ID) as num_reg, avg(Var1) as v1, 
               avg(Var2) as v2, max(Var3) as v3,
                  avg(Var4) as v4,avg(Var5) as v5, 
                  count(case when Category1  = 'First' then Health_Camp_ID end) as Format1,
                  count(case when Category1  = 'Second' then Health_Camp_ID end) as Format2,
                  count(case when Category1 = 'Third' then Health_Camp_ID end) as Format3,
                  count(case when Category2 = 'A' then Health_Camp_ID end  ) as CatA,
                  count(case when Category2 = 'B' then Health_Camp_ID end  ) as CatB,
                  count(case when Category2 = 'C' then Health_Camp_ID end  ) as CatC,
                  count(case when Category2 = 'D' then Health_Camp_ID end  ) as CatD,
                  count(case when Category2 = 'E' then Health_Camp_ID end  ) as CatE,
                  count(case when Category2 = 'F' then Health_Camp_ID end  ) as CatF,
                  count(case when Category2 = 'G' then Health_Camp_ID end  ) as CatG,
                  count(case when Category3 = 1 then Health_Camp_ID end  ) as Cat1,
                  count(case when Category3 = 2 then Health_Camp_ID end  ) as Cat2
                  from train_hc01 where Patient_ID in (select Patient_ID from train_int_mb1)
                  group by Patient_ID
                  ")

train_hc1 = sqldf("select Patient_ID , count(Health_Camp_ID) as num_reg, avg(Var1) as v1, 
               avg(Var2) as v2, max(Var3) as v3,
               avg(Var4) as v4,avg(Var5) as v5, 
               count(case when Category1  = 'First' then Health_Camp_ID end) as Format1,
               count(case when Category1  = 'Second' then Health_Camp_ID end) as Format2,
               count(case when Category1 = 'Third' then Health_Camp_ID end) as Format3,
               count(case when Category2 = 'A' then Health_Camp_ID end  ) as CatA,
               count(case when Category2 = 'B' then Health_Camp_ID end  ) as CatB,
               count(case when Category2 = 'C' then Health_Camp_ID end  ) as CatC,
               count(case when Category2 = 'D' then Health_Camp_ID end  ) as CatD,
               count(case when Category2 = 'E' then Health_Camp_ID end  ) as CatE,
               count(case when Category2 = 'F' then Health_Camp_ID end  ) as CatF,
               count(case when Category2 = 'G' then Health_Camp_ID end  ) as CatG,
               count(case when Category3 = 1 then Health_Camp_ID end  ) as Cat1,
               count(case when Category3 = 2 then Health_Camp_ID end  ) as Cat2
               from train_hc0 where Patient_ID in (select Patient_ID from train_int_mb)
               group by Patient_ID
               ")

test_hc1 = sqldf("select Patient_ID , count(Health_Camp_ID) as num_reg, avg(Var1) as v1, 
               avg(Var2) as v2, max(Var3) as v3,
                  avg(Var4) as v4,avg(Var5) as v5, 
                  count(case when Category1  = 'First' then Health_Camp_ID end) as Format1,
                  count(case when Category1  = 'Second' then Health_Camp_ID end) as Format2,
                  count(case when Category1 = 'Third' then Health_Camp_ID end) as Format3,
                  count(case when Category2 = 'A' then Health_Camp_ID end  ) as CatA,
                  count(case when Category2 = 'B' then Health_Camp_ID end  ) as CatB,
                  count(case when Category2 = 'C' then Health_Camp_ID end  ) as CatC,
                  count(case when Category2 = 'D' then Health_Camp_ID end  ) as CatD,
                  count(case when Category2 = 'E' then Health_Camp_ID end  ) as CatE,
                  count(case when Category2 = 'F' then Health_Camp_ID end  ) as CatF,
                  count(case when Category2 = 'G' then Health_Camp_ID end  ) as CatG,
                  count(case when Category3 = 1 then Health_Camp_ID end  ) as Cat1,
                  count(case when Category3 = 2 then Health_Camp_ID end  ) as Cat2
                  from test_hc0 where Patient_ID in (select Patient_ID from test_int_mb)
                  group by Patient_ID
                  ")

conv = sqldf("select Patient_ID ,Health_Camp_ID , 1 as conv from first
             union select Patient_ID ,Health_Camp_ID , 1 as conv from second
             union select Patient_ID ,Health_Camp_ID , 1 as conv from third ")

conv_perc02 = sqldf("select a.Patient_ID, count(b.Health_Camp_ID) as convert from train_hc02 as a  
                  inner join conv as b
                    on a.Patient_ID = b.Patient_ID
                    and a.Health_Camp_ID = b.Health_Camp_ID
                    group by a.Patient_ID")


conv_perc01 = sqldf("select a.Patient_ID, count(b.Health_Camp_ID) as convert from train_hc01 as a  
                  inner join conv as b
                  on a.Patient_ID = b.Patient_ID
                  and a.Health_Camp_ID = b.Health_Camp_ID
                  group by a.Patient_ID")

conv_perc0 = sqldf("select a.Patient_ID, count(b.Health_Camp_ID) as convert from train_hc0 as a  
                  inner join conv as b
                    on a.Patient_ID = b.Patient_ID
                    and a.Health_Camp_ID = b.Health_Camp_ID
                    group by a.Patient_ID")

str(train_int_mb1)
str(train_hc10)
str(conv_perc01)

final_int = sqldf("select a.Patient_ID,a.Online_Follower,a.LinkedIn_Shared,a.Twitter_Shared,a.Facebook_Shared ,Recent,
                  num_reg,v1,v2,v3,v4,v5,Format1,Format2,Format3,CatA,CatB,CatC,CatD,CatE,CatF,CatG,Cat1,Cat2,social,
                   1 as flag_l,0 as flag_l1,0 as flag_l2, convert as dep
                   from train_int_mb2 as a  
                   inner join train_hc20 as b
                   on a.Patient_ID = b.Patient_ID
                   left join conv_perc02 as c
                   on a.Patient_ID = c.Patient_ID")


final_int0 = sqldf("select a.Patient_ID,a.Online_Follower,a.LinkedIn_Shared,a.Twitter_Shared,a.Facebook_Shared ,Recent,
                  num_reg,v1,v2,v3,v4,v5,Format1,Format2,Format3,CatA,CatB,CatC,CatD,CatE,CatF,CatG,Cat1,Cat2,social,
                  0 as flag_l,1 as flag_l1,0 as flag_l2 , convert as dep
                  from train_int_mb1 as a  
                  inner join train_hc10 as b
                   on a.Patient_ID = b.Patient_ID
                   left join conv_perc01 as c
                   on a.Patient_ID = c.Patient_ID")

final_int1 = sqldf("select a.Patient_ID,a.Online_Follower,a.LinkedIn_Shared,a.Twitter_Shared,a.Facebook_Shared ,Recent,
                  num_reg,v1,v2,v3,v4,v5,Format1,Format2,Format3,CatA,CatB,CatC,CatD,CatE,CatF,CatG,Cat1,Cat2,social,
                   0 as flag_l,0 as flag_l1,1 as flag_l2 , convert as dep
                   from train_int_mb as a  
                   inner join train_hc1 as b
                   on a.Patient_ID = b.Patient_ID
                   left join conv_perc0 as c
                   on a.Patient_ID = c.Patient_ID")

final_int2 = sqldf("select a.Patient_ID,a.Online_Follower,a.LinkedIn_Shared,a.Twitter_Shared,a.Facebook_Shared ,Recent,
                  num_reg,v1,v2,v3,v4,v5,Format1,Format2,Format3,CatA,CatB,CatC,CatD,CatE,CatF,CatG,Cat1,Cat2,social,
                   1 as flag_l,0 as flag_l1,0 as flag_l2  from test_int_mb as a  
                   inner join test_hc1 as b
                   on a.Patient_ID = b.Patient_ID")

train_int = rbind(final_int0,final_int1,final_int)
train_int$dep[is.na(train_int$dep)] = 0

test_int = final_int2

feature.names <- colnames(train_int[,c(2:28)])
cat("Feature Names\n")
feature.names

nrow(train_int)
h<-sample(nrow(train_int),4000)

dval<-xgb.DMatrix(data=data.matrix(train_int[h,feature.names]),label=(train_int$dep[h]))
dtrain<-xgb.DMatrix(data=data.matrix(train_int[,feature.names]),label=(train_int$dep ))

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eval_metric = "rmse",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 6, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 500, # 1800, 
                    verbose             = 0,  #1
                    #early.stop.round    = 150,
                    watchlist           = watchlist
                    #maximize            = FALSE
)


pred1 <- predict(clf, data.matrix(test_int[,feature.names]))
submission <- data.frame(Patient_ID =test_int$Patient_ID,dep = pred1)

submission$dep = round(submission$dep,digits = 0)

final_int2 = merge(final_int2,submission)

#importance_matrix1 <- xgb.importance(feature.names, model = clf)

patient_con_map = rbind(final_int2,final_int1,final_int0,final_int)

str(patient_con_map)
patient_con_map = patient_con_map %>% group_by(Patient_ID) %>% summarise(
  num_reg = max(num_reg), 
  Format1 = max(Format1),
  Format2 = max(Format2),
  Format3 = max(Format3),
  CatA = max(CatA),
  CatB = max(CatB),
  CatC = max(CatC),
  CatD = max(CatD),
  CatE = max(CatE),
  CatF = max(CatF),
  CatG = max(CatG),
  Cat1 = max(Cat1),
  Cat2 = max(Cat2),
  flag_l = max(flag_l),
  flag_l1=max(flag_l1),
  flag_l2 = max(flag_l2),
  dep = max(dep))

train_n <- merge(x = train, y = patient_con_map, 
                by.x = "Patient_ID" , 
                by.y = "Patient_ID", 
                all.x = TRUE)

test_n <- merge(x = test, y = patient_con_map, 
              by.x = "Patient_ID" , 
              by.y = "Patient_ID", 
              all.x = TRUE)

train_n[c(4:25)][is.na(train_n[c(4:25)])] = 0
test_n[c(4:25)][is.na(test_n[c(4:25)])] = 0

train_con = merge(train_n,camp)
train_con = merge(train_con,patient)
                  
test_con = merge(test_n,camp)
test_con = merge(test_con,patient)

  
second$Health_Score = second$Health.Score
                  
train_con1 = sqldf("select a.*,b.Donation,b.Health_Score
from train_con as a  left join first as b
on a.Patient_ID = b.Patient_ID
and a.Health_Camp_ID = b.Health_Camp_ID
where Category1 = 'First'")
                  
train_con2 = sqldf("select a.*,b.Health_Score
from train_con as a  left join second as b
on a.Patient_ID = b.Patient_ID
and a.Health_Camp_ID = b.Health_Camp_ID
where Category1 = 'Second'")

train_con3 = sqldf("select a.*,b.Number_of_stall_visited,b.Last_Stall_Visited_Number
from train_con as a  left join third as b
on a.Patient_ID = b.Patient_ID and a.Health_Camp_ID = b.Health_Camp_ID
where Category1 = 'Third'")
                  
train_con1$Registration_Date[is.na(train_con1$Registration_Date)] = train_con1$Camp_Start_Date
train_con2$Registration_Date[is.na(train_con2$Registration_Date)] = train_con2$Camp_Start_Date
train_con3$Registration_Date[is.na(train_con3$Registration_Date)] = train_con3$Camp_Start_Date
                  
test_con1 = sqldf("select a.*,b.Donation,b.Health_Score
                  from test_con as a  left join first as b
                  on a.Patient_ID = b.Patient_ID
                  and a.Health_Camp_ID = b.Health_Camp_ID
                  where Category1 = 'First'")

test_con2 = sqldf("select a.*,b.Health_Score
                  from test_con as a  left join second as b
                  on a.Patient_ID = b.Patient_ID
                  and a.Health_Camp_ID = b.Health_Camp_ID
                  where Category1 = 'Second'")

test_con3 = sqldf("select a.*,b.Number_of_stall_visited,b.Last_Stall_Visited_Number
                  from test_con as a  left join third as b
                  on a.Patient_ID = b.Patient_ID
                  and a.Health_Camp_ID = b.Health_Camp_ID
                  where Category1 = 'Third'")

test_con1$Registration_Date[is.na(test_con1$Registration_Date)] = test_con1$Camp_Start_Date
test_con2$Registration_Date[is.na(test_con2$Registration_Date)] = test_con2$Camp_Start_Date
test_con3$Registration_Date[is.na(test_con3$Registration_Date)] = test_con3$Camp_Start_Date

train_con11 = sqldf("select a.*,(Registration_Date-Camp_start_Date) as reg_lag,(Camp_End_Date-Camp_start_Date) as camp_dur,
                    max(Online_Follower,LinkedIn_shared,Twitter_shared,Facebook_shared) as social_media_flag,
                    Online_Follower ,LinkedIn_shared ,Twitter_shared, Facebook_shared ,
                    (Registration_Date-First_Interaction) as firstinter_lag
                    from train_con1 as a ")
train_con11$Health_Score[is.na(train_con11$Health_Score)] = 0
train_con11$Health_Score = ifelse(train_con11$Health_Score> 0,1,0)

train_con21 = sqldf("select a.*,(Registration_Date-Camp_start_Date) as reg_lag,(Camp_End_Date-Camp_start_Date) as camp_dur,
                    max(Online_Follower,LinkedIn_shared,Twitter_shared,Facebook_shared) as social_media_flag,
                    Online_Follower ,LinkedIn_shared ,Twitter_shared, Facebook_shared ,
                    (Registration_Date-First_Interaction) as firstinter_lag
                    from train_con2 as a ")
train_con21$Health_Score[is.na(train_con21$Health_Score)] = 0
train_con21$Health_Score = ifelse(train_con21$Health_Score> 0,1,0)

train_con31 = sqldf("select a.*,(Registration_Date-Camp_start_Date) as reg_lag,(Camp_End_Date-Camp_start_Date) as camp_dur,
                    max(Online_Follower,LinkedIn_shared,Twitter_shared,Facebook_shared) as social_media_flag,
                    Online_Follower ,LinkedIn_shared ,Twitter_shared, Facebook_shared ,
                    (Registration_Date-First_Interaction) as firstinter_lag
                    from train_con3 as a ")

train_con31$Number_of_stall_visited[is.na(train_con31$Number_of_stall_visited)] = 0
train_con31$Number_of_stall_visited = ifelse(train_con31$Number_of_stall_visited> 0,1,0)

test_con11 = sqldf("select a.*,(Registration_Date-Camp_start_Date) as reg_lag,(Camp_End_Date-Camp_start_Date) as camp_dur,
                    max(Online_Follower,LinkedIn_shared,Twitter_shared,Facebook_shared) as social_media_flag,
                   Online_Follower ,LinkedIn_shared ,Twitter_shared, Facebook_shared ,
                   (Registration_Date-First_Interaction) as firstinter_lag
                   from test_con1 as a ")

test_con21 = sqldf("select a.*,(Registration_Date-Camp_start_Date) as reg_lag,(Camp_End_Date-Camp_start_Date) as camp_dur,
                   max(Online_Follower,LinkedIn_shared,Twitter_shared,Facebook_shared) as social_media_flag,
                   Online_Follower ,LinkedIn_shared ,Twitter_shared, Facebook_shared ,
                   (Registration_Date-First_Interaction) as firstinter_lag
                   from test_con2 as a ")

test_con31 = sqldf("select a.*,(Registration_Date-Camp_start_Date) as reg_lag,(Camp_End_Date-Camp_start_Date) as camp_dur,
                   max(Online_Follower,LinkedIn_shared,Twitter_shared,Facebook_shared) as social_media_flag,
                   Online_Follower ,LinkedIn_shared ,Twitter_shared, Facebook_shared ,
                   (Registration_Date-First_Interaction) as firstinter_lag
                   from test_con3 as a ")


#first

train_con11$con = train_con11$dep/train_con11$num_reg
test_con11$con = test_con11$dep/test_con11$num_reg

train_con11$con[is.na(train_con11$con)] = 0
test_con11$con[is.na(test_con11$con)] = 0

feature.names1 <- colnames(train_con11[,c(4:25,31:34,45:46)])
cat("Feature Names\n")
feature.names1

nrow(train_con11)
h<-sample(nrow(train_con11),10000)

dval1<-xgb.DMatrix(data=data.matrix(train_con11[h,feature.names1]),label=(train_con11$Health_Score[h]))
dtrain1<-xgb.DMatrix(data=data.matrix(train_con11[-h,feature.names1]),label=(train_con11$Health_Score[-h] ))

watchlist<-list(val=dval1,train=dtrain1)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 8, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf1 <- xgb.train(   params              = param, 
                    data                = dtrain1, 
                    nrounds             = 1800, # 1800, 
                    verbose             = 0,  #1
                    early.stop.round    = 150,
                    watchlist           = watchlist
                    #maximize            = FALSE
)

#importance_matrix1 <- xgb.importance(feature.names1, model = clf1)

pred1 <- predict(clf1, data.matrix(test_con11[,feature.names1]))
submission1 <- data.frame(Patient_ID =test_con11$Patient_ID,Health_Camp_ID = test_con11$Health_Camp_ID, Outcome = pred1)


#second

feature.names2 <- colnames(train_con21[,c(4:25,31:34,41,44:45)])
cat("Feature Names\n")
feature.names2

nrow(train_con21)
h<-sample(nrow(train_con21),3500)

dval2<-xgb.DMatrix(data=data.matrix(train_con21[h,feature.names2]),label=(train_con21$Health_Score[h]))
dtrain2<-xgb.DMatrix(data=data.matrix(train_con21[-h,feature.names2]),label=(train_con21$Health_Score[-h] ))

watchlist<-list(val=dval2,train=dtrain2)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 8, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

clf2 <- xgb.train(   params              = param, 
                     data                = dtrain2, 
                     nrounds             = 1800, # 1800, 
                     verbose             = 0,  #1
                     #early.stop.round    = 150,
                     watchlist           = watchlist
                     #maximize            = FALSE
)


pred2 <- predict(clf2, data.matrix(test_con21[,feature.names2]))
submission2 <- data.frame(Patient_ID =test_con21$Patient_ID,Health_Camp_ID = test_con21$Health_Camp_ID, Outcome = pred2)


#third

feature.names3 <- colnames(train_con31[,c(4:25,31:34,41,45:46)])
cat("Feature Names\n")
feature.names3

nrow(train_con31)
h<-sample(nrow(train_con31),2500)

dval3<-xgb.DMatrix(data=data.matrix(train_con31[h,feature.names3]),label=(train_con31$Number_of_stall_visited [h]))
dtrain3<-xgb.DMatrix(data=data.matrix(train_con31[-h,feature.names3]),label=(train_con31$Number_of_stall_visited[-h] ))

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
                     nrounds             = 1000, # 1800, 
                     verbose             = 0,  #1
                     #early.stop.round    = 150,
                     watchlist           = watchlist
                     #maximize            = FALSE
)


pred3 <- predict(clf3, data.matrix(test_con31[,feature.names3]))
submission3 <- data.frame(Patient_ID =test_con31$Patient_ID,Health_Camp_ID = test_con31$Health_Camp_ID, Outcome = pred3)

submission = rbind(submission1,submission2,submission3)
write.csv(submission , "submission.csv",row.names = FALSE)
