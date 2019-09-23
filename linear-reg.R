setwd("D:/Sameera/Online Courses/Analytix Labs/Data Science Using SAS and R/BA/Solutions/R/Linear Regression/New")

require(openxlsx)
datacc<-read.xlsx("D:/Sameera/Online Courses/Analytix Labs/Data Science Using SAS and R/BA/case study/Linear Regression Case.xlsx",sheet = 1)
data=datacc
View(datacc)
str(datacc)


#Some categorical variables have "-1" as a category. Change it to some other appropriate level. Then convert all
#categorical variables to factor from numeric
data$spousedcat[data["spousedcat"]==-1]=6
data$carown[data["carown"]==-1]=2
data$cartype[data["cartype"]==-1]=2
data$carcatvalue[data["carcatvalue"]==-1]=4
data$carbought[data["carbought"]==-1]=2


View(data["cartype"])
View(data["spousedcat"])

#names of variables that have to be converted to factors from numeric data type
names_factor=c("region",
"townsize",
"gender",
"agecat",
"birthmonth",
"edcat",
"jobcat",
"union",
"empcat",
"retire",
"inccat",
"default",
"jobsat",
"marital",
"spousedcat",
"homeown",
"hometype",
"addresscat",
"carown",
"cartype",
"carcatvalue",
"carbought",
"carbuy",
"commute",
"commutecat",
"reason",
"polview",
"polparty",
"polcontrib",
"vote",
"card",
"cardtype",
"cardbenefit",
"cardfee",
"cardtenurecat",
"card2",
"card2type",
"card2benefit",
"card2fee",
"card2tenurecat",
"active",
"bfast",
"churn",
"tollfree",
"equip",
"callcard",
"wireless",
"multline",
"voice",
"pager",
"internet",
"callid",
"callwait",
"forward",
"confer",
"ebill",
"owntv",
"ownvcr",
"owndvd",
"owncd",
"ownpda",
"ownpc",
"ownipod",
"owngame",
"ownfax",
"news",
"response_01",
"response_02",
"response_03")
 #convert the categorical variables into factors
data[,names_factor] <- lapply(data[,names_factor] , factor)

levels(data$spousedcat)
str(data)

#extract the names of numeric variables in vector "num_var"
num_var=names(data[sapply(data,function(x) is.numeric(x))])
num_var=num_var[-(25:34)] #delete variables like commutecar,commutepublic, commutebike etc bcoz
                          # they are already covered in "commute" variable
num_var


stats <- function(x) {
  n=nrow(x)
  a <- x[!is.na(x)]
  nmiss<-sum(is.na(x))
  m <- mean(a)
  std <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  median<-quantile(a,0.5)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*std
  LC <- m-3*std
  outlier_flag<- max>UC | min<LC
  return(c(total_obs=n,nmiss=nmiss, mean=m, stdev=std,min = min, p1=p1,p5=p5, median=median,p95=p95,p99=p99,max=max, UC=UC, LC=LC, outlier_flag=outlier_flag))
}

var_audit_report=data.frame(t(apply(data[num_var],2,stats)))

write.csv(var_audit_report,file="data audit rep.csv") #write the data audit report to a file

#variables to be capped

outlier_capp=c("age",
       "ed",
       "employ",
       "spoused",
       "reside",
       "pets",
       "pets_cats",
       "pets_dogs",
       "pets_birds",
       "pets_reptiles",
       "pets_small",
       "pets_saltfish",
       "pets_freshfish",
       "address",
       "cars",
       
       "cardtenure",
       "card2tenure",
       "carditems",
       "card2items",
       "tenure",
       "hourstv")
#variables to be removed
outlier_rem=c("income",
              "debtinc",
              "creddebt",
              "othdebt",
              "carvalue",
              "cardspent",
              "card2spent",
              "longmon",
              "longten",
              "tollmon",
              "tollten",
              "equipmon",
              "equipten",
              "cardmon",
              "cardten",
              "wiremon",
              "wireten","commutetime")

#num_var has the names of all the numerical variables
#outlier_rem has the names of those num variables that have to undergo outlier removal
#outlier_capp has the names of those num variables that have to undergo outlier capping


#outlier capping function
cap= function(x) {
  uc=quantile(x,probs=0.99,na.rm=TRUE)
  x[x>uc]=uc
  return(x)
}











#take a copy of the data in "tem" for outlier treatment
tem=data
tem[outlier_capp]=apply(tem[outlier_capp],2,cap) #outlier capping


  #outlier removal
n_col=ncol(tem)
n_col
for(i in seq(1,length(outlier_rem),by=1)){
u=quantile(tem[outlier_rem[i]],probs=0.99,na.rm=TRUE) #calculating the 99th percentile
s=sapply(tem[outlier_rem[i]],sd,na.rm=TRUE) #calculating the standard deviation
uc=u+s
v=seq(1,1,length.out = NROW(tem["income"]))
v[tem[outlier_rem[i]]>uc]=0
tem[n_col+i]=v
}
View(tem[133:150])  # 1= not outlier & 0=outlier 



tem$out_flag=apply(tem[133:150],1,sum)  #sum across columns and store in variable "out_flag"
View(tem[151])

table(tem$out_flag) #find the no. of observations where the out_flg=18, these are the obs which are NOT outliers
(1-(4802/5000))*100 #only 3.96% of obs come out to be outliers. small proportion so it's safe to delete them

data2=tem[tem$out_flag==18,1:132]  #data2 has only non-outliers (4809 observations)
View(data2)

nmiss=function(x){  #function to find number of missing values in each variable of the dataset 
  nmiss=sum(is.na(x))
  return(nmiss=nmiss)
}
n_missing=apply(data2,2,nmiss)
n_missing

mean_imp=function(x){ #mean imputation function
  m=mean(x,na.rm=T)
  x[is.na(x)]=m
  return(x)
}
#mean imputation has to be carried out only on numerical values
#the names of numerical variables that have to undergo missing value treatment
#are stored in outlier_capp and outlier_rem 

data3=data2  #transfer data to data3 that will undergo missing value imputation

#Do mean value imputation in two go
data3[outlier_capp]=apply(data2[outlier_capp],2,mean_imp)
data3[outlier_rem]=apply(data2[outlier_rem],2,mean_imp)
 #data3 has mean imputed values.
#we havent imputed missing values of logarithm variables. Mean imputation has been done only for the original variables
# from which the log variables were derived.

apply(data3[outlier_rem],2,nmiss) # check all the numerical values have 0 missing values or not
apply(data3[outlier_capp],2,nmiss)

apply(data3,2,nmiss)
#So, data3 has been obtained after outlier treatment(which included both removal and capping) and also mean value 
#imputation

# create a new variable, "total_spent"
data3$totalspent=data3$cardspent+data3$card2spent
View(data3[c("totalspent","cardspent","card2spent")])

#Assumptions check--------------------------------------------------

#normality check
hist(data3$totalspent)      #comes out to be skewed, try taking log
hist(log(data3$totalspent)) #log(totalspent) is better i.e. more close to normal distribution

data3$ln_totspent=log(data3$totalspent)     #take log of total spent. This will be our dependent variable
View(data3[c("totalspent","ln_totspent")])


num_var

#linearity check-calculate correlations----------------------


corr_matrix=cor(data3[,unlist(lapply(data3, is.numeric))])
View(corr_matrix)
# There are missing values in correlation matrix because of missing values in log variables 



# write.csv(cor_matrix,file="corr.csv")

#after seeing the correlation matrix, it seems that most variables have low correlation to ln_totspent
#try taking the log variables now (maybe they have better correlations with ln_totspent)
data_for_log=data3
n_col=ncol(data4)


#but because log variables have large number of missing, first impute those missing values by log(0.01)
ln_names=c("lninc", "lncreddebt","lnothdebt","lnlongmon","lnlongten","lntollmon","lntollten","lnequipmon",
"lnequipten","lncardmon", "lncardten" ,"lnwiremon" ,"lnwireten")
View(data_for_log["lnwireten"])


lnfun=function(x){  #make a function that replaces the missing values in log variable with log(0.01)
  x[is.na(x)]=log(0.01)
  return(x)
}

data_for_log[ln_names]=apply(data_for_log[ln_names],2,lnfun) #apply the function to all log variables
View(data_for_log["lnwireten"])


#Again, calculate the correlation matrix
corr_matrix2=data.frame(cor(data_for_log[,unlist(lapply(data3, is.numeric))]))
View(corr_matrix2)
write.csv(corr_matrix2,file="corr2.csv")

# No major difference in correlations. 






 # Try taking exponential of some low-correlation variables. See whether the correlation improved or not 
# t_names=c("age","ed","employ","debtinc","spoused","address","cars","cardtenure","card2tenure","tenure"
#           ,"hourstv","ln_totspent")
# temp[t_names]=data4[t_names]
# temp[t_names]=apply(temp[t_names],2,exp)
# cor_matrixt=data.frame(cor(temp))
# View(cor_matrixt)

# The correlation didn't improve after applying log,exp, square or sqrt transformation to these variables.
# So, leave them as such


#create dummy variables
data4=data3  #Take yet another copy of data which will have artificially created dummy variables
View(data4)

#make dummy variables for all factor variables
levels(data4$cartype)
for(i in seq(1,length(names_factor),by=1))
{
  for(level in (unique(data4[[names_factor[i]]]))){
    
    data4[paste(names(data4[names_factor[i]]),"dummy", level, sep = "_")] <- ifelse(data4[[names_factor[i]]] == level, 1, 0)
  }
}
View(data4[c("region","region_dummy_1")])
names(data4)




#as the number of dummy variables required are one less than the unique levels of that variable,
#so delete one dummy variable for each category
for(i in seq(1,length(names_factor),by=1)){
  data4[paste(names(data4[names_factor[i]]),"dummy", 1, sep = "_")]=NULL
}

data4$birthmonth_dummy_January=NULL  #delete one extra dummy variable for birthmonth
names(data4)


#convert these dummy variables to factors
require(dplyr)
n=names(select(data4,contains("dummy")))
n
data4[,n] <- lapply(data4[,n] , factor)
str(data4$owntv_dummy_0)



#data splitting
data4[ln_names]=NULL
data4["townsize_dummy_NA"]=NULL
nrow(data4)
sample_obs=sample(1:nrow(data4),size=floor(nrow(data4)*0.7))
dev=data4[sample_obs,]
val=data4[-sample_obs,]


#model building
names(dev)
apply(dev,2,nmiss)
dev=na.omit(dev)  
dev2=dev #In dev2, keep only the variables that you want to use in model building

apply(dev2,2,nmiss)




#Now,delete original factors(retain only the corresponding dummy variables),
#delete custid (customer id)
#delete cardspent,card2spent and totalspent as they would lead to multicollinearity in model
dev2[c(names_factor,"commutecar","commutemotorcycle","commutecarpool","commutebus"
       ,"commuterail","commutepublic","commutebike","commutewalk","commutenonmotor","telecommute","cardmon"
       ,"cardten","custid","cardspent","card2spent","totalspent")]=NULL
names(dev2)

fit=lm(ln_totspent~.,data=dev2)
summary(fit)

require(car)
vif(fit)  #aliased coefficients error
alias( lm(ln_totspent~.,data=dev2) )  #aliased coefficients are due to commute variables.They arent
                                      #significant anyway, so dont use them in model 
write.csv(fit$coefficients,"first_coeff.csv")

#manually pick only the significant variables.Run lm() again. T
#hen do stepwise regression using only these variables

# significant vars: age,income,carditems, card2items,townsize_dummy_3,age_cat_dummy_2,age_cat_dummy_6,agecat_dummy_3
#,edcat_dummy_2,jobcat_dummy_4,inccat_dummy_2,inccat_dummy_4,inccat_dummy_3,spousedcat_dummy_2,spousedcat_dummy_3,
#hometype_dummy_4,carown_dummy_0,reason_dummy_9,reason_dummy_2,card_dummy_3,card_dummy_2,card_dummy_4,card_dummy_5,
#cardbenefit_dummy_3,card2_dummy_5,card2_dummy_4,card2_dummy_3,card2_dummy_2,card2benefit_dummy_3,churn_dummy_0,owncd_dummy_0




dev2=dev2[c("ln_totspent","age", 
         "income", 
         "carditems", 
         "card2items", 
         "townsize_dummy_3", 
         "agecat_dummy_2", 
         "agecat_dummy_6", 
         "agecat_dummy_3", 
         "edcat_dummy_2", 
         "jobcat_dummy_4", 
         "inccat_dummy_2", 
         "inccat_dummy_4", 
         "inccat_dummy_3", 
         "spousedcat_dummy_2", 
         "spousedcat_dummy_3", 
         "hometype_dummy_4", 
         "carown_dummy_0", 
         "reason_dummy_9", 
         "reason_dummy_2", 
         "card_dummy_3", 
         "card_dummy_2", 
         "card_dummy_4", 
         "card_dummy_5", 
         "cardbenefit_dummy_3", 
         "card2_dummy_5", 
         "card2_dummy_4", 
         "card2_dummy_3", 
         "card2_dummy_2", 
         "card2benefit_dummy_3", 
         "churn_dummy_0", 
         "owncd_dummy_0" )]


fit2=lm(ln_totspent~.,data=dev2)
summary(fit2)
require(MASS)
stepAIC(fit2)  #do stepwise regression

#run model obtained after stepwise regression
fit3=lm(formula =ln_totspent ~ age + income + carditems + card2items + townsize_dummy_3 + 
          agecat_dummy_2 + agecat_dummy_3 + jobcat_dummy_4 + inccat_dummy_2 + 
          inccat_dummy_4 + inccat_dummy_3 + hometype_dummy_4 + carown_dummy_0 + 
          reason_dummy_9 + reason_dummy_2 + card_dummy_3 + card_dummy_2 + 
          card_dummy_4 + card_dummy_5 + cardbenefit_dummy_3 + card2_dummy_5 + 
          card2_dummy_4 + card2_dummy_3 + card2_dummy_2 + card2benefit_dummy_3 + 
          churn_dummy_0 + owncd_dummy_0, data = dev2)
summary(fit3)
vif(fit3)  #agecat_dummy_2 is insignificant.eliminate it. also delete agecat_dummy_3



fit4=lm(formula =ln_totspent ~ age + income + carditems + card2items + townsize_dummy_3 
        + jobcat_dummy_4 + inccat_dummy_2 + 
          inccat_dummy_4 + inccat_dummy_3 + hometype_dummy_4 + carown_dummy_0 + 
          reason_dummy_9 + reason_dummy_2 + card_dummy_3 + card_dummy_2 + 
          card_dummy_4 + card_dummy_5 + cardbenefit_dummy_3 + card2_dummy_5 + 
          card2_dummy_4 + card2_dummy_3 + card2_dummy_2 + card2benefit_dummy_3 + 
          churn_dummy_0 + owncd_dummy_0, data = dev2)
summary(fit4)
vif(fit4) #almost all vif are less than 2. So no multicollinearity in the model now. fit4 is our final model.



#validation
val=na.omit(val)  #delete any missing values present in the validation dataset
val3=val          #make extra copy of validation dataset for further scoring and deciling


#scoring
apply(val3,2,nmiss)
val3=cbind(val,pred_lnspent=predict(fit4,val),pred_spent=exp(predict(fit4,val)))
names(val)


dev3=cbind(dev2,pred_lnspent=predict(fit4,dev2),pred_spent=exp(predict(fit4,dev2)))

#plot histogram for residuals. see normality
hist(fit4$residuals)


#deciling for dev
dec_cuts=quantile(dev3$pred_spent,probs=seq(from=0.1, to=0.9, by=0.1))
dev3$dec=findInterval(dev3$pred_spent,c(-Inf,dec_cuts,Inf))
View(dev3["dec"])

dev3$dec=as.factor(dev3$dec)
dev3$totalspent=dev$totalspent #totalspent was deleted before, so add it again
View(dev3["totalspent"])


require(dplyr)

sumd=dev3 %>% group_by(dec) %>% summarise(count=n(),avg_actual=mean(totalspent),avg_pred=mean(pred_spent),
                                     sum_actual_sales=sum(totalspent)) %>% arrange(desc(dec))
sumd
write.csv(sumd,file="decile_dev.csv")


#deciling for val
dec_cuts=quantile(val3$pred_spent,probs=seq(from=0.1, to=0.9, by=0.1))

#apply(val3,2,nmiss)
val3$dec=findInterval(val3$pred_spent,c(-Inf,dec_cuts,Inf))
val3$dec=as.factor(val3$dec)
View(val3["totalspent"]) 

sum=val3 %>% group_by(dec) %>% summarize(count=n(),avg_actual=mean(totalspent),avg_pred=mean(pred_spent),
          sum_actual_sales=sum(totalspent)) %>% arrange(desc(dec))
sum


write.csv(sum,file="decile_val.csv")

#the deciling follows the correct order for both dev and val datasets.

#causal model

#calculate the standardized betas
require(QuantPsyc)
stb_dev=lm.beta(fit4)
write.csv(stb_dev,file="stb dev.csv")

#fit4=lm(formula = ln_totspent ~ lninc  + region_dummy_5 + 
 #         agecat_dummy_6 + jobcat_dummy_4 + spousedcat_dummy_2 + spousedcat_dummy_4 + 
  #        spousedcat_dummy_3 + reason_dummy_9 + reason_dummy_2 + card_dummy_3 + 
   #       card_dummy_2 + card_dummy_4 + card_dummy_5 + card2_dummy_5 + 
    #      card2_dummy_4 + card2_dummy_3 + card2_dummy_2 + card2benefit_dummy_3 + 
     #     owncd_dummy_0 + gender_dummy_0 + agecat_dummy_5, 
      #  data = dev2)

#run the same model on validation data set
fit5=lm(formula =ln_totspent ~ age + income + carditems + card2items + townsize_dummy_3 
        + jobcat_dummy_4 + inccat_dummy_2 + 
          inccat_dummy_4 + inccat_dummy_3 + hometype_dummy_4 + carown_dummy_0 + 
          reason_dummy_9 + reason_dummy_2 + card_dummy_3 + card_dummy_2 + 
          card_dummy_4 + card_dummy_5 + cardbenefit_dummy_3 + card2_dummy_5 + 
          card2_dummy_4 + card2_dummy_3 + card2_dummy_2 + card2benefit_dummy_3 + 
          churn_dummy_0 + owncd_dummy_0, data = val)


stb_val=lm.beta(fit5)
write.csv(stb_val,file="stb val.csv")
#check whether the standardized betas obtained by both dev and val datasets have the same sign or not.

#summary of model can't be exported directly to csv file. 
#shows error: cannot coerce class ""summary.lm"" to a data.frame
#So, combine coefficients, r square and adjusted r square and then export it.

model_sum_dev=cbind(summary(fit4)$coef,r.squared=summary(fit4)$r.squared,adj.r.sqr=summary(fit4)$adj.r.squared)
model_sum_val=cbind(summary(fit5)$coef,r.squared=summary(fit5)$r.squared,adj.r.sqr=summary(fit5)$adj.r.squared)


write.csv(model_sum_dev,file="model summary devel.csv")
write.csv(model_sum_val,file="model summary valid.csv")

#The difference in R.Squares between development and validation datasets is only 1%

#Calculate the final correlation matrix between model variables
model_names=c("age","income", "carditems","card2items","townsize_dummy_3","jobcat_dummy_4", 
              "inccat_dummy_2","inccat_dummy_4","inccat_dummy_3","hometype_dummy_4","carown_dummy_0", 
              "reason_dummy_9","reason_dummy_2","card_dummy_3","card_dummy_2","card_dummy_4", "card_dummy_5", 
              "cardbenefit_dummy_3","card2_dummy_5","card2_dummy_4","card2_dummy_3", 
              "card2_dummy_2", "card2benefit_dummy_3","churn_dummy_0","owncd_dummy_0")
data4=na.omit(data4) #data4 has values after outlier treatment and missing value treatment.
                      #Two missing values were still left though. Delete them.

data4[,model_names] <- lapply(data4[,model_names] , as.numeric) #convert the factors into numeric in order
                                                                #to calculate correlation matrix
corr_final=cor(data4[,model_names])
View(corr_final)
write.csv(corr_final,"corrmatrix_final.csv")




