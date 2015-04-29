library(foreign)
library(dplyr)
library(Hmisc)

rm(list=ls())

setwd("C:/Users/Wanner/Desktop/Micronutrient/Vietnam_2010")

# Read in data sets
Food = read.spss("Food.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Country_NCT = read.spss("country_nct.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Household = read.spss("HH_ADEPT.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Individual = read.spss("HM.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Prices = read.csv("Prices.csv")
Household = merge(Household,Prices,by.x=c("month"),by.y=c("Month"))

# Get Average Energy Requirements

setwd("C:/Users/Wanner/Desktop/Work/Nutrition/R")

ADER = read.csv("ADER.csv")

Individual$AGE_CLASS = NA

Individual$AGE_CLASS  = "ACOVER70"
Individual[Individual$hm_age<70,]$AGE_CLASS = "AC6569"
Individual[Individual$hm_age<65,]$AGE_CLASS = "AC6064"
Individual[Individual$hm_age<60,]$AGE_CLASS = "AC5559"
Individual[Individual$hm_age<55,]$AGE_CLASS = "AC5054"
Individual[Individual$hm_age<50,]$AGE_CLASS = "AC4549"
Individual[Individual$hm_age<45,]$AGE_CLASS = "AC4044"
Individual[Individual$hm_age<40,]$AGE_CLASS = "AC3539"
Individual[Individual$hm_age<35,]$AGE_CLASS = "AC3034"
Individual[Individual$hm_age<30,]$AGE_CLASS = "AC2529"
Individual[Individual$hm_age<25,]$AGE_CLASS = "AC2024"
Individual[Individual$hm_age<20,]$AGE_CLASS = "AC19"
Individual[Individual$hm_age<19,]$AGE_CLASS = "AC18"
Individual[Individual$hm_age<18,]$AGE_CLASS = "AC17"
Individual[Individual$hm_age<17,]$AGE_CLASS = "AC16"
Individual[Individual$hm_age<16,]$AGE_CLASS = "AC15"
Individual[Individual$hm_age<15,]$AGE_CLASS = "AC14"
Individual[Individual$hm_age<14,]$AGE_CLASS = "AC13"
Individual[Individual$hm_age<13,]$AGE_CLASS = "AC12"
Individual[Individual$hm_age<12,]$AGE_CLASS = "AC11"
Individual[Individual$hm_age<11,]$AGE_CLASS = "AC10"
Individual[Individual$hm_age<10,]$AGE_CLASS = "AC09"
Individual[Individual$hm_age<9,]$AGE_CLASS = "AC08"
Individual[Individual$hm_age<8,]$AGE_CLASS = "AC07"
Individual[Individual$hm_age<7,]$AGE_CLASS = "AC06"
Individual[Individual$hm_age<6,]$AGE_CLASS = "AC05"
Individual[Individual$hm_age<5,]$AGE_CLASS = "AC04"
Individual[Individual$hm_age<4,]$AGE_CLASS = "AC03"
Individual[Individual$hm_age<3,]$AGE_CLASS = "AC02"
Individual[Individual$hm_age<2,]$AGE_CLASS = "AC01"
Individual[Individual$hm_age<1,]$AGE_CLASS = "AC00"
Individual$AGE_CLASS = as.factor(Individual$AGE_CLASS)

Individual$U5MRthreshold = ">10"
Individual$FAOST_CODE = 237
Individual[Individual$gender==1,]$gender="Male"
Individual[Individual$gender==2,]$gender="Female"
Individual$Year = 2010

Individual = merge(Individual,ADER,by.x=c("FAOST_CODE","U5MRthreshold","Year","AGE_CLASS","gender"),
                   by.y=c("FAOST_CODE","U5MRthreshold","Year","AGE_CLASS","Gender"))

Data_Temp = 
  Individual %>%
  group_by(hh_no) %>%
  summarise(Total_Requirement = sum(AER_1.85))

Individual = merge(Individual,Data_Temp,by=c("hh_no"))

rm("ADER")

# Deflate the income
Household$Inc = Household$new_inc * (Household$Average_CPI/Household$CPI)

Household$pop_weight = Household$hh_wgt*Household$HH_size

Data = merge(Food,Country_NCT,by.x=c("item_code"),by.y=c("item_cod"))
Data = merge(Data,Household,by=c("hh_no"))

# Deflate the food expenditure
Data$fd_mv = Data$fd_mv * (Data$Average_FPI/Data$FPI)

Data$Calories = NA

# Calculate the calories
Data$Calories = (Data$fd_pro*4+Data$fd_fat*9+Data$fd_car*4+Data$fd_fib*2+Data$fd_alc*7)*
  ((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)

Data$Vitamin_A =  Data$VitaminA*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)
Data$Vitamin_B1 =  Data$VitaminB1*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)
Data$Vitamin_B2 =  Data$VitaminB2*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)
Data$Vitamin_B6 =  Data$VitaminB6*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)
Data$Vitamin_B12 =  Data$VitaminB12*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)
Data$Vitamin_C =  Data$Vit_C*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)

# Calculate the calorie cost for the household by ignoring food away from home
Data_Temp = 
  Data %>%
  group_by(hh_no) %>%
  summarise(Calorie_Cost=sum(Calories[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]),
            Vitamin_A_Cost=sum(Vitamin_A[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]),
            Vitamin_B1_Cost=sum(Vitamin_B1[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]),
            Vitamin_B2_Cost=sum(Vitamin_B2[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]),
            Vitamin_B6_Cost=sum(Vitamin_B6[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]),
            Vitamin_B12_Cost=sum(Vitamin_B12[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]),
            Vitamin_C_Cost=sum(Vitamin_C[!is.na(Calories)])/sum(fd_mv[!is.na(Calories)]))

Household = merge(Household,Data_Temp,by=c("hh_no"))

# Calculate income quintiles using household weights
Inc_Quintiles = wtd.quantile(Household$Inc, 
                             weights=(Household$hh_wgt), 
                            probs=c(.2,.4,.6,.8,1))

Inc_Deciles = wtd.quantile(Household$Inc, 
                             weights=(Household$hh_wgt), 
                             probs=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

Household$Inc_Quint[Household$Inc<=Inc_Quintiles[5]] = 5
Household$Inc_Quint[Household$Inc<=Inc_Quintiles[4]] = 4
Household$Inc_Quint[Household$Inc<=Inc_Quintiles[3]] = 3
Household$Inc_Quint[Household$Inc<=Inc_Quintiles[2]] = 2
Household$Inc_Quint[Household$Inc<=Inc_Quintiles[1]] = 1

# Calculate the calorie cost by region, area, and income quintile using household weights
Data_Temp = 
  Household %>%
  group_by(region, urb_rur, Inc_Quint) %>%
  summarise(Imputation_reg_area_inc = median(Calorie_Cost,na.rm=TRUE),
            Imputation_VitA_reg_area_inc = median(Vitamin_A_Cost,na.rm=TRUE),
            Imputation_VitB1_reg_area_inc = median(Vitamin_B1_Cost,na.rm=TRUE),
            Imputation_VitB2_reg_area_inc = median(Vitamin_B2_Cost,na.rm=TRUE),
            Imputation_VitB6_reg_area_inc = median(Vitamin_B6_Cost,na.rm=TRUE),
            Imputation_VitB12_reg_area_inc = median(Vitamin_B12_Cost,na.rm=TRUE),
            Imputation_VitC_reg_area_inc = median(Vitamin_C_Cost,na.rm=TRUE))

Household = merge(Household,Data_Temp,by=c("region","urb_rur","Inc_Quint"))
Data = merge(Data,Household[,c("hh_no","Imputation_reg_area_inc",
                               "Imputation_VitA_reg_area_inc",
                               "Imputation_VitB1_reg_area_inc",
                               "Imputation_VitB2_reg_area_inc",
                               "Imputation_VitB6_reg_area_inc",
                               "Imputation_VitB12_reg_area_inc",
                               "Imputation_VitC_reg_area_inc")],by=c("hh_no"))

# Calculate the calorie cost by region and area using household weights
Data_Temp = 
  Household %>%
  group_by(region, urb_rur) %>%
  summarise(Imputation_reg_area = median(Calorie_Cost,na.rm=TRUE),
            Imputation_VitA_reg_area = median(Vitamin_A_Cost,na.rm=TRUE),
            Imputation_VitB1_reg_area = median(Vitamin_B1_Cost,na.rm=TRUE),
            Imputation_VitB2_reg_area = median(Vitamin_B2_Cost,na.rm=TRUE),
            Imputation_VitB6_reg_area = median(Vitamin_B6_Cost,na.rm=TRUE),
            Imputation_VitB12_reg_area = median(Vitamin_B12_Cost,na.rm=TRUE),
            Imputation_VitC_reg_area = median(Vitamin_C_Cost,na.rm=TRUE))

Household = merge(Household,Data_Temp,by=c("region","urb_rur"))
Data = merge(Data,Household[,c("hh_no","Imputation_reg_area",
                               "Imputation_VitA_reg_area",
                               "Imputation_VitB1_reg_area",
                               "Imputation_VitB2_reg_area",
                               "Imputation_VitB6_reg_area",
                               "Imputation_VitB12_reg_area",
                               "Imputation_VitC_reg_area")],by=c("hh_no"))

# Try imputing calories for food away from home using the highest level of disaggregation
Data[is.na(Data$Calories),]$Calories = Data[is.na(Data$Calories),]$fd_mv * Data[is.na(Data$Calories),]$Imputation_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Calories),]$Calories = Data[is.na(Data$Calories),]$fd_mv * Data[is.na(Data$Calories),]$Imputation_reg_area

# Try imputing Vitamin A for food away from home using the highest level of disaggregation
Data[is.na(Data$Vitamin_A),]$Vitamin_A = Data[is.na(Data$Vitamin_A),]$fd_mv * Data[is.na(Data$Vitamin_A),]$Imputation_VitA_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Vitamin_A),]$Vitamin_A = Data[is.na(Data$Vitamin_A),]$fd_mv * Data[is.na(Data$Vitamin_A),]$Imputation_VitA_reg_area

# Try imputing Vitamin B1 for food away from home using the highest level of disaggregation
Data[is.na(Data$Vitamin_B1),]$Vitamin_B1 = Data[is.na(Data$Vitamin_B1),]$fd_mv * Data[is.na(Data$Vitamin_B1),]$Imputation_VitB1_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Vitamin_B1),]$Vitamin_B1 = Data[is.na(Data$Vitamin_B1),]$fd_mv * Data[is.na(Data$Vitamin_B1),]$Imputation_VitB1_reg_area

# Try imputing Vitamin B2 for food away from home using the highest level of disaggregation
Data[is.na(Data$Vitamin_B2),]$Vitamin_B2 = Data[is.na(Data$Vitamin_B2),]$fd_mv * Data[is.na(Data$Vitamin_B2),]$Imputation_VitB2_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Vitamin_B2),]$Vitamin_B2 = Data[is.na(Data$Vitamin_B2),]$fd_mv * Data[is.na(Data$Vitamin_B2),]$Imputation_VitB2_reg_area

# Try imputing Vitamin B6 for food away from home using the highest level of disaggregation
Data[is.na(Data$Vitamin_B6),]$Vitamin_B6 = Data[is.na(Data$Vitamin_B6),]$fd_mv * Data[is.na(Data$Vitamin_B6),]$Imputation_VitB6_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Vitamin_B6),]$Vitamin_B6 = Data[is.na(Data$Vitamin_B6),]$fd_mv * Data[is.na(Data$Vitamin_B6),]$Imputation_VitB6_reg_area

# Try imputing Vitamin B12 for food away from home using the highest level of disaggregation
Data[is.na(Data$Vitamin_B12),]$Vitamin_B12 = Data[is.na(Data$Vitamin_B12),]$fd_mv * Data[is.na(Data$Vitamin_B12),]$Imputation_VitB12_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Vitamin_B12),]$Vitamin_B12 = Data[is.na(Data$Vitamin_B12),]$fd_mv * Data[is.na(Data$Vitamin_B12),]$Imputation_VitB12_reg_area

# Try imputing Vitamin C for food away from home using the highest level of disaggregation
Data[is.na(Data$Vitamin_C),]$Vitamin_C = Data[is.na(Data$Vitamin_C),]$fd_mv * Data[is.na(Data$Vitamin_C),]$Imputation_VitC_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Vitamin_C),]$Vitamin_C = Data[is.na(Data$Vitamin_C),]$fd_mv * Data[is.na(Data$Vitamin_C),]$Imputation_VitC_reg_area

# Calculate the total number of calories for the household
Data_Household = 
  Data %>%
  group_by(hh_no) %>%
  summarise(Calories=sum(Calories),
            Vitamin_A = sum(Vitamin_A),
            Vitamin_B1 = sum(Vitamin_B1),
            Vitamin_B2 = sum(Vitamin_B2),
            Vitamin_B6 = sum(Vitamin_B6),
            Vitamin_B12 = sum(Vitamin_B12),
            Vitamin_C = sum(Vitamin_C))
            
Individual = merge(Data_Household,Individual,by=c("hh_no"))
Individual = merge(Individual,Household,by=c("hh_no"))

options(digits=10)

Individual$Vitamin_A = (Individual$AER_1.85/Individual$Total_Requirement)*Individual$Vitamin_A
Individual$Vitamin_B1 = (Individual$AER_1.85/Individual$Total_Requirement)*Individual$Vitamin_B1
Individual$Vitamin_B2 = (Individual$AER_1.85/Individual$Total_Requirement)*Individual$Vitamin_B2
Individual$Vitamin_B6 = (Individual$AER_1.85/Individual$Total_Requirement)*Individual$Vitamin_B6
Individual$Vitamin_B12 = (Individual$AER_1.85/Individual$Total_Requirement)*Individual$Vitamin_B12
Individual$Vitamin_C = (Individual$AER_1.85/Individual$Total_Requirement)*Individual$Vitamin_C

Individual = Individual[Individual$hm_age>=1,]

wtd.mean(Individual$Calories/Individual$HH_size,weights=Individual$hh_wgt)

Individual$Req_A[Individual$hm_age>=1&Individual$gender=="Male"] = 210 
Individual$Req_A[Individual$hm_age>=1&Individual$gender=="Female"] = 210
Individual$Req_A[Individual$hm_age>=4&Individual$gender=="Male"] = 275 
Individual$Req_A[Individual$hm_age>=4&Individual$gender=="Female"] = 275
Individual$Req_A[Individual$hm_age>=9&Individual$gender=="Male"] = 445 
Individual$Req_A[Individual$hm_age>=9&Individual$gender=="Female"] = 420
Individual$Req_A[Individual$hm_age>=14&Individual$gender=="Male"] = 630 
Individual$Req_A[Individual$hm_age>=14&Individual$gender=="Female"] = 485
Individual$Req_A[Individual$hm_age>=19&Individual$gender=="Male"] = 625 
Individual$Req_A[Individual$hm_age>=19&Individual$gender=="Female"] = 500
Individual$Req_A[Individual$hm_age>=31&Individual$gender=="Male"] = 625 
Individual$Req_A[Individual$hm_age>=31&Individual$gender=="Female"] = 500
Individual$Req_A[Individual$hm_age>=51&Individual$gender=="Male"] = 625 
Individual$Req_A[Individual$hm_age>=51&Individual$gender=="Female"] = 500
Individual$Req_A[Individual$hm_age>=70&Individual$gender=="Male"] = 625 
Individual$Req_A[Individual$hm_age>=70&Individual$gender=="Female"] = 500

Individual$Req_B1[Individual$hm_age>=1&Individual$gender=="Male"] = .4 
Individual$Req_B1[Individual$hm_age>=1&Individual$gender=="Female"] = .4
Individual$Req_B1[Individual$hm_age>=4&Individual$gender=="Male"] = .5 
Individual$Req_B1[Individual$hm_age>=4&Individual$gender=="Female"] = .5
Individual$Req_B1[Individual$hm_age>=9&Individual$gender=="Male"] = .7 
Individual$Req_B1[Individual$hm_age>=9&Individual$gender=="Female"] = .7
Individual$Req_B1[Individual$hm_age>=14&Individual$gender=="Male"] = 1 
Individual$Req_B1[Individual$hm_age>=14&Individual$gender=="Female"] = .9
Individual$Req_B1[Individual$hm_age>=19&Individual$gender=="Male"] = 1 
Individual$Req_B1[Individual$hm_age>=19&Individual$gender=="Female"] = .9
Individual$Req_B1[Individual$hm_age>=31&Individual$gender=="Male"] = 1 
Individual$Req_B1[Individual$hm_age>=31&Individual$gender=="Female"] = .9
Individual$Req_B1[Individual$hm_age>=51&Individual$gender=="Male"] = 1 
Individual$Req_B1[Individual$hm_age>=51&Individual$gender=="Female"] = .9
Individual$Req_B1[Individual$hm_age>=70&Individual$gender=="Male"] = 1 
Individual$Req_B1[Individual$hm_age>=70&Individual$gender=="Female"] = .9

Individual$Req_B2[Individual$hm_age>=1&Individual$gender=="Male"] = .4 
Individual$Req_B2[Individual$hm_age>=1&Individual$gender=="Female"] = .4
Individual$Req_B2[Individual$hm_age>=4&Individual$gender=="Male"] = .5 
Individual$Req_B2[Individual$hm_age>=4&Individual$gender=="Female"] = .5
Individual$Req_B2[Individual$hm_age>=9&Individual$gender=="Male"] = .8 
Individual$Req_B2[Individual$hm_age>=9&Individual$gender=="Female"] = .8
Individual$Req_B2[Individual$hm_age>=14&Individual$gender=="Male"] = 1.1 
Individual$Req_B2[Individual$hm_age>=14&Individual$gender=="Female"] = .9
Individual$Req_B2[Individual$hm_age>=19&Individual$gender=="Male"] = 1.1 
Individual$Req_B2[Individual$hm_age>=19&Individual$gender=="Female"] = .9
Individual$Req_B2[Individual$hm_age>=31&Individual$gender=="Male"] = 1.1 
Individual$Req_B2[Individual$hm_age>=31&Individual$gender=="Female"] = .9
Individual$Req_B2[Individual$hm_age>=51&Individual$gender=="Male"] = 1.1 
Individual$Req_B2[Individual$hm_age>=51&Individual$gender=="Female"] = .9
Individual$Req_B2[Individual$hm_age>=70&Individual$gender=="Male"] = 1.1 
Individual$Req_B2[Individual$hm_age>=70&Individual$gender=="Female"] = .9

Individual$Req_B6[Individual$hm_age>=1&Individual$gender=="Male"] = .4 
Individual$Req_B6[Individual$hm_age>=1&Individual$gender=="Female"] = .4
Individual$Req_B6[Individual$hm_age>=4&Individual$gender=="Male"] = .5 
Individual$Req_B6[Individual$hm_age>=4&Individual$gender=="Female"] = .5
Individual$Req_B6[Individual$hm_age>=9&Individual$gender=="Male"] = .8 
Individual$Req_B6[Individual$hm_age>=9&Individual$gender=="Female"] = .8
Individual$Req_B6[Individual$hm_age>=14&Individual$gender=="Male"] = 1.1 
Individual$Req_B6[Individual$hm_age>=14&Individual$gender=="Female"] = 1
Individual$Req_B6[Individual$hm_age>=19&Individual$gender=="Male"] = 1.1 
Individual$Req_B6[Individual$hm_age>=19&Individual$gender=="Female"] = 1.1
Individual$Req_B6[Individual$hm_age>=31&Individual$gender=="Male"] = 1.1 
Individual$Req_B6[Individual$hm_age>=31&Individual$gender=="Female"] = 1.1
Individual$Req_B6[Individual$hm_age>=51&Individual$gender=="Male"] = 1.4 
Individual$Req_B6[Individual$hm_age>=51&Individual$gender=="Female"] = 1.3
Individual$Req_B6[Individual$hm_age>=70&Individual$gender=="Male"] = 1.4 
Individual$Req_B6[Individual$hm_age>=70&Individual$gender=="Female"] = 1.3

Individual$Req_B12[Individual$hm_age>=1&Individual$gender=="Male"] = .7 
Individual$Req_B12[Individual$hm_age>=1&Individual$gender=="Female"] = .7
Individual$Req_B12[Individual$hm_age>=4&Individual$gender=="Male"] = 1 
Individual$Req_B12[Individual$hm_age>=4&Individual$gender=="Female"] = 1
Individual$Req_B12[Individual$hm_age>=9&Individual$gender=="Male"] = 1.5 
Individual$Req_B12[Individual$hm_age>=9&Individual$gender=="Female"] = 1.5
Individual$Req_B12[Individual$hm_age>=14&Individual$gender=="Male"] = 2 
Individual$Req_B12[Individual$hm_age>=14&Individual$gender=="Female"] = 2
Individual$Req_B12[Individual$hm_age>=19&Individual$gender=="Male"] = 2 
Individual$Req_B12[Individual$hm_age>=19&Individual$gender=="Female"] = 2
Individual$Req_B12[Individual$hm_age>=31&Individual$gender=="Male"] = 2 
Individual$Req_B12[Individual$hm_age>=31&Individual$gender=="Female"] = 2
Individual$Req_B12[Individual$hm_age>=51&Individual$gender=="Male"] = 2 
Individual$Req_B12[Individual$hm_age>=51&Individual$gender=="Female"] = 2
Individual$Req_B12[Individual$hm_age>=70&Individual$gender=="Male"] = 2 
Individual$Req_B12[Individual$hm_age>=70&Individual$gender=="Female"] = 2

Individual$Req_C[Individual$hm_age>=1&Individual$gender=="Male"] = 13 
Individual$Req_C[Individual$hm_age>=1&Individual$gender=="Female"] = 13
Individual$Req_C[Individual$hm_age>=4&Individual$gender=="Male"] = 22
Individual$Req_C[Individual$hm_age>=4&Individual$gender=="Female"] = 22
Individual$Req_C[Individual$hm_age>=9&Individual$gender=="Male"] = 39 
Individual$Req_C[Individual$hm_age>=9&Individual$gender=="Female"] = 39
Individual$Req_C[Individual$hm_age>=14&Individual$gender=="Male"] = 63 
Individual$Req_C[Individual$hm_age>=14&Individual$gender=="Female"] = 56
Individual$Req_C[Individual$hm_age>=19&Individual$gender=="Male"] = 75 
Individual$Req_C[Individual$hm_age>=19&Individual$gender=="Female"] = 60
Individual$Req_C[Individual$hm_age>=31&Individual$gender=="Male"] = 75 
Individual$Req_C[Individual$hm_age>=31&Individual$gender=="Female"] = 60
Individual$Req_C[Individual$hm_age>=51&Individual$gender=="Male"] = 75 
Individual$Req_C[Individual$hm_age>=51&Individual$gender=="Female"] = 60
Individual$Req_C[Individual$hm_age>=70&Individual$gender=="Male"] = 75 
Individual$Req_C[Individual$hm_age>=70&Individual$gender=="Female"] = 60

Mean_A = wtd.mean(Individual$Vitamin_A,weights=Individual$hh_wgt)
CV_A = sqrt(wtd.var(Individual$Vitamin_A, weights=Individual$hh_wgt))/Mean_A
Mean_Req_A = wtd.mean(Individual$Req_A,weights=Individual$hh_wgt)
CV_Req_A = .2

Mean_B1 = wtd.mean(Individual$Vitamin_B1,weights=Individual$hh_wgt)
CV_B1 = sqrt(wtd.var(Individual$Vitamin_B1, weights=Individual$hh_wgt))/Mean_B1
Mean_Req_B1 = wtd.mean(Individual$Req_B1,weights=Individual$hh_wgt)
CV_Req_B1 = .1

Mean_B2 = wtd.mean(Individual$Vitamin_B2,weights=Individual$hh_wgt)
CV_B2 = sqrt(wtd.var(Individual$Vitamin_B2, weights=Individual$hh_wgt))/Mean_B2
Mean_Req_B2 = wtd.mean(Individual$Req_B2,weights=Individual$hh_wgt)
CV_Req_B2 = .1

Mean_B6 = wtd.mean(Individual$Vitamin_B6,weights=Individual$hh_wgt)
CV_B6 = sqrt(wtd.var(Individual$Vitamin_B6, weights=Individual$hh_wgt))/Mean_B6
Mean_Req_B6 = wtd.mean(Individual$Req_B6,weights=Individual$hh_wgt)
CV_Req_B6 = .1

Mean_B12 = wtd.mean(Individual$Vitamin_B12,weights=Individual$hh_wgt)
CV_B12 = sqrt(wtd.var(Individual$Vitamin_B12, weights=Individual$hh_wgt))/Mean_B12
Mean_Req_B12 = wtd.mean(Individual$Req_B12,weights=Individual$hh_wgt)
CV_Req_B12 = .1

Mean_C = wtd.mean(Individual$Vitamin_C,weights=Individual$hh_wgt)
CV_C = sqrt(wtd.var(Individual$Vitamin_C, weights=Individual$hh_wgt))/Mean_C
Mean_Req_C = wtd.mean(Individual$Req_C,weights=Individual$hh_wgt)
CV_Req_C = .1

Probability_A = function(x) {(1-pnorm(x,mean=Mean_Req_A,sd=CV_Req_A*Mean_Req_A))*
                                       (1/x)*dnorm(log(x),mean=log(Mean_A)-log(CV_A^2+1)/2,sd=sqrt(log(CV_A^2+1)))}
Probability_B1 = function(x) {(1-pnorm(x,mean=Mean_Req_B1,sd=CV_Req_B1*Mean_Req_B1))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B1)-log(CV_B1^2+1)/2,sd=sqrt(log(CV_B1^2+1)))}
Probability_B2 = function(x) {(1-pnorm(x,mean=Mean_Req_B2,sd=CV_Req_B2*Mean_Req_B2))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B2)-log(CV_B2^2+1)/2,sd=sqrt(log(CV_B2^2+1)))}
Probability_B6 = function(x) {(1-pnorm(x,mean=Mean_Req_B6,sd=CV_Req_B6*Mean_Req_B6))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B6)-log(CV_B6^2+1)/2,sd=sqrt(log(CV_B6^2+1)))}
Probability_B12 = function(x) {(1-pnorm(x,mean=Mean_Req_B12,sd=CV_Req_B12*Mean_Req_B12))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B12)-log(CV_B12^2+1)/2,sd=sqrt(log(CV_B12^2+1)))}
Probability_C = function(x) {(1-pnorm(x,mean=Mean_Req_C,sd=CV_Req_C*Mean_Req_C))*
                                (1/x)*dnorm(log(x),mean=log(Mean_C)-log(CV_C^2+1)/2,sd=sqrt(log(CV_C^2+1)))}

PoI_A = integrate(Probability_A,0,Inf)
PoI_EAR_A = pnorm(log(Mean_Req_A),mean=log(Mean_A)-log(CV_A^2+1)/2,sd=sqrt(log(CV_A^2+1)))

PoI_B1 = integrate(Probability_B1,0,Inf)
PoI_EAR_B1 = pnorm(log(Mean_Req_B1),mean=log(Mean_B1)-log(CV_B1^2+1)/2,sd=sqrt(log(CV_B1^2+1)))

PoI_B2 = integrate(Probability_B2,0,Inf)
PoI_EAR_B2 = pnorm(log(Mean_Req_B2),mean=log(Mean_B2)-log(CV_B2^2+1)/2,sd=sqrt(log(CV_B2^2+1)))

PoI_B6 = integrate(Probability_B6,0,Inf)
PoI_EAR_B6 = pnorm(log(Mean_Req_B6),mean=log(Mean_B6)-log(CV_B6^2+1)/2,sd=sqrt(log(CV_B6^2+1)))

PoI_B12 = integrate(Probability_B12,0,Inf)
PoI_EAR_B12 = pnorm(log(Mean_Req_B12),mean=log(Mean_B12)-log(CV_B12^2+1)/2,sd=sqrt(log(CV_B12^2+1)))

PoI_C = integrate(Probability_C,0,Inf)
PoI_EAR_C = pnorm(log(Mean_Req_C),mean=log(Mean_C)-log(CV_C^2+1)/2,sd=sqrt(log(CV_C^2+1)))

# Initialize the moments using income decile to be zero

IncomeSecondCentralMoment_A = 0
IncomeSecondCentralMoment_B1 = 0
IncomeSecondCentralMoment_B2 = 0
IncomeSecondCentralMoment_B6 = 0
IncomeSecondCentralMoment_B12 = 0
IncomeSecondCentralMoment_C = 0


{for (i in 1:length(Inc_Deciles)) {
  
  # Intialize the moments for the 1st income decile
  if (i ==1) {
    
    IncomeSecondCentralMoment_A = ((wtd.mean(Individual$Vitamin_A[Individual$Inc<=Inc_Deciles[i]],
                                           weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])
                                  -Mean_A)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B1 = ((wtd.mean(Individual$Vitamin_B1[Individual$Inc<=Inc_Deciles[i]],
                                              weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])
                                     -Mean_B1)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B2 = ((wtd.mean(Individual$Vitamin_B2[Individual$Inc<=Inc_Deciles[i]],
                                              weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])
                                     -Mean_B2)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B6 = ((wtd.mean(Individual$Vitamin_B6[Individual$Inc<=Inc_Deciles[i]],
                                              weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])
                                     -Mean_B6)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B12 = ((wtd.mean(Individual$Vitamin_B12[Individual$Inc<=Inc_Deciles[i]],
                                              weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])
                                     -Mean_B12)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_C = ((wtd.mean(Individual$Vitamin_C[Individual$Inc<=Inc_Deciles[i]],
                                              weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])
                                     -Mean_C)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]])/sum(Individual$hh_wgt))
  }
  
  # Add on to the moments by going through the rest of the income deciles
  else {
    
    IncomeSecondCentralMoment_A = IncomeSecondCentralMoment_A + 
      ((wtd.mean(Individual$Vitamin_A[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]],
                 weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])
        -Mean_A)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B1 = IncomeSecondCentralMoment_B1 + 
      ((wtd.mean(Individual$Vitamin_B1[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]],
                 weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])
        -Mean_B1)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B2 = IncomeSecondCentralMoment_B2 + 
      ((wtd.mean(Individual$Vitamin_B2[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]],
                 weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])
        -Mean_B2)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B6 = IncomeSecondCentralMoment_B6 + 
      ((wtd.mean(Individual$Vitamin_B6[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]],
                 weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])
        -Mean_B6)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_B12 = IncomeSecondCentralMoment_B12 + 
      ((wtd.mean(Individual$Vitamin_B12[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]],
                 weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])
        -Mean_B12)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])/sum(Individual$hh_wgt))
    
    IncomeSecondCentralMoment_C = IncomeSecondCentralMoment_C + 
      ((wtd.mean(Individual$Vitamin_C[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]],
                 weights=Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])
        -Mean_C)^2)*(sum(Individual$hh_wgt[Individual$Inc<=Inc_Deciles[i]&Individual$Inc>Inc_Deciles[i-1]])/sum(Individual$hh_wgt))
      
  }
  
}}

# Calculate the CV due to income which is the CV calculated by grouping by income deciles

CVDueToIncome_A = (sqrt(IncomeSecondCentralMoment_A *(length(Data$Vitamin_A)/(length(Data$Vitamin_A)-1))))/Mean_A
CVDueToIncome_B1 = (sqrt(IncomeSecondCentralMoment_B1 *(length(Data$Vitamin_B1)/(length(Data$Vitamin_B1)-1))))/Mean_B1
CVDueToIncome_B2 = (sqrt(IncomeSecondCentralMoment_B2 *(length(Data$Vitamin_B2)/(length(Data$Vitamin_B2)-1))))/Mean_B2
CVDueToIncome_B6 = (sqrt(IncomeSecondCentralMoment_B6 *(length(Data$Vitamin_B6)/(length(Data$Vitamin_B6)-1))))/Mean_B6
CVDueToIncome_B12 = (sqrt(IncomeSecondCentralMoment_B12 *(length(Data$Vitamin_B12)/(length(Data$Vitamin_B12)-1))))/Mean_B12
CVDueToIncome_C = (sqrt(IncomeSecondCentralMoment_C *(length(Data$Vitamin_C)/(length(Data$Vitamin_C)-1))))/Mean_B1

CV_Inc_A = sqrt(CVDueToIncome_A^2+.2^2)
CV_Inc_B1 = sqrt(CVDueToIncome_B1^2+.2^2)
CV_Inc_B2 = sqrt(CVDueToIncome_B2^2+.2^2)
CV_Inc_B6 = sqrt(CVDueToIncome_B6^2+.2^2)
CV_Inc_B12 = sqrt(CVDueToIncome_B12^2+.2^2)
CV_Inc_C = sqrt(CVDueToIncome_C^2+.2^2)

Probability_A = function(x) {(1-pnorm(x,mean=Mean_Req_A,sd=CV_Req_A*Mean_Req_A))*
                                (1/x)*dnorm(log(x),mean=log(Mean_A)-log(CV_Inc_A^2+1)/2,sd=sqrt(log(CV_Inc_A^2+1)))}

Probability_B1 = function(x) {(1-pnorm(x,mean=Mean_Req_B1,sd=CV_Req_B1*Mean_Req_B1))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B1)-log(CV_Inc_B1^2+1)/2,sd=sqrt(log(CV_Inc_B1^2+1)))}


Probability_B2 = function(x) {(1-pnorm(x,mean=Mean_Req_B2,sd=CV_Req_B1*Mean_Req_B2))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B2)-log(CV_Inc_B2^2+1)/2,sd=sqrt(log(CV_Inc_B2^2+1)))}


Probability_B6 = function(x) {(1-pnorm(x,mean=Mean_Req_B6,sd=CV_Req_B6*Mean_Req_B6))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B6)-log(CV_Inc_B6^2+1)/2,sd=sqrt(log(CV_Inc_B6^2+1)))}


Probability_B12 = function(x) {(1-pnorm(x,mean=Mean_Req_B12,sd=CV_Req_B12*Mean_Req_B12))*
                                (1/x)*dnorm(log(x),mean=log(Mean_B12)-log(CV_Inc_B12^2+1)/2,sd=sqrt(log(CV_Inc_B12^2+1)))}


Probability_C = function(x) {(1-pnorm(x,mean=Mean_Req_C,sd=CV_Req_C*Mean_Req_C))*
                                (1/x)*dnorm(log(x),mean=log(Mean_C)-log(CV_Inc_C^2+1)/2,sd=sqrt(log(CV_Inc_C^2+1)))}

PoI_Inc_A = integrate(Probability_A,0,Inf)
PoI_Inc_EAR_A = pnorm(log(Mean_Req_A),mean=log(Mean_A)-log(CV_Inc_A^2+1)/2,sd=sqrt(log(CV_Inc_A^2+1)))

PoI_Inc_B1 = integrate(Probability_B1,0,Inf)
PoI_Inc_EAR_B1 = pnorm(log(Mean_Req_B1),mean=log(Mean_B1)-log(CV_Inc_B1^2+1)/2,sd=sqrt(log(CV_Inc_B1^2+1)))

PoI_Inc_B2 = integrate(Probability_B2,0,Inf)
PoI_Inc_EAR_B2 = pnorm(log(Mean_Req_B2),mean=log(Mean_B2)-log(CV_Inc_B2^2+1)/2,sd=sqrt(log(CV_Inc_B2^2+1)))

PoI_Inc_B6 = integrate(Probability_B6,0,Inf)
PoI_Inc_EAR_B6 = pnorm(log(Mean_Req_B6),mean=log(Mean_B6)-log(CV_Inc_B6^2+1)/2,sd=sqrt(log(CV_Inc_B6^2+1)))

PoI_Inc_B12 = integrate(Probability_B12,0,Inf)
PoI_Inc_EAR_B12 = pnorm(log(Mean_Req_B12),mean=log(Mean_B12)-log(CV_Inc_B12^2+1)/2,sd=sqrt(log(CV_Inc_B12^2+1)))

PoI_Inc_C = integrate(Probability_C,0,Inf)
PoI_Inc_EAR_C = pnorm(log(Mean_Req_C),mean=log(Mean_C)-log(CV_Inc_C^2+1)/2,sd=sqrt(log(CV_Inc_C^2+1)))

Output = cbind(rbind(PoI_A$value,PoI_EAR_A,PoI_Inc_A$value,PoI_Inc_EAR_A),
               rbind(PoI_B1$value,PoI_EAR_B1,PoI_Inc_B1$value,PoI_Inc_EAR_B1),
               rbind(PoI_B2$value,PoI_EAR_B2,PoI_Inc_B2$value,PoI_Inc_EAR_B2),
               rbind(PoI_B6$value,PoI_EAR_B6,PoI_Inc_B6$value,PoI_Inc_EAR_B6),
               rbind(PoI_B12$value,PoI_EAR_B12,PoI_Inc_B12$value,PoI_Inc_EAR_B12),
               rbind(PoI_C$value,PoI_EAR_C,PoI_Inc_C$value,PoI_Inc_EAR_C))

write.csv(Output,"Micronutrient_Vietnam.csv")
