library(foreign)
library(dplyr)
# Author: Nathan Wanner
# Date: 28 April 2015
# This script calculate the prevalence of inadequacy of 6 nutrients from a National Household Survey
# for Vietnam conducted in 2010.

# The script will calculate the prevalence of inadequacy using information directly from the distribution
# of food consumption as well as with alternative parameters.

# Our data contains records on food aquisition
# We will change food quantities in grams to quantities of 6 different nutrients using
# nutrient density information for foods
# We will then impute missing nutrient quantities

# Then we will compute distributional parameters for the use of the calculation of 
# the prevalence of inadeacy for each of the 6 nutrients.

# Lastly, we will calculate the prevalence of inadequacy for each of the 6 nutrients.

library(Hmisc)
library(tidyr)

rm(list=ls())

setwd("C:/Users/Wanner/Desktop/Micronutrient/Vietnam_2010")

# Read in data sets
Food = read.spss("Food.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Country_NCT = read.spss("country_nct.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Household = read.spss("HH_ADEPT.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Individual = read.spss("HM.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Prices = read.csv("Prices.csv")
Household = merge(Household,Prices,by.x=c("month"),by.y=c("Month"))

# Get Average Energy Requirements for calories and nutrients
setwd("C:/Users/Wanner/Desktop/Work/Nutrition/R")
Requirements = read.csv("Requirements.csv")
ADER = read.csv("ADER.csv")

# Create age classes in individual file so that requirements can be merged in from ADER
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

# Other necessary variables to allow for the merging with the ADER file
Individual$U5MRthreshold = ">10"
Individual$FAOST_CODE = 237
Individual[Individual$gender==1,]$gender="Male"
Individual[Individual$gender==2,]$gender="Female"
Individual$Year = 2010

# Merge the ADER file
Individual = merge(Individual,ADER,by.x=c("FAOST_CODE","U5MRthreshold","Year","AGE_CLASS","gender"),
                   by.y=c("FAOST_CODE","U5MRthreshold","Year","AGE_CLASS","Gender"))

# Get rid of the ADER dataset which is quite big
rm("ADER")

# Calculate the total energy requirement for the household
# This info will be used later to distribute nutrients within the household
Data_Temp = 
  Individual %>%
  group_by(hh_no) %>%
  summarise(Total_Requirement = sum(AER_1.85))

# Now the Individual file has Individual requirements and the sum of these requirements
# We will use the ratio of individual requirements to the total requirement later to distribute 
# intra-household distribution of nutrients.
Individual = merge(Individual,Data_Temp,by=c("hh_no"))

# Now we will swtich to the main dataset we will be working with
# We will combine the Food, Country_NCT, and Household data files.

# First, let's deflate the income variable using the CPIs
# Deflate the income
Household$Inc = Household$new_inc * (Household$Average_CPI/Household$CPI)

# Now merge in the Food and Country_NCT data files
Data = merge(Food,Country_NCT,by.x=c("item_code"),by.y=c("item_cod"))
Data = merge(Data,Household,by=c("hh_no"))

# Deflate the food expenditure
Data$fd_mv = Data$fd_mv * (Data$Average_FPI/Data$FPI)

# Re-arrange the dataset so that we have one record for each nutrient
# The dataset will now have one record per household, per food item, per nutrient.
# Our 6 nutrients that we will work with are Vitamin A, Vitamin B1, Vitamin B2,
# Vitamin B6, Vitamin B12, and Vitamin C
Data = gather(select(Data,one_of("hh_no",
                                 "item_code",
                                 "fd_qty",
                                 "fd_mv",
                                 "refuse",
                                 "VitaminA",
                                 "VitaminB1",
                                 "VitaminB2",
                                 "VitaminB6",
                                 "VitaminB12",
                                 "Vit_C",
                                 "Inc",
                                 "hh_wgt",
                                 "region",
                                 "urb_rur",
                                 "HH_size")),
              key = Nutrient,value = Density,-hh_no,-item_code,-fd_qty,-fd_mv,-refuse,-Inc,-hh_wgt,-region,-urb_rur,-HH_size)

# Use nutrient densities from the Country_NCT file to come up with quantities of nutrients.
Data$Quantity = Data$Density*((Data$fd_qty-Data$fd_qty*(Data$refuse/100))/100)

# Calculate income quantiles using household weights
# The quintiles will be used as a grouping variable for impuation of missing food quantities.
# The deciles will be used as a grouping variable for an alternative calculation of one of the parameters
# needed to calculate the prevalence of inadequacy.
Inc_Quintiles = wtd.quantile(Household$Inc, 
                             weights=(Household$hh_wgt), 
                             probs=c(0,.2,.4,.6,.8))

Inc_Deciles = wtd.quantile(Household$Inc, 
                           weights=(Household$hh_wgt), 
                           probs=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9))

# Below we assign each household to the appropriate income quantile
Data = 
  Data %>%
  group_by(hh_no) %>%
  mutate(Inc_Quint = findInterval(Inc,Inc_Quintiles),
         Inc_Dec = findInterval(Inc,Inc_Deciles))

# Here we compute two different nutrient costs for imputation
# One is calculated by grouping by region, area (urban/rural), and income quintile for each nutrient
# A second nutrient cost for imputation is calculated without considering income quintile
Data_Temp = 
  Data %>%
  group_by(hh_no,Nutrient) %>%
  mutate(Cost=sum(Quantity[!is.na(Quantity)])/sum(fd_mv[!is.na(Quantity)])) %>%
  distinct(hh_no,Nutrient)  %>%
  group_by(region, urb_rur,Inc_Quint,Nutrient) %>%
  mutate(Imputation_reg_area_inc = median(Cost,na.rm=TRUE)) %>%
  group_by(region, urb_rur,Nutrient) %>%
  mutate(Imputation_reg_area = median(Cost,na.rm=TRUE))

# We were not able to mutate the imputation variables at the original
# structure of our dataset.  So we will have to merge it back in

# This merge takes quite a bit of time
Data = merge(Data,Data_Temp[,c("hh_no","Nutrient","Imputation_reg_area_inc","Imputation_reg_area")],by=c("hh_no","Nutrient"))

# Do the imputation of missing food quantities
# Try imputing calories for food away from home using the highest level of disaggregation
Data[is.na(Data$Quantity),]$Quantity = Data[is.na(Data$Quantity),]$fd_mv * 
  Data[is.na(Data$Quantity),]$Imputation_reg_area_inc
# Impute any values that are still missing using next level of disaggregation
Data[is.na(Data$Quantity),]$Quantity = Data[is.na(Data$Quantity),]$fd_mv * 
  Data[is.na(Data$Quantity),]$Imputation_reg_area

# Calculate the total amount of each nutrient for each household
Household_Nutrient = 
  Data %>%
  group_by(hh_no,Nutrient) %>%
  summarise(Total=sum(Quantity),
            Inc_Dec = max(Inc_Dec),
            hh_wgt = max(hh_wgt),
            HH_size = max(HH_size))

# Now we will merge back in the information from the Individual file neede to distribute
# food within the household.
# Note that we were not able to work with one dataset containing also this information, because the
# dataset would have been too big.
Individual_Nutrient = merge(Household_Nutrient,Individual,by=c("hh_no"))

# Distribute food within the household according to the ratio of the individual calorie requirement
# to the total household calorie requirement
Individual_Nutrient$Quantity = (Individual_Nutrient$AER_1.85/Individual_Nutrient$Total_Requirement)*Individual_Nutrient$Total

# We only have nutrient requirement data for individuals of age 1 or greater
# So from now on, we will only work with these individuals
Individual_Nutrient = Individual_Nutrient[Individual_Nutrient$hm_age>=1,]

# Create the appropriate age classes to merge in nutrient requirements
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=1] = 1
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=4] = 2
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=9] = 3
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=14] = 4
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=19] = 5
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=31] = 6
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=51] = 7
Individual_Nutrient$Nutrient_Age_Class[Individual_Nutrient$hm_age>=70] = 8

# Merge in the requirements by sex and age class for our 6 nutrients
Individual_Nutrient = merge(Individual_Nutrient,Requirements,by=c("Nutrient","Nutrient_Age_Class","gender"))

# Calculate distributional parameters for each of the 6 nutrients
# These will be the mean and CV of intake as well as the mean of requirement
Individual_Nutrient = 
  Individual_Nutrient %>%
  group_by(Nutrient) %>%
  mutate(Mean = wtd.mean(Quantity,weights=hh_wgt),
         CV = sqrt(wtd.var(Quantity, weights=hh_wgt))/Mean,
         Mean_Req = wtd.mean(Req,weights=hh_wgt),
         Sum = sum(hh_wgt),
         Number = n())

Individual_Nutrient$Inc_Dec = as.factor(Individual_Nutrient$Inc_Dec)

# Here we will calculate an alternative form for the CV of nutrient intake.
# This CV calculation groups by income decile for the calculation of the CV
Nutrient = 
  Individual_Nutrient %>%
  group_by(Nutrient,Inc_Dec) %>%
  mutate(IncomeSecondCentralMoment_Inc_Dec = ((wtd.mean(Quantity,weights=hh_wgt)-max(Mean))^2)*(sum(hh_wgt)/max(Sum))) %>%
  distinct(Nutrient,Inc_Dec)  %>%
  group_by(Nutrient) %>%
  summarise(IncomeSecondCentralMoment = sum(IncomeSecondCentralMoment_Inc_Dec),
            Mean = max(Mean),
            CV = max(CV),
            Mean_Req = max(Mean_Req),
            Number = max(Number))
  
# Finish the alternative calculation of the CV of nutrient intake
Nutrient$CVDueToIncome = sqrt(Nutrient$IncomeSecondCentralMoment*(Nutrient$Number/(Nutrient$Number-1)))/Nutrient$Mean

Nutrient$CV_Inc = sqrt(Nutrient$CVDueToIncome^2+.2^2)

# The last distributional parameter (the CV of requirement) is easy
# The value is .2 for Vitamn A and .1 for the other nutrients
Nutrient$CV_Req[Nutrient$Nutrient=="VitaminA"] = .2
Nutrient$CV_Req[Nutrient$Nutrient!="VitaminA"] = .1

# Here is our function that will calculate the prevalence of inadequacy
PoI = function(Parameters) {
  
  Probability = function(x) {
    
    (1-pnorm(x,mean=Parameters[1],sd=Parameters[2]*Parameters[1]))*
      (1/x)*dnorm(log(x),mean=log(Parameters[3])-log(Parameters[4]^2+1)/2,sd=sqrt(log(Parameters[4]^2+1)))    
    
  }
  
  PoI = integrate(Probability,0,Inf)$value
  
}

# Now, using the function above, we will calculate the prevalence of inadequacy with the empirical mean and CV
# as well as the empirical mean and alternative form for the CV
# for each nutrient.
Nutrient = 
  Nutrient %>%
  group_by(Nutrient) %>%
  mutate(Prevalence = PoI(c(Mean_Req,CV_Req,Mean,CV)),
         Prevalence_EAR = pnorm(log(Mean_Req),mean=log(Mean)-log(CV^2+1)/2,sd=sqrt(log(CV^2+1))),
         Prevalence_Inc = PoI(c(Mean_Req,CV_Req,Mean,CV_Inc)),
         Prevalence_EAR_Inc = pnorm(log(Mean_Req),mean=log(Mean)-log(CV_Inc^2+1)/2,sd=sqrt(log(CV_Inc^2+1))))

# Output our results.
write.csv(Nutrient[,c("Nutrient","Prevalence","Prevalence_EAR","Prevalence_Inc","Prevalence_EAR_Inc")],"Output.csv")
