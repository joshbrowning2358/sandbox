# 1, Load all the variables you need to re-create FinalDataStructure in
# 2. Files cleaning; selecting the desired variables in each file
# 3. Merging PrimoBlocco and SecondoBlocco
# 4. Loading the numeric values in GDPcurrent-NCU-countries.xls
# 5. Export the dataframe in R to an Excel (.csv) file
# 6. This code produces, reading row by row, histograms, plots and summary
# stats. and saves them in the current directory
# 7. Save the history file


# Clean the workspace
rm(list = ls())

#
##
###
# Old
# tm1 <- system.time({
###
##
#

# Note: you generally don't want to wrap your whole script in a system.time() 
# call.  Instead, write your code and then use something like 
# system.time(source("filename.R")) to measure computation time.  Or, create a
# time object and look at the difference:

#
##
###
# New
tm1 <- Sys.time()
###
##
#

library("XLConnect")

# Section One ----------------------------------------------
# Load all the variables you need to re-create FinalDataStructure in
# 1.NAE_DictionaryForMerge.xlsx

# Set the path and list the files 
setwd("T:/Team_working_folder/Eco/Macro_AIM/1.AIM_DataSteps/1.2AIM_CapturedData/DataMerge_2015")
setwd("~/Documents/Github/sandbox/R_group/VanderDonckt/")
list.files(path = ".")

# Read FaostatAreas2015.csv & CountryISOcode.csv
#
##
###
# Old
Block1 = read.csv("FaostatAreas2015.csv")
Block3 = read.csv("CountryISOcode.csv")
###
##
#

#
##
###
# New
faostatAreas = read.csv("FaostatAreas2015.csv")
countryCodes = read.csv("CountryISOcode.csv")
###
##
#

str(faostatAreas)
# Print the headings of the 2 files (faostatAreas & countryCodes)
head(faostatAreas)
head(countryCodes)
# We need these variables from the 2 files 
# FAOSTATAreas2015.csv: CountryISOCode  CountryName	CountryFAOCode (faostatAreas)
# CountryISOcode.csv: CurrencyCode (countryCodes)

# Read 1.NAE_DictionaryForMerge.xlsx (3rd spreadsheet)
Block2 = loadWorkbook("1.NAE_DictionaryForMerge.xlsx")
#
##
###
# Old
B2 = readWorksheet(Block2, sheet=3)
head(B2)
###
##
#
# Edit (JOSH): It's easier to skip the first row so that column names come in at
# the right place.  You have to manually assign one, but that's not bad.
#
##
###
# Old
B2 = readWorksheet(Block2, sheet=3, startRow = 2)
head(B2)
colnames(B2)[colnames(B2) == "Col12"] = "In.AIM.DB."
###
##
#

# We need these variables from the file (Block2)
# NAE_DictionaryForMerge.xlsx: ActivityCode  ActivityName	ISIC	IndicatorCode	IndicatorName
# NAE_DictionaryForMerge.xlsx: BYear	Units	OriginalDB

# Section Two ----------------------------------------------
# Files cleaning; selecting the desired variables in each file

# Remove the rows with empty cells in faostatAreas, e.g., USSR has no ISO code,
# hence we drop it

faostatAreas <- faostatAreas[, c("ISO", "Country", "FAOCode")]
colnames(faostatAreas) <- c("CountryISOCode", "CountryName", "CountryFAOCode")
faostatAreas[faostatAreas == ""] <- NA
faostatAreas <- subset(faostatAreas, rowSums(is.na(faostatAreas))==0)
# An alternative approach to the above code, that may be a tiny bit faster (as
# it avoids having to create and then remove NAs):
filter <- apply(faostatAreas, 1, function(x) any(x == ""))
faostatAreas <- faostatAreas[!filter, ]

# In countryCodes we select the country name and the ISO.4217.Currency.Code
#
##
###
# Old
B3a=countryCodes[2]
B3b=countryCodes[8]
countryCodes = data.frame(B3a, B3b)
###
##
#

# Note (JOSH): It's often much safer to refer to variables by name rather than
# location (i.e. second and eigth column).  This allows you to change the input
# data without completely messing up the code.
#
##
###
# New
countryCodes = countryCodes[, c("Common.Name", "ISO.4217.Currency.Code")]
###
##
#

#
##
###
# Old
# Now we merge countryCodes & faostatAreas 
# First we add a 4th column to PrimoBlocco (with NAs)
PrimoBlocco <- faostatAreas
PrimoBlocco[,4] <- NA

# Then we start merging faostatAreas & countryCodes: the resulting file is faostatAreas
# PrimoBlocco <- data.matrix(PrimoBlocco)
# TerzoBlocco <- data.matrix(TerzoBlocco)
# We want to add currency codes to PrimoBlocco (from countryCodes)
head(PrimoBlocco)
head(countryCodes)

for (i in 1:nrow(PrimoBlocco)) {  
  for (j in 1:nrow(countryCodes)) { 
    if (!length(agrep(PrimoBlocco[i,2],countryCodes[j,1], value = FALSE, fixed = TRUE)==1)) {
      PrimoBlocco[i,4] <- PrimoBlocco[i,4] 
    } else {  
      if (agrep(PrimoBlocco[i,2],countryCodes[j,1], value = FALSE, fixed = TRUE)==1) {
        PrimoBlocco[i,4] <- as.character(countryCodes[j,2]) 
      }
    }
  }
}

# Assign to PrimoBlocco the 4 desired column names
colnames(PrimoBlocco) <- c("CountryISOCode", "CountryName",
                           "CountryFAOCode", "CurrencyCode")
###
##
#

#
##
###
# New
rMerge <- merge(faostatAreas, countryCodes)
head(rMerge)
dim(rMerge)
dim(faostatAreas)




rMerge <- merge(faostatAreas, countryCodes, by.x = "CountryName",
             by.y = "Common.Name")
head(rMerge)
dim(rMerge)
dim(faostatAreas)




rMerge <- merge(faostatAreas, countryCodes, by.x = "CountryName",
             by.y = "Common.Name", all.x = TRUE)
head(rMerge)
dim(rMerge)
dim(faostatAreas)



colnames(rMerge)[colnames(rMerge) == "ISO.4217.Currency.Code"] = "CurrencyCode"
###
##
#

dim(rMerge)
dim(PrimoBlocco)
compare = merge(rMerge, PrimoBlocco, by = c("CountryISOCode", "CountryName",
                                            "CountryFAOCode"),
                suffixes = c(".merge", ".for"))
dim(compare)
View(compare)
filter = compare$CurrencyCode.merge != compare$CurrencyCode.for
View(compare[filter, ])
(1:10)[c(T, F, T, F, rep(NA, 6))]
## Remove the NA's from filter
filter[is.na(filter)] = FALSE
## But, we still want to check for differences when one is NA and the other is
## not...
filter = filter |
    (is.na(compare$CurrencyCode.merge) & !is.na(compare$CurrencyCode.for)) |
    (is.na(compare$CurrencyCode.for) & !is.na(compare$CurrencyCode.merge))
View(compare[filter, ])

## Examine a few cases closer:
PrimoBlocco[PrimoBlocco[, 4] == "AWG", ]
PrimoBlocco[!is.na(PrimoBlocco[, 4]) & PrimoBlocco[, 4] == "AWG", ]
countryCodes[countryCodes[, 2] == "AWG", ]
agrep("Cuba", "Aruba", value = FALSE, fixed = TRUE)
PrimoBlocco[!is.na(PrimoBlocco[, 4]) & PrimoBlocco[, 4] == "SHP", ]
countryCodes[countryCodes[, 2] == "SHP", ]
agrep("Spain", "Saint Helena", value = FALSE, fixed = TRUE)


#
##
###
# Old
# Now focus on SecondoBlocco (from B2, i.e., 1.NAE_DictionaryForMerge.xlsx -- 3rd spreadsheet) )
# In B2, select (for each country) only the 5 variables we are interested in, GFCF, GDP, 3 VA (Total Ec, Agric., Manif.)
B2 <- subset(B2, B2$In.AIM.DB. == "Yes")
#head(B2)
# In B2, select only the desired variables (columns) that are present in 1.NAE_DictionaryForMerge.xlsx -- FinalDataStructure 
a <- c(B2[6]) #ActivityCode
b <- c(B2[7]) #ActivityName
c <- c(B2[8]) #ISIC
d <- c(B2[4]) #IndicatorCode
e <- c(B2[5]) #IndicatorName
f <- c(B2[11])#BYear  	
g <- c(B2[10])#Units
h <- c(B2[9]) #OriginalDB
# Create the data.frame structure
SecondoBlocco = data.frame(a, b, c, d, e, f, g, h)
# Assign to SecondoBlocco the 8 desired column names
colnames(SecondoBlocco) <- c("ActivityCode", "ActivityName", "ISIC",
                             "IndicatorCode", "IndicatorName", "BYear",
                             "Units", "OriginalDB")
###
##
#

#
##
###
# New
SecondoBloccoNew <- subset(B2, In.AIM.DB. == "Yes",
    select = c(ActivityCode, ActivityName, ISIC, IndicatorCode,
               IndicatorName.1, BYear, Units, OriginalDB))
###
##
#

SecondoBlocco
SecondoBloccoNew
SecondoBlocco == SecondoBloccoNew
colnames(SecondoBlocco) <- c("ActivityCode", "ActivityName", "ISIC",
                             "IndicatorCode", "IndicatorName", "BYear",
                             "Units", "OriginalDB")

# Section Three ----------------------------------------------
# Merging PrimoBlocco and SecondoBlocco

#
##
###
# Old
# Replicate PrimoBlocco 5 times (5 variables)
PrimoBloccoRep <- do.call(rbind, replicate(nrow(SecondoBlocco), as.matrix(PrimoBlocco), simplify=FALSE))
dim(PrimoBloccoRep)
5 * nrow(PrimoBlocco)

# # The aim here below is to re-arrange PrimoBloccoRep in this order:
# # 1  AM	Armenia	    1	AMD
# #	2	 AM	Armenia	    1	AMD
# #	3	 AM	Armenia	    1	AMD
# #	4	 AM	Armenia	    1	AMD
# #	5	 AM	Armenia	    1	AMD
# #	6	 AF	Afghanistan	2	AFN
# #	7	 AF	Afghanistan	2	AFN
# #	8	 AF	Afghanistan	2	AFN
# #	9	 AF	Afghanistan	2	AFN
# #	10 AF	Afghanistan	2	AFN
# #	11 AL	Albania	    3	ALL
# # etc.
# 
# This is what the following loop does
for (i in 1:nrow(PrimoBlocco)) {
  PrimoBloccoRep[(nrow(SecondoBlocco)*(i-1)+1):(nrow(SecondoBlocco)*i),1:4] <-
      do.call(rbind, replicate(nrow(SecondoBlocco), as.matrix(PrimoBlocco[i,]), simplify=FALSE))
  #print(do.call(rbind, replicate(nrow(SecondoBlocco), as.matrix(PrimoBlocco[i,]), simplify=FALSE)))
}
rownames(PrimoBloccoRep) <- 1:nrow(PrimoBloccoRep)

# Replicate SecondoBlocco 239 times (239 countries)
SecondoBloccoRep <- do.call(rbind, replicate(nrow(PrimoBlocco),
                                             as.matrix(SecondoBlocco), simplify=FALSE))

# Now merge the 2 matrices in a data.frame (called FinalData)
FinalData = data.frame(PrimoBloccoRep, SecondoBloccoRep)
# Assign to FinalData the desired column names
colnames(FinalData) <-
    c("CountryISOCode", "CountryName", "CountryFAOCode", "CurrencyCode",
      "ActivityCode", "ActivityName",  "ISIC",  "IndicatorCode",
      "IndicatorName",	"BYear",	"Units",	"OriginalDB")
###
##
#

#
##
###
# New
# Merging by "NULL" means that you get a cartesian product: every row of the
# first data.frame joins to every row of the second data.frame.
FinalData2 <- merge(PrimoBlocco, SecondoBlocco, by = NULL)
###
##
#

dim(FinalData2)
dim(FinalData)
## Not always easy to check equality:
FinalData2 == FinalData
head(FinalData2)
head(FinalData)
dim(PrimoBlocco)
dim(SecondoBlocco)
239*5

# Section Four ----------------------------------------------
# Loading the numeric values in GDPcurrent-NCU-countries.xls

setwd("T:/Team_working_folder/Eco/Macro_AIM/1.AIM_DataSteps/1.2AIM_CapturedData/DataMerge_2015")  
list.files(path = ".")
Data = loadWorkbook("GDPcurrent-NCU-countries.xls")
dataloaded = readWorksheet(Data, sheet=1)
dataloaded <- format(dataloaded, scientific = FALSE)

#
##
###
# Old
dataload <- data.frame(CountryName = c(dataloaded[1]), IndicatorName = c(dataloaded[3]),
                       ActivityCode = c(dataloaded[4]), Col5 = c(dataloaded[5]),
                       Col6 = c(dataloaded[6]), Col7 = c(dataloaded[7]),
                       Col8 = c(dataloaded[8]), Col9 = c(dataloaded[9]),
                       Col10 = c(dataloaded[10]), Col11 = c(dataloaded[11]),
                       Col12 = c(dataloaded[12]), Col13 = c(dataloaded[13]),
                       Col14 = c(dataloaded[14]), Col15 = c(dataloaded[15]),
                       Col16 = c(dataloaded[16]), Col17 = c(dataloaded[17]),
                       Col18 = c(dataloaded[18]), Col19 = c(dataloaded[19]),
                       Col20 = c(dataloaded[20]), Col21 = c(dataloaded[21]),
                       Col22 = c(dataloaded[22]), Col23 = c(dataloaded[23]),
                       Col24 = c(dataloaded[24]), Col25 = c(dataloaded[25]),
                       Col26 = c(dataloaded[26]), Col27 = c(dataloaded[27]),
                       Col28 = c(dataloaded[28]), Col29 = c(dataloaded[29]),
                       Col30 = c(dataloaded[30]), Col31 = c(dataloaded[31]),
                       Col32 = c(dataloaded[32]), Col33 = c(dataloaded[33]),
                       Col34 = c(dataloaded[34]), Col35 = c(dataloaded[35]),
                       Col36 = c(dataloaded[36]), Col37 = c(dataloaded[37]),
                       Col38 = c(dataloaded[38]), Col39 = c(dataloaded[39]),
                       Col40 = c(dataloaded[40]), Col41 = c(dataloaded[41]),
                       Col42 = c(dataloaded[42]), Col43 = c(dataloaded[43]),
                       Col44 = c(dataloaded[44]), Col45 = c(dataloaded[45]),
                       Col46 = c(dataloaded[46]), Col47 = c(dataloaded[47]),
                       Col48 = c(dataloaded[48]))
colnames(dataload) <- c("CountryName", "IndicatorName", "ActivityCode",
                        "1970", "1971", "1972", "1973", "1974", "1975",
                        "1976", "1977", "1978", "1979", "1980", "1981",
                        "1982", "1983", "1984", "1985", "1986", "1987",
                        "1988", "1989", "1990", "1991", "1992", "1993",
                        "1994", "1995", "1996", "1997", "1998", "1999",
                        "2000", "2001", "2002", "2003", "2004", "2005",
                        "2006", "2007", "2008", "2009", "2010", "2011",
                        "2012", "2013")
nrowdataload <- nrow(dataload)
dataload <- as.matrix(dataload)
###
##
#

#
##
###
# New
Data = loadWorkbook("GDPcurrent-NCU-countries.xls")
dataloaded = readWorksheet(Data, sheet=1)
dataloaded <- format(dataloaded, scientific = FALSE)
dataload <- dataloaded
dataload$Currency <- NULL
colnames(dataload) <- c("CountryName", "IndicatorName", "ActivityCode", 1970:2013)

nrowdataload <- nrow(dataload)
###
##
#

nrowFinalData <- nrow(FinalData)

# Merging the two datasets (FinalData and dataload) when appropriate
# AtQ_01t99
# 
FinalData[1:5, 2]
dataload[1:5, 1]
FinalData[1:5, 9]
dataload[1:5, 2]
FinalData[1:5, 5]
dataload[1:5, 3]

#
##
###
# Old
for (i in 1:nrowdataload) {
  if (dataload[i,2] == "Total Value Added") {
      dataload[i,2] <- "Value Added"
  } else {
  if (dataload[i,2] == "Gross Domestic Product (GDP)") {
      dataload[i,2] <- "Gross Domestic Product"
  } else {
  if (dataload[i,2] == "Gross fixed capital formation (including Acquisitions less disposals of valuables)") {
      dataload[i,2] <- "Gross fixed capital formation"
  } else {
  if (dataload[i,3] == "AtB_01t05") {
      dataload[i,2] <- "Value Added"
  } else {
      if (dataload[i,3] == "D_15t37") {
      dataload[i,2] <- "Value Added"
          }  
        }
      }
    }
  }
}
###
##
#

dataload$IndicatorName[dataload$IndicatorName == "Total Value Added"] = "Value Added"
dataload$IndicatorName[dataload$IndicatorName == "Gross Domestic Product (GDP)"] =
    "Gross Domestic Product"
dataload$IndicatorName[dataload$IndicatorName == "Gross fixed capital formation (including Acquisitions less disposals of valuables)"] =
    "Gross fixed capital formation"
dataload$IndicatorName[dataload$ActivityCode == "AtB_01t05"] = "Value Added"
dataload$IndicatorName[dataload$ActivityCode == "D_15t37"] = "Value Added"

FinalDataStructure <- merge(FinalData, dataload, by= c("CountryName", "IndicatorName", "ActivityCode"), all.x = TRUE)
dim(FinalData)
dim(FinalDataStructure)
head(FinalDataStructure)
dim(dataload)
## Where did the extra rows come from???

library(dplyr)
dataload$count = 1
dataload %>%
    group_by(CountryName, IndicatorName, ActivityCode) %>%
    summarize(rowCount = sum(count))
dim(dataload) # 3712 but only 3695 rows in summary
dataload %>%
    group_by(CountryName, IndicatorName, ActivityCode) %>%
    summarize(rowCount = sum(count)) %>%
    filter(rowCount > 1)
# So, 'The former Yugoslav Republic of Macedonia' has duplicate records
dataload %>%
    filter(CountryName == 'The former Yugoslav Republic of Macedonia')

# Section Five ----------------------------------------------
# Export the dataframe in R to an Excel (.csv) file
write.csv(FinalDataStructure, file = "FinalDataStructure.csv", row.names = FALSE)

print(Sys.time() - tm1)
# to run it up to here it takes moreless 25 mins


# Section Six ----------------------------------------------
# This code produces, reading row by row, histograms, plots and summary stats. and saves them in the current directory


# Start writing to an output file
sink('analysis-output.txt')
cat("====================================================================\n")
cat("Descriptive statistics : mean,median,25th and 75th quartiles,min,max\n")
cat("====================================================================\n")

for (i in 1:nrow(FinalData)) { print(i)

#  FinalDataNum <- ts(FinalData[i,13:56])
  
  if (length(sum(!is.na(FinalData[i,13:56]))>0)==1) { 
    }
  else {
  
  #hist(as.numeric(FinalData[i,13:56]), breaks=11, col = "lightblue", border = "pink", main=as.character(FinalData[i,2:2]), xlab=as.character(FinalData[i,9:9])) 
    
  #require(xts)
  #PCP <- ts(as.numeric(FinalData[i,13:56]), frequency = 1, start = 1970)
  #ts_plot <- plot(as.xts(PCP), major.format = "%Y", main=as.character(FinalData[i,2:2]))
  #ts_plot
  }

    #nam <- paste(as.character(FinalData[i,2:2]), i, "eps", sep = ".")
  print("------------------------------------------------------------")
  suppressWarnings(print(as.character(FinalData[i,2:2]))) 
  suppressWarnings(print(as.character(FinalData[i,9:9])))
  suppressWarnings(print(as.character(FinalData[i,5:5])))
  suppressWarnings(print(summary(as.numeric(FinalData[i,13:56]))))
}

  #fivenum(as.numeric(FinalData[i,13:56]))
  #print("------------------------------------------------------------")
  #dev.copy2eps(file=nam, horizontal=F)


# Section Seven ----------------------------------------------
# Save the history file

timestamp(stamp = date(),
          prefix = "##------ ", suffix = " ------##",
          quiet = FALSE)

# Stop writing to the file
sink()

# Append to the file (analysis-output.txt): look in the current directory
sink('analysis-output.txt', append=TRUE)
sink()


#history(max.show=200) # display 200 previous commands
#savehistory(file=".Rhistory")
