tm1 <- system.time(
{
# Section One ----------------------------------------------
# Load all the variables you need to re-create FinalDataStructure in 1.NAE_DictionaryForMerge.xlsx

# Clean the workspace
rm(list = ls())

# Set the path and list the files 
setwd("T:/Team_working_folder/Eco/Macro_AIM/1.AIM_DataSteps/1.2AIM_CapturedData/DataMerge_2015")
list.files(path = ".")

# Read FaostatAreas2015.csv & CountryISOcode.csv
Block1 = read.csv("FaostatAreas2015.csv")
Block3 = read.csv("CountryISOcode.csv")
#str(Block1)
# Print the headings of the 2 files (Block1 & Block3)
#head(Block1)
#head(Block3)
# We need these variables from the 2 files 
# FAOSTATAreas2015.csv: CountryISOCode  CountryName	CountryFAOCode (Block1)
# CountryISOcode.csv: CurrencyCode (Block3)

# Change the path in order to read other files
setwd("T:/Team_working_folder/Eco/Macro_AIM/1.AIM_DataSteps/1.2AIM_CapturedData")  
library("XLConnect")
list.files(path = ".")

# Read 1.NAE_DictionaryForMerge.xlsx (3rd spreadsheet)
Block2 = loadWorkbook("1.NAE_DictionaryForMerge.xlsx")
B2 = readWorksheet(Block2, sheet=3)
# Print the headings of Block2
#head(Block2)
# We need these variables from the file (Block2)
# NAE_DictionaryForMerge.xlsx: ActivityCode  ActivityName	ISIC	IndicatorCode	IndicatorName
# NAE_DictionaryForMerge.xlsx: BYear	Units	OriginalDB


# Section Two ----------------------------------------------
# Files cleaning; selecting the desired variables in each file

# Remove the rows with empty cells in Block1, e.g., USSR has no ISO code, hence we drop it
PrimoBlocco <- data.frame(CountryISOCode = c(Block1[2]), CountryName = c(Block1[3]), CountryFAOCode = c(Block1[1]))
PrimoBlocco[PrimoBlocco == ""] <- NA
PrimoBlocco <- subset(PrimoBlocco, rowSums(is.na(PrimoBlocco))==0)
# Assign to Block1 the desired column names
colnames(PrimoBlocco) <- c("CountryISOCode", "CountryName", "CountryFAOCode")
PrimoBlocco <- subset(PrimoBlocco, select=c("CountryISOCode", "CountryName", "CountryFAOCode"))

# In TerzoBlocco we select the country name and the ISO.4217.Currency.Code
B3a=Block3[2]
B3b=Block3[8]
TerzoBlocco = data.frame(B3a, B3b)

# Now we merge PrimoBlocco & TerzoBlocco 
# First we add a 4th column to PrimoBlocco (with NAs)
PrimoBlocco[,4] <- NA

# Then we start merging PrimoBlocco & TerzoBlocco: the resulting file is PrimoBlocco 
# PrimoBlocco <- data.matrix(PrimoBlocco)
# TerzoBlocco <- data.matrix(TerzoBlocco)
for (i in 1:nrow(PrimoBlocco)) {  
  for (j in 1:nrow(TerzoBlocco)) { 
    if (!length(agrep(PrimoBlocco[i,2],TerzoBlocco[j,1], value = FALSE, fixed = TRUE)==1)) {
      PrimoBlocco[i,4] <- PrimoBlocco[i,4] 
    } else {  
      if (agrep(PrimoBlocco[i,2],TerzoBlocco[j,1], value = FALSE, fixed = TRUE)==1) {
        PrimoBlocco[i,4] <- as.character(TerzoBlocco[j,2]) 
      }
    }
  }
}
# Assign to PrimoBlocco the 4 desired column names
colnames(PrimoBlocco) <- c("CountryISOCode", "CountryName", "CountryFAOCode", "CurrencyCode")

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
colnames(SecondoBlocco) <- c("ActivityCode", "ActivityName",  "ISIC",  "IndicatorCode",	"IndicatorName",	"BYear",	"Units",	"OriginalDB")


# Section Three ----------------------------------------------
# Merging PrimoBlocco and SecondoBlocco

# Replicate PrimoBlocco 5 times (5 variables)
PrimoBloccoRep <- do.call(rbind, replicate(nrow(SecondoBlocco), as.matrix(PrimoBlocco), simplify=FALSE))

# The aim here below is to re-arrange PrimoBloccoRep in this order:
# 1  AM	Armenia	    1	AMD
#	2	 AM	Armenia	    1	AMD
#	3	 AM	Armenia	    1	AMD
#	4	 AM	Armenia	    1	AMD
#	5	 AM	Armenia	    1	AMD
#	6	 AF	Afghanistan	2	AFN
#	7	 AF	Afghanistan	2	AFN
#	8	 AF	Afghanistan	2	AFN
#	9	 AF	Afghanistan	2	AFN
#	10 AF	Afghanistan	2	AFN
#	11 AL	Albania	    3	ALL
# etc.

# This is what the following loop does
for (i in 1:nrow(PrimoBlocco)) {    
  PrimoBloccoRep[(nrow(SecondoBlocco)*(i-1)+1):(nrow(SecondoBlocco)*i),1:4] <- do.call(rbind, replicate(nrow(SecondoBlocco), as.matrix(PrimoBlocco[i,]), simplify=FALSE))
  #print(do.call(rbind, replicate(nrow(SecondoBlocco), as.matrix(PrimoBlocco[i,]), simplify=FALSE)))
}
rownames(PrimoBloccoRep) <- 1:nrow(PrimoBloccoRep)

# Replicate SecondoBlocco 239 times (239 countries)
SecondoBloccoRep <- do.call(rbind, replicate(nrow(PrimoBlocco), as.matrix(SecondoBlocco), simplify=FALSE))

# Now merge the 2 matrices in a data.frame (called FinalData)
FinalData = data.frame(PrimoBloccoRep, SecondoBloccoRep)
# Assign to FinalData the desired column names
colnames(FinalData) <- c("CountryISOCode", "CountryName", "CountryFAOCode", "CurrencyCode", "ActivityCode", "ActivityName",  "ISIC",  "IndicatorCode",  "IndicatorName",	"BYear",	"Units",	"OriginalDB")


# Section Four ----------------------------------------------
# Loading the numeric values in GDPcurrent-NCU-countries.xls

setwd("T:/Team_working_folder/Eco/Macro_AIM/1.AIM_DataSteps/1.2AIM_CapturedData/DataMerge_2015")  
list.files(path = ".")
Data = loadWorkbook("GDPcurrent-NCU-countries.xls")
dataloaded = readWorksheet(Data, sheet=1)
dataloaded <- format(dataloaded, scientific = FALSE)

dataload <- data.frame(CountryName = c(dataloaded[1]), IndicatorName = c(dataloaded[3]), ActivityCode = c(dataloaded[4]), Col5 = c(dataloaded[5]), Col6 = c(dataloaded[6]), Col7 = c(dataloaded[7]), Col8 = c(dataloaded[8]), Col9 = c(dataloaded[9]), Col10 = c(dataloaded[10]), Col11 = c(dataloaded[11]), Col12 = c(dataloaded[12]), Col13 = c(dataloaded[13]), Col14 = c(dataloaded[14]), Col15 = c(dataloaded[15]), Col16 = c(dataloaded[16]), Col17 = c(dataloaded[17]), Col18 = c(dataloaded[18]), Col19 = c(dataloaded[19]), Col20 = c(dataloaded[20]), Col21 = c(dataloaded[21]), Col22 = c(dataloaded[22]), Col23 = c(dataloaded[23]), Col24 = c(dataloaded[24]), Col25 = c(dataloaded[25]), Col26 = c(dataloaded[26]), Col27 = c(dataloaded[27]), Col28 = c(dataloaded[28]), Col29 = c(dataloaded[29]), Col30 = c(dataloaded[30]), Col31 = c(dataloaded[31]), Col32 = c(dataloaded[32]), Col33 = c(dataloaded[33]), Col34 = c(dataloaded[34]), Col35 = c(dataloaded[35]), Col36 = c(dataloaded[36]), Col37 = c(dataloaded[37]), Col38 = c(dataloaded[38]), Col39 = c(dataloaded[39]), Col40 = c(dataloaded[40]), Col41 = c(dataloaded[41]), Col42 = c(dataloaded[42]), Col43 = c(dataloaded[43]), Col44 = c(dataloaded[44]), Col45 = c(dataloaded[45]), Col46 = c(dataloaded[46]), Col47 = c(dataloaded[47]), Col48 = c(dataloaded[48]))
colnames(dataload) <- c("CountryName", "IndicatorName", "ActivityCode",  "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
nrowdataload <- nrow(dataload)
dataload <- as.matrix(dataload)

FinalData$"1970" <- c(rep(NA, nrow(FinalData)))
FinalData$"1971" <- c(rep(NA, nrow(FinalData)))
FinalData$"1972" <- c(rep(NA, nrow(FinalData)))
FinalData$"1973" <- c(rep(NA, nrow(FinalData)))
FinalData$"1974" <- c(rep(NA, nrow(FinalData)))
FinalData$"1975" <- c(rep(NA, nrow(FinalData)))
FinalData$"1976" <- c(rep(NA, nrow(FinalData)))
FinalData$"1977" <- c(rep(NA, nrow(FinalData)))
FinalData$"1978" <- c(rep(NA, nrow(FinalData)))
FinalData$"1979" <- c(rep(NA, nrow(FinalData)))
FinalData$"1980" <- c(rep(NA, nrow(FinalData)))
FinalData$"1981" <- c(rep(NA, nrow(FinalData)))
FinalData$"1982" <- c(rep(NA, nrow(FinalData)))
FinalData$"1983" <- c(rep(NA, nrow(FinalData)))
FinalData$"1984" <- c(rep(NA, nrow(FinalData)))
FinalData$"1985" <- c(rep(NA, nrow(FinalData)))
FinalData$"1986" <- c(rep(NA, nrow(FinalData)))
FinalData$"1987" <- c(rep(NA, nrow(FinalData)))
FinalData$"1988" <- c(rep(NA, nrow(FinalData)))
FinalData$"1989" <- c(rep(NA, nrow(FinalData)))
FinalData$"1990" <- c(rep(NA, nrow(FinalData)))
FinalData$"1991" <- c(rep(NA, nrow(FinalData)))
FinalData$"1992" <- c(rep(NA, nrow(FinalData)))
FinalData$"1993" <- c(rep(NA, nrow(FinalData)))
FinalData$"1994" <- c(rep(NA, nrow(FinalData)))
FinalData$"1995" <- c(rep(NA, nrow(FinalData)))
FinalData$"1996" <- c(rep(NA, nrow(FinalData)))
FinalData$"1997" <- c(rep(NA, nrow(FinalData)))
FinalData$"1998" <- c(rep(NA, nrow(FinalData)))
FinalData$"1999" <- c(rep(NA, nrow(FinalData)))
FinalData$"2000" <- c(rep(NA, nrow(FinalData)))
FinalData$"2001" <- c(rep(NA, nrow(FinalData)))
FinalData$"2002" <- c(rep(NA, nrow(FinalData)))
FinalData$"2003" <- c(rep(NA, nrow(FinalData)))
FinalData$"2004" <- c(rep(NA, nrow(FinalData)))
FinalData$"2005" <- c(rep(NA, nrow(FinalData)))
FinalData$"2006" <- c(rep(NA, nrow(FinalData)))
FinalData$"2007" <- c(rep(NA, nrow(FinalData)))
FinalData$"2008" <- c(rep(NA, nrow(FinalData)))
FinalData$"2009" <- c(rep(NA, nrow(FinalData)))
FinalData$"2010" <- c(rep(NA, nrow(FinalData)))
FinalData$"2011" <- c(rep(NA, nrow(FinalData)))
FinalData$"2012" <- c(rep(NA, nrow(FinalData)))
FinalData$"2013" <- c(rep(NA, nrow(FinalData)))

nrowFinalData <- nrow(FinalData)
FinalData <- as.matrix(FinalData)

# Merging the two datasets (FinalData and dataload) when appropriate
# AtQ_01t99
for (i in 1:nrowFinalData) {  print(i)
   for (j in 1:nrowdataload) { 
      if (!length(agrep(FinalData[i,2],dataload[j,1], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,9],dataload[j,2], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,5],dataload[j,3], value = FALSE, fixed = TRUE)==1)) {
        FinalData[i,2] <- FinalData[i,2] 
      } 
      else {  
      if (agrep(FinalData[i,2],dataload[j,1], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,9],dataload[j,2], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,5],dataload[j,3], value = FALSE, fixed = TRUE)==1) { 
        for (k in 13:56) {
          FinalData[i,k] <- dataload[j,k-9] 
        }
      }
      }
   }
}

# "AtB_01t05"
for (i in 1:nrowFinalData) { print(i)
    for (j in 1:nrowdataload) {  
      if (!length(agrep(FinalData[i,5],"AtB_01t05", value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,5],dataload[j,3], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,2],dataload[j,1], value = FALSE, fixed = TRUE)==1)) {
        FinalData[i,2] <- FinalData[i,2]
      } 
      else {
      if (agrep(FinalData[i,5],"AtB_01t05", value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,5],dataload[j,3], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,2],dataload[j,1], value = FALSE, fixed = TRUE)==1) {              
        for (k in 13:56) {
          FinalData[i,k] <- dataload[j,k-9] 
        }
      }
      }
    }
}

# "D_15t37"      
for (i in 1:nrowFinalData) { print(i)
    for (j in 1:nrowdataload) {  
      if (!length(agrep(FinalData[i,5], "D_15t37", value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,5],dataload[j,3], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,2],dataload[j,1], value = FALSE, fixed = TRUE)==1)) {
        FinalData[i,2] <- FinalData[i,2]
      } 
      else {
      if (agrep(FinalData[i,5], "D_15t37", value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,5],dataload[j,3], value = FALSE, fixed = TRUE)==1 & agrep(FinalData[i,2],dataload[j,1], value = FALSE, fixed = TRUE)==1) 
           {              
        for (k in 13:56) {
          FinalData[i,k] <- dataload[j,k-9] 
         }
         }
         }
    }
}


# Section Five ----------------------------------------------
# Export the dataframe in R to an Excel (.csv) file
write.csv(FinalData, file = "FinalDataStructure.csv")

})
print(tm1)
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
