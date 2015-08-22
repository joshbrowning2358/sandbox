#MASTER FILE FOR SCRAPING DATA FOR HOUSING PRICE ANALYSIS IN ROME
library(rvest)
library(data.table)
library(stringr)

##Get working directory 
sys <- Sys.info()
if(sys[4] == "JOSH_LAPTOP"){
  setwd("~/GitHub/sandbox/R_group/RealEstate/")
} else if(sys[5] == "x86_64"){
  setwd("~/Dropbox/web_scraping_imm")  #Mac
} else {
  setwd("C:/Users/rahija/Dropbox/web_scraping_imm") #FAO
}

########################################################################
#create urls to scrape
starturl <- "http://www.immobiliare.it/prezzi-mq/Lazio/CAP_"
caps <- c("00118", "00119", "00121", "00122", "00123", "00124", "00125",
          "00126", "00127", "00128", "00131", "00132", "00133", "00134",
          "00135", "00136", "00137", "00138", "00139", "00141", "00142",
          "00143", "00144", "00145", "00146", "00147", "00148", "00149",
          "00151", "00152", "00153", "00154", "00155", "00156", "00157",
          "00158", "00159", "00161", "00162", "00163", "00164", "00165",
          "00166", "00167", "00168", "00169", "00171", "00172", "00173",
          "00174", "00175", "00176", "00177", "00178", "00179", "00181",
          "00182", "00183", "00184", "00185", "00186", "00187", "00188",
          "00189", "00191", "00192", "00193", "00195", "00196", "00197",
          "00198", "00199")
endurl <- "-Roma"

wholeurls <- c()
index <- 1:length(caps)

for (i in index){
  wholeurl_temp <- paste(starturl,caps[i],endurl)
  
  wholeurls <- append(wholeurls, wholeurl_temp)
}

#remove any spaces
wholeurls <- gsub("\\ ","", wholeurls)

#####################################################################
#   SCRAPE URLS BY LOOPING AND STORE ALL SCRAPED DATA INTO ONE LIST #
#####################################################################

index <- 1:length(wholeurls)
master.list <- c()

for (i in index){
  html <- html(wholeurls[i])
  cast <-html_nodes(html, ".colValue , #heatmapTableStreet a")
  master_templist <- html_text(cast)
  master.list <- append(master.list,master_templist)
}

####################################################################
# TURN LONG LIST INTO DATAFRAME, CLEAN UP, SEPERATE ADDRESS AND CAP#
# ADD CURRENT AND DATE TO DATASET                                  #  
####################################################################


#scrape and convert values
source('R/harvestinfo.R')
master <- harvestinfo(master = master.list)
master$price <- gsub("\\.", "", master$price)
master$price <- as.numeric(as.character(master$price))
master$address <- as.character(as.character(master$address))

##split address to store cap in seperate column
test <- as.character(master$address) #vector
address <- sapply(strsplit(as.character(test), "\\,"), "[",1)
cap <- sapply(strsplit(as.character(test), "\\,"), "[",2)

cap <- gsub("\\ ", "", cap)
cap <- as.data.frame(cap)
address <- as.data.frame(address)


date <- rep(as.POSIXlt(Sys.time()), length(master$price))
master<- cbind(address,cap,master$price, date)
colnames(master) <- c("address", "cap", "price", "date")
master$cap <- as.character(master$cap)
master$address <- as.character(master$address)
################################################

################################################################
####### REMOVE NAs
master <- subset(master, !(master$address == 'na'))
master <- subset(master, !(master$cap == "NA"))
master <- subset(master, !(master$price == "NA"))
###############################################







