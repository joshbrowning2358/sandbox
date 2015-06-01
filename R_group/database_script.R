utline:
## 1. Connect to Oracle database
## 2. Connect to Microsoft SQL Server database
## 3. sqldf package
## 4. batch execution
## 

###############################################################################
## 1. Connect to Oracle database
###############################################################################

## First, you need to make sure you have a file which allows you to connect
## with the database (in this case, an Oracle connection requires an OJDBC
## jar file).  I downloaded one and saved it at ~/ojdbc14.jar

list.files("~/../Desktop/")
ojdbcPath <- "~/../Desktop/ojdbc14.jar"
# ojdbcPath <- "C:/sqldeveloper/jdbc/lib/ojdbc6.jar"

## Sidenote: the location of a file, such as the .jar file, could be a useful
## thing to put in your .Rprofile (just to help you remember where it is).

## Next, we set up the connection.  You'll need to know the database name, port,
## username, password, etc.

library(RJDBC)
library(DBI)

install.packages("RPostgreSQL")
library(RPostgreSQL)

con <- dbConnect(drv=drv_generic,
                 "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
                 port=vvvv,
                 user="demo", 
                 password="demo",
                 dbname="ggg")

drv <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
                   classPath = ojdbcPath)
dbstring <- "jdbc:oracle:thin:@lprdbwo1:3310:fstp"
connection <- DBI::dbConnect(drv = drv, dbstring, user = "demo",
                             password = "demo")
is(connection)
connection

## Now, we can query the tables!

wholeTable <- DBI::dbGetQuery(connection,
                              statement = "SELECT *
                              FROM FAOSTAT.TSV_ICS_UPDBAL_YR")
is(wholeTable)
head(wholeTable)
dim(wholeTable)

## That's alot of data, and we may not need it all.  We can use SQL to subset
## the data for us.

smallerTable <- DBI::dbGetQuery(connection,
                                statement =
                                  "SELECT AREA, ITEM, ELE, NUM_2010, SYMB_2010
                                FROM FAOSTAT.TSV_ICS_UPDBAL_YR
                                WHERE AREA <= 100")
is(smallerTable)
head(smallerTable)
dim(smallerTable)

## Now, let's say that, for each unique item, element and year, we want the
## range of values seen.  And, we'd like the resulting table to be sorted from
## largest value to smallest with year as a row.

years <- 2008:2010

sqlResult <- DBI::dbGetQuery(connection,
                             statement =
                               "SELECT
                             ITEM,
                             ELE,
                             2010,
                             max(NUM_2010) - min(NUM_2010)    as VALUE
                             FROM
                             FAOSTAT.TSV_ICS_UPDBAL_YR
                             GROUP BY
                             ITEM,
                             ELE
                             ORDER BY
                             VALUE desc")

## Use SQL
getOneYear <- function(year){
  sqlResult <- DBI::dbGetQuery(connection,
                               statement = paste0(
                                 "SELECT
                                 ITEM,
                                 ELE,
                                 ", year, "  as YEAR,
                                 max(NUM_", year, ") - min(NUM_", year, ")    as VALUE
                                 FROM
                                 FAOSTAT.TSV_ICS_UPDBAL_YR
                                 GROUP BY
                                 ITEM,
                                 ELE"))
  sqlResult
}
sqlFinalResult <- lapply(years, getOneYear)
sqlFinalResult <- do.call("rbind", sqlFinalResult)
sqlFinalResult <- sqlFinalResult[order(sqlFinalResult$VALUE,
                                       decreasing = TRUE), ]

## Use R solutions:

initialTable <- DBI::dbGetQuery(connection,
                                statement =
                                  "SELECT AREA, ITEM, ELE, NUM_2008, NUM_2009, NUM_2010
                                FROM FAOSTAT.TSV_ICS_UPDBAL_YR")
head(initialTable)
numColumns <- grepl("^NUM_[0-9]{4}$", colnames(initialTable))
numColumns <- (1:length(numColumns))[numColumns]
initialTable <- tidyr::gather(initialTable, key = "YEAR", value = "VALUE",
                              numColumns)
head(initialTable)
initialTable$YEAR <- gsub("NUM_", "", initialTable$YEAR)
head(initialTable)
dim(initialTable) # Why so much larger than before?

## Use dplyr
library(dplyr)
dplyrSolution <- initialTable %>%
  dplyr::group_by(ITEM, ELE, YEAR) %>%
  dplyr::mutate(RANGE = max(VALUE) - min(VALUE)) %>%
  dplyr::arrange(RANGE)
dplyrSolution
## WHy do I have so many rows?  I should have only 8088...
dplyrSolution <- initialTable %>%
  dplyr::group_by(ITEM, ELE, YEAR) %>%
  dplyr::summarize(RANGE = max(VALUE) - min(VALUE)) %>%
  dplyr::arrange(RANGE)
dplyrSolution
## Why isn't it ordered by RANGE?
dplyrSolution <- dplyrSolution %>%
  group_by() %>%
  arrange(-RANGE)
dplyrSolution
class(dplyrSolution) # Still a data.frame, but more!

## Use data.table
library(data.table)
temporaryTable <- data.table(initialTable)
datatableSolution <- temporaryTable[, max(VALUE) - min(VALUE), by = c("ITEM", "ELE", "YEAR")]
setnames(datatableSolution, "V1", "RANGE")
datatableSolution <- datatableSolution[order(-RANGE), ]
datatableSolution
class(datatableSolution) # Still a data.frame, but more!

dim(sqlFinalResult)
dim(dplyrSolution)
dim(datatableSolution)

## Test for equality:

summary(sqlFinalResult$VALUE - dplyrSolution$RANGE)
summary(datatableSolution$RANGE - dplyrSolution$RANGE)
summary(sqlFinalResult$VALUE - datatableSolution$RANGE)
which(data.frame(datatableSolution)[, 1] != data.frame(dplyrSolution)[, 1])

data.frame(dplyrSolution)[15:18, ]
data.frame(datatableSolution)[15:18, ]

###############################################################################
## 2. Connect to Microsoft SQL Server database
###############################################################################

## The ODBC driver for SQL Server should be installed by default on Windows.
## 1. Open "Administrative Tools" and "ODBC Data Sources (32 bit)"
## 2. On the "User DSN" tab press "Add"
## 3. Select "SQL Server"
## 4. Provide the server name.  In this case, we use HQWPRFAOSTATDB1\TEST
  ## 5. Choose "SQL Server Authentication" and use the login Warehouse/w@reh0use
## 6. Choose default dataset of Warehouse_data (not really sure what this does)
## 7. Test Connection and finish.

# Same name as set in step 4
library(RODBC)
connection <- RODBC::odbcConnect("FAO_Test", uid = "Warehouse", 
                                 pwd = "w@reh0use")
data <- RODBC::sqlQuery(channel = connection, query = "
                        SELECT  d.Var1Code, d.Var2Code, c.ResultVarCode, d.Var4Code,
                        (d.Value * c.Mult) / d1.Value AS Value
                        FROM  Warehouse_data.dbo.DataQC   AS d1 
                        INNER JOIN  Warehouse_data.dbo.DataQC   AS d 
                        ON  d.Var1Code = d1.Var1Code
                        AND d.Var2Code = d1.Var2Code
                        AND d.Var4Code = d1.Var4Code
                        INNER JOIN Utility.dbo.vDomainVarCalc AS c 
                        ON  d.Var3Code  = c.NumVarCode
                        AND d1.Var3Code = c.DenVarCode
                        WHERE ISNULL(d1.Value,0) > 0
                        AND	d.Value          IS NOT NULL 
                        AND c.DomainCode     = 'QC' 
                        AND c.DenVarBCode    IS NULL
                        AND d.Var2Code       = 15
                        AND d.Var4Code       = 2013")

install.packages("FAOSTAT")
library(FAOSTAT)
FAOSTAT::FAOsearch()
vignette("FAOSTAT")
vignette(all = FALSE)

# data <- RODBC::sqlQuery(channel = connection, query = "
#   SELECT TOP 50(*)
#   FROM  Warehouse_data.dbo.DataQC")
# data

is(data)
dim(data)
data

###############################################################################
## 3. sqldf package
###############################################################################

## If you're comfortable with SQL, the sqldf package could be useful.  It
## essentially gives you the ability to write SQL code against R datasets.

library(sqldf)
data
sqlMethod <- sqldf::sqldf("select * from data")
rMethod <- data[data$Var1Code <= 120, ]
all(sqlMethod == rMethod)
identical(sqlMethod, rMethod) # Not exactly the same, row numbers differ

## However, sqldf is not very efficient (in terms of processing data in R).  If
## you're more comfortable with the syntax, and if you're doing operations on
## small datasets, sqldf should be fine.  But, you should consider moving to
## dplyr or data.table for these kinds of operations in R.

## sqldf can be really useful for reading in data, though:
?read.csv.sql
ggplot2::diamonds
write.csv(ggplot2::diamonds, file = "~/testDiamonds.csv", row.names = F)
read.csv.sql(file = "~/testDiamonds.csv",
             sql = "SELECT carat, cut
             FROM file
             WHERE price >= 18000")

###############################################################################
## 4. batch execution
###############################################################################

## Suppose you want to execute an R script from another program.  Here we'll 
## look at executing it in batch mode from the terminal.  To run this example,
## copy and paste the code below into a new script.  Then, from the terminal,
## run Rscript batchScript.R 2010

## Configuring batch mode on windows is a bit challenging, as you have to 
## manually set your path variable.  To do this, go to Computer -> Properties ->
## Advanced System Settings -> Advanced -> Environment Variables -> Edit and add
## your R directory.

library(RODBC)

## Read in arguments, check that they are valid
args <- commandArgs(trailingOnly = TRUE)
if(length(args) != 1)
  stop("Expecting exactly one argument: year")
year <- as.numeric(args[[1]])
stopifnot(year >= 1960 & year <= 2015)

## Read in data
connection <- RODBC::odbcConnect("FAO_Test", uid = "Warehouse", 
                                 pwd = "w@reh0use")
data <- RODBC::sqlQuery(channel = connection, query = paste0("
                                                             SELECT  d.Var1Code, d.Var2Code, c.ResultVarCode, d.Var4Code,
                                                             (d.Value * c.Mult) / d1.Value AS Value
                                                             FROM  Warehouse_data.dbo.DataQC   AS d1 
                                                             INNER JOIN  Warehouse_data.dbo.DataQC   AS d 
                                                             ON  d.Var1Code = d1.Var1Code
                                                             AND d.Var2Code = d1.Var2Code
                                                             AND d.Var4Code = d1.Var4Code
                                                             INNER JOIN Utility.dbo.vDomainVarCalc AS c 
                                                             ON  d.Var3Code  = c.NumVarCode
                                                             AND d1.Var3Code = c.DenVarCode
                                                             WHERE ISNULL(d1.Value,0) > 0
                                                             AND	d.Value          IS NOT NULL 
                                                             AND c.DomainCode     = 'QC' 
                                                             AND c.DenVarBCode    IS NULL
                                                             AND d.Var2Code       = 15
                                                             AND d.Var4Code       = ", year))

data$robustMean = MASS::huber(data$Value)$mu
write.csv(data, file = paste0("~/processedData", year, ".csv"))

