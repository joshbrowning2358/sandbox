Let me provide an example of using self-created R function.

In original code we have following relevant lines:

library(foreign)

# Read in data sets
Food = read.spss("Food.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Country_NCT = read.spss("country_nct.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Household = read.spss("HH_ADEPT.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Individual = read.spss("HM.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Prices = read.csv("Prices.csv")
Household = merge(Household,Prices,by.x=c("month"),by.y=c("Month"))

Here we repeat the same parameters (,to.data.frame=TRUE,use.value.labels=FALSE) in each call of read.spss(). If we want to change one parameter we have to do 4 manual changes in our code. Let's create a function and run it for our files.

library(foreign)

read1 <- function(filename) {
  read.spss(filename, 
            to.data.frame = TRUE, 
            use.value.labels = FALSE)
}

Food        <- read1("Food.sav")
Country_NCT <- read1("country_nct.sav")
Household   <- read1("HH_ADEPT.sav")
Individual  <- read1("HM.sav")

We can improve it more in several way.

Firstly, we can avoid loading of foreign library: we use from the
package only one function and only in one place. Better approach is not
to load library when it's possible to avoid. Because every new library
adds to your R-workspace bunch of its functions. And there could be
conflicts between functions with similar names but from different
packages.

You can call function without loading library with double colon foreign::read.spss() 

Additional bonus here, that future reader of your code will easily
understand from which package this function is.

read1 <- function(filename) {
  foreign::read.spss(filename, 
                     to.data.frame = TRUE, 
                     use.value.labels = FALSE)
}

Next improvement is 
