# Example of using of user-defined R-function

Author: [Alexander Matrunich](mailto:aleksandr.matrunich@fao.org)

Let me provide an example of applying of user-created R function.

## Base point

In [original code](https://github.com/rockclimber112358/sandbox/commit/e635ac36ae9ed70bd318fc332166f634767ba0c0#diff-7ce0e781009c261132780b53ab2b24bd) we have following relevant lines:

```R
library(foreign)

# Read in data sets
Food = read.spss("Food.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Country_NCT = read.spss("country_nct.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Household = read.spss("HH_ADEPT.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Individual = read.spss("HM.sav",to.data.frame=TRUE,use.value.labels=FALSE)
```
## Wrap read.spss() by our function

Here we repeat the same parameters `to.data.frame=TRUE,use.value.labels=FALSE` in each call of read.spss(). If we want to change one parameter we have to do 4 manual changes in our code. Let's create a function and run it for our files.

```R
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
```

We can improve it more in several way.

## Don't load library when it's possible

Firstly, we can avoid loading of foreign library: we use from the
package only one function and only in one place. Better approach is not
to load library when it's possible to avoid. Because every new library
adds to your R-workspace bunch of its functions. And there could be
conflicts between functions with similar names but from different
packages.

You can call function without loading library with double colon `foreign::read.spss()` 

Additional bonus here, that future reader of your code will easily
understand from which package this function is.

```R
read1 <- function(filename) {
  foreign::read.spss(filename, 
                     to.data.frame = TRUE, 
                     use.value.labels = FALSE)
}
```

## Get rid of warnings

Next improvement could be to kill off warnings genereted by `read.spss()` while it 
reads in SPSS-files. I don't remember cases where `read.spss()` doesn't generate such 
warnings, so it's safe to supress them and make running of your code more clear.

```R
read1 <- function(filename) {
  suppressWarnings(foreign::read.spss(filename, 
                                      to.data.frame = TRUE, 
                                      use.value.labels = FALSE))
}
```
## Lower case for column names

Later in the code there are several mergins. In some cases column names in datasets  
differ only in lower/upper case. But user has to manually specify, that he/she wants to merge 
by column "gender" from data1 and by column "Gender" from data2. We can get rid of it by 
converting all column names in lower case. We'll do it at loading stage inside of our function.

```R
read1 <- function(filename) {
  data <- suppressWarnings(foreign::read.spss(filename, 
                                      to.data.frame = TRUE, 
                                      use.value.labels = FALSE))
  colnames(data) <- tolower(colnames(data))
  data
}
```

## Let's switch from foreign to haven

Suppose, for reading SPSS-files we decided to switch from base `foreing` package 
to [new Hadley's `haven`](https://github.com/hadley/haven). All we need is to change 
our function.

```R
read1 <- function(filename) {
  data <- haven::read_sav(filename)
  colnames(data) <- tolower(colnames(data))
  data
}
```

And following code remains untouched.

```R
Food        <- read1("Food.sav")
Country_NCT <- read1("country_nct.sav")
Household   <- read1("HH_ADEPT.sav")
Individual  <- read1("HM.sav")
```
