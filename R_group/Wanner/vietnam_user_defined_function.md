# Example of using of user-defined R-function

Author: [Alexander Matrunich](mailto:aleksandr.matrunich@fao.org)

This is an example of applying of user-created R function.

## Base point

In
[original code](https://github.com/rockclimber112358/sandbox/commit/e635ac36ae9ed70bd318fc332166f634767ba0c0#diff-7ce0e781009c261132780b53ab2b24bd)
we have following relevant lines to read data into R data.frames from SPSS
data-sets :

```R
library(foreign)

# Read in data sets
Food = read.spss("Food.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Country_NCT = read.spss("country_nct.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Household = read.spss("HH_ADEPT.sav",to.data.frame=TRUE,use.value.labels=FALSE)
Individual = read.spss("HM.sav",to.data.frame=TRUE,use.value.labels=FALSE)
```
## Wrap read.spss() in our new function

Here we use the same parameter values
(`to.data.frame=TRUE,use.value.labels=FALSE`) in each individual call of the
read.spss() function. Unfortunately if we want to changing a single parameter
value requires explicitly editing code at four individual locations in our
code. This can be simplified allowing the value of each parameter value to be
changed only once thus ensuring consistency and avoiding potential errors.

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

We can also improve the code in several other ways.

## Avoid loading entire library's when unnecessary

We can avoid loading the complete "foreign" library because we use only one of
its many functions, and only in one place in our code. A better approach is
avoid loading the complete library and call only the specific function required
using the double colon notation such as`foreign::read.spss()`.  Every new
library increases the R-workspace memory requirements. This method also avoids
potential conflicts between functions with similar names but from different
packages.  Future reader of your code will also easily understand which
package this particular function is from.

```R
read1 <- function(filename) {
  foreign::read.spss(filename, 
                     to.data.frame = TRUE, 
                     use.value.labels = FALSE)
}
```

## Eliminate unnecessary warnings

The next improvement is to eliminate unnecessary warnings generated by
`read.spss()` when reading SPSS-files. `read.spss()` always generates warnings,
in our experience, so it's safe to suppress them using "suppressWarnings" and
make running of your code more clear.

```R
read1 <- function(filename) {
  suppressWarnings(foreign::read.spss(filename, 
                                      to.data.frame = TRUE, 
                                      use.value.labels = FALSE))
}
```
## Consistent lower case for column names

Later in the code there are several merging operations. In some cases column names in datasets  
differ only in lower/upper case. So the user has to explicitly specify, the
column names to be merged such as by column "gender" from data1 and by column
"Gender" from data2. We can make all column name cases consistent by converting
them all to lower case using "tolower". We'll do this at loading stage inside of our function.

```R
read1 <- function(filename) {
  data <- suppressWarnings(foreign::read.spss(filename, 
                                      to.data.frame = TRUE, 
                                      use.value.labels = FALSE))
  colnames(data) <- tolower(colnames(data))
  data
}
```

## Let's switch from "foreign" library to "haven" library

Suppose, for reading SPSS-files we decided to switch from using the base
`foreign` package (library) to a new
[Hadley Wickham's `haven`](https://github.com/hadley/haven) package. All we need
is to change the function called from the original "foreign" to the new "haven" one.

```R
read1 <- function(filename) {
  data <- haven::read_sav(filename)
  colnames(data) <- tolower(colnames(data))
  data
}
```

The following requires no modification to the rest of the code.

```R
Food        <- read1("Food.sav")
Country_NCT <- read1("country_nct.sav")
Household   <- read1("HH_ADEPT.sav")
Individual  <- read1("HM.sav")
```
