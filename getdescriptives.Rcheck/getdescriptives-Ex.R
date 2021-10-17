pkgname <- "getdescriptives"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('getdescriptives')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("get_descriptives")
### * get_descriptives

flush(stderr()); flush(stdout())

### Name: get_descriptives
### Title: Function to obtain descriptive statistics of a given list of
###   dataframes or patients.
### Aliases: get_descriptives

### ** Examples

get_descriptives(mtcars)
get_descriptives(list(list(mtcars[1:10,]), mtcars[11:20,]))



cleanEx()
nameEx("hello")
### * hello

flush(stderr()); flush(stdout())

### Name: hello
### Title: Hello, World!
### Aliases: hello

### ** Examples

hello()



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
