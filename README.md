The purpose of this package is for density distribution estimation in
the context of each observation not having the same probability of
detection. The motivating example is estimating carcass fall
distributions at a wind farm. 

To clone:

git clone https://lar-git.west-inc.com/jstudyvin/windAC.git


Install from R:

```
library(devtools)
library(getPass)

## your git lab user name
userName <- 'jstudyvin'

## remove all installation 
remove.packages('windAC')

## tells devtools to NOT convert warnings into errors
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)


devtools::install_git(url="https://lar-git.west-inc.com/jstudyvin/windAC.git",
                      subdir='/windAC/',
                      credentials = git2r::cred_user_pass(userName, getPass::getPass())
                      )

```
