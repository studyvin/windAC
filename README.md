The purpose of this package is for density distribution estimation in
the context of each observation not having the same probability of
detection. The motivating example is estimating carcass fall
distributions at a wind farm. 


Install from R:

```

## remove all installation 
remove.packages('windAC')

## tells devtools to NOT convert warnings into errors
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

devtools::install_github('studyvin/windAC',subdir='windAC')
```
