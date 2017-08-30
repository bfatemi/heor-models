# isds [![Travis-CI Build Status](https://travis-ci.org/bfatemi/isds.svg?branch=master)](https://travis-ci.org/bfatemi/isds) [![Coverage Status](https://img.shields.io/codecov/c/github/bfatemi/isds/master.svg)](https://codecov.io/github/bfatemi/isds?branch=master)

## Goal of isds

## Example

Run entire PSM workflow, for every available hospital, or a given hospital ID:

```R

runPSM()               # all hospitals
runPSM(hospID = 11111) # for one hospital

```

Run PSM workflow in modules:


```R

getRawData() # Get raw data
getDataQTI() # Get starting dataset (cleaned and filtered)



```