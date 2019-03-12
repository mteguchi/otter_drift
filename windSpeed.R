# Average Wind data for otter drift study
# easier to do this in matlab... DONE in matlab.

# Tomo Eguchi
# 12 February 2014

rm(list=ls())
library(circular)

source("~/Documents/R_Work/TomosFunctions.R")
D00 <- dirSelector()

runDate <- Sys.Date()

datafile1995 <- paste(D00, 'publications/Young_OtterDriftStudy/OceData_1995.csv',
                  sep = "")
datafile1996 <- paste(D00, 'publications/Young_OtterDriftStudy/OceData_1996.csv',
                      sep = "")

dat1995 <- read.table(file = datafile1995, sep=",", header = TRUE)
dat1995 <- read.table(file = datafile1996, sep=",", header = TRUE)

names(dat1995)
