# Otter drift study - comparison of carcass and dummies

# Tomo Eguchi
# 12 February 2014

rm(list=ls())

library(nlme)
library(gamm4)

source("~/R/TomosFunctions.R")
D00 <- dirSelector()

#runDate <- Sys.Date()

# datafile <- paste(D00$DPub, 'Young_OtterDriftStudy/Data_14Feb2014a.csv',
#                   sep = "")
# moved data files to local
datafile <- 'Data_14Feb2014a.csv'

# windfile <- paste(D00, 'publications/Young_OtterDriftStudy/WindData1.csv',
#                   sep = "")

dat <- read.table(file = datafile, sep=",", header = TRUE)
#windDat <- read.table(file = windfile, sep = ",", header = TRUE)

dat_M <- vector(mode = "numeric", length = dim(dat)[1])
dat_Y <- vector(mode = "numeric", length = dim(dat)[1])
for (k in 1:dim(dat)[1]){
  dat_M[k] <- as.numeric(strsplit(as.character(dat$Date[k]), "/")[[1]][1])
  dat_Y[k] <- as.numeric(strsplit(as.character(dat$Date[k]), "/")[[1]][3])
}

dat$dat_M <- dat_M
dat$dat_Y <- dat_Y - min(dat_Y)
#dat <- dat0[-comments]

# carcass <- dat[dat$Type == 'carcass',]
# dummy <- dat[dat$Type == 'dummy', ]
# 
# # get total distance moved for each object type
# uniqID_C <- unique(carcass$ID)
# totalD_C <- vector(mode = "numeric", length = length(uniqID_C))
# days_C <- vector(mode = "numeric", length = length(uniqID_C))
# month_C <- vector(mode = "numeric", length = length(uniqID_C))
# for (k in 1:length(uniqID_C)){
#   tmp <- carcass[carcass$ID == uniqID_C[k],]
#   totalD_C[k] <- sum(tmp$distInKm)
#   days_C[k] <- tmp$DaysSinceRelease[dim(tmp)[1]]
#   month_C[k] <- as.numeric(strsplit(as.character(tmp$Date[1]), "/")[[1]][1])
# }
# 
# # for dummies
# uniqID_D <- unique(dummy$ID)
# totalD_D <- vector(mode = "numeric", length = length(uniqID_D))
# days_D <- vector(mode = "numeric", length = length(uniqID_D))
# month_D <- vector(mode = "numeric", length = length(uniqID_D))
# for (k in 1:length(uniqID_D)){
#   tmp <- dummy[dummy$ID == uniqID_D[k],]
#   totalD_D[k] <- sum(tmp$distInKm)
#   days_D[k] <- tmp$DaysSinceRelease[dim(tmp)[1]]
#   month_D[k] <- as.numeric(strsplit(as.character(tmp$Date[1]), "/")[[1]][1])
# }
# 
# #hist(totalD_C)
# #hist(totalD_D)
# 
# carcass <- as.factor(c(rep(1, length(uniqID_C)), rep(0, length(uniqID_D))))
# totalDist <- c(totalD_C, totalD_D)
# days <- c(days_C, days_D)
# months <- c(month_C, month_D)
# 
# dat2 <- as.data.frame(cbind(carcass, totalDist, days, months)) 

dat3_mat <- dat[,c("DaysSinceRelease", "windSpd", "windDir", 
                   "windSpdSum", "windDirSum", "dat_M")]
dat3_mat_std <- scale(dat3_mat)
dat3_df <- as.data.frame(cbind(dat3_mat_std, 
                               dat[, c("Type", "distInKm", "dat_Y", "ID")]))
# names(dat3_df)[dim(dat3_mat_std)[2]+1] <- "Type"
# names(dat3_df)[dim(dat3_mat_std)[2]+2] <- "distInKm"
# names(dat3_df)[dim(dat3_mat_std)[2]+3] <- "dat_Y"
# names(dat3_df)[dim(dat3_mat_std)[2]+4] <- "ID"


# mixed effects models
model1 <- as.formula('distInKm ~ Type + DaysSinceRelease + windSpdSum + windDirSum')

fit1_lme <- lme(model1, 
                random = ~ 1 + DaysSinceRelease|ID, 
                data = dat3_df,
                control = lmeControl(maxIter = 1000, 
                                     msMaxIter = 5000,
                                     niterEM = 5000),
                method = "ML")

summary(fit1_lme)

model2 <- as.formula('distInKm ~ Type + DaysSinceRelease + windDirSum')
fit2_lme <- lme(model2, 
                random = ~ 1 + DaysSinceRelease|ID, 
                data = dat3_df,
                control = lmeControl(maxIter = 1000, 
                                     msMaxIter = 5000,
                                     niterEM = 5000),
                method = "ML")

summary(fit2_lme)

model3 <- as.formula('distInKm ~ DaysSinceRelease + windDirSum')
fit3_lme <- lme(model3, 
                random = ~ 1 + DaysSinceRelease|ID, 
                data = dat3_df,
                control = lmeControl(maxIter = 1000, 
                                     msMaxIter = 5000,
                                     niterEM = 5000),
                method = "ML")

summary(fit_lme3)

AIC(fit_lme1, fit_lme2, fit_lme3)

#What about non-linear fits using gamm?
fit2_gam <- gamm4(distInKm ~ s(DaysSinceRelease, bs = "cr", k = 5, by = Type) + 
                    s(windDirSum, bs = "cr",k = 5, by = Type),
                  random = ~(1 + DaysSinceRelease|ID),
                  data = dat3_df,
                  family = gaussian)


# logistic regression - strand or not using wind etc. as predictors
# invlogit <- function(x) { 1 / (1 + exp(-x)) }
# invlogit2 <- function(x) {exp(x)/(1 + exp(x))}
# 
# # convert beached and not-beached into 1/0
# dat$fBeached <- rep(0, times = dim(dat)[1])
# dat$fBeached[dat$Status == "beached"] <- 1
# dat$fCarcass <- rep(0, times = dim(dat)[1])
# dat$fCarcass[dat$Type == 'carcass'] <- 1
# 
# # I can't remember how to set up the repeated measures... 
# fit1_1 <- glm(fBeached ~ 1|ID + fCarcass + DaysSinceRelease,
#               data = dat,
#               family = binomial(link="logit"))
