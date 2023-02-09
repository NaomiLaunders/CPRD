rm(list = ls(all.names = TRUE))
library(dplyr)
library(tidyselect)
library(lubridate)

#load data
load(FILEPATH/CPRDIMD.Rdata")

#Remove some of the variables we don't need
CPRD<-select(CPRD, -pracid, -diagnosis_date, -frd, -internal, -SMICode, -patienttypeid, -prescr, -reggap, -regstat, -tod, -toreason, -true_diag, -vmid, -lcd, 
                   -Height, -Weight, -HeightDate, -WeightDate, -heightage, -weightage, -everdate, -Date18, -Date100)

#Follow up time
CPRD$FUTime<-CPRD$end-(CPRD$Outcome)
CPRD$FUTime<-as.numeric(CPRD$FUTime)/365.25

#Baseline time
CPRD$BaselineTime<-CPRD$case_index - CPRD$crd
CPRD$BaselineTime<- as.numeric(CPRD$BaselineTime/365.25)

#Age groups
CPRD$AgeGroup<-0
CPRD$AgeGroup[CPRD$AgeAtDiag<41]<-"18-40"
CPRD$AgeGroup[CPRD$AgeAtDiag>=41&CPRD$AgeAtDiag<65]<-"41-64"
CPRD$AgeGroup[CPRD$AgeAtDiag>=65]<-"65+"

CPRD$AgeGroup<-as.factor(CPRD$AgeGroup)

#Age at death
CPRD$AgeAtDeath<-CPRD$deathdate-CPRD$dob
CPRD$AgeAtDeath<-as.numeric(CPRD$AgeAtDeath)
CPRD$AgeAtDeath<-CPRD$AgeAtDeath/365.25

#Lau Comorbidities
CPRD$BaseNoComorb<-0
CPRD$BaseNoComorb[CPRD$count_base==0]<-1
CPRD$BaseOneComorb<-0
CPRD$BaseOneComorb[CPRD$count_base==1]<-1
CPRD$BaseAtLeastOneComorb<-0
CPRD$BaseAtLeastOneComorb[CPRD$count_base>=1]<-1
CPRD$BaseMoreComorb<-0
CPRD$BaseMoreComorb[CPRD$count_base>1]<-1

CPRD$BaseNoComorb<-as.factor(CPRD$BaseNoComorb)
CPRD$BaseOneComorb<-as.factor(CPRD$BaseOneComorb)
CPRD$BaseMoreComorb<-as.factor(CPRD$BaseMoreComorb)
CPRD$BaseAtLeastOneComorb<-as.factor(CPRD$BaseAtLeastOneComorb)

CPRD$NoComorb<-0
CPRD$NoComorb[CPRD$Lau_Total==0]<-1
CPRD$OneComorb<-0
CPRD$OneComorb[CPRD$Lau_Total==1]<-1
CPRD$AtLeastOneComorb<-0
CPRD$AtLeastOneComorb[CPRD$Lau_Total>=1]<-1
CPRD$MoreComorb<-0
CPRD$MoreComorb[CPRD$Lau_Total>1]<-1

CPRD$NoComorb<-as.factor(CPRD$NoComorb)
CPRD$OneComorb<-as.factor(CPRD$OneComorb)
CPRD$MoreComorb<-as.factor(CPRD$MoreComorb)
CPRD$AtLeastOneComorb<-as.factor(CPRD$AtLeastOneComorb)

#Save changes
save(CPRD, file="FILEPATH/CPRDVars.Rdata")
