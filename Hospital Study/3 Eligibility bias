#Libraries
library(dplyr)
library(lubridate)
library(tableone)

#Clear environment and load file

rm(list = ls(all.names = TRUE))

load("FILEPATH/CPRDVars.Rdata")

table(CPRD$hes_e, useNA="ifany")
CPRD$hes_e[is.na(CPRD$hes_e)]<-0

table(CPRD$hes_e, CPRD$comparator,useNA="ifany")
prop.table(table(CPRD$hes_e, CPRD$comparator, useNA="ifany"),2)

#Add in missing variables
CPRD$BMICatMiss[is.na(CPRD$BMICatMiss)]<-0
CPRD$BMICatMiss<-as.factor(CPRD$BMICatMiss)
CPRD$BMICat<-as.factor(CPRD$BMICat)

CPRD$currsmoke[CPRD$currsmoke==2]<-1
CPRD$currsmoke[CPRD$currsmoke==3]<-2
CPRD$currsmoke[CPRD$currsmoke==4]<-3

CPRD$currsmokemiss<-CPRD$currsmoke
CPRD$currsmokemiss[is.na(CPRD$currsmokemiss)]<-"missing"
CPRD$currsmoke[is.na(CPRD$currsmoke)]<-3
CPRD$currsmoke<-as.factor(CPRD$currsmoke)


CPRD$Alcohol_Base<-as.factor(CPRD$Alcohol_Base)
CPRD$Alcohol<-as.factor(CPRD$Alcohol)
CPRD$Drugs_Base<-as.factor(CPRD$Drugs_Base)
CPRD$Drugs<-as.factor(CPRD$Drugs)
CPRD$PatientIMDMiss<-CPRD$PatientIMD
CPRD$PatientIMDMiss[is.na(CPRD$PatientIMDMiss)]<-"Missing"

####Eligible####
#Only include those that are eligible for linkage
Hosp<-subset(CPRD, hes_e==1)

#Ensure they are still matched 1:1
MatchedCount<-Hosp%>%
  group_by (case_patid)%>%
  summarise(Freq=n())

#Only keep if there are at least 2 patients with the same id
Keep<-subset(MatchedCount, MatchedCount$Freq>1)
Hosp<-subset(Hosp, (Hosp$case_patid %in% Keep$case_patid))

#Only keep if one of these is the patient
Cases<-subset(Hosp, comparator==0)
Hosp<-subset(Hosp, case_patid %in% Cases$patid)

save(Hosp, file="R:/pathfinder/Naomi/HospAdmissions/Hosp.rdata")

####Ineligbible####
#Create a comparison of those who are not eligible
UnHosp<-subset(CPRD, hes_e==0)

#Apply the same matching logic
MatchedCount<-UnHosp%>%
  group_by (case_patid)%>%
  summarise(Freq=n())

Keep<-subset(MatchedCount, MatchedCount$Freq>1)
UnHosp<-subset(UnHosp, (UnHosp$case_patid %in% Keep$case_patid))

Cases<-subset(UnHosp, comparator==0)
UnHosp<-subset(UnHosp, case_patid %in% Cases$patid)

#Bind them together to compare eligibility
CPRD<-rbind(Hosp, UnHosp)

####Comparison of linked and unlinked patients####
MyVars<-c("source","diagn", "AgeAtStart", "AgeAtDiag", "gender", "region", "PatientIMDMiss", "ethn_missing", "BMICatMiss", "currsmokemiss","Alcohol", "Drugs", "NoComorb", "OneComorb", "MoreComorb", "died", "AgeAtDeath", "FUTime", "BaselineTime")

Table1<-CreateTableOne(vars=MyVars, strata="hes_e", data=CPRD)
print(Table1, nonnormal=TRUE)

Table1Exp <- print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, catDigits=2)

write.csv(Table1Exp, file = "FILEPATH/Outputs/Tables/SupTableOne.csv")
