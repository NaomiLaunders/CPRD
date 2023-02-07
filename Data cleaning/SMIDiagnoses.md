# Pull in SMI diagnosis and dates for the cohort
Note, this is dependent on [the observations already being converted to R files.](https://github.com/NaomiLaunders/NaomiLaunders/blob/main/Data%20cleaning/Limit%20observations%20to%20those%20for%20our%20cohort.md)  

## Load the SMI cohort and libraries
````library(tidyverse)
load("FILEPATH/AllSMIClean.Rdata")
````
## Load the SMI codelists
Note, these codelists will be uploaded in due course, and are available on request.

````
load("FILEPATH/CodeLists/FinalGoldSMIRead.Rdata")
load("FILEPATH/CodeLists/FinalAurumSMIRead.Rdata")
````

## Aurum: Loop through observation files and select only those which are related to SMI diagnosis

````
#Set path to where selected .Rdata files are stored and create a list of all files
setwd ('FILEPATH/Observations/NewAurum')
Observation_files <- list.files()

#Generate file numbers in the format that the files are labelled
num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select those that are SMI 
for (i in (1:length(num))) {
  FileName<-paste0("FILEPATH/NewAurum/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/SMI", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, medcodeid, obsdate, enterdate)
  PatSMI<-merge(x=AurumSMI, y=PatObs, by ="medcodeid", all.y = FALSE, all.x=FALSE)
#Change some variables so later we can easily bind to gold table
  PatSMI<-select(PatSMI, patid, medcode = medcodeid, FullDiag, `Read.code`=originalreadcode, `QOF.code`, Group, eventdate=obsdate, sysdate=enterdate)
  save(PatSMI, file = NewFile)
  print(num[i])
}
rm(PatObs)
rm(PatSMI)
````
## Create one list of Aurum SMI observation
````
#Load the first SMI observation file
load("FILEPATH/VariableExtracts/AURUM/SMI001.Rdata")
PatSMIAurum<-PatSMI
rm(PatSMI)

#Bind all subsequent observation files
for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/SMI", num[i], ".Rdata")
  load(NewFile)
  PatSMIAurum<-rbind(x=PatSMIAurum, y=PatSMI)
  print(num[i])
}
rm(PatSMI)

save(PatSMIAurum, file="FILEPATH/VariableExtracts/MergedObs/AURUMSMI.Rdata")
````
## GOLD: Loop through observation files and select only those which are related to SMI diagnosis
````
setwd ('FILEPATH/Observations/NewGold')
Observation_files <- list.files()

num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select those that are SMI 

for (i in (1:length(num))) {
  FileName<-paste0("FILEPATH/Observations/NewGold/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/GOLD/SMI", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate)
  PatSMI<-merge(x=GoldSMI, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatSMI<-select(PatSMI, patid, medcode, FullDiag, `Read.code`, `QOF.code`, Group, eventdate, sysdate)
  save(PatSMI, file = NewFile)
  print(num[i])
}
rm(PatObs)
rm(PatSMI)
````
## Create one list of GOLD SMI observations
````
load("FILEPATH/VariableExtracts/GOLD/SMI001.Rdata")
PatSMIGold<-PatSMI
rm(PatSMI)

for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/GOLD/SMI", num[i], ".Rdata")
  load(NewFile)
  PatSMIGold<-rbind(x=PatSMIGold, y= PatSMI)
  print(num[i])
}
rm(PatSMI)

save(PatSMIGold, file="FILEPATH/VariableExtracts/MergedObs/GoldSMI.Rdata")
````
## Create one list of SMI related observations

````
PatSMIAll<-rbind(PatSMIAurum, PatSMIGold)

#If eventdate is <=01/01/1900 take sysdate
PatSMIAll$eventdate<-as.Date(PatSMIAll$eventdate, format="%d/%m/%Y")
PatSMIAll$sysdate<-as.Date(PatSMIAll$sysdate, format="%d/%m/%Y")

PatSMIAll$eventdate[PatSMIAll$eventdate<="1900/01/01"]<-NA
PatSMIAll$sysdate[PatSMIAll$sysdate<="1900/01/01"]<-NA

PatSMIAll$eventdate[is.na(PatSMIAll$eventdate)]<-PatSMIAll$sysdate[is.na(PatSMIAll$eventdate)]

SMIObs<-select(PatSMIAll, patid, FullDiag, QOF=`QOF.code`, SMIDate=eventdate, SMIRead=`Read.code`, diag=Group)

save(SMIObs, file="FILEPATH/VariableExtracts/MergedObs/SMIObs.Rdata")
````
## Find first and last SMI diagnosis date, and diagnostic category
````
SMIDate<-SMIObs%>%
  group_by(patid)%>%
  mutate(diagnosis_date = min(eventdate), SMIMaxDate = max(eventdate))%>%
  subset(eventdate == diagnosis_date)%>%
#If multiple on the same day, take schizoprehnia, then bipolar disorder then other psychoses  
  mutate(Select=case_when(LastDiag=="Schizophrenia" ~ 1,
                          LastDiag=="Bipolar" ~ 2,
                          LastDiag=="Other" ~ 3,
                          TRUE ~ 0))%>%
  mutate(Choose=min(Select))%>%
  subset(Choose==Select)%>%
  distinct(patient_id, .keep_all = TRUE)
  
SMIDate<-select(SMIDate, patid, diagnosis_date, SMIMaxDate, diagn = FullDiag)
AllSMI<-merge(x=AllSMI, y=SMIDate, by="patid", all.x=TRUE, all.y=TRUE)
save(AllSMI, file="FILEPATH/AllSMIClean.Rdata"
````
