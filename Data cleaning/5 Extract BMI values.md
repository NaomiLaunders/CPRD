# Pulling out BMI observations for a cohort
BMI can be recorded as a value, as a category, or can be calculated from weight and height values. Therefore, we pull out all three and in this instance I have taken the heaviest ever recorded.

Note, this is dependent on [observations saved as .rdata files](https://github.com/NaomiLaunders/CPRD/blob/main/Data%20cleaning/1%20Limit%20observations%20to%20those%20for%20our%20cohort.md)
## Find listed BMI value for Aurum
````
library(tidyverse)
#Generate file numbers
setwd ('FILEPATH/Observations/AurumObservation')
Observation_files <- list.files()

num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select all BMI observations
for (i in (1:length(num))) {
  FileName<-paste0("FILPATH/NewAurum/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/BMI", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, obsdate, enterdate, medcodeid, value, numunitid)
  PatBMI<-subset(PatObs, medcodeid=="100716012" |medcodeid=="923861000006112"|medcodeid=="2196071000000116" |medcodeid=="1995621000006110" |medcodeid=="2160062010" |medcodeid=="253845014" |medcodeid=="253846010" |medcodeid=="253847018" |medcodeid=="253848011" |medcodeid=="253848011"|medcodeid=="453856012")
  PatBMI<-select(PatBMI, patid, obsdate, enterdate, medcodeid, value, numunitid)
  save(PatBMI, file = NewFile)
}
rm(PatObs)
rm(PatBMI)

#Merge all BMI Observations
load("FILEPATH/VariableExtracts/AURUM/BMI001.Rdata")
PatBMIAll<-PatBMI
rm(PatBMI)

for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/BMI", num[i], ".Rdata")
  load(NewFile)
  PatBMIAll<-rbind(x=PatBMIAll, y= PatBMI)
  print(num[i])
}
rm(PatBMI)

#Check dates
PatBMIAll$obsdate<-as.Date(PatBMIAll$obsdate, format= "%d/%m/%Y")
PatBMIAll$enterdate<-as.Date(PatBMIAll$enterdate, format= "%d/%m/%Y")
length(which(is.na(PatBMIAll$obsdate)))
PatBMIAll$obsdate[is.na(PatBMIAll$obsdate)]<-PatBMIAll$enterdate[is.na(PatBMIAll$obsdate)]

#Check units
table(PatBMIAll$numunitid)
Units<-read.table("FILEPATH/CPRD Data look up files/CPRD Aurum/numunit.txt", header=TRUE, quote="", fill=TRUE, sep="\t")
PatBMIAll<-merge(x=PatBMIAll, y=Units, by="numunitid", all.x=TRUE, all.y=FALSE)
PatBMIAll$Description<-as.character(PatBMIAll$Description)
table(PatBMIAll$Description)

#Remove those with no values or BMI less than 10/greater than 80.
PatBMIAll<-subset(PatBMIAll, !is.na(value))
PatBMIAll<-subset(PatBMIAll, value<=80&value>=10)
  
#Take largest per patient
PatBMIAll<-PatBMIAll%>%
  group_by(patid)%>%
  mutate(MaxBMI= max(value))%>%
  subset(MaxBMI == value)%>%
  distinct(patid,.keep_all = TRUE)

````
## Find height and weight and calculate BMI for Aurum
````
#Generate file numbers
setwd ('FILEPATH/Observations/AurumObservation')
Observation_files <- list.files()

num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select all weight observations
for (i in (1:length(num))) {
  FileName<-paste0("FILEPATH/Observations/NewAurum/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/weight", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, obsdate, enterdate, medcodeid, value, numunitid)
  PatWeight<-subset(PatObs, medcodeid=="1910901000006116" |medcodeid=="1910911000006118" |medcodeid=="59311000006113"|medcodeid=="253677014"|medcodeid=="923851000006110" )
  PatWeight<-select(PatWeight, patid, obsdate, enterdate, medcodeid, value, numunitid)
  save(PatWeight, file = NewFile)
}
rm(PatObs)
rm(PatWeight)

#Merge all weight Obs

load("FILEPATH/VariableExtracts/AURUM/weight001.Rdata")
PatWeightAll<-PatWeight
rm(PatWeight)

for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/weight", num[i], ".Rdata")
  load(NewFile)
  PatWeightAll<-rbind(x=PatWeightAll, y= PatWeight)
  print(num[i])
}
rm(PatWeight)

#Check dates
PatWeightAll$obsdate<-as.Date(PatWeightAll$obsdate, format= "%d/%m/%Y")
PatWeightAll$enterdate<-as.Date(PatWeightAll$enterdate, format= "%d/%m/%Y")
length(which(is.na(PatWeightAll$obsdate)))
PatWeightAll$obsdate[is.na(PatWeightAll$obsdate)]<-PatWeightAll$enterdate[is.na(PatWeightAll$obsdate)]
length(which(is.na(PatWeightAll$obsdate)))

#remove if value is na
PatWeightAll<-subset(PatWeightAll, !is.na(value))

#Ensure in KG
Units<-read.table("FILEPATH/CPRD Data look up files/CPRD Aurum/numunit.txt", header=TRUE, quote="", fill=TRUE, sep="\t")
PatWeightAll<-merge(x=PatWeightAll, y=Units, by="numunitid", all.x=TRUE, all.y=FALSE)
PatWeightAll$Description<-as.character(PatWeightAll$Description)
table(PatWeightAll$Description, PatWeightAll$numunitid)

PatWeightAll$value<-ifelse((PatWeightAll$numunitid==6265|PatWeightAll$numunitid==2318), PatWeightAll$value*6.35029, PatWeightAll$value)
summary(PatWeightAll$value)

#Use Caliber upper bound
PatWeightAll<-subset(PatWeightAll, value<=500)

#Select max weight
PatWeightAll<-PatWeightAll%>%
  group_by(patid)%>%
  mutate(MaxWeight= max(value))%>%
  subset(MaxWeight == value)%>%
  distinct(patid,.keep_all = TRUE)

#Height
#Select all Height
for (i in (1:length(num))) {
  FileName<-paste0("FILEPATH/Observations/NewAurum/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/height", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, obsdate, enterdate, medcodeid, value, numunitid)
  PatHeight<-subset(PatObs, medcodeid=="1910921000006114" |medcodeid=="923831000006115"|medcodeid=="253669010"|medcodeid=="1910931000006112")
  PatHeight<-select(PatHeight, patid, obsdate, enterdate, medcodeid, value, numunitid)
  save(PatHeight, file = NewFile)
}
rm(PatObs)
rm(PatHeight)

#~~~Merge all Height Obs

load("FILEPATH/VariableExtracts/AURUM/height001.Rdata")
PatHeightAll<-PatHeight
rm(PatHeight)

for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/height", num[i], ".Rdata")
  load(NewFile)
  PatHeightAll<-rbind(x=PatHeightAll, y= PatHeight)
  print(num[i])
}
rm(PatHeight)

#Check dates
PatHeightAll$obsdate<-as.Date(PatHeightAll$obsdate, format= "%d/%m/%Y")
PatHeightAll$enterdate<-as.Date(PatHeightAll$enterdate, format= "%d/%m/%Y")
length(which(is.na(PatHeightAll$obsdate)))
PatHeightAll$obsdate[is.na(PatHeightAll$obsdate)]<-PatHeightAll$enterdate[is.na(PatHeightAll$obsdate)]
length(which(is.na(PatHeightAll$obsdate)))

#Convert to metres
PatHeightAll<-merge(x=PatHeightAll, y=Units, by="numunitid", all.x=TRUE, all.y=FALSE)
PatHeightAll$Description<-as.character(PatHeightAll$Description)
table(PatHeightAll$Description, PatHeightAll$numunitid)
table(PatHeightAll$Description, PatHeightAll$numunitid)

PatHeightAll$value<-ifelse((PatHeightAll$numunitid==122|PatHeightAll$numunitid==408),PatHeightAll$value/100, PatHeightAll$value)
summary(PatHeightAll$value)

#remove those above Caliber threshold
PatHeightAll<-subset(PatHeightAll, value<=2.5)

#Select max Height
PatHeightAll<-PatHeightAll%>%
  group_by(patid)%>%
  mutate(MaxHeight= max(value))%>%
  subset(MaxHeight == value)%>%
  distinct(patid,.keep_all = TRUE)
````
## Find BMI category for Aurum
Note, codelists will be uploaded in due course, or available on request.
````
#Pull in code lists
OverweightA<-read.table("FILEPATH/Codelists/AurumOverweightNL.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("MedCodeId"="character"))
OverweightA$BMICat<-2
UnderweightA<-read.table("FILEPATH/Codelists/AurumUnderweightNL.txt", header=TRUE, quote="", fill=TRUE, sep="\t",  colClasses = c("MedCodeId"="character"))
UnderweightA$BMICat<-1
ObeseA<-read.table("FILEPATH/Codelists/AurumObesityNL.txt", header=TRUE, quote="", fill=TRUE, sep="\t",  colClasses = c("MedCodeId"="character"))
ObeseA$BMICat<-3

BMICatA<-rbind(OverweightA, UnderweightA, ObeseA)

#Generate file numbers
setwd ('FILEPATH/Observations/AurumObservation')
Observation_files <- list.files()

num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select all BMI obs
for (i in (1:length(num))) {
  FileName<-paste0("FILEPATH/Observations/NewAurum/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/BMICat", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, obsdate, enterdate, medcodeid, value)
  PatBMICat<-merge(x=BMICatA, y=PatObs, by.x="MedCodeId", by.y = "medcodeid", all.y = FALSE, all.x=FALSE)
  PatBMICat<-select(PatBMICat, patid, obsdate, enterdate, MedCodeId, value, BMICat)
  save(PatBMICat, file = NewFile)
}
rm(PatObs)
rm(PatBMICat)

#Merge all observations

load("FILEPATH/VariableExtracts/AURUM/BMICat001.Rdata")
PatBMICatAll<-PatBMICat
rm(PatBMICat)

for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/AURUM/BMICat", num[i], ".Rdata")
  load(NewFile)
  PatBMICatAll<-rbind(x=PatBMICatAll, y= PatBMICat)
  print(num[i])
}
rm(PatBMICat)

#Check dates
PatBMICatAll$obsdate<-as.Date(PatBMICatAll$obsdate, format= "%d/%m/%Y")
PatBMICatAll$enterdate<-as.Date(PatBMICatAll$enterdate, format= "%d/%m/%Y")
length(which(is.na(PatBMICatAll$obsdate)))
PatBMICatAll$obsdate[is.na(PatBMICatAll$obsdate)]<-PatBMICatAll$enterdate[is.na(PatBMICatAll$obsdate)]
length(which(is.na(PatBMICatAll$obsdate)))

#Check duplicates and take obese, then overweight then underweight

PatBMICatAll<-PatBMICatAll%>%
  group_by(patid)%>%
  mutate (Freq = n(), MaxBMICat = max(BMICat), MinBMICat = min(BMICat))%>%
  subset(MaxBMICat = BMICat)%>%
  distinct(patid,.keep_all = TRUE)
````
## Find listed BMI value for GOLD
````
#Pull in the "additional" table for GOLD
BMIG<-read.table("FILEPATH/GoldAdditional/18_288R_GOLD_Extract_Additional_001.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMIG2<-read.table("FILEPATH/GoldAdditional/18_288R_GOLD_Extract_Additional_002.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMIG3<-read.table("FILEPATH/GoldAdditional/18_288R_GOLD_Extract_Additional_003.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))

#Bind these tables together
BMIG<-rbind(BMIG, BMIG2, BMIG3)

#Select observations that are just related to BMI
BMIG<-subset(BMIG, enttype==13| enttype==14)

#Select just for study patients
load("FILEPATH/AllSMIClean.Rdata")
Pats<-select(AllSMI, patid)
BMIG<-merge(x=Pats, y=BMIG, by="patid", all.x=FALSE, all.y=FALSE)

#Change the value field to numeric. For BMI, the data is included in data3
BMIG$data3[BMIG$data3==""]<-NA
BMIG$data3<-as.numeric(as.character(BMIG$data3))

#Check min and max and include only those that are the right range
summary(BMIG$data3)
BMIG$data3[BMIG$data3>80|BMIG$data3<10]<-NA

#In GOLD, the event date is still included in the clinical table, joined on adid
#Bring in eventdate just for clinical (first 19 observation files for this study)
num<-c(1:19)
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

load("FILPATH/NewGold/Obs001.Rdata")
PatBMIGold<-subset(PatObs, enttype==13|enttype==14)
rm(PatObs)

for (i in (2:length(num))) {
  NewFile<-paste0("FILPATH/NewGold/Obs", num[i], ".Rdata")
  load(NewFile)
  PatObs<-subset(PatObs, enttype==13|enttype==14)
  PatBMIGold<-rbind(x=PatBMIGold, y=PatObs)
  print(num[i])
}
rm(PatObs)

PatBMIGold$patad<-paste0(PatBMIGold$patid, "-", PatBMIGold$adid)
BMIG$patad<-paste0(BMIG$patid, "-", BMIG$adid)

BMIdate<-merge(x=BMIG, y=PatBMIGold, by="patad", all.x=TRUE, all.y=FALSE)

BMIdate$eventdate<-as.Date(BMIdate$eventdate, format= "%d/%m/%Y")
BMIdate$sysdate<-as.Date(BMIdate$sysdate, format= "%d/%m/%Y")
length(which(is.na(BMIdate$eventdate)))
BMIdate$eventdate[is.na(BMIdate$eventdate)]<-BMIdate$sysdate[is.na(BMIdate$eventdate)]
length(which(is.na(BMIdate$eventdate)))

BMIG<-select(BMIdate, patid.x, data3, enttype.x, eventdate)
BMIG$BMI<-BMIG$data3
BMIG<-subset(BMIG, !is.na(BMI))

#Largest per patient
BMIG<-BMIG%>%
  group_by(patid.x)%>%
  mutate(MaxBMI= max(BMI))%>%
  subset(MaxBMI == value)%>%
  distinct(patid,.keep_all = TRUE)
  
 ````
## Find height and weight and calculate BMI for Aurum
````
#For GOLD we can use the already extracted entytpe 13 and 14

WeightHeightG<-select(BMIdate, patid.x, data1, data2, enttype.x, eventdate)

#Weight
WeightHeightG$Weight<-as.numeric(as.character(WeightHeightG$data1))
WeightHeightG$Weight[WeightHeightG$enttype==14]<-NA

#Check max
summary(WeightHeightG$Weight)
#Use Caliber upper bound
WeightG<-subset(WeightHeightG, Weight<=500&!is.na(Weight))

#Largest per patient
WeightG<-WeightG%>%
  group_by(patid.x)%>%
  mutate(MaxWeight= max(Weight)%>%
  subset(MaxWeight == Weight)%>%
  distinct(patid,.keep_all = TRUE)

#Height
WeightHeightG$Height<-as.numeric(as.character(WeightHeightG$data1))
WeightHeightG$Height[WeightHeightG$enttype==13]<-NA

#Check max
summary(WeightHeightG$Height)
#Use Caliber upper bound
HeightG<-subset(WeightHeightG, Height<=2.5&!is.na(Height))

#Largest per patient
HeightG<-HeightG%>%
  group_by(patid.x)%>%
  mutate(MaxHeight= max(Height))%>%
  subset(MaxHeight == Height)%>%
  distinct(patid,.keep_all = TRUE)
````
## Find BMI category for GOLD
````
OverweightG<-read.table("FILEPATH/Codelists/GoldOverweightNL.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("medcode"="character"))
OverweightG$BMICat<-2
UnderweightG<-read.table("FILEPATH/Codelists/GoldUnderweightNL.txt", header=TRUE, quote="", fill=TRUE, sep="\t",  colClasses = c("medcode"="character"))
UnderweightG$BMICat<-1
ObeseG<-read.table("FILEPATH/Codelists/GoldObesityNL.txt", header=TRUE, quote="", fill=TRUE, sep="\t",  colClasses = c("medcode"="character"))
ObeseG$BMICat<-3

#Generate file numbers

setwd ('FILEPATH/Observations/NewGold/Obs')
Observation_files <- list.files()

num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select only observations that are BMI-based
setwd ('R:/pathfinder/Naomi')
for (i in (1:length(num))) {
  FileName<-paste0("FILEPATH/Observations/NewAurum/Obs", num[i], ".Rdata")
  NewFile<-paste0("FILEPATH/VariableExtracts/GOLD/BMICat", num[i], ".Rdata")
  load(FileName)
  PatObs<-select(PatObs, patid, eventdate, sysdate, medcode)
  PatBMICat<-merge(x=BMICatG, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatBMICat<-select(PatBMICat, patid, eventdate, sysdate, medcode, BMICat)
  save(PatBMICat, file = NewFile)
}
rm(PatObs)
rm(PatBMICat)

#~~~Merge all Ethn Obs files to one dataframe

load("FILEPATH/VariableExtracts/GOLD/BMICat001.Rdata")
PatBMICatGold<-PatBMICat
rm(PatBMICat)

for (i in (2:length(num))) {
  NewFile<-paste0("FILEPATH/VariableExtracts/GOLD/BMICat", num[i], ".Rdata")
  load(NewFile)
  PatBMICatGold<-rbind(x=PatBMICatGold, y= PatBMICat)
  print(num[i])
}
rm(PatBMICat)

setwd ('R:/pathfinder/Naomi')
PatBMICatGold$eventdate<-as.Date(PatBMICatGold$eventdate, format= "%d/%m/%Y")
PatBMICatGold$sysdate<-as.Date(PatBMICatGold$sysdate, format= "%d/%m/%Y")
length(which(is.na(PatBMICatGold$eventdate)))
PatBMICatGold$eventdate[is.na(PatBMICatGold$eventdate)]<-PatBMICatGold$sysdate[is.na(PatBMICatGold$eventdate)]
length(which(is.na(PatBMICatGold$eventdate)))

#Check duplicates and take HEAVIEST

PatBMICatGold<-PatBMICatGold%>%
  group_by(patid)%>%
  mutate (Freq = n(), MaxBMICat = max(BMICat), MinBMICat = min(BMICat))%>%
  subset(MaxBMICat = BMICat)%>%
  distinct(patid,.keep_all = TRUE)
  ````
  
  
  


