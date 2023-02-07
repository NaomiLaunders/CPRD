# Limiting observations to our data set

The observation files provided from CPRD in this project included observations for patients not included in our data set. Therefore, the first step was to limit all observations to those that apply to the patients we are interested in.

## Load clean data and create patient lists

````library(tidyverse)
load("AllSMIClean.Rdata")
Patients<-select(AllSMI, patid, source)
PatAurum<-subset(Patients, Patients$source=="Aurum")
PatGold<-subset(Patients, Patients$source=="Gold")
````  
## Create a function that loads the observations and ensures medcodes and patient IDs stay as character variables  

````
#Gold
ReadObsG <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", fill=TRUE, dec = ".", quote="", colClasses=(c(medcode="character", patid="character")))
}
#Aurum
ReadObsA <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", colClasses=c(medcodeid="character", patid="character"))
}
````  

## Aurum: Loop through observation files and save them as new R files limited to just the patients I am interested in  

````
#Set path to where original .txt files from CPRD are stored and create a list of all files
setwd ('FILEPATH/observations/AurumObservation')
Observation_files <- list.files()

#Generate file numbers in the format that the .txt files are labelled
num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

#Select only those that are related to the patients of interest
for (i in (1:length(num))) {
#set file names to import (FileName) and to save to (NewFile)
  FileName<-paste0("FILEPATH/observations/AurumObservation/18_288R_Aurum_Extract_Observation_", num[i], ".txt")
  NewFile<-paste0("FILEPATH/observations/NewAurum/Obs", num[i], ".Rdata")
#Read in the file using the function defined above
  tempobs<-ReadObsA(FileName)
#merge to patient list and only keep if present in both
  PatObs<-merge(x=PatAurum, y=tempobs, by = "patid", all.y = FALSE, all.x=FALSE)
#Save to new file as .Rdata
  save(PatObs, file = NewFile)
#Add a print function to tell you where you are in the loop
  print(num[i])
}

#Remove files
rm(tempobs)
rm(PatObs)
````  

## Gold: Loop through clinical, referral and test files and save as general observations to mimic Aurum  

````
#Set path to where original .txt files from CPRD are stored and create a list of all files
setwd ('FILEPATH/observations/GoldClinical')
Observation_files <- list.files()

setwd ('FILEPATH/observations/GoldReferal')
Observation_files1 <- list.files()

setwd ('FILEPATH/observations/GoldTest')
Observation_files2 <- list.files()

#Generate file numbers in the format that the .txt files are labelled
num<-c(1:length(Observation_files))
num[num<100&num>9]<-paste0("0", num[num<100&num>9])
for (i in 1:9) {
  num[i]<-paste0("00", num[i])
}

num1<-c(1:length(Observation_files1))
num1[num1<100&num1>9]<-paste0("0", num1[num1<100&num1>9])
for (i in 1:1) {
  num1[i]<-paste0("00", num1[i])
}


num2<-c(1:length(Observation_files2))
num2[num2<100&num2>9]<-paste0("0", num2[num2<100&num2>9])
for (i in 1:9) {
  num2[i]<-paste0("00", num2[i])
}

#Select only those that are related to the patients of interest for clinical table
for (i in (1:length(Observation_files))) {
  FileName<-paste0("FILEPATH/observations/GoldClinical/18_288R_GOLD_Extract_Clinical_", num[i], ".txt") 
  NewFile<-paste0("FILEPATH/observations/NewGold/Obs", num[i], ".Rdata")
  tempobs<-ReadObsG(FileName)
  PatObs<-merge(x=PatGold, y=tempobs, by = "patid", all.y = FALSE, all.x=FALSE)
  save(PatObs, file = NewFile)
  print(num[i])
}

#Create a list of numbers so they run sequentially from those in clinical, to those in referral to those in test tables
num1a<-c(length(Observation_files):length(Observation_files)+length(Observation_files1))
num1a[num1a<100&num1a>9]<-paste0("0", num1a[num1a<100&num1a>9])

num2a<-c(length(Observation_files1)+length(Observation_files)+1:length(Observation_files2))
num2a[num2a<100&num2a>9]<-paste0("0", num2a[num2a<100&num2a>9])

#Select only those that are related to the patients of interest for referral table
for (i in (1:length(Observation_files1))) {
  FileName<-paste0("FILEPATH/observations/GoldReferal/18_288R_GOLD_Extract_Referral_", num1[i], ".txt") 
  NewFile<-paste0("FILEPATH/observations/NewGold/Simp/Obs", num1a[i], ".Rdata")
  tempobs<-ReadObsG(FileName)
  PatObs<-merge(x=PatGold, y=tempobs, by = "patid", all.y = FALSE, all.x=FALSE)
  save(PatObs, file = NewFile)
  print(num[i])
}

#Select only those that are related to the patients of interest for referral table
for (i in (1:length(Observation_files2))) {
  FileName<-paste0("FILEPATH/observations/GoldTest/18_288R_GOLD_Extract_Test_", num2[i], ".txt") 
  NewFile<-paste0("FILEPATH/observations/NewGold/Simp/Obs", num2a[i], ".Rdata")
  tempobs<-ReadObsG(FileName)
  PatObs<-merge(x=PatGold, y=tempobs, by = "patid", all.y = FALSE, all.x=FALSE)
  save(PatObs, file = NewFile)
  print(num[i])
}
#clear workspace
rm(list = ls(all.names = TRUE))
````
