library(dplyr)
library(lubridate)

#Clear environment

rm(list = ls(all.names = TRUE))

#Load full clean data

load ("FILEPATH/MatchedFullNew2022.Rdata")

####Create a start and end date for each patient: For this study 01/04/00 to 01/03/17 and update ages####
Matched$start<-pmax(as.Date("2000-04-01"),Matched$crd,Matched$Date18,  na.rm=TRUE)
Matched$end<-pmin(as.Date("2017-03-31"), Matched$tod, Matched$deathdate, Matched$lcd, Matched$Date100, na.rm=TRUE)

#Age at start
Matched$AgeAtStart<-as.numeric(Matched$start-Matched$dob)/365.25
summary(Matched$AgeAtStart)

#Age at end
Matched$AgeAtEnd<-as.numeric(Matched$end-Matched$dob)/365.25
summary(Matched$AgeAtEnd)

#Age at SMI diagnosis (and diagnosis of matched SMI case for comparators)
Matched$AgeAtDiag<-Matched$case_index-Matched$dob
Matched$AgeAtDiag<-as.numeric(Matched$AgeAtDiag)
Matched$AgeAtDiag<-Matched$AgeAtDiag/365.25

####Drop invalid data####

#Create a table to store the results of exclusions so can easily create a flow chart for pubs
Consort<-data.frame("Count" = length(Matched$patid), "Description" = c("Original"), "Eligible" = sum(Matched$hes_e))

##1/Cases in England (because we dont have linked hospital data outside of England)
Matched<-subset(Matched, region!="Scotland"& region!="Wales"&region!="Northern Ireland")
Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("Not in England"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

##2/Index date in study period
Matched<-subset(Matched,(Matched$case_index>="2000-04-01" & Matched$case_index<="2016-03-31"))
Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("SMI diagnosis not in study period"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

##3/ Age
#Drop those under 18 at index 
Matched<-subset(Matched,(year(Matched$case_index)-Matched$yob>=18))
Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("Under 18 at SMI diagnosis"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1)
#Drop those over 100 at index
Matched<-subset(Matched,(year(Matched$case_index)-Matched$yob<=100))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Over 100 at SMI diagnosis"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1)

##4/ End before index date
Matched<-subset(Matched,(end>=case_index))
Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("Exit before SMI diagnosis"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

##5/Diagnosis
#diagnosed before 2000
Matched<-subset(Matched, Matched$diagnosis_date>"2000-03-31"|is.na(Matched$diagnosis_date))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Diagnosed before entry"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1)

#diagnosed after study ends
Matched<-subset(Matched, Matched$diagnosis_date<"2017-03-31"|is.na(Matched$diagnosis_date))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Diagnosed after study ends"), "Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

#Drop those cases diagnosed prior to 18
Matched<-subset(Matched, (Matched$AgeAtDiag>=18&Matched$comparator==0)|is.na(Matched$AgeAtDiag)|Matched$comparator==1)
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Diagnosed before 18"), "Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1)

##6/ Drop any that are remaining with start after end (24, most are death date just prior to registration)
StartEnd<-subset(Matched, Matched$start>Matched$end)
StartEnd<-select(StartEnd, patid, start, end, lcd, tod, Date100, deathdate, crd, Date18)
Matched<-subset(Matched, Matched$start<=Matched$end)
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Main start after Main end"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1)

##7/Missing region
Matched<-subset(Matched, !is.na(Matched$region))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("No region"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

##8/Without one year follow up
Matched$FUTime<-Matched$end-(Matched$case_index+1)
Matched$FUTime<-as.numeric(Matched$FUTime)
length(which(Matched$FUTime<364))

Matched<-subset(Matched, Matched$FUTime>=365.25)
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Under 1 years follow up"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

#Keep those with matched controls (1:1 at least)
MatchedCount<-Matched%>%
  dplyr::group_by (case_patid)%>%
  dplyr::summarise(Freq=n())

#Only keep if there are at least two patients
Keep<-subset(MatchedCount, MatchedCount$Freq>1)
Matched<-subset(Matched, (Matched$case_patid %in% Keep$case_patid))

#Only keep if at least one of those patients is a case

Cases<-subset(Matched, comparator==0)
Matched<-subset(Matched, case_patid %in% Cases$patid)
length(which(!(Matched$case_patid %in% Matched$patid)))
Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("Unmatched"),"Eligible" = sum(Matched$hes_e))
Consort<- rbind(Consort,Consort1 )

####Save numbers for flow chart####
#Generate consort
Consort
Consort<-Consort %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort<-select(Consort, -Lag)
save(Consort, file = "R:/pathfinder/Naomi/HospAdmissions/FlowChart.Rdata")

####Sort deaths and check dates####
#Died after exit but within follow up - didnt die in this study
length(which(Matched$deathdate>"2017-03-31"&Matched$deathdate<="2019-03-31"))
Matched$died[Matched$deathdate>"2017-03-31"&Matched$deathdate<="2019-03-31"]<-0
Matched$deathdate[Matched$deathdate>"2017-03-31"&Matched$deathdate<="2019-03-31"]<-NA

#Died after 100, but within study period - didnt die in follow up
length(which(Matched$deathdate>Matched$Date100&Matched$deathdate<="2019-03-31"))
Matched$died[Matched$deathdate>Matched$Date100&Matched$deathdate<="2019-03-31"]<-0
Matched$deathdate[Matched$deathdate>Matched$Date100&Matched$deathdate<="2019-03-31"]<-NA

#Who died after end date (110 patients)
length(which(Matched$deathdate>Matched$end))
length(which(Matched$deathdate>Matched$end&Matched$end==Matched$tod))

#If died more than 6 months after tod they didnt die
length(which(Matched$died==1&is.na(Matched$deathdate)))
Matched$DeathTime<-Matched$deathdate-Matched$end
length(which(Matched$DeathTime>182&Matched$deathdate>Matched$end))
Matched$died[Matched$DeathTime>182&Matched$deathdate>Matched$end]<-0
Matched$deathdate[Matched$DeathTime>182&Matched$deathdate>Matched$end]<-NA

#If died within 6 months, end date become date of death
length(which(Matched$DeathTime<=182&Matched$DeathTime>0))
died<-subset(Matched,Matched$DeathTime<=182&Matched$DeathTime>0)
died<-select(died, patid, deathdate, end, tod, lcd, Date100, start, crd, Date18, source, DeathTime)
length(which(died$DeathTime>30))
Matched$end[Matched$DeathTime<=182&Matched$DeathTime>0&!is.na(Matched$DeathTime)]<-Matched$deathdate[Matched$DeathTime<=182&Matched$DeathTime>0&!is.na(Matched$DeathTime)]

#Check
Matched$DeathTime<-if_else(Matched$deathdate>Matched$end, Matched$deathdate-Matched$end, 0)
length(which(Matched$DeathTime>182&Matched$deathdate>Matched$end))
length(which(Matched$DeathTime<=182&Matched$DeathTime>0))
length(which(Matched$deathdate>Matched$end))

#Follow up starts the day after SMI diagnosis
CPRD$Outcome<-CPRD$case_index+1

#Save clean file
save(CPRD, file = "FILEPATH/CleanCPRD.Rdata")

#Clear environment

rm(list = ls(all.names = TRUE))

