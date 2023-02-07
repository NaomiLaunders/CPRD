# Creating a cohort with specific inclusion and exclusion criteria
Note, this assumes the study start and end date as set in the [CPRD merged cohort] (https://github.com/NaomiLaunders/NaomiLaunders/blob/main/Data%20cleaning/CPRDMergedCohort.md) file.

The comparator variable is SMI = 0, no SMI = 1.

## Create a file to count the drop outs
````
library(tidyverse)
load("AllSMIClean.Rdata")
Matched<-AllSMI
Consort<-data.frame("Count" = length(Matched$patid), "Description" = c("Original"), "SMI"=length(Matched$comparator==0))
````
## Drop those that don't meet study criteria

````
#Drop those under 18 by end of follow up
Matched<-subset(Matched,(year(Matched$end)-Matched$yob>=18))
Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("Under 18"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#Drop those over 100 at beginning of follow up
Matched<-subset(Matched,(year(Matched$start)-Matched$yob<100))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Over 100"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#Drop those with death before start
Matched<-subset(Matched, Matched$deathdate>Matched$start|is.na(Matched$deathdate))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Death before start"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#Drop those who transfer out prior to study date
Matched<-subset(Matched, Matched$tod>Matched$start|is.na(Matched$tod))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Transfer before start"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#Drop those with LCD out prior to study date
Matched<-subset(Matched, Matched$lcd>Matched$start|is.na(Matched$lcd))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("LCD before start"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#other date error
dates<-subset(Matched, Matched$start>Matched$end)
dates<-select(dates, patid, start, end, crd, Date18, tod, deathdate, Date100, yob, lcd)
Matched<-subset(Matched, Matched$start<=Matched$end)
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Main start after Main end"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#diagnosed before study starts
Matched<-subset(Matched, Matched$diagnosis_date>"1999-12-31"|is.na(Matched$diagnosis_date))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Diagnosed before study starts"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1 )

#diagnosed after study ends
Matched<-subset(Matched, Matched$diagnosis_date<"2019-01-01"|is.na(Matched$diagnosis_date))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Diagnosed after study ends"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1 )

#Diagnosed prior to 18
Matched<-subset(Matched, (Matched$AgeAtDiag>=18&Matched$comparator==0)|is.na(Matched$AgeAtDiag)|Matched$comparator==1)
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Diagnosed before 18"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#Without one year follow up
Matched$FUTime<-Matched$end-Matched$start
Matched$FUTime<-as.numeric(Matched$FUTime)
length(which(Matched$FUTime<365.25))

Matched<-subset(Matched, Matched$FUTime>=365.25)
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("Under 1 years follow up"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1)

#With missing region
Matched<-subset(Matched, !is.na(Matched$region))
Consort1<- data.frame("Count" =length(Matched$patid), "Description" = c("No region"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1 )
````
## Keep only those that are matched at least 1:1
````
#Ensure at least two instances of the case patient ID
MatchedCount<-Matched%>%
  group_by(case_patid)%>%
  summarise(Freq=n())
table(MatchedCount$Freq, useNA="ifany")
Drop<-subset(MatchedCount, MatchedCount$Freq<2)
Matched<-merge(x=Matched, y=Drop, by="case_patid", all.x=TRUE, all.y=FALSE)
length(which(!is.na(Matched$Freq)))
Matched<-subset(Matched, is.na(Matched$Freq))
Matched$Freq<-NULL

#Ensure at least one of those instances is a case
length(which(!(Matched$case_patid %in% Matched$patid)))
Matched<-subset(Matched, Matched$case_patid %in% Matched$patid)

Consort1<-data.frame("Count" =length(Matched$patid), "Description" = c("Unmatched"), "SMI"=length(Matched$comparator==0))
Consort<- rbind(Consort,Consort1 )
````
## Show drop out numbers
````
Consort<-Consort %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort<-select(Consort, -Lag)
save(Consort, file = "FILEPATH/Consort.Rdata")
````
## Sort out core variables which might be recorded following the end of follow up
````
#If died after 31/12/2018 didnt die
length(which(Matched$deathdate>as.Date("2018-12-31")))
Matched$died[Matched$deathdate>as.Date("2018-12-31")]<-0
Matched$deathdate[Matched$deathdate>as.Date("2018-12-31")]<-NA

length(which(Matched$deathdate>Matched$Date100))
Matched$died[Matched$deathdate>Matched$Date100]<-0
Matched$deathdate[Matched$deathdate>Matched$Date100]<-NA

#If died after transfer, didn't die
length(which(Matched$deathdate>Matched$end))
Matched$died[Matched$deathdate>Matched$end]<-0
Matched$deathdate[Matched$deathdate>Matched$end]<-NA

#Save clean file
save(Matched, file = "FILEPATH/MMFinClean.Rdata")
