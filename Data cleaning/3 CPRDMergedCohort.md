# Cohort creation
Here I have a cohort of patients with and without SMI based in CPRD GOLD and CPRD Aurum. They were defined based on the SMI code list (link to follow), and matched on 5-year age band, sex, primary care practice and registration date. 

The main dataframe is called AllSMI. When CPRD Aurum and CPRD GOLD data are merged, some fields need cleaning.

## Clean fields
Determine the class of all variables  
````str(AllSMI)````  

Change some to factors or dates
````
DateChange<- c(5, 7, 8, 18, 20, 21, 25, 27, 32, 33, 35, 37)
AllSMI[,DateChange] <-lapply(AllSMI[,DateChange], as.Date, format= "%d/%m/%Y")
FactorChange<- c(9, 10, 11, 12, 14, 17, 19, 22, 26, 28, 33, 36 )
AllSMI[,FactorChange] <-lapply(AllSMI[,FactorChange], as.factor)
````  

Merge fields that are different between GOLD and Aurum

````
#SMI diagnostic codes
AllSMI<-rename(AllSMI, SMICode = medcodeid)
AllSMI$SMICode[is.na(AllSMI$SMICode)]<-AllSMI$medcode
AllSMI$medcode<-NULL

#Deaths
AllSMI$died<-0
AllSMI$died[!is.na(AllSMI$deathdate)|!is.na(AllSMI$cprd_ddate)]<-1

#registration dates
AllSMI$deathdate<-if_else(!is.na(AllSMI$deathdate), AllSMI$deathdate, AllSMI$cprd_ddate)
AllSMI$tod<-if_else(!is.na(AllSMI$tod), AllSMI$tod, AllSMI$regenddate)
AllSMI$crd<-if_else(!is.na(AllSMI$regstartdate), AllSMI$regstartdate, AllSMI$crd)
AllSMI<-select(AllSMI, -regstartdate, -regenddate, -cprd_ddate, -regend)
````  
## Create some very basic fields
````
#Date of birth
AllSMI$dob<-paste0("01/01/", AllSMI$yob)
AllSMI$dob<-as.Date(AllSMI$dob, "%d/%m/%Y")

#Date turned 18 and 100
AllSMI$Date18<-AllSMI$dob+years(18)
AllSMI$Date100<-AllSMI$dob+years(100)

#Start and end of cohort follow up
AllSMI$start<-pmax(AllSMI$crd,AllSMI$Date18, AllSMI$diagnosis_date, as.Date("2000-01-01"), na.rm=TRUE)
AllSMI$end<-pmin(as.Date("2018-12-31"), AllSMI$tod, AllSMI$deathdate, AllSMI$lcd, AllSMI$Date100, na.rm=TRUE)

#Age at start
AllSMI$AgeAtStart<-as.numeric(AllSMI$start-AllSMI$dob)/365.25
summary(AllSMI$AgeAtStart)

#Age at end
AllSMI$AgeAtEnd<-as.numeric(AllSMI$end-AllSMI$dob)/365.25
summary(AllSMI$AgeAtEnd)

#Age at SMI diagnosis
AllSMI$AgeAtDiag<-AllSMI$diagnosis_date-AllSMI$dob
AllSMI$AgeAtDiag<-as.numeric(AllSMI$AgeAtDiag)
AllSMI$AgeAtDiag<-AllSMI$AgeAtDiag/365.25
````


