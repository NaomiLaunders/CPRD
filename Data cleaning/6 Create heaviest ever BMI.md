# Defining heaviest ever recorded BMI
This is dependent on [extracting the calculated BMI, BMI values and BMI categories](https://github.com/NaomiLaunders/CPRD/blob/main/Data%20cleaning/5%20Extract%20BMI%20values.md).

## BMI Values

````
#Aurum
AllSMI<-merge(x=AllSMI, y=PatBMIAll, by ="patid", all.y = FALSE, all.x=TRUE)
AllSMI<-select(AllSMI, -numunitid, -medcodeid, -Description, -enterdate)
names(AllSMI)[names(AllSMI)=="value"]<-"BMIValue"
names(AllSMI)[names(AllSMI)=="obsdate"]<-"BMIValueDate"

#Gold
AllSMI<-merge(x=AllSMI, y=BMIG, by.x ="patid", by.y="patid.x", all.y = FALSE, all.x=TRUE)
AllSMI$BMIValue[is.na(AllSMI$BMIValue)]<-AllSMI$BMI[is.na(AllSMI$BMIValue)]
AllSMI$BMIValueDate[is.na(AllSMI$BMIValueDate)]<-AllSMI$eventdate[is.na(AllSMI$BMIValueDate)]
AllSMI<-select(AllSMI, -BMI, -eventdate, -MaxBMI.x, -MaxBMI.y, -data3, -enttype.x)

tapply(AllSMI$BMIValue, AllSMI$source, summary)
AllSMI$BMIage<-as.numeric(format(AllSMI$BMIValueDate, "%Y"))-AllSMI$yob
summary(AllSMI$BMIage)
````
## BMI calculations
````
#Height
#Aurum
AllSMI<-merge(x=AllSMI, y=PatHeightAll, by ="patid", all.y = FALSE, all.x=TRUE)
AllSMI<-select(AllSMI, -numunitid, -medcodeid, -Description, -enterdate)
names(AllSMI)[names(AllSMI)=="value"]<-"HeightA"
names(AllSMI)[names(AllSMI)=="obsdate"]<-"HeightDate"

#Gold
AllSMI<-merge(x=AllSMI, y=HeightG, by.x ="patid", by.y="patid.x", all.y = FALSE, all.x=TRUE)
AllSMI$HeightDate<-if_else(is.na(AllSMI$HeightDate), AllSMI$eventdate, AllSMI$HeightDate)
AllSMI$Height[is.na(AllSMI$Height)]<-AllSMI$HeightA[is.na(AllSMI$Height)]
AllSMI<-select(AllSMI, -eventdate, -Weight, -enttype.x, -data1, -data2, -HeightA)

#Weight
#Aurum
AllSMI<-merge(x=AllSMI, y=PatWeightAll, by ="patid", all.y = FALSE, all.x=TRUE)
AllSMI<-select(AllSMI, -numunitid, -medcodeid, -Description, -enterdate)
names(AllSMI)[names(AllSMI)=="value"]<-"WeightA"
names(AllSMI)[names(AllSMI)=="obsdate"]<-"WeightDate"

#Gold
AllSMI<-merge(x=AllSMI, y=WeightG, by.x ="patid", by.y="patid.x", all.y = FALSE, all.x=TRUE)
AllSMI$WeightDate<-if_else(is.na(AllSMI$WeightDate), AllSMI$eventdate, AllSMI$WeightDate)
AllSMI$Weight[is.na(AllSMI$Weight)]<-AllSMI$WeightA[is.na(AllSMI$Weight)]
AllSMI<-select(AllSMI, -eventdate, -enttype.x, -data1, -data2, -WeightA)

#Check height was taken as an adult
AllSMI$heightage<-as.numeric(format(AllSMI$HeightDate, "%Y"))-AllSMI$yob
table(AllSMI$heightage)
AllSMI$Height[AllSMI$heightage<18]<-NA

#Check weight was taken as an adult
AllSMI$weightage<-as.numeric(format(AllSMI$WeightDate, "%Y"))-AllSMI$yob
table(AllSMI$weightage)
AllSMI$Weight[AllSMI$weightage<18]<-NA

#Check height and weight
summary(AllSMI$Height)
length(which(AllSMI$Height<1))
AllSMI$Height[AllSMI$Height<1]<-NA

summary(AllSMI$Weight)
length(which(AllSMI$Weight<40))
AllSMI$Height[AllSMI$Weight<40]<-NA

#Dont keep height and weight unless both are present
AllSMI$Height[is.na(AllSMI$Weight)]<-NA
AllSMI$Weight[is.na(AllSMI$Height)]<-NA
summary(AllSMI$Weight)
summary(AllSMI$Height)

#Calculate BMI
AllSMI$BMICalc<-AllSMI$Weight/(AllSMI$Height^2)
summary(AllSMI$BMICalc)
AllSMI$BMICalc[AllSMI$BMICalc>80|AllSMI$BMICalc<10]<-NA
summary(AllSMI$BMICalc)
````
## BMI categories
````
#Pull in BMI categories
#Aurum
AllSMI<-merge(x=AllSMI, y=PatBMICatAll, by ="patid", all.y = FALSE, all.x=TRUE)
AllSMI<-select(AllSMI,-MedCodeId, -value, -Freq, -enterdate, -MaxBMICat, -MinBMICat)
names(AllSMI)[names(AllSMI)=="obsdate"]<-"BMICatDate"
names(AllSMI)[names(AllSMI)=="BMICat"]<-"BMICatRead"

#Gold
AllSMI<-merge(x=AllSMI, y=PatEthnGold, by ="patid", all.y = FALSE, all.x=TRUE)
AllSMI$BMICatDate<-if_else(is.na(AllSMI$BMICatDate), AllSMI$eventdate, AllSMI$BMICatDate)
AllSMI$BMICatRead<-ifelse(is.na(AllSMI$BMICatRead), AllSMI$BMICat, AllSMI$BMICatRead)
AllSMI<-select(AllSMI,-medcode, -eventdate, -sysdate, -MaxBMICat, -MinBMICat, -BMICat)
````
## Take the heaviest
````
#Take worst of BMIValue, BMICalc
AllSMI$BMI<-NA
AllSMI$BMI<-pmax(AllSMI$BMICalc,AllSMI$BMIValue, na.rm = TRUE)
summary(AllSMI$BMI)

#Generate BMI categories and take worst of that and BMICat
AllSMI$BMIGrp<-cut(AllSMI$BMI, breaks=c(0, 18.5, 25, 30, Inf), right=FALSE, labels=c("<18.5", "18.5-<25", "25-<30", "30+"))
table(AllSMI$BMIGrp)

#Take heaviest of BMIGrp and BMICat
table(AllSMI$BMIGrp, AllSMI$BMICatRead, useNA="ifany")
AllSMI$BMIGrp<-as.numeric(AllSMI$BMIGrp)
#Set BMICatRead so that 1-under, 2- norm, 3-over, 4 - obese
AllSMI$BMICatRead[AllSMI$BMICatRead==3]<-4
AllSMI$BMICatRead[AllSMI$BMICatRead==2]<-3

AllSMI$BMICat<-pmax(AllSMI$BMICatRead,AllSMI$BMIGrp, na.rm = TRUE)

#Set missing to "normal"
AllSMI$BMIMiss<-AllSMI$BMICat
AllSMI$BMICat[is.na(AllSMI$BMICat)]<-2
table(AllSMI$BMICat, useNA="ifany")

#Clean up
AllSMI<-select(AllSMI, -BMICalc, -BMIValue, -BMICatRead, -BMIGrp, -BMIage)
````
