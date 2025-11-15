#this is to detect specific conditions

#######change your directory here
setwd("C:/Users/#redacted/temp archive")
#######change your directory here

################# base loading

#Mav1<-read.csv("all mav tub and cas 1.csv",stringsAsFactors=FALSE)
#Mav2<-read.csv("all mav tub and cas 2.csv",stringsAsFactors=FALSE)
#Mav3<-read.csv("all mav tub and cas 3.csv",stringsAsFactors=FALSE)
#production<-rbind(Mav1,Mav2,Mav3)
#rm(Mav1,Mav2,Mav3)

production<-read.csv("all delaware tub and cas.csv",stringsAsFactors=FALSE)

production$PI_S.Value<-NULL
names(production)[names(production)=="PI_T Value"]<-"PI_T.Value"
names(production)[names(production)=="PI_C Value"]<-"PI_C.Value"

production$PI_T.Value[production$PI_T.Value<0]<-NA
production$PI_C.Value[production$PI_C.Value<0]<-NA

#################### remove plunger and gas lift wells by name

prodfilter<-data.frame(production$facility_desc)
prodfilter<-unique(prodfilter)

names(prodfilter)[names(prodfilter)=="production.facility_desc"]<-"facility_desc"

prodfilter$excludePLG<-regexpr("PLG",prodfilter$facility_desc)
prodfilter$excludeGasLift<-regexpr("Gas Lift",prodfilter$facility_desc)

prodfilter$excludePLG[prodfilter$excludePLG<0]<-0
prodfilter$excludeGasLift[prodfilter$excludeGasLift<0]<-0

prodfilter$exclude<-(prodfilter$excludePLG+prodfilter$excludeGasLift)
prodfilter$excludePLG<-NULL
prodfilter$excludeGasLift<-NULL

prodfilter$facility_desc <- gsub(" Well Meter","",prodfilter$facility_desc)
prodfilter$facility_desc <- gsub(" Well PLG","",prodfilter$facility_desc)
prodfilter$facility_desc <- gsub(" Gas Lift","",prodfilter$facility_desc)
prodfilter$facility_desc <- gsub(" Well Pilot","",prodfilter$facility_desc)
prodfilter$facility_desc <- gsub(" Well","",prodfilter$facility_desc)
prodfilter$facility_desc <- gsub(" PLG","",prodfilter$facility_desc)

ComFilter= by(prodfilter, INDICES = list(prodfilter$facility_desc), FUN=function(ComFilter_Fun) {
  
SumExclude<-sum(ComFilter_Fun$exclude)
  
  return(data.frame(facility_desc = ComFilter_Fun$facility_desc[1], exclude=SumExclude))
  
})
ComFilter= do.call("rbind", ComFilter)

production$facility_desc <- gsub(" Well Meter","",production$facility_desc)
production$facility_desc <- gsub(" Well PLG","",production$facility_desc)
production$facility_desc <- gsub(" Gas Lift","",production$facility_desc)
production$facility_desc <- gsub(" Well Pilot","",production$facility_desc)
production$facility_desc <- gsub(" Well","",production$facility_desc)
production$facility_desc <- gsub(" PLG","",production$facility_desc)

production<-merge(production,ComFilter,by="facility_desc")

production[production$exclude>0,]<-NA
production<-na.omit(production)
production$exclude<-NULL

####################### format dates

production$Timestamp=as.POSIXct(strptime(production$Timestamp,"%m/%d/%Y %H:%M"))
production<-na.omit(production)
production$dates = as.Date(production$Timestamp, format="%m/%d/%Y")
production$Week = paste(strftime(production$dates,format="%Y"),strftime(production$dates,format="%W"))

#################### remove gas lift by casing pressure

MonthCasPress= by(production, INDICES = list(production$facility_desc), FUN=function(MonthCaPress_Fun) {
  
  MeanCasing<-median(MonthCaPress_Fun$PI_C.Value)
  MonthDate<-format(MonthCaPress_Fun$dates, "%m")
  
  return(data.frame(facility_desc = MonthCaPress_Fun$facility_desc[1], MeanCasing=MeanCasing[1],MonthDate=MonthDate[1]))
  
})
MonthCasPress= do.call("rbind", MonthCasPress)

MaxMonthCasPress= by(MonthCasPress, INDICES = list(MonthCasPress$facility_desc,MonthCasPress$MonthDate), FUN=function(MaxMonthCaPress_Fun) {
  
  MaxCasing<-max(MaxMonthCaPress_Fun$MeanCasing)
  
  return(data.frame(facility_desc = MaxMonthCaPress_Fun$facility_desc[1], MaxCasing))
  
})
MaxMonthCasPress= do.call("rbind", MaxMonthCasPress)

production<-merge(production,MaxMonthCasPress,by="facility_desc")

production[production$MaxCasing>450,]<-NA

production<-na.omit(production)

production$MaxCasing<-NULL
production$MonthDate<-NULL
production$PI_C.Value<-NULL

####################### end of formatting and filters, start of functions

WellCount<-length(unique(production$facility_desc))


for(n in WellCount)
{
  MinMaxMedDay = by(production, INDICES = list(production$facility_desc, production$dates), FUN=function(MinMaxMed_Day) {
    
    MMMD=(max(MinMaxMed_Day$PI_T.Value, na.rm=TRUE)-min(MinMaxMed_Day$PI_T.Value, na.rm=TRUE))/median(MinMaxMed_Day$PI_T.Value, na.rm=TRUE)
#    AvgProd=mean(MinMax_Day$FI_G.Value) 
    
    return(data.frame(dates = MinMaxMed_Day$dates[1], facility_desc = MinMaxMed_Day$facility_desc[1], MMMD))
    
  })
  MinMaxMedDay = do.call("rbind", MinMaxMedDay)
#  MinMaxMedDay<-MinMaxMedDay[MinMaxMedDay$AvgProd>20,]
  
  MinMaxMedDay$Week = paste(strftime(MinMaxMedDay$dates,format="%Y"),strftime(MinMaxMedDay$dates,format="%W"))
  
  
  MinMaxMedWeek = by(MinMaxMedDay, INDICES = list(MinMaxMedDay$facility_desc, MinMaxMedDay$Week), FUN=function(MinMaxMed_Week) {
    
    MMMW<-median(MinMaxMed_Week$MMMD)
    DaysInWeek<-length(unique(MinMaxMed_Week$dates))
    
    return(data.frame(Week = MinMaxMed_Week$Week[1], facility_desc = MinMaxMed_Week$facility_desc[1], MMMW,DaysInWeek))
    
  })
  MinMaxMedWeek = do.call("rbind", MinMaxMedWeek)
}

MinMaxMedWeek$MMMW[MinMaxMedWeek$MMMW>2]<-2
MinMaxMedWeek$MMMW[MinMaxMedWeek$MMMW<0]<-0

write.table(MinMaxMedWeek,file="MinMaxMedWeek.csv",sep=",",row.names=FALSE)
#write.table(production,file="production.csv",sep=",",row.names=FALSE)

rm(list = ls())

################# download from CygNet, cex file

#<expRoot vhs="MAVERICK.VHS01" earlyDate="3/1/2015 12:00:00 AM" lateDate="1/1/2016 2:00:00 AM" valueType="0" rollUnit="0" rollPer="1" rollTosu="0" filterUnrel="1" filterSame="0" vhsOnly="0" #dynTag="0" fileTag="" dynRoll="1" rowLayout="2" expHdr="1" expMs="0" dtFmt="%c" fileName="C:\Users\#redacted\temp archive\all mav tub and cas 1.csv" fileAppend="0">
#	<tags>
#		<tag0 tag="MAVERICK.UIS01::*.PI_T" rollType="0"/><tag1 tag="MAVERICK.UIS01::*.PI_C" rollType="0"/></tags>
#	<cols>
#		<col0 col="Timestamp"/><col1 col="facility_desc"/><col2 col="Value"/></cols></expRoot>

################# download from CygNet, cex file

message("finished")
