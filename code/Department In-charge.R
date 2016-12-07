library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(ggmap)
library(lubridate)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
###tr call
trdata=read.csv("311_Call_Center_Tracking_Data.csv")

colnames(trdata)[1]="Date"
trdata$Date=mdy(trdata$Date)
trdata$year=year(trdata$Date)
trdata$Time=as.character(trdata$Time)
trdata$Time=hms(trdata$Time)
trdata$Hour=hour(trdata$Time)
trdata$Hour=as.integer(trdata$Hour)
trdata$Day=wday(trdata$Date,label=T,abbr=T)
trdata2=filter(trdata,Date >= ymd("2011-01-01") & Date < ymd("2015-01-01"))
trdata3=trdata2%>%
  filter(!Department.Name=="")%>%
  filter(!Service.Name=="")%>%
  group_by(Department.Name, Service.Name)%>%
  summarise(Count = n())
trdatabar=trdata2%>%
  filter(!Department.Name=="")%>%
  filter(!Service.Name=="")%>%
  group_by(year,Department.Name)%>%
  summarise(Count = n())%>%
  arrange(-Count)


trfit=head(trdatabar[order(trdatabar$Count,decreasing = TRUE),],20)


ggplot(trfit,aes(Count,reorder(Department.Name,Count)))+ 
  geom_point(color="Blue",size=2)+
  facet_wrap(~year,nrow = 2)+
  ggtitle("Number of calls for top 6 departments Over time")+
  xlab("Counts")+
  ylab("")



###service
serdata=read.csv("MyLA311_Service_Request_Data_2016.csv")
serdata$CreatedDate=as.character(serdata$CreatedDate)
serdata$CreatedDate=str_replace_all(serdata$CreatedDate,"AM","")
serdata$CreatedDate=str_replace_all(serdata$CreatedDate,"PM","")
serdata$CreatedDate=mdy_hms(serdata$CreatedDate)
serdata2=filter(serdata,CreatedDate>=ymd("2016-01-01") & CreatedDate < ymd("2016-09-30"))
serdata2$PolicePrecinct=as.character(serdata2$PolicePrecinct)
pro=as.data.frame(serdata2$PolicePrecinct)

## PolicePrecint word cloud

pro2=pro%>%
  filter(!serdata2$PolicePrecinct=="")%>%
  group_by(serdata2$PolicePrecinct)%>%
  summarise(Count = n())
  
set.seed(1234)
wordcloud(words = pro2$`serdata2$PolicePrecinct`, freq = pro2$Count,random.order=FALSE,  
          colors=brewer.pal(8, "Dark2"))


pro3=serdata2%>%
  filter(!RequestType=="")%>%
  filter(!Owner=="")%>%
  group_by(Owner,RequestType)%>%
  summarise(Count = n())
  
ggplot(pro3, aes(x=Owner,y=Count,fill=RequestType)) + 
  geom_bar(stat="identity",position="dodge")



