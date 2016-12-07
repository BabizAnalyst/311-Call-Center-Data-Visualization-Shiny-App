call <- read.csv("311_Call_Center_Tracking_Data.csv")
request=read.csv("MyLA311_Service_Request_Data_2016.csv")

call$Time=hms(call$Time)
call$Date=ymd(call$Date)
request$CreatedDate=ymd_hms(request$CreatedDate)

# Call
## Issue Analysis


callissue=call %>%
  group_by(Service.Name) %>%
  filter(Service.Name!="") %>%
  filter(Service.Name!="") %>%
  summarise(count=n())

arrange=order(callissue$count,decreasing=T)
callissue=callissue[arrange,]
callissue20=head(callissue,20)

ggplot(callissue20,aes(x=reorder(Service.Name,count),y=count))+
  geom_bar(stat="identity",color="black",fill='lightblue')+
  xlab("Issue Type")+ylab("Count")+
  ggtitle("Top 20 Issues by Call 311")+
  coord_flip()+theme(axis.text=element_text(face='bold'))

## Seasonality

heatmapcall=call %>%
  mutate(Day=wday(Date,label=T,abbr=T),
         Hour=hour(Time)) %>%
  filter(Service.Name!="") %>%
  group_by(Day,Hour) %>%
  summarise(count=n())

ggplot(heatmapcall,aes(x=Day,y=factor(Hour),fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="darkblue",guide=F)+
  xlab("Day of Week")+ylab("Hour of Day")+
  ggtitle("Issues occurred in Day of Week vs. Hour of Day")


callym=call %>%
  mutate(Month=month(call$Date,label=T,abbr=T),Year=year(call$Date)) %>%
  filter(Service.Name!="") %>%
  group_by(Year,Month) %>%
  summarise(count=n())

callym$Year=as.factor(callym$Year)
ggplot(callym,aes(x=Month,y=count,group=Year,color=Year))+
  geom_line()+scale_color_manual(values=c('blue','green','black','red'))+
  xlab("Month")+ylab("count")+ggtitle("Issues occurred By Month and Year")


# Request 
## Issue

requestissue=request %>%
  filter(!is.na(RequestType)) %>%
  group_by(RequestType) %>%
  summarise(count=n())

arrangee=order(requestissue$count,decreasing=T)
requestissue=requestissue[arrangee,]

requestissue$Percent=(requestissue$count/sum(requestissue$count))*100

plot_ly(requestissue,labels=~RequestType,values=~Percent,type='pie') %>%
  layout(title = 'Issues By MyLA Request',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


ggplot(requestissue,aes(x=reorder(RequestType,Percent),y=Percent))+
  geom_point(color="orange",size=3)+
  xlab("Issue Type")+ylab("Percent of Issues")+
  ggtitle("Issues By MyLA Request")+
  coord_flip()+theme(axis.text=element_text(face='bold'))


## Seasonality

heatmaprequest=request %>%
  filter(!is.na(RequestType)) %>%
  mutate(Day=wday(CreatedDate,label=T,abbr=T),Hour=hour(CreatedDate)) %>%
  group_by(Day,Hour) %>%
  summarise(count=n())

ggplot(heatmaprequest,aes(x=Day,y=factor(Hour),fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="orange",guide=F)+
  xlab("Day of Week")+ylab("Hour of Day")+ggtitle("Issue By Day of Week vs. Hour of Day")

requestqm=request %>%
  filter(!is.na(RequestType)) %>%
  mutate(Quarter=quarter(CreatedDate),Month=month(CreatedDate,label=T,abbr=T)) %>%
  group_by(Quarter,Month) %>%
  summarise(count=n())


ggplot(requestqm,aes(x=Month,y=count,fill=factor(Quarter)))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("red","orange","yellow"))+
  xlab("Month")+ylab("count")+ggtitle("Issues By Quarter vs. Month")


ggplot(requestqm,aes(x=Quarter,y=count,fill=Month))+
  geom_bar(stat="identity",position=position_dodge(0.8))

request$CreatedDate=ymd_hms(request$CreatedDate)
requestcloud = request %>%
  filter(!is.na(RequestType)) %>%
  mutate(Day=wday(CreatedDate,label=T,abbr=T),Hour=hour(CreatedDate)) %>%
  filter(Day=="Mon" & Hour=="10")

wordCorpus = Corpus(VectorSource(requestcloud$RequestType))
## get the colors
pal = brewer.pal(9,"Oranges")
?brewer.pal
display.brewer.pal(9,"Oranges")

## drop the first 4 colors from the palett (not easy to see)
pal = pal[-c(1:4)]

set.seed(100)
wordcloud(words=wordCorpus,colors=pal,random.order=F)
