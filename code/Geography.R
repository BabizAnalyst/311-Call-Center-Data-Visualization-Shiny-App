library(dplyr)
library(ggplot2)
library(ggmap)
zip=read.csv("https://gist.githubusercontent.com/erichurst/7882666/raw/5bdc46db47d9515269ab12ed6fb2850377fd869e/US%2520Zip%2520Codes%2520from%25202013%2520Government%2520Data")

call=read.csv("311_Call_Center_Tracking_Data.csv")




call=call%>% mutate(year=year(Date))%>%
  filter(year>=2011&year<=2014)

callzip = merge(call,zip,by.x="Zip.Code",by.y="ZIP",all.x=T)

noNA = callzip %>%
  filter(LAT != "NA")

max(noNA$Zip.Code)

noNA2 = filter(noNA,Zip.Code<=91608 & Zip.Code>=90001)

callzip=saveRDS(callzip,"callzip.rds")


noNA3 = noNA2 %>%
  group_by(Service.Name) %>%
  summarise(Count = n()) %>%
  arrange(-Count) %>%
  filter(Count > 50000) %>%
  filter(Service.Name != "")

noNA4 = noNA2 %>%
  filter(Service.Name %in% c("Bulky Item Pick-up",
                             "Online Request for Permit Inspection",
                             "Graffiti Removal - Community Beautification",
                             "Report a Property Violation",
                             "Subject Specialty Group",
                             "Building Construction Permits",
                             "Pool Noise Inspection",
                             "877 ASK-LAPD - Non-emergency Police Service"))
LosAngeles="Los Angeles"
LAmap=qmap(LosAngeles,maptype="roadmap")
LAmap


LAmap+
  geom_jitter(data = noNA4, aes(x = LNG, y = LAT, 
                                color = Service.Name), width = 0.1,h=0.1)

LAmap+
  stat_density2d(data=noNA2,aes(x=LNG,y=LAT,fill=..level..),
                 geom="polygon",alpha=0.5)+
  scale_fill_gradient(low= "white", high = "#bd0026")+
  facet_wrap(~year)

data1=data%>%
  group_by(Zip.Code)%>%
  summarise(count=n())%>%
  arrange(-count)%>%
  slice(1:20)
str(data1)
data1$Zip.Code=as.factor(data1$Zip.Code)
data1=subset(data1,Zip.Code!="")
##dot plots
ggplot(data1,aes(x=count,y=reorder(Zip.Code,count)))+
  geom_point(color="blue")+
  ylab("Zipcode")

#####request
data=read.csv("MyLA311_Service_Request_Data_2016.csv")

unique(data$ActionTaken)
ranksix=c("Bulky Items","Graffiti Removal","Metal/Household Appliances","Illegal Dumping Pickup","Electronic Waste","Dead Animal Removal")
data1=filter(data,RequestType %in% ranksix )

LAMap <- qmap("Los Angeles", color = "bw")

LAMap +
  geom_point(data = data1,
             aes(x = Longitude, y = Latitude, color = RequestType), size =1.5,alpha=0.3)

data2=data1%>%
  group_by(APC)%>%
  summarise(count=n())%>%
  arrange(-count)