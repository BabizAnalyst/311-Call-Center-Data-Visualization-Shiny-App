
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)

call <- read.csv("311_Call_Center_Tracking_Data.csv")

### 1. Main resolution for top 12 service type

transfer = call %>%
  group_by(Call.Resolution) %>%
  summarise(Count = n())
  

service_resolution2 = call %>%
  filter(Call.Resolution %in% c("Warm Transfer (City)",
                                "Transfer (City)",
                                "Service Request Processed",
                                "Gave Caller Information",
                                "Referred To County",
                                "Got Voicemail (City)")) %>%
  filter(Service.Name %in% c("Bulky Item Pick-up",
                             "Online Request for Permit Inspection",
                             "Graffiti Removal - Community Beautification",
                             "Subject Specialty Group",
                             "Report a Property Violation",
                             "Building Construction Permits",
                             "877 ASK-LAPD - Non-emergency Police Service",
                             "Pool Noise Inspection",
                             "Report streetlight outages",
                             "Maintenance of Public Street Landscape",
                             "Official Police Garage Tow (OPG) - LAPD",
                             "Code Engineer")) %>%
  group_by(Call.Resolution, Service.Name) %>%
  summarise(Count = n())

ggplot(service_resolution2, aes(x = reorder(Call.Resolution,Count)
                                , y = Count)) +
  geom_bar(stat = "identity", fill = "turquoise3", color = "black") +
  facet_wrap(~Service.Name, ncol = 3) +
  coord_flip() +
  xlab("Call Resolution") +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Main resolution for top 12 service type")


### 2. Change of Service and Resolution over Time

call$Year = year(call$Date)
service_resolution5 = call %>%
  filter(Call.Resolution %in% c("Warm Transfer (City)",
                                "Transfer (City)",
                                "Service Request Processed",
                                "Gave Caller Information",
                                "Referred To County",
                                "Got Voicemail (City)")) %>%
  filter(Service.Name %in% c("Bulky Item Pick-up",
                             "Online Request for Permit Inspection",
                             "Graffiti Removal - Community Beautification",
                             "Subject Specialty Group",
                             "Report a Property Violation",
                             "Building Construction Permits",
                             "877 ASK-LAPD - Non-emergency Police Service",
                             "Pool Noise Inspection",
                             "Report streetlight outages",
                             "Maintenance of Public Street Landscape",
                             "Official Police Garage Tow (OPG) - LAPD",
                             "Code Engineer")) %>%
  group_by(Year, Call.Resolution, Service.Name) %>%
  summarise(Count = n())

ggplot(service_resolution5, aes(x = Year, y = Count,
                                fill = Call.Resolution)) +
  geom_area(position = "stack") +
  facet_wrap(~Service.Name) +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Change of Service and Resolution over Time")