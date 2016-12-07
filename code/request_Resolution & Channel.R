
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)

### Load data
request <- read.csv("MyLA311_Service_Request_Data_2016.csv")

### 1. Status of Different Request Type
request_status = request %>%
  group_by(RequestType, Status) %>%
  summarise(Count = n())

ggplot(request_status, aes(x = Status, y = Count)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  facet_wrap(~RequestType, ncol = 3) +
  coord_flip() +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Status of Different Request Type")

### 2. Feedback
feedback = request %>%
  filter(RequestType %in% "Feedback") %>%
  group_by(Status, RequestType, RequestSource) %>%
  summarise(Count = n())

ggplot(feedback, aes(x = RequestSource, y = Count, fill = Status)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("coral3", "darkturquoise", 
                               "hotpink2", "cornflowerblue", "darksalmon")) +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Feedback") +
  xlab("Request Source")

### 3. Homeless Encampment
homeless = request %>%
  filter(RequestType %in% "Homeless Encampment") %>%
  filter(RequestSource %in% c("Self Service",
                              "Mobile App",
                              "Email",
                              "Call")) %>%
  group_by(Status, RequestType, RequestSource) %>%
  summarise(Count = n())

ggplot(homeless, aes(x = RequestSource, y = Count, fill = Status)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("coral3", "darkturquoise", 
                               "hotpink2", "cornflowerblue", "darksalmon")) +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Homeless Encampment") +
  xlab("Request Source")

### 4. Overall Trends in Calls and Apps
request$YM = str_sub(request$CreatedDate, end = 7)
request$YM = as.factor(request$YM)

call_app = request %>%
  filter(RequestSource %in% c("Call",
                              "Driver Self Report",
                              "Self Service",
                              "Mobile App",
                              "Email",
                              "Web Form")) %>%
  group_by(YM, RequestSource) %>%
  summarise(Count = n())

ggplot(call_app, aes(x = YM, y = Count, 
                     color = RequestSource, 
                     group = RequestSource)) +
  geom_line(stat = "identity", size = 2.5) +
  ylim(c(0, 60000)) +
  xlab("Time") +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Overall Trends of Different Request Source")

### 5. Request Source of Different Service Type
request_source = request %>%
  filter(RequestSource %in% c("Call", 
                              "Driver Self Report", 
                              "Mobile App",
                              "Self Service", 
                              "Email")) %>%
  filter(RequestType %in% c("Bulky Items",
                            "Dead Animal Removal",
                            "Electronic Waste",
                            "Graffiti Removal",
                            "Illegal Dumping Pickup",
                            "Metal/Household Appliances")) %>%
  group_by(RequestType, RequestSource) %>%
  summarise(Count = n())


ggplot(request_source, aes(x = RequestSource, y = Count)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  facet_wrap(~RequestType) +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Request Source of Different Service Type")

### 6. Request Source of Different Service Type 2 
request_source2 = request %>%
  filter(RequestSource %in% c("Call", 
                              "Driver Self Report", 
                              "Mobile App",
                              "Self Service", 
                              "Email")) %>%
  filter(RequestType %in% c("Feedback",
                            "Homeless Encampment",
                            "Multiple Streetlight Issue",
                            "Other",
                            "Report Water Waste",
                            "Single Streetlight Issue")) %>%
  group_by(RequestType, RequestSource) %>%
  summarise(Count = n())
ggplot(request_source2, aes(x = RequestSource, y = Count)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  facet_wrap(~RequestType) +
  theme(axis.title = element_text(face="bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 8)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  ggtitle("Request Source of Different Service Type")