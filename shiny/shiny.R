library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(ggmap)
library(lubridate)
library(wordcloud)



call=read.csv("311_Call_Center_Tracking_Data.csv")
colnames(call)[1]="Date"
call$Date=mdy(call$Date)
call$year=year(call$Date)
call$Time=as.character(call$Time)
call$Time=hms(call$Time)
call$Hour=hour(call$Time)
call$Hour=as.integer(call$Hour)
call$Day=wday(call$Date,label=T,abbr=T)
call=filter(call,Date >= ymd("2011-01-01") & Date < ymd("2015-01-01"))

request=read.csv("MyLA311_Service_Request_Data_2016.csv")
request$CreatedDate = mdy_hms(request$CreatedDate)
request$Hour=hour(request$CreatedDate)
request$Date=date(request$CreatedDate)
request$YM = str_sub(request$CreatedDate, end = 7)
request$YM = as.factor(request$YM)
request=filter(request,Date>=ymd("2016-01-01") & Date <= ymd("2016-09-30"))
request$Hour=as.integer(request$Hour)
request$Date=as.character(request$Date)
request$Date=ymd(request$Date)



#ui interface controll function
ui <- navbarPage("LA Non-enmergency",
                 theme = shinytheme("cerulean"),
                 
                 tabPanel("Introduction",
                          includeHTML("C:/Users/Xuehang Pan/Desktop/BAS1/545/pro/include.html")
                          
                          
                 ),
                 
                 tabPanel("311CallLA",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                dateRangeInput("date", label = "Date range",start="2011-01-01",end="2014-12-31"),
                                br(),
                                sliderInput("hour", label = "Hour of interest:",min = 0, max = 24, value = c(0, 24))),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs", 
                                            tabPanel("Issue", plotOutput("a_issue"),tableOutput("a_issuetb")),
                                            tabPanel("Seasonality", plotOutput("a_season"),plotOutput("a_season2"),
                                                     fluidRow(
                                                       column(6,tableOutput("a_seasontb")),
                                                       column(6,tableOutput("a_seasontb2")))), 
                                            tabPanel("Department", plotOutput("a_depart"),tableOutput("a_departb")),
                                            tabPanel("Resolution", plotOutput("a_reso"),tableOutput("a_resotb"))
                                            
                                )
                              )
                            )
                          )     
                 ),
                 tabPanel("Request MyLA",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                dateRangeInput("date2", label = "Date range",start="2016-01-01",end="2016-09-30"),
                                br(),
                                sliderInput("hour2", label = "Hour of interest:",min = 0, max = 24, value = c(0, 24))),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs", 
                                            tabPanel("Geographic",
                                                     fluidRow(
                                                       column(4,checkboxGroupInput("Items", label = "Items",
                                                                                   choices = list("Bulky Items" = 1,
                                                                                                  "Graffiti Removal" = 2, "Metal/Household Appliances" = 3,"Illegal Dumping Pickup"=4,"Electronic Waste"=5,"Dead Animal Removal"=6),
                                                                                   selected = 1)
                                                       ),
                                                       column(8,plotOutput("b_geo")))
                                            ),
                                            tabPanel("Issue", plotOutput("b_issue"),tableOutput("b_issuetb")),
                                            tabPanel("Seasonality", plotOutput("b_season"),plotOutput("b_season2"),tableOutput("b_seasontb")), 
                                            tabPanel("Department", plotOutput("b_depart"),tableOutput("b_departb")),
                                            tabPanel("Resolution", plotOutput("b_reso"),plotOutput("b_reso2"),tableOutput("b_resotb"),tableOutput("a_resotb2"))
                                            
                                            
                                )
                              )
                            )
                          )     
                 )
                 
                 
                 
                 
                 
)

#provide different output according to the user input
server <- function(input, output){
  
  
  
  
  output$a_issue <- renderPlot({
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
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
    
  })
  
  output$a_issuetb <-  renderTable({
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    callissue=call %>%
      group_by(Service.Name) %>%
      filter(Service.Name!="") %>%
      filter(Service.Name!="") %>%
      summarise(count=n())
    
    arrange=order(callissue$count,decreasing=T)
    callissue=callissue[arrange,]
    callissue20=head(callissue,20)
    
    callissue20
    
  })
  
  output$a_season <- renderPlot({
    
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    heatmapcall=call %>%
      filter(Service.Name!="") %>%
      group_by(Day,Hour) %>%
      summarise(count=n())
    
    ggplot(heatmapcall,aes(x=Day,y=factor(Hour),fill=count))+
      geom_tile()+
      scale_fill_gradient(low="white",high="darkblue",guide=F)+
      xlab("Day of Week")+ylab("Hour of Day")+
      ggtitle("Issues occurred in Day of Week vs. Hour of Day")
    
  })
  output$a_seasontb <-  renderTable({
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    heatmapcall=call %>%
      filter(Service.Name!="") %>%
      group_by(Day,Hour) %>%
      summarise(count=n())
    heatmapcall
    
  })
  
  output$a_season2 <- renderPlot({
    
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    callym=call %>%
      mutate(Month=month(call$Date,label=T,abbr=T),Year=year(call$Date)) %>%
      filter(Service.Name!="") %>%
      group_by(Year,Month) %>%
      summarise(count=n())
    
    callym$Year=as.factor(callym$Year)
    ggplot(callym,aes(x=Month,y=count,group=Year,color=Year))+
      geom_line()+scale_color_manual(values=c('blue','green','black','red'))+
      xlab("Month")+ylab("count")+ggtitle("Issues occurred By Month and Year")
    
  })
  
  output$a_seasontb2 <-  renderTable({
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    callym=call %>%
      mutate(Month=month(call$Date,label=T,abbr=T),Year=year(call$Date)) %>%
      filter(Service.Name!="") %>%
      group_by(Year,Month) %>%
      summarise(count=n())
    
    callym$Year=as.factor(callym$Year)
    callym
  })
  
  output$a_depart <- renderPlot({
    
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    trdatabar=call%>%
      filter(!Department.Name=="")%>%
      filter(!Service.Name=="")%>%
      group_by(year,Department.Name)%>%
      summarise(Count = n())%>%
      arrange(-Count)
    
    
    trfit=head(trdatabar[order(trdatabar$Count,decreasing = TRUE),],20)
    
    ggplot(trfit,aes(Count,reorder(Department.Name,Count)))+ 
      geom_point(color="Blue",size=2)+
      facet_wrap(~year,nrow = 2)+
      ggtitle("Number of calls for top departments Over time")+
      xlab("Counts")+
      ylab("")
    
  })
  
  output$a_departb <-  renderTable({
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
    trdatabar=call%>%
      filter(!Department.Name=="")%>%
      filter(!Service.Name=="")%>%
      group_by(year,Department.Name)%>%
      summarise(Count = n())%>%
      arrange(-Count)
    
    
    trfit=head(trdatabar[order(trdatabar$Count,decreasing = TRUE),],20)
    trfit
    
  })
  
  output$a_reso <- renderPlot({
    
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
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
    
  })
  output$a_resotb <-  renderTable({
    call=filter(call,Date >= ymd(as.character(input$date[1])) & Date <= ymd(as.character(input$date[2])))
    call=filter(call, Hour>=input$hour[1] & Hour<=input$hour[2])
    
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
    
    service_resolution2
  })
  
  
  output$b_issue <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    requestissue=request %>%
      filter(!is.na(RequestType)) %>%
      group_by(RequestType) %>%
      summarise(count=n())
    
    arrangee=order(requestissue$count,decreasing=T)
    requestissue=requestissue[arrangee,]
    
    requestissue$Percent=(requestissue$count/sum(requestissue$count))*100
    ggplot(requestissue,aes(x=reorder(RequestType,Percent),y=Percent))+
      geom_point(color="orange",size=3)+
      xlab("Issue Type")+ylab("Percent of Issues")+
      ggtitle("Issues By MyLA Request")+
      coord_flip()+theme(axis.text=element_text(face='bold'))
  })
  
  output$b_issuetb <-  renderTable({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    requestissue=request %>%
      filter(!is.na(RequestType)) %>%
      group_by(RequestType) %>%
      summarise(count=n())
    
    arrangee=order(requestissue$count,decreasing=T)
    requestissue=requestissue[arrangee,]
    
    requestissue$Percent=(requestissue$count/sum(requestissue$count))*100
    requestissue
    
  })
  
  
  output$b_season <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    heatmaprequest=request %>%
      filter(!is.na(RequestType)) %>%
      mutate(Day=wday(CreatedDate,label=T,abbr=T)) %>%
      group_by(Day,Hour) %>%
      summarise(count=n())
    
    ggplot(heatmaprequest,aes(x=Day,y=factor(Hour),fill=count))+
      geom_tile()+
      scale_fill_gradient(low="white",high="orange",guide=F)+
      xlab("Day of Week")+ylab("Hour of Day")+ggtitle("Issue By Day of Week vs. Hour of Day")
    
  })
  
  output$b_seasontb <-  renderTable({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    heatmaprequest=request %>%
      filter(!is.na(RequestType)) %>%
      mutate(Day=wday(CreatedDate,label=T,abbr=T)) %>%
      group_by(Day,Hour) %>%
      summarise(count=n())
    heatmaprequest
    
    
  })
  
  output$b_season2 <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    requestcloud = request %>%
      filter(!is.na(RequestType)) %>%
      mutate(Day=wday(CreatedDate,label=T,abbr=T),Hour=hour(CreatedDate)) %>%
      filter(Day=="Mon" & Hour=="10")
    
    wordCorpus = Corpus(VectorSource(requestcloud$RequestType))
    ## get the colors
    pal = brewer.pal(9,"Oranges")
    
    ## drop the first 4 colors from the palett (not easy to see)
    pal = pal[-c(1:4)]
    
    set.seed(100)
    wordcloud(words=wordCorpus,colors=pal,random.order=F)
  })
  
  
  
  output$b_depart <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    pro3=request%>%
      filter(!RequestType=="")%>%
      filter(!Owner=="")%>%
      group_by(Owner,RequestType)%>%
      summarise(Count = n())
    
    ggplot(pro3, aes(x=Owner,y=Count,fill=RequestType)) + 
      geom_bar(stat="identity",position="dodge")
    
  })
  
  output$b_departb <-  renderTable({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    pro3=request%>%
      filter(!RequestType=="")%>%
      filter(!Owner=="")%>%
      group_by(Owner,RequestType)%>%
      summarise(Count = n())
    pro3
    
  })
  
  output$b_reso <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
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
    
  })
  
  output$b_resotb <-  renderTable({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    request_status = request %>%
      group_by(RequestType, Status) %>%
      summarise(Count = n())
    request_status
  })
  
  output$b_reso2 <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    call_app = request %>%
      filter(RequestSource %in% c("Call",
                                  "Driver Self Report",
                                  "Self Service",
                                  "Mobile App",
                                  "Email",
                                  "Web Form")) %>%
      group_by(YM, RequestSource) %>%
      summarise(Count = n())
    
    ggplot(call_app, aes(x = as.factor(YM), y = Count, 
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
  })
  
  output$b_resotb2 <-  renderTable({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    call_app = request %>%
      filter(RequestSource %in% c("Call",
                                  "Driver Self Report",
                                  "Self Service",
                                  "Mobile App",
                                  "Email",
                                  "Web Form")) %>%
      group_by(YM, RequestSource) %>%
      summarise(Count = n())
    call_app
  })
  
  output$b_geo <- renderPlot({
    request=filter(request,Date >= ymd(as.character(input$date2[1])) & Date <= ymd(as.character(input$date2[2])))
    request=filter(request, Hour>=input$hour2[1] & Hour<=input$hour2[2])
    
    ranksix=c("Bulky Items","Graffiti Removal","Metal/Household Appliances","Illegal Dumping Pickup","Electronic Waste","Dead Animal Removal")
    
    
    LAMap <- qmap("Los Angeles", color = "bw",zoom=10)
    
    type=""
    for(i in input$Items){
      if(i==1){
        type=c(type,ranksix[1])}
      if(i==2){
        type=c(type,ranksix[2])  }
      if(i==3){
        type=c(type,ranksix[3])  }
      if(i==4){
        type=c(type,ranksix[4])  }
      if(i==5){
        type=c(type,ranksix[5])  }
      if(i==6){
        type=c(type,ranksix[6])  }
      
    }
    data1=filter(data,RequestType %in% type )
    
    LAMap +
      geom_point(data = data1,
                 aes(x = Longitude, y = Latitude, color = RequestType), size =1.5,alpha=0.3)
    
    
  })
}


#Launch the app
shinyApp(ui= ui, server = server)
