#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Learn to Fly - Group 6 - CS424 Spring 2017
# Inspired by the sample R +  Shiny example for CS 424 Spring 2018 UIC - Andy Johnson
# www.evl.uic.edu/aej/424
#test
# Libraries to include
# library(car)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(plotly)
library(reshape2)
library(plyr)
library(data.table)
library(scales)
library(gridExtra)
##  "13930","Chicago, IL: Chicago O'Hare International"   
### "13232","Chicago, IL: Chicago Midway International"

#Keep all data files in the 'Data' folder!!

#Load Lookup Tables
print("Reading holiday tables")
holidays = fread("Flight_Data/holidays.csv", header = T)[,.(V2,V3)]
setnames(holidays,c("holidays","date"))
holidays = data.frame(holidays)[2:11,]
holidays$date = as.Date(holidays$date, "%m-%d-%y")
carrier_lookup = read.csv("Flight_Data/L_CARRIER_HISTORY.csv_")
colnames(carrier_lookup) = c("Code", "carrier_name")
airport_lookup = read.csv("Flight_Data/L_AIRPORT_ID.csv")
colnames(airport_lookup) = c("Code", "airport_name")
ohare_rain = read.csv("Flight_Data/2017-ohare-rain.csv", sep = ",")

#Load Flight data
print("Reading data tables")
portdir = read.csv("Flight_Data/L_AIRPORT_ID.csv")
Jan=read.csv("Flight_Data/Jan.csv")
Feb=read.csv("Flight_Data/Feb.csv")
Mar=read.csv("Flight_Data/Mar.csv")
Apr=read.csv("Flight_Data/Apr.csv")
May=read.csv("Flight_Data/May.csv")
June=read.csv("Flight_Data/June.csv")
July=read.csv("Flight_Data/July.csv")
Aug=read.csv("Flight_Data/Aug.csv")
Sept=read.csv("Flight_Data/Sept.csv")
Oct=read.csv("Flight_Data/Oct.csv")
Nov=read.csv("Flight_Data/Nov.csv")
Dec=read.csv("Flight_Data/Dec.csv")

####This is done because our data does not have a "Month category"

Jan["Month"] <- rep(1,length(Jan[[1]]))


Feb["Month"] <- rep(2,length(Feb[[1]]))


Mar["Month"] <- rep(3,length(Mar[[1]]))

Apr["Month"] <- rep(4,length(Apr[[1]]))


May["Month"] <- rep(5,length(May[[1]]))


June["Month"] <- rep(6,length(June[[1]]))


July["Month"] <- rep(7,length(July[[1]]))


Aug["Month"] <- rep(8,length(Aug[[1]]))


Sept["Month"] <- rep(9,length(Sept[[1]]))


Oct["Month"] <- rep(10,length(Oct[[1]]))


Nov["Month"] <- rep(11,length(Nov[[1]]))


Dec["Month"] <- rep(12,length(Dec[[1]]))

#Merge flight data
Month=list(Jan,Feb,Mar,Apr,May,June,July,Aug,Sept,Oct,Nov,Dec)
rm(Jan,Feb,Mar,Apr,May,June,July,Aug,Sept,Oct,Nov,Dec)
#Add Month dataframe with data for the whole year
Month_df = rbindlist(Month,fill=TRUE)
Month_df$FL_DATE = as.Date(Month_df$FL_DATE) #diff
Monthnames=c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")
daynames=c("MON","TUES","WED","THURS","FRI","SAT","SU")

dep.hours=substr(Month_df$DEP_TIME,1,nchar(Month_df$DEP_TIME)-2);
dep.hours[nchar(dep.hours)==0]<-'24'

arr.hours=substr(Month_df$ARR_TIME,1,nchar(Month_df$ARR_TIME)-2);
arr.hours[nchar(arr.hours)==0]<-'24'

####Create a new column which specifies the hour more clearly. Used by heatmap
Month_df$DEP_TIME2=as.numeric(dep.hours)
Month_df$ARR_TIME2=as.numeric(arr.hours)

# Add lookup fields
Month_with_names = lapply(Month, function(x) merge(x, carrier_lookup, by.x = "CARRIER", by.y = "Code", incomparables = NA, all.x = TRUE))
colnames(airport_lookup) = c("Code", "origin_airport")
Month_with_names = lapply(Month_with_names, function(x) merge(x, airport_lookup, by.x = "ORIGIN_AIRPORT_ID", by.y = "Code", incomparables = NA, all.x = TRUE))
colnames(airport_lookup) = c("Code", "dest_airport")
Month_with_names = lapply(Month_with_names, function(x) merge(x, airport_lookup, by.x = "DEST_AIRPORT_ID", by.y = "Code", incomparables = NA, all.x = TRUE))

#-----------------------
# Grade A last two points
#

data = copy(Month_df)
setnames(data, tolower(names(data)))

data[,":="(
  fl_date = as.Date(fl_date, "%Y-%m-%d"),
  dummy = 1)]

data[,":="(
  month_name = month(fl_date))]

busy_days = data[,.(daily_flight_count = length(month_name)), by = "fl_date"][order(-daily_flight_count)]

#holidays= head(busy_days[,.(holidays = as.character(daily_flight_count), date = fl_date)],10)
extra_cancellations = data[,.(
  cancellations = sum(cancelled, na.rm = T),
  flight_count = length(cancelled)),
  by = c("fl_date","origin_city_name")]

extra_cancellations[,":="(
  perc_cancellations = cancellations/ flight_count)
  ]

heavyCancellations = extra_cancellations[cancellations>0 & perc_cancellations>0.3][order(-cancellations)][]

specialDays = list(
  "Heavy Cancellations" = heavyCancellations[, .(fl_date, flight_count, cancellations, perc_cancellations = paste(round(perc_cancellations*100,2),"%", sep = ""))],
  "Holidays" = data.table(holidays)[, .(date, holidays)],
  "Busy Days" = busy_days
)
data[, ":="(origin_state= substr(origin_city_name, nchar(as.character(origin_city_name))-1,100),
            dest_state = substr(dest_city_name, nchar(as.character(dest_city_name))-1,100))]


depCount = data[,.(departure_count = sum(dummy)), by = "origin_state"]
depCount[,perc_departures := departure_count/sum(departure_count)]
arrivalCount = data[,.(arrival_count = sum(dummy)), by = "dest_state"]
arrivalCount[,perc_arrivals := arrival_count/ sum(arrival_count)]

allTakeOffs = merge(depCount[,.(state = origin_state, departure_count, 
                                perc_departures = paste(round(perc_departures*100,2),"%", sep = ""))], arrivalCount[,.(state = dest_state, arrival_count, perc_arrivals = paste(round(perc_arrivals*100,2), "%", sep = ""))], by = "state", all = T)

days=c(1,2,3,4,5,6,7) #diff
names(days)=c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")



# assume all of the tsv files in this directory are data of the same kind that I want to visualize

choices_airport=unique(Month_df$ORIGIN_CITY_NAME)
choices_airport=choices_airport[order(choices_airport)]
choices_day=names(days)
choices_delay=c("NAS DELAY","WEATHER DELAY","CARRIER DELAY","SECURITY DELAY","LATE AIRCRAFT DELAY")
choices_fl_num=unique(Month_df$FL_NUM)
choices_fl_num=choices_fl_num[order(choices_fl_num)]


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Sp 18 Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "item0"),

      
      menuItem("Chicago Flights",tabName = "menu_1",
         selectInput("Airport", "Airport", c("Chicago O'Hare", "Chicago Midway","Both")),
         selectInput("month", "Month", c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")),
         selectInput("timeframe", "Timeframe", c("1-24","AM-PM")),#menuItem("Inputs", tabName = "item1"), menuItem(tabname="Chicago Flights",
         
          menuSubItem("Overall Flights", tabName = "item2"),
          menuSubItem("Arrivals",tabName = "item3a"),
          menuSubItem("Departures",tabName = "item3b"),
          menuSubItem("Weekly",tabName = "item3c"),
          menuSubItem("Delays",tabName = "item3d"),
          menuSubItem("Rain Cancellations",tabName = "item11")
      ),
      menuItem("USA Domestic Flights",tabName = "menu_2",
          menuSubItem("Arrivals/Departures",tabName = "item3"),
       #   menuSubItem("Top 15 Destinations", tabName = "item4"),
          menuSubItem("Landing/TakeOffs",tabName = "item5"),
          menuSubItem("Special Dates",tabName = "item6"),
          menuSubItem("Delays",tabName = "delayss"),
          menuSubItem("Flight Metrics",tabName = "item7"),
      #    menuSubItem("Outliers",tabName = "item8"),
          menuSubItem("Monthly Heat Maps", tabName = "item9"),
          menuSubItem("Weekly Heat Maps",tabName = "item10")
        )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "menu_1", 
              fluidRow(
                h1("Homepage 1")
              )
      ),
      tabItem(tabName = "menu_2", 
              fluidRow(
                h1("Homepage 2")
              )
      ),
      tabItem(tabName = "item0",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset0", 
                       tabPanel("Information",
                                htmlOutput("info"))
                )
              )
      ),
      tabItem(tabName = "item1",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset1", 
                       #Ask the user to give inputs about the Airport, Month, and timeframe
                       tabPanel("Inputs", 
                                #selectInput("Airport", "Airport", c("Chicago O'Hare", "Chicago Midway","Both")),
                                selectInput("month", "Month", c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")),
                                selectInput("timeframe", "timeframe", c("1-24","AM-PM")))
                )
              )
      ),
      tabItem(tabName = "item2",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset2", 
                       #selectInput("Airport", "Airport", c("Chicago O'Hare", "Chicago Midway","Both")),
                       tabPanel("Flight Plot",box( title = "Flights By Airline", solidHeader = TRUE, status = "primary", width = 12, plotOutput("AirlineFlightPlot",height="800px")) ),
                       tabPanel("Flight Table", box(style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("AirlineFlightTable"))  ),
                       #Part 2-b 
                       tabPanel("Hourly Flights", box(title = "Airline Hourly Flights", solidHeader = TRUE, status = "primary", width = 12, plotOutput("HourlyFlights",height="800px"))  ),
                       tabPanel("Hourly Table", box(style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Airline Hourly Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("HourlyTable"))  )
                )
              )
      ),
      #################################PART B
      tabItem(tabName = "item3",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3", 
                       tabPanel("2017 Overall",box( title = "2017 Overall Arrivals and Departures By Hour", solidHeader = TRUE, status = "primary", width = 12, plotOutput("arrival_departure_times",height="750px")) ),
                       tabPanel("2017 Overall Arrivals",box( title = "2017 Overall Arrivals", solidHeader = TRUE, status = "primary", width = 12, plotOutput("arrival_departure_2017",height="750px")) )
                )
              )
      ),
      #################################Part 2-e
      tabItem(tabName = "item3a",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3a", 
                       tabPanel("Arrival Flights",box( title = "Arrival Flights", solidHeader = TRUE, status = "primary", width = 12, plotOutput("ArrivalFlightsPlot",height="800px")) ),
                       tabPanel("Arrival Flights Table", box(style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Arrival Flights Table", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("ArrivalFlightsTable"))  )
                       
                )
              )
      ),
      
      #Part 2-e
      tabItem(tabName = "item3b",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3b", 
                       tabPanel("Depart Flights",box( title = "Depart Flights", solidHeader = TRUE, status = "primary", width = 12, plotOutput("DepartFlightsPlot", height="800px")) ),
                       tabPanel("Depart Flights Table", box(style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Depart Flights Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("DepartFlightsTable"))  )
                       
                )
              )
      ),
      #Part 2-c
      tabItem(tabName = "item3c",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3c", 
                       tabPanel("Weekly Flights",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 12, plotOutput("WeeklyFlightsPlot",height="750px")) ),
                       tabPanel("Weekly Flights Table",box( style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Weekly Flights Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("WeeklyFlightsTable",height="750px")) )
                       
                )
              )
      ),
      
      #Part 2-d
      
      tabItem(tabName = "item3d",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3d", 
                       tabPanel("Arrival Delays",box( title = "Arrival Delays", solidHeader = TRUE, status = "primary", width = 12, plotOutput("ArrivalDelays",height="800px")) ),
                       tabPanel("Arrival Delay Table",box( style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Arrival Delay Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("ArrivalDelayTable",height="750px")) ),
                       tabPanel("Depart Delays",box( title = "Depart Delays", solidHeader = TRUE, status = "primary", width = 12, plotOutput("DepartDelays",height="800px")) ),
                       tabPanel("Depart Delay Table",box( style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",title = "Depart Delay Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("DepartDelayTable",height="750px")) )
                )
              )
      ),
 
      
      #################################PART A BEGINS HERE
      tabItem(tabName = "item5",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset5", 
                       tabPanel("State Info",
                                #selectInput("State", "State", c("AK","AL","AR","AZ","CA","CO","CT","DC","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")),
                                box(style = "font-family:Arial, Helvetica, sans-serif;font-size:24px",title = "Flight Landing and Take-0ff Info", solidHeader = TRUE, status = "primary", width = 10,dataTableOutput("takeOffs",height="75px")))
                )
              )
      ),
      tabItem(tabName = "item6",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset6", 
                       tabPanel("Special Dates",
                                selectInput("dateType", "Which dates would you like to see?", names(specialDays)),
                                box( style = "font-family:Arial, Helvetica, sans-serif;font-size:24px",title = "", solidHeader = TRUE, status = "primary", width = 12,dataTableOutput("special_days",height="75px"))
                       )
                )
              )
      ),
      
      #################################PART GRAD BEGINS HERE
      tabItem(tabName = "item7",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset7", 
                       tabPanel("Distance Metrics",
                                sliderInput("range", "Flight Distance:", min = 0, max = 7000,value = c(0,7000)),
                                selectInput("units", "Units", c("miles","kilometers")),
                                
                                numericInput("binwidth", "Bin Width:", 100, min = 50, max = 500),
                                tabPanel("Flights by Distance",box( title = "Flights by Distance", 
                                                                    solidHeader = TRUE, status = "primary", width = 10, 
                                                                    plotOutput("distance_range_plot",height="750px")))),
                       tabPanel("Time Metrics",
                                sliderInput("time_range", "Flight Time (minutes):", min = 0, max = 600, value = c(0,600)),
                                
                                numericInput("binwidth2", "Bin Width:", 10, min = 4, max = 200),
                                box( title = "Flights by Air Time", solidHeader = TRUE, status = "primary", width = 10, 
                                     plotOutput("time_range_plot",height="750px")))
                       
                       
                )
              )
      ),
      #################################Part A begins here
      
      tabItem(tabName = "item8",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset8", 
                       tabPanel("Airport Information",
                                selectInput("Select_Airport", "Select Airport", choices_airport),
                                box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("Lauderdale_airport",height="1000px")) ),
                       tabPanel("A day of the week",
                                selectInput("Select_Day_of_the_Week", "Select Day of the Week", choices_day),
                                box( style = "font-family:Arial, Helvetica, sans-serif;font-size:30px",footer="In the total delay plot the range is from 0-1250 minutes delay. The darker the shade the more is the delay. The size of the plot also increase with the increase in the delay",title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("one_day_of_week",height="1000px")) ),
                       tabPanel("Flight Information",selectInput("Flight_No", "Select Flight No", choices_fl_num,selected="40"),box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("airline_200",height="1000px")) ),
                       
                       tabPanel("One day of the year",dateInput("date", "Date:", min="2017-01-01",max="2017-12-31", format = "yyyy-mm-dd",value="2017-01-01"),box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("one_day",height="750px")) )
                )
              )
      ),
      
      ######GRADUATE HEAT MAPS
      tabItem(tabName = "item9",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset2", 
                       tabPanel("Monthly Heat Maps",box( title = "Monthly Flights", solidHeader = TRUE, status = "primary", width = 12, plotOutput("MonthlyHeatMap",height="900px")) )
                )
              )
      ),
      
      tabItem(tabName = "item10",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3", 
                       tabPanel("Weekly Heat Maps",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("WeeklyHeatMap",height="900px")) )
                )
              )
      ),
      tabItem(tabName = "item11",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset4", 
                       tabPanel("Rain Cancellations",box( title = "", solidHeader = TRUE, status = "primary", width = 10, plotOutput("OhareRain",height="900px")) )
                )
              )
      ),
      tabItem(tabName = "delayss",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id="delaysss",
                       tabPanel("Delay Causes",box( title = "Delay Causes", solidHeader = TRUE, status = "primary", width = 10, plotOutput("delay_Plot",height="750px")) )
                )
              )
      )
      
    )
  )
)


server <- function(input, output) {
  output$info <- renderUI({
    HTML('<p><b>Authors:</b>  Shoaib Khan, Pedro Borges, Megan Hauck, and Namaswi Chandarana</p>
<p>This is an app to explore thousands of flights in Illinois. It allows users to compare number of flights, departures, arrivals and delays based on their distances, cities, airlines, and time</p>
<p>The application was made for UICs Spring 2018 CS 424, Visualization and Visual Analytics, and Professor Andy Johnson.</p>
         <p><b>Required libaries</b>: shiny, ggplot2, shinydashboard</p>
          <p>Run time may be slow, as the app was meant to be run on the EVL server</p>
         <a href=,"https://www.youtube.com/watch?v=QOsSvCdj2_w",">Video</a>
         <p> <b>Data Source:</b>TBAhttps://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time</p>          <p> <b>Original app Source code:</b> https://github.com/sadupk/learning-to-fly</p> 
        <p>Some changes have been made to the app by Shoaib Khan since the original</p>

         <p> <b>Current version Source code:</b> https://github.com/skhan230/Flight-app/</p> 
')
        
    
      })
  
  theme_set(theme_grey(base_size = 37)) 
  
  #Helper functions delcared here
  getValue<-function(timeValue){
    
    if(timeValue<100 & timeValue>=0){
      new_min =  0
      new_max =  99
      old_min =  0
      old_max =  59
    }
    else if(timeValue<200 & timeValue>=100){
      new_min = 100
      new_max = 199
      old_min =100
      old_max =  159
    }
    else if(timeValue<300 & timeValue>=200){
      new_min = 200
      new_max = 299
      old_min =200
      old_max =  259
    }
    else if(timeValue<400 & timeValue>=300){
      new_min = 300
      new_max = 399
      old_min =300
      old_max =  359
    }
    else if(timeValue<500 & timeValue>=400){
      new_min = 400
      new_max = 499
      old_min =400
      old_max =  459
    }
    else if(timeValue<600 & timeValue>=500){
      new_min = 500
      new_max = 599
      old_min =500
      old_max =  559
    }
    else if(timeValue<700 & timeValue>=600){
      new_min = 600
      new_max = 699
      old_min = 600
      old_max = 659
    }
    else if(timeValue<800 & timeValue>=700){
      new_min = 700
      new_max = 799
      old_min = 700
      old_max = 759
    }
    else if(timeValue<900 & timeValue>=800){
      new_min = 800
      new_max = 899
      old_min = 800
      old_max = 859
    }
    else if(timeValue<1000 & timeValue>=900){
      new_min = 900
      new_max = 999
      old_min = 900
      old_max = 959
    }
    else if(timeValue<1100 & timeValue>=1000){
      new_min = 1000
      new_max = 1099
      old_min = 1000
      old_max = 1059
    }
    else if(timeValue<1200 & timeValue>=1100){
      new_min = 1100
      new_max = 1199
      old_min = 1100
      old_max = 1159
    }
    else if(timeValue<1300 & timeValue>=1200){
      new_min = 1200
      new_max = 1299
      old_min = 1200
      old_max = 1259
    }
    else if(timeValue<1400 & timeValue>=1300){
      new_min = 1300
      new_max = 1399
      old_min = 1300
      old_max = 1359
    }
    else if(timeValue<1500 & timeValue>=1400){
      new_min = 1400
      new_max = 1499
      old_min = 1400
      old_max = 1459
    }
    else if(timeValue<1600 & timeValue>=1500){
      new_min = 1500
      new_max = 1599
      old_min = 1500
      old_max = 1559
    }
    else if(timeValue<1700 & timeValue>=1600){
      new_min = 1600
      new_max = 1699
      old_min = 1600
      old_max = 1659
    }
    else if(timeValue<1800 & timeValue>=1700){
      new_min = 1700
      new_max = 1799
      old_min = 1700
      old_max = 1759
    }
    else if(timeValue<1900 & timeValue>=1800){
      new_min = 1800
      new_max = 1899
      old_min = 1800
      old_max = 1859
    }
    else if(timeValue<2000 & timeValue>=1900){
      new_min = 1900
      new_max = 1999
      old_min = 1900
      old_max = 1959
    }
    else if(timeValue<2100 & timeValue>=2000){
      new_min = 2000
      new_max = 2099
      old_min = 2000
      old_max = 2059
    }
    else if(timeValue<2200 & timeValue>=2100){
      new_min = 2100
      new_max = 2199
      old_min = 2100
      old_max = 2159
    }
    else if(timeValue<2300 & timeValue>=2200){
      new_min = 2200
      new_max = 2299
      old_min = 2200
      old_max = 2259
    }
    else if(timeValue<2300 & timeValue>=2200){
      new_min = 2200
      new_max = 2299
      old_min = 2200
      old_max = 2259
    }
    else if(timeValue<2400 & timeValue>=2300){
      new_min = 2300
      new_max = 2399
      old_min = 2300
      old_max = 2359
    }
    
    
    return ((new_max - new_min) / (old_max - old_min) * (timeValue - old_min) + new_min)
  }
  
  getarrivals<- function(Airline)
  {
    
    arrtimes=Airline$ARR_TIME
    arrtimes <- as.character(unlist(Airline$ARR_TIME))
    arrivals=list()
    
    
    arr=arrtimes[nchar(arrtimes)<3 & !is.na(arrtimes)]
    arrivals[1]=length(arr)
    
    for (hour in 1:9)
    {
      h=toString(hour)
      
      arr=arrtimes[startsWith(arrtimes,h) & nchar(arrtimes)==3 & !is.na(arrtimes)]
      arrivals[hour+1]=length(arr)
      
    }
    
    for (hour in 10:24)
    {
      h=toString(hour)
      
      arr=arrtimes[startsWith(arrtimes,h) & nchar(arrtimes)==4 & !is.na(arrtimes)]
      arrivals[hour+1]=length(arr)
      
    }
    
    arrivals=unlist(arrivals)
    
    arrivals[1]=arrivals[1]+arrivals[25]
    
    
    arrivals=arrivals[c(1:24)]
    
    return(arrivals)
  }
  
  
  getdeps<- function(Airline)
  {
    
    dep=Airline$DEP_TIME
    deptimes <- as.character(unlist(Airline$DEP_TIME))
    departures=list()
    
    dep=deptimes[nchar(deptimes)<3 & !is.na(deptimes)]
    departures[1]=length(dep)
    
    for (hour in 1:9)
    {
      h=toString(hour)
      
      dep=deptimes[startsWith(deptimes,h) & nchar(deptimes)==3 & !is.na(deptimes)]
      departures[hour+1]=length(dep)
    }
    
    for (hour in 10:24)
    {
      h=toString(hour)
      
      dep=deptimes[startsWith(deptimes,h) & nchar(deptimes)==4 & !is.na(deptimes)]
      departures[hour+1]=length(dep)
    }
    
    departures=unlist(departures)
    departures[1]=departures[1]+departures[25]
    departures=departures[c(1:24)]
    
    return(departures)
  }
  
  midwaycolors=c("coral","coral4")
  oharecolors=c("cyan4","cyan")
  lowcol="White";
  highcol="Red";
  border="Black";
  base_size=22;
  ####TOTAL FLIGHTS
  distributedweek=data.frame(table(Month_df$DEP_TIME2,Month_df$DAY_OF_WEEK))
  names(distributedweek)=c("Hour","Week","Freq");
  wp1<-ggplot(distributedweek, aes(Week, Hour)) + geom_tile(aes(fill = Freq),    colour = border)+
    scale_x_discrete(breaks=c(1:7),labels=daynames) + 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    geom_tile(aes(fill = Freq),    colour = border)+
    scale_fill_gradient(low = lowcol,   high = highcol)+
    ggtitle("# Flights")
  
  distributedmt=data.frame(table(Month_df$DEP_TIME2,Month_df$Month))
  names(distributedmt)=c("Hour","Month","Freq");
  mp1<-ggplot(distributedmt, aes(Month, Hour)) + geom_tile(aes(fill = Freq),    colour = border) +  
    scale_x_discrete(breaks=c(1:12),labels=Monthnames) + 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    geom_tile(aes(fill = Freq),    colour = border)+
    scale_fill_gradient(low = lowcol,   high = highcol)+
    ggtitle("# Flights")
  
  
  ##Cancel
  missingmonth=c();
  for (i in 1:12)
  {
    m=length(Month_df[Month_df$Month==i & Month_df$CANCELLATION_CODE=='B' & is.na(Month_df$DEP_TIME2),][[1]]);
    
    Fr=distributedmt[distributedmt$Month==i,]$Freq
    
    mtprob=Fr/sum(Fr)
    r=rep(m,24); 
    
    mnext=mtprob*r
    missingmonth=c(missingmonth,mnext)
  }
  
  missingweek=c();
  
  for (i in 1:7)
  {
    m=length(Month_df[Month_df$DAY_OF_WEEK==i & Month_df$CANCELLATION_CODE=='B' & is.na(Month_df$DEP_TIME2),][[1]]);
    
    Fr=distributedweek[distributedweek$Week==i,]$Freq
    mtprob=Fr/sum(Fr)
    
    r=rep(m,24); 
    
    mnext=mtprob*r
    missingweek=c(missingweek,mnext)
#    print(i)
  }
  
  
  
  
  CANCWK=data.frame(table(Month_df$DEP_TIME2,Month_df$DAY_OF_WEEK,Month_df$CANCELLATION_CODE))
  names(CANCWK)=c("Hour","Week","Problem","Freq");
  CANCWK=CANCWK[CANCWK$Problem=='B',]
  CANCWK$Freq=CANCWK$Freq+missingweek;
  wp2<-ggplot(CANCWK, aes(Week, Hour)) + geom_tile(aes(fill =Freq),    colour = border) +  
    scale_x_discrete(breaks=c(1:7),labels=daynames) + 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    geom_tile(aes(fill = Freq),    colour = border)+
    scale_fill_gradient(low = lowcol,   high = highcol)+
    ggtitle("Cancelled Flights")
  
  
  CANCMTH=data.frame(table(Month_df$DEP_TIME2,Month_df$Month,Month_df$CANCELLATION_CODE))
  names(CANCMTH)=c("Hour","Month","Problem","Freq");
  CANCMTH=CANCMTH[CANCMTH$Problem=='B',]
  CANCMTH$Freq=CANCMTH$Freq+missingmonth;
  mp2<-ggplot(CANCMTH, aes(Month, Hour)) + geom_tile(aes(fill =Freq),    colour = border) +  
    scale_x_discrete(breaks=c(1:12),labels=Monthnames) + 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    geom_tile(aes(fill = Freq),    colour = border)+
    scale_fill_gradient(low = lowcol,   high = highcol)+
    ggtitle("Cancelled Flights")
  
  print("Half way Loaded")
  
  
  ##Delayed   MAY STILL NEED TO CHECK THIS
  Month_df$WTF_DELAY=ceiling(as.numeric(Month_df$WEATHER_DELAY)/(as.numeric(Month_df$WEATHER_DELAY)+0.001))
  
  DELWK=data.frame(table(Month_df$DEP_TIME2,Month_df$DAY_OF_WEEK,Month_df$WTF_DELAY))
  names(DELWK)=c("Hour","Week","Problem","Freq");
  DELWK=DELWK[DELWK$Problem=='1',]
  wp3<-ggplot(DELWK, aes(Week, Hour)) + geom_tile(aes(fill =Freq),    colour = border) +  
    scale_x_discrete(breaks=c(1:7),labels=daynames) + 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    geom_tile(aes(fill = Freq),    colour = border)+
    scale_fill_gradient(low = lowcol,   high = highcol)+
    ggtitle("Delayed Flights")
  
  
  DELMTH=data.frame(table(Month_df$DEP_TIME2,Month_df$Month,Month_df$WTF_DELAY))
  names(DELMTH)=c("Hour","Month","Problem","Freq");
  DELMTH=DELMTH[DELMTH$Problem=='1',]
  mp3<-ggplot(DELMTH, aes(Month, Hour)) + geom_tile(aes(fill =Freq),    colour = border) +  
    scale_x_discrete(breaks=c(1:12),labels=Monthnames) + 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    geom_tile(aes(fill = Freq),    colour = border)+
    scale_fill_gradient(low = lowcol,   high = highcol)+
    ggtitle("Delayed Flights")
  
  
  
  ##Delayedtime
  distributedmonth=aggregate(WEATHER_DELAY ~ Month + DEP_TIME2, data = Month_df, sum, na.rm = TRUE)
  names(distributedmonth)=c("Month","Hour","Minutes");
  mp4<-ggplot(distributedmonth, aes(Month, Hour)) + geom_tile(aes(fill = Minutes),    colour = border) +
    scale_fill_gradient(low = lowcol,   high = highcol)+ 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    labs(x = "Month",y = "Hour") +
    scale_x_discrete(limits=Monthnames,expand = c(0, 0)) +
    scale_y_discrete(limits=0:24,expand = c(0, 0)) +
    ggtitle("Delayed Time")
  
  
  distributedweek=aggregate(WEATHER_DELAY ~ DAY_OF_WEEK+ DEP_TIME2, data = Month_df, sum, na.rm = TRUE)
  names(distributedweek)=c("Week","Hour","Minutes");
  wp4<-ggplot(distributedweek, aes(Week, Hour)) + geom_tile(aes(fill = Minutes),    colour = border) +  
    scale_fill_gradient(low = lowcol,   high = highcol)+ 
    theme_grey(base_size = base_size) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0))+
    labs(x = "Day",y = "Hour") +
    scale_x_discrete(limits=daynames,expand = c(0, 0)) +
    scale_y_discrete(limits=0:24,expand = c(0, 0))+
    ggtitle("Delayed Time") 
  print("Almost Fully loaded")
  
  ############################################Part 2-a
  output$AirlineFlightPlot <- renderPlot({   ###  VX airlines went out of business in 2003  :)  
    
    
    if(input$Airport=="Both")
    {
      ports=c("13232", "13930")   #####May want to reconsider this
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      airportsdepart=data.frame(table(departures$CARRIER))
      airportsarrival=data.frame(table(arrivals$CARRIER))
      airporttimes=data.frame(ID=airportsdepart[[1]],departing_Midway=airportsdepart[[2]],arrivals_Midway=airportsarrival[[2]])
      melted1=melt(airporttimes, id="ID")
      
      
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
      airportsdepart=data.frame(table(departures$CARRIER))
      airportsarrival=data.frame(table(arrivals$CARRIER))
      airporttimes=data.frame(ID=airportsdepart[[1]],departing_Ohare=airportsdepart[[2]],arrivals_Ohare=airportsarrival[[2]])
      melted2=melt(airporttimes, id="ID")
      
      
      melted=rbind(melted1,melted2) 
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable), position = "dodge")+
        labs(x = "Airline",y = "# Flights")+ 
        scale_fill_manual("legend", values = c("departing_Midway" = midwaycolors[1], "arrivals_Midway" = midwaycolors[2], "departing_Ohare" = oharecolors[1], "arrivals_Ohare" = oharecolors[2]))
    }  
    
    else
    {
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]
      airportsdepart=data.frame(table(departures$CARRIER))
      airportsarrival=data.frame(table(arrivals$CARRIER))
      airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])
      melted=melt(airporttimes, id="ID")
      
      colors=midwaycolors 
      
      if(input$Airport=="Chicago O'Hare")
      {
        colors=oharecolors 
      }
      
      
      
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable), position = "dodge") +labs(x = "Airline",y = "# Flights")+ 
        scale_fill_manual("", values = c("departing" = colors[1], "arrivals" = colors[2]))
      
    }
  })
  
  ############################################Part 2-a
  output$AirlineFlightTable <- DT::renderDataTable(
    
    
    
    DT::datatable({ 
      
      
      if(input$Airport=="Both")
      {
        ports=c("13232", "13930")
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
        airportsdepart1=data.frame(table(departures$CARRIER))
        airportsarrival1=data.frame(table(arrivals$CARRIER))
        
        
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
        airportsdepart2=data.frame(table(departures$CARRIER))
        airportsarrival2=data.frame(table(arrivals$CARRIER))
        
        airporttimes=data.frame(
          ID=airportsdepart1[[1]],
          departing_Midway=airportsdepart1[[2]],
          arrivals_Midway=airportsarrival1[[2]],
          departing_Ohare=airportsdepart2[[2]],
          arrivals_Ohare=airportsarrival2[[2]]
        )
      }  
      
      else
      {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]
        airportsdepart=data.frame(table(departures$CARRIER))
        airportsarrival=data.frame(table(arrivals$CARRIER))
        airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])
        
      }
      airporttimes
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )
  
  
  
  ############################################Part 2-b
  output$HourlyTable <- DT::renderDataTable(
    
    
    
    DT::datatable({ 
      
      
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      if (input$Airport=="Both")
      {
        ports=c("13930","13232")
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
        departures=getdeps(Airline)
        Airline=Month[Month$DEST_AIRPORT_ID==ports[1],] ###POSTCHANGE
        arrivals=getarrivals(Airline)
        times=c(0:23)
        #TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
        departures2=getdeps(Airline)
        Airline=Month[Month$DEST_AIRPORT_ID==ports[2],]  ###POSTCHANGE
        arrivals2=getarrivals(Airline)
        times=c(0:23)
        if(input$timeframe=="AM-PM")
        {
          times=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          
        }
        else
        {
          times=c(0:23)
        }
        TravelTimes=data.frame(Times=times,Arrivals_Midway=arrivals2,  Departures_Midway=departures2,Arrivals_Ohare=arrivals,  Departures_ohare=departures)
        
        
      }  
      else
      {
        # Airline=Month[Month$ORIGIN_AIRPORT_ID==input$Airport,] ##### PROBLEM
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        departures=getdeps(Airline)
        Airline=Month[Month$DEST_AIRPORT_ID==Airportname,]    ###POSTCHANGE
        arrivals=getarrivals(Airline)
        
        times=c(0:23)
        ####LASTCHANGE
        if(input$timeframe=="AM-PM")
        {
          times=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          
        }
        else
        {
          times=c(0:23)
        }
        TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        
      } 
      TravelTimes
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )
  
  
  ############################################Part 2-b
  output$HourlyFlights<- 
    
    renderPlot({
      
      
      Month=Month[Monthnames ==input$month]
      
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      
      if (input$Airport=="Both")
      {
        ports=c("13930","13232")
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
        departures=getdeps(Airline)
        Airline=Month[Month$DEST_AIRPORT_ID==ports[1],]    ##POST CHANGE
        arrivals=getarrivals(Airline)
        times=c(0:23)
        TravelTimes=data.frame(Times=times,Arrivals_Ohare=arrivals,  Departures_Ohare=departures)
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
        departures=getdeps(Airline)
        Airline=Month[Month$DEST_AIRPORT_ID==ports[2],]    ###POSTCHANGE
        arrivals=getarrivals(Airline)
        times=c(0:23)
        TravelTimes2=data.frame(Times=times,Arrivals_Midway=arrivals,  Departures_Midway=departures)
        
        if(input$timeframe=="1-24")
        {
          timeframe=c(0:23)
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") + 
            scale_x_discrete( name ="hour",breaks=(0:23),limits=timeframe)+
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals_Ohare",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures_Ohare",group=1))+
            geom_line(size=2,aes(y = TravelTimes[[2]], colour = "Arrivals_Ohare",group=1))+
            geom_line(size=2,aes(y = TravelTimes[[3]], colour = "Departures_Ohare",group=1))  +
            geom_point(aes(y = TravelTimes2[[2]], colour = "Departures_Midway",group=1))+
            geom_point(aes(y = TravelTimes2[[3]], colour = "Departures_Midway",group=1))+
            geom_line(size=2,aes(y = TravelTimes2[[2]], colour = "Arrivals_Midway",group=1))+
            geom_line(size=2,aes(y = TravelTimes2[[3]], colour = "Departures_Midway",group=1))+ 
            theme(axis.text.x=element_text(angle = 90, hjust = 0))+ 
            scale_color_manual("legend", values = c("Departures_Midway" =  midwaycolors[1], "Arrivals_Midway" = midwaycolors[2], "Departures_Ohare" = oharecolors[1], "Arrivals_Ohare" = oharecolors[2]),
                               breaks=c("Departures_Midway", "Arrivals_Midway", "Departures_Ohare","Arrivals_Ohare"),
                               labels=c("Departures Midway", "Arrivals Midway", "Departures Ohare","Arrivals Ohare")
            )
          
        }
        
        
        else
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          TravelTimes$Times <- factor(times, levels = times)  ###POSTCHANGE
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") +
            scale_x_discrete( name ="hour",labels=timeframe)+  ####POSTCHANGE
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals_Ohare",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures_Ohare",group=1))+
            geom_line(size=2,aes(y = TravelTimes[[2]], colour = "Arrivals_Ohare",group=1))+
            geom_line(size=2,aes(y = TravelTimes[[3]], colour = "Departures_Ohare",group=1))  +
            geom_point(size=2,aes(y = TravelTimes2[[2]], colour = "Departures_Midway",group=1))+
            geom_point(aes(y = TravelTimes2[[3]], colour = "Departures_Midway",group=1))+
            geom_line(size=2,aes(y = TravelTimes2[[2]], colour = "Arrivals_Midway",group=1))+
            geom_line(size=2,aes(y = TravelTimes2[[3]], colour = "Departures_Midway",group=1))+ 
            theme(axis.text.x=element_text(angle = 90, hjust = 0))+ 
            scale_color_manual("", values = c("Departures_Midway" =  midwaycolors[1], "Arrivals_Midway" = midwaycolors[2], "Departures_Ohare" = oharecolors[1], "Arrivals_Ohare" = oharecolors[2]))
          
        }
        
      }  
      else
      {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        departures=getdeps(Airline)
        Airline=Month[Month$DEST_AIRPORT_ID==Airportname,] ###POSTCHANGE
        arrivals=getarrivals(Airline)
        times=c(0:23)
        TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        colors=midwaycolors 
        
        if(input$Airport=="Chicago O'Hare")
        {
          colors=oharecolors 
        }
        
        
        
        
        
        if(input$timeframe=="1-24")
        {
          
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") + 
            scale_x_discrete( name ="Hour",breaks=c(0:23),limits=times)+ ###lastchange
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1),size=2)+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1),size=2)+
            geom_line(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1),size=2)+
            geom_line(aes(y = TravelTimes[[3]], colour = "Departures",group=1),size=2)+
            theme(axis.text.x=element_text(angle = 90, hjust = 0))+ 
            scale_color_manual("", values = c("Departures" = colors[1], "Arrivals" = colors[2]))
        }
        
        
        
        else
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          TravelTimes$Times <- factor(times, levels = times)  ###POSTCHANGE
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") +
            scale_x_discrete( name ="Hour",labels=timeframe)+###POSTCHANGE
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
            geom_line(size=2,aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_line(size=2,aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
            theme(axis.text.x=element_text(angle = 90, hjust = 0))+ 
            scale_color_manual("", values = c("Departures" = colors[1], "Arrivals" = colors[2]))
          
          
        }
        
      } 
      
      
    })
  
  
  
  ###########################################Part 2-e
  output$ArrivalFlightsPlot <- renderPlot({
    if(input$Airport=="Both")
    {
      
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      ports=c("13232", "13930")
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      go_to=data.frame(table(departures$DEST_AIRPORT_ID))
      ind=order(go_to[[2]],decreasing = T)
      indtop=ind[1:15]
      go_top=go_to[indtop,]
      go_top=go_top[complete.cases(go_top),]
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      go_to=data.frame(table(departures$DEST_AIRPORT_ID))
      ind=order(go_to[[2]],decreasing = T)
      indtop=ind[1:15] 
      go_top2=go_to[indtop,]
      go_top2=go_top2[complete.cases(go_top2),]
      go_tos=merge(go_top,go_top2, by="Var1",all=TRUE)
      go_tos[is.na(go_tos)] = 0
      go_tos=data.frame(ID=go_tos[[1]],Midway=go_tos[[2]],Ohare=go_tos[[3]])
      melted=melt(go_tos, id='ID')
      
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable),position = "dodge")+ 
        theme(axis.text.x=element_text(angle = 90, hjust = 0))+ 
        scale_fill_manual("", values = c("Midway" = midwaycolors[1], "Ohare" = oharecolors[1]))+
        labs(x = "Airline",y = "# Flights")
      
    }
    ### Be a bit a wary of this else clause, code  inside it is okay though
    else{
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
      go_to=data.frame(table(departures$DEST_AIRPORT_ID))
      ind=order(go_to[[2]],decreasing = T)
      indtop=ind[1:15]
      go_top=go_to[indtop,]
      go_top=go_top[complete.cases(go_top),]
      
      
      
      
      if(input$Airport=="Chicago O'Hare")
      {
        colors=oharecolors[1]      
        ggplot(data=go_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge",fill=colors)+ 
          theme(axis.text.x=element_text(angle = 90, hjust = 0))+
          labs(x = "Airline",y = "# Flights")
      }
      
      else
      {
        colors=midwaycolors[1]      
        ggplot(data=go_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge",fill=colors)+ 
          theme(axis.text.x=element_text(angle = 90, hjust = 0))+
          labs(x = "Airline",y = "# Flights")
        
      }
      
      
      
    }
  })
  
  ###########################################Part 2-e
  output$ArrivalFlightsTable <- DT::renderDataTable(
    
    
    DT::datatable({
      if(input$Airport=="Both")    #####
      {
        
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        ports=c("13232", "13930")
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
        go_to=data.frame(table(departures$DEST_AIRPORT_ID))
        ind=order(go_to[[2]],decreasing = T)
        indtop=ind[1:15]
        go_top=go_to[indtop,]
        go_top=go_top[complete.cases(go_top),]
        
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
        go_to=data.frame(table(departures$DEST_AIRPORT_ID))
        ind=order(go_to[[2]],decreasing = T)
        indtop=ind[1:15]
        go_top2=go_to[indtop,]
        go_top2=go_top2[complete.cases(go_top2),]
        
        go_tos=merge(go_top,go_top2, by="Var1",all=TRUE)
        go_tos[is.na(go_tos)] = 0
        go_tos=data.frame(ID=go_tos[[1]],Midway=go_tos[[2]],Ohare=go_tos[[3]])
        
      }
      ### Be a bit a wary of this else clause, code  inside code is statistically okay though
      else{
        
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        ports=c("13232", "13930")
        departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,] #####
        go_to=data.frame(table(departures$DEST_AIRPORT_ID))
        ind=order(go_to[[2]],decreasing = T)
        indtop=ind[1:15]
        go_top=go_to[indtop,]
        go_top=go_top[complete.cases(go_top),]
        go_tos=go_top
        names(go_tos)=c("Airline","# Flights")
        
      }
      go_tos
    },  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) )
  )
  
  
  
  ###########################################Part 2-e
  output$DepartFlightsPlot <- renderPlot({
    
    if(input$Airport=="Both")
    {
      ports=c("13232", "13930")
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
      ind=order(come_from[[2]],decreasing = T)
      indtop=ind[1:15]
      come_top=come_from[indtop,]
      come_top=come_top[complete.cases(come_top),]
      
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
      come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
      ind=order(come_from[[2]],decreasing = T)
      indtop=ind[1:15]
      come_top2=come_from[indtop,]
      come_top2=come_top2[complete.cases(come_top2),]
      
      come_froms=merge(come_top,come_top2, by="Var1",all=TRUE)
      come_froms[is.na(come_froms)] = 0
      come_froms=data.frame(ID=come_froms[[1]],Midway=come_froms[[2]],ohare=come_froms[[3]])
      
      melted=melt(come_froms, id='ID')
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable),position = "dodge")+ 
        theme(axis.text.x=element_text(angle = 90, hjust = 0))+ 
        #scale_fill_manual("legend", values = c("Midway" = midwaycolors[1], "Ohare" = "red"))+   ######COLOR PROBLEM HERE
        labs(x = "Airline",y = "# Flights")+scale_fill_manual(values=c(midwaycolors[1], oharecolors[1]), 
                                                              name="",
                                                              breaks=c("Midway", "ohare"),
                                                              labels=c("Midway", "Ohare"))
      
      
      
      
    }
    
    else {
      
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,] 
      come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
      ind=order(come_from[[2]],decreasing = T)
      indtop=ind[1:15]
      come_top=come_from[indtop,]
      come_top=come_top[complete.cases(come_top),]
      
      
      if(input$Airport=="Chicago O'Hare")
      {
        colors=oharecolors[1]
        print(colors)     
        ggplot(data=come_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge",fill=oharecolors[1])+ 
          theme(axis.text.x=element_text(angle = 90, hjust = 0))+
          labs(x = "Airline",y = "# Flights")
      }
      
      else
      {
        colors=midwaycolors[1]  
        print(colors)   
        ggplot(data=come_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge",fill=midwaycolors[1])+ 
          theme(axis.text.x=element_text(angle = 90, hjust = 0))+
          labs(x = "Airline",y = "# Flights")
        
      }
      
      
      
      
      #      ggplot(data=come_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge")
      
    }
    
    
  })
  
  ###########################################Part 2-e
  output$DepartFlightsTable <- DT::renderDataTable(
    
    
    DT::datatable({ 
      if(input$Airport=="Both")
      {
        ports=c("13232", "13930")
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
        come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
        ind=order(come_from[[2]],decreasing = T)
        indtop=ind[1:15]
        come_top=come_from[indtop,]
        come_top=come_top[complete.cases(come_top),]
        
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
        come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
        ind=order(come_from[[2]],decreasing = T)
        indtop=ind[1:15]
        come_top2=come_from[indtop,]
        come_top2=come_top2[complete.cases(come_top2),]
        
        come_froms=merge(come_top,come_top2, by="Var1",all=TRUE)
        come_froms[is.na(come_froms)] = 0
        come_froms=data.frame(ID=come_froms[[1]],Midway=come_froms[[2]],ohare=come_froms[[3]])
        
        
      }
      
      else {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,] ###
        come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
        ind=order(come_from[[2]],decreasing = T)
        indtop=ind[1:15]
        come_top=come_from[indtop,]
        come_top=come_top[complete.cases(come_top),]
        come_froms=come_top
        names(come_froms)=c("Airline","# Flights")
        
      }
      come_froms
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )
  
  ###########################################Part 2-c
  output$WeeklyFlightsPlot <- renderPlot({
    #c
    if (input$Airport=="Both")
    {
      ports=c("13232", "13930")
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      
      arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
      dep_day1=data.frame(table(departures$DAY_OF_WEEK))
      
      
      arrivals2=Month[Month$DEST_AIRPORT_ID==ports[2],]
      departures2=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      
      arr_day2=data.frame(table(arrivals2$DAY_OF_WEEK))
      dep_day2=data.frame(table(departures2$DAY_OF_WEEK))
      
      daily_data=data.frame(ID=daynames,Midway_Arrival=arr_day1[[2]],Ohare_arrival=arr_day2[[2]],Midway_dep=dep_day1[[2]],Ohare_dep=dep_day2[[2]])
      melted=melt(daily_data, id='ID')
      melted$ID <- factor(c(1:7), levels = c(1:7))###POSTCHANGE
      
      ggplot(melted, aes(x=ID, y=value,  color=variable, group=variable))+ geom_line(size=2)+
        scale_x_discrete( name="Day",breaks=1:7,labels=names(days))+       
        theme(axis.text.x=element_text(angle = 90, hjust = 0))+
        scale_color_manual("", values = c("Midway_dep" = midwaycolors[1], "Midway_Arrival" = midwaycolors[2], "Ohare_dep" = oharecolors[1], "Ohare_arrival" = oharecolors[2]))
      
    }
    
    
    else
    {
      colors=midwaycolors 
      
      if(input$Airport=="Chicago O'Hare")
      {
        colors=oharecolors 
      }
      
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]   ####
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]  ###
      
      arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
      dep_day1=data.frame(table(departures$DAY_OF_WEEK))
      
      daily_data=data.frame(ID=arr_day1[[1]],arrivals=arr_day1[[2]],departures=dep_day1[[2]])
      melted=melt(daily_data, id='ID')
      ggplot(melted, aes(x=ID, y=value,  color=variable, group=variable))+ geom_line(size=2)+
        scale_x_discrete( name="Day",breaks=1:7,labels=names(days))+       
        scale_color_manual("", values = c("arrivals" = colors[1], "departures" = colors[2]))+
        theme(axis.text.x=element_text(angle = 90, hjust = 0))
      
      
    }  
    
  })
  
  
  ###########################################Part 2-c
  output$WeeklyFlightsTable <- DT::renderDataTable(
    
    
    DT::datatable({
      if (input$Airport=="Both")
      {
        ports=c("13232", "13930")
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
        
        arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
        dep_day1=data.frame(table(departures$DAY_OF_WEEK))
        
        
        arrivals2=Month[Month$DEST_AIRPORT_ID==ports[2],]
        departures2=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
        
        arr_day2=data.frame(table(arrivals2$DAY_OF_WEEK))
        dep_day2=data.frame(table(departures2$DAY_OF_WEEK))
        
        daily_data=data.frame(ID=daynames,Arrivals_Midway=arr_day1[[2]],Arrivals_Ohare=arr_day2[[2]],Departures_Miday=dep_day1[[2]],Departures_Ohare=dep_day2[[2]])
        
      }
      
      
      else
      {
        
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
        
        arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]   ####
        departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]  ###
        arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
        dep_day1=data.frame(table(departures$DAY_OF_WEEK))
        daily_data=data.frame(ID=daynames,Arrivals=arr_day1[[2]],Departures=dep_day1[[2]])
        
        
      }
      
      daily_data
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE)
    )
  )
  
  
  
  ###########################################Part 2-d
  output$ArrivalDelays <- renderPlot({
    Month=Month[Monthnames ==input$month]
    Month=Month[[1]]
    Month=Month[Month$CANCELLED==0,]
    
    
    
    
    if (input$Airport=="Both")
    {
      if(input$timeframe=="AM-PM")
      {
        timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
        
      }
      else
      {
        timeframe=c(0:23)
      }
      ports=c("13232", "13930")
      delaysdata=Month[Month$DEST_AIRPORT_ID==ports[1] & Month$ARR_DEL15==1 ,]###Postchange
      Airline=Month[Month$DEST_AIRPORT_ID==ports[1],]###Postchange
      delays=getarrivals(delaysdata)
      arrivals=getarrivals(Airline)
      flights=c(delays,(arrivals-delays))
      
      delaysdata=Month[Month$DEST_AIRPORT_ID==ports[2] & Month$ARR_DEL15==1 ,] ###Postchange
      Airline=Month[Month$DEST_AIRPORT_ID==ports[2],]###Postchange
      delays=getarrivals(delaysdata)
      arrivals=getarrivals(Airline)
      flights2=c(delays,(arrivals-delays))
      
      
      ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
      times=rep(c(0:23),2)
      t=rep("delays",24)
      d=rep("totals",24)
      coloring=c(d,t)
      TravelTimes=data.frame(Times=times,Midway=flights,Ohare=flights2)
      melted=melt(TravelTimes, id="Times")
      melted$Coloring=coloring
      ggplot(melted, aes(x=Times, y=value)) + geom_bar(stat="identity",colour="white",aes(fill=melted$Coloring))+ 
        scale_x_continuous( name="Hour",breaks=0:23,labels=timeframe)+           
        theme(axis.text.x=element_text(angle = 90, hjust = 0))+
        facet_grid(~ variable)+ scale_fill_discrete(name = "")
      
    }
    
    else
    {
      if(input$timeframe=="AM-PM")
      {
        timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
        
      }
      else
      {
        timeframe=c(0:23)
      }
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      delaysdata=Month[Month$DEST_AIRPORT_ID==Airportname & Month$ARR_DEL15==1 ,]###Postchange
      Airline=Month[Month$DEST_AIRPORT_ID==Airportname,]###Postchange
      delays=getarrivals(delaysdata)
      arrivals=getarrivals(Airline)
      times=rep(c(0:23),2)
      t=rep("delays",24)
      d=rep("totals",24)
      coloring=c(d,t)
      flights=c(delays,(arrivals-delays))
      
      TravelTimes=data.frame(Times=times,Flights=flights,Coloring=coloring)
      ggplot(TravelTimes, aes(x=Times, y=Flights)) + geom_bar(stat="identity",   colour="white",aes(fill=Coloring))+ 
        scale_x_continuous( name="Hour",breaks=0:23,labels=timeframe)+           
        theme(axis.text.x=element_text(angle = 90, hjust = 0))
    }
  })
  
  
  ###########################################Part 2-d
  output$DepartDelays <- renderPlot({
    Month=Month[Monthnames ==input$month]
    #Month=read.csv("Feb.csv")
    Month=Month[[1]]
    Month=Month[Month$CANCELLED==0,]
    
    
    if (input$Airport=="Both")
    {
      if(input$timeframe=="AM-PM")
      {
        timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
        
      }
      else
      {
        timeframe=c(0:23)
      }
      ports=c("13232", "13930")
      delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[1] & Month$DEP_DEL15==1 ,]
      Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      delays=getdeps(delaysdata)
      departs=getdeps(Airline)
      flights=c(delays,(departs-delays))
      
      delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[2] & Month$DEP_DEL15==1 ,]
      Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      delays=getdeps(delaysdata)
      departs=getdeps(Airline)
      flights2=c(delays,(departs-delays))
      
      
      ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
      times=rep(c(0:23),2)
      t=rep("delays",24)
      d=rep("totals",24)
      coloring=c(d,t)
      TravelTimes=data.frame(Times=times,Midway=flights,Ohare=flights2)
      melted=melt(TravelTimes, id="Times")
      melted$Coloring=coloring
      ggplot(melted, aes(x=Times, y=value)) + geom_bar(stat="identity",colour="white",aes(fill=melted$Coloring))+
        scale_x_continuous( name="Hour",breaks=0:23,labels=timeframe)+           
        theme(axis.text.x=element_text(angle = 90, hjust = 0))+
        facet_grid(~ variable)+ scale_fill_discrete(name = "")
      
    }
    
    else
    {
      
      if(input$timeframe=="AM-PM")
      {
        timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
        
      }
      else
      {
        timeframe=c(0:23)
      }
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      delaysdata=Month[Month$ORIGIN_AIRPORT_ID==Airportname & Month$DEP_DEL15==1 ,]
      Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
      delays=getdeps(delaysdata)
      departs=getdeps(Airline)
      times=rep(c(0:23),2)
      t=rep("delays",24)
      d=rep("totals",24)
      coloring=c(d,t)
      flights=c(delays,(departs-delays))
      
      TravelTimes=data.frame(Times=times,Flights=flights,Coloring=coloring)
      ggplot(TravelTimes, aes(x=Times, y=Flights)) + geom_bar(stat="identity", colour="white",aes(fill=Coloring))+ 
        scale_x_continuous( name="Hour",breaks=0:23,labels=timeframe)+           
        theme(axis.text.x=element_text(angle = 90, hjust = 0))
    }
    
  })
  
  
  ###########################################Part 2-d
  output$DepartDelayTable <- DT::renderDataTable(
    
    
    DT::datatable({
      Month=Month[Monthnames ==input$month]
      #Month=read.csv("Feb.csv")
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      
      if (input$Airport=="Both")
      {
        
        if(input$timeframe=="AM-PM")
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          
        }
        else
        {
          timeframe=c(0:23)
        }
        ports=c("13232", "13930")
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[1] & Month$DEP_DEL15==1 ,]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
        delays1=getdeps(delaysdata)
        departs1=getdeps(Airline)
        flights1=c(delays1, departs1-delays1)   ###LASTCHANGE
        
        
        
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[2] & Month$DEP_DEL15==1 ,]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
        delays2=getdeps(delaysdata)
        departs2=getdeps(Airline)
        flights2=c(delays2, departs2-delays2)   ###LASTCHANGE
        
        
        
        ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
        times=timeframe#rep(c(0:23),2)
        t=rep("delays",24)
        d=rep("totals",24)
        coloring=c(d,t)
        TravelTimes=data.frame(Times=times,Midway_delays=delays1,Ohare_delays=delays2,Midway_Proportion1=signif(100*delays1/departs1,digits=4),Ohare_Proportion=signif(100*delays2/departs2,digits=4))   ###POSTCHANGE2
        
        
        melted=melt(TravelTimes, id="Times")
        melted$Coloring=coloring
        ggplot(melted, aes(x=Times, y=value)) + geom_bar(stat="identity",colour="white",aes(fill=melted$Coloring))+
          scale_x_continuous( name="Hour",breaks=1:24,labels=timeframe)+           
          theme(axis.text.x=element_text(angle = -90, hjust = 0))+
          facet_grid(~ variable)
      }
      
      else
      {
        if(input$timeframe=="AM-PM")
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          
        }
        else
        {
          timeframe=c(0:23)
        }
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==Airportname & Month$DEP_DEL15==1 ,]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        delays=getdeps(delaysdata)
        departs=getdeps(Airline)
        times=timeframe#rep(c(0:23),2)
        flights=c(delays, delays/departs)
        
        TravelTimes=data.frame(Times=times,Delays=delays,Proportion=signif(100*(delays/departs),digits=4))
        
      }
      
      TravelTimes
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE)
    )
  )
  
  
  
  ###########################################Part 2-d
  output$ArrivalDelayTable <- DT::renderDataTable(
    
    
    DT::datatable({
      Month=Month[Monthnames ==input$month]
      #Month=read.csv("Feb.csv")
      Month=Month[[1]]
      Month=Month[Month$CANCELLED==0,] ###POSTCHANGE
      
      
      if (input$Airport=="Both")
      {
        
        if(input$timeframe=="AM-PM")
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          
        }
        else
        {
          timeframe=c(0:23)
        }
        ports=c("13232", "13930")
        delaysdata=Month[Month$DEST_AIRPORT_ID==ports[1] & Month$ARR_DEL15==1 ,]   ##POST CHANGE
        Airline=Month[Month$DEST_AIRPORT_ID==ports[1],]   ###POST CHANGE
        delays1=getarrivals(delaysdata)
        arrivals1=getarrivals(Airline)
        
        delaysdata=Month[Month$DEST_AIRPORT_ID==ports[2] & Month$ARR_DEL15==1 ,]##POST CHANGE
        Airline=Month[Month$DEST_AIRPORT_ID==ports[2],]##POST CHANGE
        delays2=getarrivals(delaysdata)
        arrivals2=getarrivals(Airline)
        
        
        times=timeframe#rep(c(0:23),2)
        TravelTimes=data.frame(Times=times,Midway_Delays=delays1,Midwa_Proportion1=signif(100*delays1/arrivals1,digits=4),
                               Ohare_Delays=delays2,Ohare_Proportion=signif(100*delays2/arrivals2,digits=4))
      }
      
      else
      {
        
        if(input$timeframe=="AM-PM")
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          
        }
        else
        {
          timeframe=c(0:23)
        }
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        delaysdata=Month[Month$DEST_AIRPORT_ID==Airportname & Month$ARR_DEL15==1 ,]##POST CHANGE
        Airline=Month[Month$DEST_AIRPORT_ID==Airportname,]##POST CHANGE
        delays=getarrivals(delaysdata)
        arrivals=getarrivals(Airline)
        times=timeframe#rep(c(0:23),2)
        
        flights=c(delays,(delays/arrivals))
        
        TravelTimes=data.frame(Times=times,Delays=delays,Proportion=signif(100*(delays/arrivals),digits=4))
        
      }
      TravelTimes
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE)
    )
  )
  
  ###################PART B BEGINS HERE
  
  output$arrival_departure_times <- renderPlot({
    times=c(0:23)
    travel_times = data.frame(times=times,
                              arrivals = getarrivals(Month_df),
                              departures = getdeps(Month_df)) %>%
      melt(., id = "times")
    
    ggplot(travel_times, aes(x=times, y = value, colour = variable)) +scale_colour_manual(values=c("#472151", "#ffa345"), 
                                                                                          name="",
                                                                                          breaks=c("arrivals", "departures"))+
      
      geom_line(size=2) +scale_x_continuous(limits=c(0,24),
                                            breaks=0:12*2,
                                            labels=c(paste(0:5*2,"am"),
                                                     "12 pm",
                                                     paste(7:11*2-12,"pm"),
                                                     "0 am")) +
      labs(x="Time", y="# Flights")
  })
  
  output$arrival_departure_2017 <- renderPlot({ #diff
    Month_df$month = factor(Monthnames[as.numeric(format(as.Date(Month_df$FL_DATE,format="%Y-%m-%d"),"%m"))], levels=Monthnames)
    Month_freq = select(Month_df, month, CARRIER) %>%
      table() %>%
      data.frame()
    ggplot(Month_freq, aes(x = month, y = Freq, group = CARRIER)) +
      aes(colour = CARRIER) +
      stat_summary(fun.y = "sum", geom = "line",size=2) +
      coord_trans(y = "log10") +
      scale_y_continuous( breaks = trans_breaks('log10', function(x) 10^x),
                          labels = trans_format('log10', math_format(10^.x))) +
      labs(x="2017 Months", y="Number of Flights")
  })
  
  output$top_15_dest_Plot <- renderPlot({ 
    dest_count = data.frame(table(Month_df$DEST_CITY_NAME))
    top_15_dest = dest_count[order(-dest_count$Freq),][1:15,]$Var1 %>% factor()
    Month_top_15 = Month_df[Month_df$ORIGIN_CITY_NAME %in% top_15_dest]
    Month_top_15$ORIGIN_CITY_NAME = factor(Month_top_15$ORIGIN_CITY_NAME, levels = rev(top_15_dest))
    Month_top_15$FL_DATE = as.Date(Month_top_15$FL_DATE)
    Month_top_15$month = format(Month_top_15$FL_DATE, '%b')
    ggplot(Month_top_15, aes(factor(month, levels = month.abb))) +
      geom_bar(aes(fill = factor(ORIGIN_CITY_NAME))) +
      labs(x ="", y = "# Flights") + theme(legend.title = element_blank(),axis.text.x=element_text(angle = 90, hjust = 0))
    
  })
  
  output$delay_Plot <- renderPlot({
    Month_df$month = factor(Monthnames[as.numeric(format(as.Date(Month_df$FL_DATE,format="%Y-%m-%d"),"%m"))], levels=Monthnames)
    Month_delay = Month_df[,c("month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY")] %>%
      melt(id = "month") %>% na.omit()
    Month_delay = Month_delay[Month_delay$value>0]
    ggplot(Month_delay, aes(x = month, y = value, group =variable)) +  #change
      aes(colour = variable) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      stat_summary(fun.y = "sum", geom = "line",size=2) +
      coord_trans(y = "log10") +scale_colour_manual(name="Delays",values = c("SECURITY_DELAY"="#cc5490", "WEATHER_DELAY"="#FF6D31","NAS_DELAY"="#73B66B","CARRIER_DELAY"="#FFCB18","LATE_AIRCRAFT_DELAY"="#29A2C6"), 
                                                    breaks=c("SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY","CARRIER_DELAY","LATE_AIRCRAFT_DELAY"),
                                                    labels=c("SECURITY DELAY", "WEATHER DELAY", "NAS DELAY","CARRIER DELAY","LATE AIRCRAFT DELAY")) +
      scale_y_continuous( breaks = trans_breaks('log10', function(x) 10^x),
                          labels = trans_format('log10', math_format(10^.x))) +
      labs(x="2017 Months", y="Number of Delays")
  })
  ###################PART A BEGINS HERE
  output$takeOffs <-renderDataTable(
    allTakeOffs[,.(State = state, `Departure Count` = departure_count, `% Departures` =perc_departures, `Arrival Count` = arrival_count, `% Arrivals` = perc_arrivals)], options = list(pageLength= 5)
  )
  
  output$special_days <-DT::renderDataTable(
    
    DT::datatable({
      
      dataSpecial = as.data.table(specialDays[input$dateType])
      
      if(input$dateType == "Heavy Cancellations"){
        setnames(dataSpecial, c("Date", "Flights","Cancellations", "Percentage Cancellations"))}
      else if (input$dateType == "Holidays"){
        setnames(dataSpecial, c("Date", "Holiday Name"))}
      else  {setnames(dataSpecial, c("Date", "No. of Flights"))}
      
      data.frame(dataSpecial)[1:10,]}, options = list(pageLength= 10)
    ))
  ####################C Part Begins here
  
  
  
  
  output$Lauderdale_airport<-renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    
    display_data = Month_df[,c("month","DEP_TIME","ARR_TIME","ORIGIN_CITY_NAME","DEST_CITY_NAME")]
    display_data_dest=display_data[DEST_CITY_NAME==input$Select_Airport]#
    display_data_dest=subset(display_data_dest,select =c(month,ARR_TIME))
    
    display_data_org=display_data[ORIGIN_CITY_NAME==input$Select_Airport]#
    display_data_org=subset(display_data_org,select =c(month,DEP_TIME))
    names(display_data_org) <- c("month", "Departures")
    
    display_data_org=melt(display_data_org,id="month")
    display_data_org=na.omit(display_data_org)
    display_data_org$value<-apply(display_data_org[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    names(display_data_dest) <- c("month", "Arrivals")
    display_data_dest=melt(display_data_dest,id="month")
    display_data_dest=na.omit(display_data_dest)
    display_data_dest$value<-apply(display_data_dest[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    binded_data=rbind(display_data_dest,display_data_org)
    
    
    ##################
    #Jan
    Jan__melted=binded_data[binded_data$month=='Jan']
    Jan_gg<-ggplot(Jan__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(title="Jan")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Feb
    Feb__melted=binded_data[binded_data$month=='Feb']
    Feb_gg<-ggplot(Feb__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Feb")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Mar
    Mar__melted=binded_data[binded_data$month=='Mar']
    Mar_gg<-ggplot(Mar__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Mar")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Apr
    Apr__melted=binded_data[binded_data$month=='Apr']
    Apr_gg<-ggplot(Apr__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Apr")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #May
    May__melted=binded_data[binded_data$month=='May']
    May_gg<-ggplot(May__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="May")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Jun
    Jun__melted=binded_data[binded_data$month=='Jun']
    Jun_gg<-ggplot(Jun__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Jun")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Jul
    Jul__melted=binded_data[binded_data$month=='Jul']
    Jul_gg<-ggplot(Jul__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Jul")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Aug
    Aug__melted=binded_data[binded_data$month=='Aug']
    Aug_gg<-ggplot(Aug__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Aug")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Sept
    Sep__melted=binded_data[binded_data$month=='Sep']
    Sep_gg<-ggplot(Sep__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Sept")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Oct
    Oct__melted=binded_data[binded_data$month=='Oct']
    Oct_gg<-ggplot(Oct__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Oct")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Nov
    Nov__melted=binded_data[binded_data$month=='Nov']
    Nov_gg<-ggplot(Nov__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Nov")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Dec
    Dec__melted=binded_data[binded_data$month=='Dec']
    Dec_gg<-ggplot(Dec__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      labs(title="Dec")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    
    
    
    grid.arrange(Jan_gg,Feb_gg,Mar_gg,Apr_gg,May_gg,Jun_gg,Jul_gg,Aug_gg,Sep_gg,Oct_gg,Nov_gg,Dec_gg,ncol=6)
    
  })
  
  output$one_day_of_week<-renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    monday = Month_df[,c("DAY_OF_WEEK","month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]
    
    monday=monday[DAY_OF_WEEK==days[[input$Select_Day_of_the_Week]]]#
    
    
    monday=na.omit(monday)
    monday$total_delay=monday$SECURITY_DELAY+monday$WEATHER_DELAY+monday$NAS_DELAY+monday$CARRIER_DELAY+monday$LATE_AIRCRAFT_DELAY
    monday_melted = monday[,c("month", "DEP_TIME", "ARR_TIME")]
    monday_melted=melt(monday_melted,id='month')
    monday_melted$value<-apply(monday_melted[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    monday_delay=monday[,c("month", "DEP_TIME", "total_delay")]
    monday_delay$DEP_TIME<-apply(monday_delay[,c('DEP_TIME')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    ##################
    #############################JAN
    Jan_monday_melted=monday_melted[monday_melted$month=='Jan']
    Jan_monday_delay=monday_delay[monday_delay$month=='Jan']
    Jan_gg1<-ggplot(Jan_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Jan_gg2<-ggplot(Jan_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      # geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Jan_gg<-grid.arrange(Jan_gg1,Jan_gg2,ncol=2,top="JAN",widths=c(2,1))
    ##################################
    ################################Feb
    Feb_monday_melted=monday_melted[monday_melted$month=='Feb']
    Feb_monday_delay=monday_delay[monday_delay$month=='Feb']
    Feb_gg1<-ggplot(Feb_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Feb_gg2<-ggplot(Feb_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      # geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Feb_gg<-grid.arrange(Feb_gg1,Feb_gg2,ncol=2,top="Feb",widths=c(2,1))
    ###################################
    ################################ MAR
    Mar_monday_melted=monday_melted[monday_melted$month=='Mar']
    Mar_monday_delay=monday_delay[monday_delay$month=='Mar']
    Mar_gg1<-ggplot(Mar_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Mar_gg2<-ggplot(Mar_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Mar_gg<-grid.arrange(Mar_gg1,Mar_gg2,ncol=2,top="MAR",widths=c(2,1))
    ########################################
    #####################################Apr
    Apr_monday_melted=monday_melted[monday_melted$month=='Apr']
    Apr_monday_delay=monday_delay[monday_delay$month=='Apr']
    Apr_gg1<-ggplot(Apr_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Apr_gg2<-ggplot(Apr_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Apr_gg<-grid.arrange(Apr_gg1,Apr_gg2,ncol=2,top="Apr",widths=c(2,1))
    #######################################
    ####################################May
    May_monday_melted=monday_melted[monday_melted$month=='May']
    May_monday_delay=monday_delay[monday_delay$month=='May']
    May_gg1<-ggplot(May_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    May_gg2<-ggplot(May_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      # scale_colour_gradient(low = "#FF6D31", high = "#9C2A00",limits=c(0,1250))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    May_gg<-grid.arrange(May_gg1,May_gg2,ncol=2,top="May",widths=c(2,1))
    #######################################
    #####################################June
    June_monday_melted=monday_melted[monday_melted$month=='Jun']
    June_monday_delay=monday_delay[monday_delay$month=='Jun']
    June_gg1<-ggplot(June_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    June_gg2<-ggplot(June_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    June_gg<-grid.arrange(June_gg1,June_gg2,ncol=2,top="June",widths=c(2,1))
    ################################################
    #########################################July
    Jul_monday_melted=monday_melted[monday_melted$month=='Jul']
    Jul_monday_delay=monday_delay[monday_delay$month=='Jul']
    Jul_gg1<-ggplot(Jul_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      # scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="Hour") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Jul_gg2<-ggplot(Jul_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Jul_gg<-grid.arrange(Jul_gg1,Jul_gg2,ncol=2,top="July",widths=c(2,1))
    #################################################
    #############################################Aug
    Aug_monday_melted=monday_melted[monday_melted$month=='Aug']
    Aug_monday_delay=monday_delay[monday_delay$month=='Aug']
    Aug_gg1<-ggplot(Aug_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Aug_gg2<-ggplot(Aug_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Aug_gg<-grid.arrange(Aug_gg1,Aug_gg2,ncol=2,top="Aug",widths=c(2,1))
    ########################################
    #############################################Sept
    Sep_monday_melted=monday_melted[monday_melted$month=='Sep']
    Sep_monday_delay=monday_delay[monday_delay$month=='Sep']
    Sep_gg1<-ggplot(Sep_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Sep_gg2<-ggplot(Sep_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Sep_gg<-grid.arrange(Sep_gg1,Sep_gg2,ncol=2,top="Sep",widths=c(2,1))
    #######################################
    #####################################Oct
    Oct_monday_melted=monday_melted[monday_melted$month=='Oct']
    Oct_monday_delay=monday_delay[monday_delay$month=='Oct']
    Oct_gg1<-ggplot(Oct_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Oct_gg2<-ggplot(Oct_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Oct_gg<-grid.arrange(Oct_gg1,Oct_gg2,ncol=2,top="Oct",widths=c(2,1))
    ####################################
    ################################Nov
    Nov_monday_melted=monday_melted[monday_melted$month=='Nov']
    Nov_monday_delay=monday_delay[monday_delay$month=='Nov']
    Nov_gg1<-ggplot(Nov_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Nov_gg2<-ggplot(Nov_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Nov_gg<-grid.arrange(Nov_gg1,Nov_gg2,ncol=2,top="Nov",widths=c(2,1))
    ##################################
    ######################################Dec
    Dec_monday_melted=monday_melted[monday_melted$month=='Dec']
    Dec_monday_delay=monday_delay[monday_delay$month=='Dec']
    Dec_gg1<-ggplot(Dec_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    Dec_gg2<-ggplot(Dec_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF5733", high = "#400000",limits=c(0,1250))+ 
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") +theme(legend.position="none")+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Dec_gg<-grid.arrange(Dec_gg1,Dec_gg2,ncol=2,top="Dec",widths=c(2,1))
    ########################################
    grid.arrange(Jan_gg,Feb_gg,Mar_gg,Apr_gg,May_gg,June_gg,Jul_gg,Aug_gg,Sep_gg,Oct_gg,Nov_gg,Dec_gg,ncol=6)
  })
  

  output$one_day <- renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    day=Month_df[Month_df$FL_DATE==input$date]
    day = day[,c("month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]
    day=na.omit(day)
    day$total_delay=day$SECURITY_DELAY+day$WEATHER_DELAY+day$NAS_DELAY+day$CARRIER_DELAY+day$LATE_AIRCRAFT_DELAY
    day_melted = day[,c("month", "DEP_TIME", "ARR_TIME")]
    day_melted=melt(day_melted,id='month')
    day_melted$value<-apply(day_melted[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    day_delay=day[,c("month", "DEP_TIME", "total_delay")]
    day_delay$DEP_TIME<-apply(day_delay[,c('DEP_TIME')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    gg1<-ggplot(day_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable), size = 3, shape=1,stroke=1.5)+scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) +
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+ theme(text = element_text(size=28),legend.text=element_text(size=25))+
      
      labs(x="", y="Hour") + theme(legend.position="none")+ 
      scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Take Offs","Landings"))+scale_colour_manual(values = c("ARR_TIME"="#472151", "DEP_TIME"="#ffa345")) 
    gg2<-ggplot(day_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(colour=total_delay,size=total_delay+10), shape=1,stroke=2)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF5733", high = "#400000")+ 
      labs(y="",x="") + theme(text = element_text(size=28),legend.text=element_text(size=25))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+scale_size_continuous(guide=FALSE)+labs(colour = "Delay(Min)") 
    grid.arrange(gg1,gg2,ncol=2,widths=c(2,1))
    
  })
  output$airline_200 <- renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    
    display_data = Month_df[,c("FL_NUM","month","DEP_TIME","ARR_TIME")]
    display_data_dest=display_data[display_data$FL_NUM==input$Flight_No]#
    display_data_dest=subset(display_data_dest,select =c(month,ARR_TIME,DEP_TIME))
    names(display_data_dest) <- c("month", "Arrivals","Departures")
    display_data_dest=melt(display_data_dest,id="month")
    display_data_dest=na.omit(display_data_dest)
    display_data_dest$value<-apply(display_data_dest[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    binded_data=display_data_dest
    
    
    
    ##################
    #Jan
    Jan__melted=binded_data[binded_data$month=='Jan']
    Jan_gg<-ggplot(Jan__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable),size = 3)+
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      # theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(title="Jan")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Feb
    Feb__melted=binded_data[binded_data$month=='Feb']
    Feb_gg<-ggplot(Feb__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Feb")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Mar
    Mar__melted=binded_data[binded_data$month=='Mar']
    Mar_gg<-ggplot(Mar__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Mar")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Apr
    Apr__melted=binded_data[binded_data$month=='Apr']
    Apr_gg<-ggplot(Apr__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Apr")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #May
    May__melted=binded_data[binded_data$month=='May']
    May_gg<-ggplot(May__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="May")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Jun
    Jun__melted=binded_data[binded_data$month=='Jun']
    Jun_gg<-ggplot(Jun__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Jun")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Jul
    Jul__melted=binded_data[binded_data$month=='Jul']
    Jul_gg<-ggplot(Jul__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Jul")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Aug
    Aug__melted=binded_data[binded_data$month=='Aug']
    Aug_gg<-ggplot(Aug__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Aug")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Sept
    Sep__melted=binded_data[binded_data$month=='Sep']
    Sep_gg<-ggplot(Sep__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Sept")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Oct
    Oct__melted=binded_data[binded_data$month=='Oct']
    Oct_gg<-ggplot(Oct__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Oct")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Nov
    Nov__melted=binded_data[binded_data$month=='Nov']
    Nov_gg<-ggplot(Nov__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      # theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Nov")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("Departures", "ARR_TIME"),labels = c("Departures","Arrivals"))
    ##################
    #Dec
    Dec__melted=binded_data[binded_data$month=='Dec']
    Dec_gg<-ggplot(Dec__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("Arrivals"="red", "Departures"="blue")) +
      geom_point(aes(colour = variable),size = 3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+theme(text = element_text(size=28),legend.text=element_text(size=25))+
      #theme(axis.text.x=element_text(angle = 90, hjust = 0))+
      expand_limits( y=c(0, 24))+
      labs(title="Dec")+scale_colour_manual(values = c("Arrivals"="#472151", "Departures"="#ffa345")) +
      labs(x="", y="Hour") + theme(legend.position="none")#+ scale_x_discrete(limit = c("DEP_TIME", "ARR_TIME"),labels = c("Departures","Arrivals"))
    
    
    
    grid.arrange(Jan_gg,Feb_gg,Mar_gg,Apr_gg,May_gg,Jun_gg,Jul_gg,Aug_gg,Sep_gg,Oct_gg,Nov_gg,Dec_gg,ncol=6)
    
  })
  ###################PART GRAD BEGINS HERE
  unitChoice <- reactive({
    input$units
  })
  
  binWidth <- reactive({
    input$binwidth
  })
  
  sliderValues <- reactive({
    input$range
  })
  
  output$distance_range_plot <- renderPlot({
    dist_min = sliderValues()[1]
    dist_max = sliderValues()[2]
    
    if (unitChoice() == "miles") {
      dist_values = Month_df[(Month_df$DISTANCE >= dist_min) & (Month_df$DISTANCE <= dist_max)]$DISTANCE
      y_label = "(miles)"
    }
    else {
      dist_values = Month_df$DISTANCE*1.609
      dist_values = dist_values[(dist_values >= dist_min) & (dist_values <= dist_max)]
      y_label = "(kilometer)"
    }
    qplot(dist_values, geom="histogram", binwidth=binWidth(), fill=I("grey"), 
          col=I("black")) + 
      
      ylab("# Flights") +
      xlab(paste("Distance", y_label))
  })
  
  sliderValues2 <- reactive({
    input$time_range
  })
  
  binWidth2 <- reactive({
    input$binwidth2
  })
  
  output$time_range_plot <- renderPlot({
    
    time_min = sliderValues2()[1]
    time_max = sliderValues2()[2]
    
    time_values = Month_df[(Month_df$AIR_TIME >= time_min) & (Month_df$AIR_TIME <= time_max)]$AIR_TIME
    qplot(time_values, geom="histogram", binwidth=binWidth2(), fill=I("grey"), 
          col=I("black")) +
      ylab("# Flights") +
      xlab("Flight Length (min)")
  })
  
  
  output$MonthlyHeatMap <- renderPlot({
    print("full1")
    grid.arrange(mp1,mp2,mp3,mp4)
    
    #grid.arrange(wp1, wp2,wp3,wp4, ncol=2,nrow=2)
  })  
  
  output$WeeklyHeatMap <- renderPlot({
    print("full2")
    grid.arrange(wp1,wp2,wp3,wp4)
    
    #grid.arrange(wp1, wp2,wp3,wp4, ncol=2,nrow=2)
  })  
  
  output$OhareRain <- renderPlot({
    ohare_rain$Date = as.Date(paste("2017", ohare_rain$Month , ohare_rain$day , sep = "-" ))
    ohare_rain$precipitation.in.[ohare_rain$precipitation.in. == "T"] = 0
    ohare_rain$precipitation.in. = as.numeric(levels(ohare_rain$precipitation.in.))[ohare_rain$precipitation.in.]
    
    ohare = Month_df[Month_df$ORIGIN_AIRPORT_ID == 13930] %>%
      select(., CANCELLED, Month, FL_DATE)
    ohare = aggregate(ohare$CANCELLED, by=list(ohare$FL_DATE), sum)
    ohare = merge(ohare , ohare_rain, by.x = "Group.1", by.y = "Date", incomparables = NA, all.x = TRUE)
    
    ggplot(ohare, aes(Group.1)) + 
      stat_smooth(aes(y = x, colour = "cancelled"), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
      geom_point(aes(y = precipitation.in.*60, colour = "precipitation"),size=3) + 
      scale_y_continuous(sec.axis = sec_axis(~./60, name = "Precipitation (in)")) +
      labs(y = "# Flights Cancelled",x="2017 Months", title = "Cancellations at O'Hare")
  })
  }

shinyApp(ui = ui, server = server)