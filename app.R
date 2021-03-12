

#424Project2- AmalJosy Johnson

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)


#-----------------------------------------------Reading 2018 File------------------------------------------------------------------------------------------------------------------------

data <- read.csv(file = "2018data.csv",fill = TRUE, sep = ",", header = TRUE,stringsAsFactors=FALSE )

filtData <- subset(data,select=c("PSTATABB","PNAME","LAT","LON","PLFUELCT","PLGENACL","PLGENAOL","PLGENAGS","PLGENANC","PLGENAHY","PLGENABM","PLGENAWI","PLGENASO","PLGENAGT","Others","PLTNPR","PLTRPR"))
names(filtData)[names(filtData) == "PSTATABB"] <- "PlantStateAbb"
names(filtData)[names(filtData) == "PNAME"] <- "Plantname"
names(filtData)[names(filtData) == "LON"] <- "Longitude"
names(filtData)[names(filtData) == "LAT"] <- "Latitude"
names(filtData)[names(filtData) == "PLGENACL"] <- "Coal"
names(filtData)[names(filtData) == "PLGENAOL"] <- "Oil"
names(filtData)[names(filtData) == "PLGENAGS"] <- "Gas"
names(filtData)[names(filtData) == "PLGENANC"] <- "Nuclear"
names(filtData)[names(filtData) == "PLGENAHY"] <- "Hydro"
names(filtData)[names(filtData) == "PLGENABM"] <- "Biomass"
names(filtData)[names(filtData) == "PLGENAWI"] <- "Wind"
names(filtData)[names(filtData) == "PLGENASO"] <- "Solar"
names(filtData)[names(filtData) == "PLGENAGT"] <- "Geothermal"
names(filtData)[names(filtData) == "PLFUELCT"] <- "Source"
names(filtData)[names(filtData) == "PLTNPR"] <- "NonRenewable"
names(filtData)[names(filtData) == "PLTRPR"] <- "Renewable"


filtData$Coal <- as.numeric(gsub(",","",filtData$Coal))
filtData$Oil <- as.numeric(gsub(",","",filtData$Oil))
filtData$Gas <- as.numeric(gsub(",","",filtData$Gas))
filtData$Nuclear <- as.numeric(gsub(",","",filtData$Nuclear))
filtData$Hydro <- as.numeric(gsub(",","",filtData$Hydro))
filtData$Biomass <- as.numeric(gsub(",","",filtData$Biomass))
filtData$Wind <- as.numeric(gsub(",","",filtData$Wind))
filtData$Solar <- as.numeric(gsub(",","",filtData$Solar))
filtData$Geothermal <- as.numeric(gsub(",","",filtData$Geothermal))
filtData$Others <- as.numeric(gsub(",","",filtData$Others))

filtData$Longitude = as.numeric(filtData$Longitude)
filtData$Latitude = as.numeric(filtData$Latitude)
filtData$Source = as.factor(filtData$Source)


filtData <-  subset(filtData, filtData$Coal>=0)
filtData <-  subset(filtData, filtData$Oil>=0)
filtData <-  subset(filtData, filtData$Gas>=0)
filtData <-  subset(filtData, filtData$Nuclear>=0)
filtData <-  subset(filtData, filtData$Hydro>=0)
filtData <-  subset(filtData, filtData$Biomass>=0)
filtData <-  subset(filtData, filtData$Wind>=0)
filtData <-  subset(filtData, filtData$Solar>=0)
filtData <-  subset(filtData, filtData$Geothermal>=0)
filtData <-  subset(filtData, filtData$Others>=0)

#filtData <- filtData[complete.cases(filtData),]
filtData<-subset(filtData,filtData$Source!="")
filtData<-subset(filtData,filtData$Renewable!="")
filtData<-subset(filtData,filtData$NonRenewable!="")

illData<-subset(filtData,filtData$PlantStateAbb == "IL")
pal= colorFactor(palette=c("orange", "pink", 
                           "green","red", "black", "lightblue", "purple", "brown",
                           "yellow", "lightgreen"),
domain = filtData$Source)


#-----------------------------------------------=------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------Reading 2000 File------------------------------------------------------------------------------------------------------------------------
data1 <- read.csv(file = "2000data.csv",fill = TRUE, sep = ",", header = TRUE,stringsAsFactors=FALSE ,fileEncoding="latin1")
filtData1 <- subset(data1,select=c("PSTATABB.","PNAME.","LAT.","LON.","PLPRIMFL.","PLGENACL.","PLGENAOL.","PLGENAGS.","PLGENANC.","PLGENAHY.","PLGENABM.","PLGENAWI.","PLGENASO.","PLGENAGT.","Others","PLTNPR","PLTRPR"))
names(filtData1)[names(filtData1) == "PSTATABB."] <- "PlantStateAbb"
names(filtData1)[names(filtData1) == "PNAME."] <- "Plantname"
names(filtData1)[names(filtData1) == "LON."] <- "Longitude"
names(filtData1)[names(filtData1) == "LAT."] <- "Latitude"
names(filtData1)[names(filtData1) == "PLGENACL."] <- "Coal"
names(filtData1)[names(filtData1) == "PLGENAOL."] <- "Oil"
names(filtData1)[names(filtData1) == "PLGENAGS."] <- "Gas"
names(filtData1)[names(filtData1) == "PLGENANC."] <- "Nuclear"
names(filtData1)[names(filtData1) == "PLGENAHY."] <- "Hydro"
names(filtData1)[names(filtData1) == "PLGENABM."] <- "Biomass"
names(filtData1)[names(filtData1) == "PLGENAWI."] <- "Wind"
names(filtData1)[names(filtData1) == "PLGENASO."] <- "Solar"
names(filtData1)[names(filtData1) == "PLGENAGT."] <- "Geothermal"
names(filtData1)[names(filtData1) == "PLPRIMFL."] <- "Source"
names(filtData1)[names(filtData1) == "PLTNPR"] <- "NonRenewable"
names(filtData1)[names(filtData1) == "PLTRPR"] <- "Renewable"
filtData1$Longitude = as.numeric(filtData1$Longitude)
filtData1$Latitude = as.numeric(filtData1$Latitude)
filtData1$Source = as.factor(filtData1$Source)
filtData1$Coal = as.numeric(filtData1$Coal)
filtData1$Oil = as.numeric(filtData1$Oil)
filtData1$Gas = as.numeric(filtData1$Gas)
filtData1$Nuclear = as.numeric(filtData1$Nuclear)
filtData1$Hydro = as.numeric(filtData1$Hydro)
filtData1$Biomass = as.numeric(filtData1$Biomass)
filtData1$Wind = as.numeric(filtData1$Wind)
filtData1$Solar = as.numeric(filtData1$Solar)
filtData1$Geothermal = as.numeric(filtData1$Geothermal)
filtData1$Others = as.numeric(filtData1$Others)
filtData1 <-  subset(filtData1, filtData1$Coal>=0)
filtData1 <-  subset(filtData1, filtData1$Oil>=0)
filtData1 <-  subset(filtData1, filtData1$Gas>=0)
filtData1 <-  subset(filtData1, filtData1$Nuclear>=0)
filtData1 <-  subset(filtData1, filtData1$Hydro>=0)
filtData1 <-  subset(filtData1, filtData1$Biomass>=0)
filtData1 <-  subset(filtData1, filtData1$Wind>=0)
filtData1 <-  subset(filtData1, filtData1$Solar>=0)
filtData1 <-  subset(filtData1, filtData1$Geothermal>=0)
filtData1 <-  subset(filtData1, filtData1$Others>=0)
filtData1<-subset(filtData1,filtData1$Source!="")
filtData1<-subset(filtData1,filtData1$Renewable!="")
filtData1<-subset(filtData1,filtData1$NonRenewable!="")


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------Reading 2010 File------------------------------------------------------------------------------------------------------------------------

data2 <- read.csv(file = "2010data.csv",fill = TRUE, sep = ",", header = TRUE,stringsAsFactors=FALSE )

filtData2 <- subset(data2,select=c("PSTATABB","PNAME","LAT","LON","PLPFGNCT","PLGENACL","PLGENAOL","PLGENAGS","PLGENANC","PLGENAHY","PLGENABM","PLGENAWI","PLGENASO","PLGENAGT","Others","PLTNPR","PLTRPR"))
names(filtData2)[names(filtData2) == "PSTATABB"] <- "PlantStateAbb"
names(filtData2)[names(filtData2) == "PNAME"] <- "Plantname"
names(filtData2)[names(filtData2) == "LON"] <- "Longitude"
names(filtData2)[names(filtData2) == "LAT"] <- "Latitude"
names(filtData2)[names(filtData2) == "PLGENACL"] <- "Coal"
names(filtData2)[names(filtData2) == "PLGENAOL"] <- "Oil"
names(filtData2)[names(filtData2) == "PLGENAGS"] <- "Gas"
names(filtData2)[names(filtData2) == "PLGENANC"] <- "Nuclear"
names(filtData2)[names(filtData2) == "PLGENAHY"] <- "Hydro"
names(filtData2)[names(filtData2) == "PLGENABM"] <- "Biomass"
names(filtData2)[names(filtData2) == "PLGENAWI"] <- "Wind"
names(filtData2)[names(filtData2) == "PLGENASO"] <- "Solar"
names(filtData2)[names(filtData2) == "PLGENAGT"] <- "Geothermal"
names(filtData2)[names(filtData2) == "PLPFGNCT"] <- "Source"
names(filtData2)[names(filtData2) == "PLTNPR"] <- "NonRenewable"
names(filtData2)[names(filtData2) == "PLTRPR"] <- "Renewable"


filtData2$Coal <- as.numeric(gsub(",","",filtData2$Coal))
filtData2$Oil <- as.numeric(gsub(",","",filtData2$Oil))
filtData2$Gas <- as.numeric(gsub(",","",filtData2$Gas))
filtData2$Nuclear <- as.numeric(gsub(",","",filtData2$Nuclear))
filtData2$Hydro <- as.numeric(gsub(",","",filtData2$Hydro))
filtData2$Biomass <- as.numeric(gsub(",","",filtData2$Biomass))
filtData2$Wind <- as.numeric(gsub(",","",filtData2$Wind))
filtData2$Solar <- as.numeric(gsub(",","",filtData2$Solar))
filtData2$Geothermal <- as.numeric(gsub(",","",filtData2$Geothermal))
filtData2$Others <- as.numeric(gsub(",","",filtData2$Others))

filtData2$Longitude = as.numeric(filtData2$Longitude)
filtData2$Latitude = as.numeric(filtData2$Latitude)
filtData2$Source = as.factor(filtData2$Source)


filtData2 <-  subset(filtData2, filtData2$Coal>=0)
filtData2 <-  subset(filtData2, filtData2$Oil>=0)
filtData2 <-  subset(filtData2, filtData2$Gas>=0)
filtData2 <-  subset(filtData2, filtData2$Nuclear>=0)
filtData2 <-  subset(filtData2, filtData2$Hydro>=0)
filtData2 <-  subset(filtData2, filtData2$Biomass>=0)
filtData2 <-  subset(filtData2, filtData2$Wind>=0)
filtData2 <-  subset(filtData2, filtData2$Solar>=0)
filtData2 <-  subset(filtData2, filtData2$Geothermal>=0)
filtData2 <-  subset(filtData2, filtData2$Others>=0)


filtData2<-subset(filtData2,filtData2$Source!="")
filtData2<-subset(filtData2,filtData2$Renewable!="")
filtData2<-subset(filtData2,filtData2$NonRenewable!="")


illData2<-subset(filtData2,filtData2$PlantStateAbb == "IL")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------filtData1[filtData1 == "AK"] <- "Alaska"
filtData2[filtData2 == "AK"] <- "Alaska"
filtData2[filtData2 == "AL"] <- "Alabama"
filtData2[filtData2 == "AR"] <- "Arkansas"
filtData2[filtData2 == "AZ"] <- "Arizona"
filtData2[filtData2 == "CA"] <- "California"
filtData2[filtData2 == "CO"] <- "Colorado"
filtData2[filtData2 == "CT"] <- "Connecticut"
filtData2[filtData2 == "DC"] <- "District of Columbia"
filtData2[filtData2 == "DE"] <- "Delaware"
filtData2[filtData2 == "FL"] <- "Florida"
filtData2[filtData2 == "GA"] <- "Georgia"
filtData2[filtData2 == "HI"] <- "Hawaii"
filtData2[filtData2 == "ID"] <- "Idaho"
filtData2[filtData2 == "IA"] <- "Iowa"
filtData2[filtData2 == "IL"] <- "Illinois"
filtData2[filtData2 == "IN"] <- "Indiana"
filtData2[filtData2 == "KS"] <- "Kansas"
filtData2[filtData2 == "KY"] <- "Kentucky"
filtData2[filtData2 == "LA"] <- "Louisiana"
filtData2[filtData2 == "MA"] <- "Massachusetts"
filtData2[filtData2 == "MD"] <- "MaryLand"
filtData2[filtData2 == "ME"] <- "Maine"
filtData2[filtData2 == "MI"] <- "Michigan"
filtData2[filtData2 == "MN"] <- "Minnesota"
filtData2[filtData2 == "MO"] <- "Missouri"
filtData2[filtData2 == "MS"] <- "Mississippi"
filtData2[filtData2 == "MT"] <- "Montana"
filtData2[filtData2 == "NC"] <- "North Carolina"
filtData2[filtData2 == "ND"] <- "North Dakota"
filtData2[filtData2 == "NE"] <- "Nebraska"
filtData2[filtData2 == "NH"] <- "New Hampshire"
filtData2[filtData2 == "NJ"] <- "New Jersey"
filtData2[filtData2 == "NM"] <- "New Mexico"
filtData2[filtData2 == "NV"] <- "Nevada"
filtData2[filtData2 == "NY"] <- "New York"
filtData2[filtData2 == "OH"] <- "Ohio"
filtData2[filtData2 == "OK"] <- "Oklahoma"
filtData2[filtData2 == "OR"] <- "Orgeon"
filtData2[filtData2 == "PA"] <- "Pennsylvania"
filtData2[filtData2 == "RI"] <- "Rhode Island"
filtData2[filtData2 == "SC"] <- "South Carolina"
filtData2[filtData2 == "SD"] <- "South Dakota"
filtData2[filtData2 == "TN"] <- "Tennesse"
filtData2[filtData2 == "TX"] <- "Texas"
filtData2[filtData2 == "UT"] <- "Utah"
filtData2[filtData2 == "VA"] <- "Virginia"
filtData2[filtData2 == "VT"] <- "Vermont"
filtData2[filtData2 == "WA"] <- "Washington"
filtData2[filtData2 == "WI"] <- "Wisconsin"
filtData2[filtData2 == "WV"] <- "West Virginia"
filtData2[filtData2 == "WY"] <- "Wyoming"

filtData1[filtData1 == "AL"] <- "Alabama"
filtData1[filtData1 == "AR"] <- "Arkansas"
filtData1[filtData1 == "AZ"] <- "Arizona"
filtData1[filtData1 == "CA"] <- "California"
filtData1[filtData1 == "CO"] <- "Colorado"
filtData1[filtData1 == "CT"] <- "Connecticut"
filtData1[filtData1 == "DC"] <- "District of Columbia"
filtData1[filtData1 == "DE"] <- "Delaware"
filtData1[filtData1 == "FL"] <- "Florida"
filtData1[filtData1 == "GA"] <- "Georgia"
filtData1[filtData1 == "HI"] <- "Hawaii"
filtData1[filtData1 == "ID"] <- "Idaho"
filtData1[filtData1 == "IA"] <- "Iowa"
filtData1[filtData1 == "IL"] <- "Illinois"
filtData1[filtData1 == "IN"] <- "Indiana"
filtData1[filtData1 == "KS"] <- "Kansas"
filtData1[filtData1 == "KY"] <- "Kentucky"
filtData1[filtData1 == "LA"] <- "Louisiana"
filtData1[filtData1 == "MA"] <- "Massachusetts"
filtData1[filtData1 == "MD"] <- "MaryLand"
filtData1[filtData1 == "ME"] <- "Maine"
filtData1[filtData1 == "MI"] <- "Michigan"
filtData1[filtData1 == "MN"] <- "Minnesota"
filtData1[filtData1 == "MO"] <- "Missouri"
filtData1[filtData1 == "MS"] <- "Mississippi"
filtData1[filtData1 == "MT"] <- "Montana"
filtData1[filtData1 == "NC"] <- "North Carolina"
filtData1[filtData1 == "ND"] <- "North Dakota"
filtData1[filtData1 == "NE"] <- "Nebraska"
filtData1[filtData1 == "NH"] <- "New Hampshire"
filtData1[filtData1 == "NJ"] <- "New Jersey"
filtData1[filtData1 == "NM"] <- "New Mexico"
filtData1[filtData1 == "NV"] <- "Nevada"
filtData1[filtData1 == "NY"] <- "New York"
filtData1[filtData1 == "OH"] <- "Ohio"
filtData1[filtData1 == "OK"] <- "Oklahoma"
filtData1[filtData1 == "OR"] <- "Orgeon"
filtData1[filtData1 == "PA"] <- "Pennsylvania"
filtData1[filtData1 == "RI"] <- "Rhode Island"
filtData1[filtData1 == "SC"] <- "South Carolina"
filtData1[filtData1 == "SD"] <- "South Dakota"
filtData1[filtData1 == "TN"] <- "Tennesse"
filtData1[filtData1 == "TX"] <- "Texas"
filtData1[filtData1 == "UT"] <- "Utah"
filtData1[filtData1 == "VA"] <- "Virginia"
filtData1[filtData1 == "VT"] <- "Vermont"
filtData1[filtData1 == "WA"] <- "Washington"
filtData1[filtData1 == "WI"] <- "Wisconsin"
filtData1[filtData1 == "WV"] <- "West Virginia"
filtData1[filtData1 == "WY"] <- "Wyoming"


filtData[filtData == "AK"] <- "Alaska"
filtData[filtData == "AL"] <- "Alabama"
filtData[filtData == "AR"] <- "Arkansas"
filtData[filtData == "AZ"] <- "Arizona"
filtData[filtData == "CA"] <- "California"
filtData[filtData == "CO"] <- "Colorado"
filtData[filtData == "CT"] <- "Connecticut"
filtData[filtData == "DC"] <- "District of Columbia"
filtData[filtData == "DE"] <- "Delaware"
filtData[filtData == "FL"] <- "Florida"
filtData[filtData == "GA"] <- "Georgia"
filtData[filtData == "HI"] <- "Hawaii"
filtData[filtData == "ID"] <- "Idaho"
filtData[filtData == "IA"] <- "Iowa"
filtData[filtData == "IL"] <- "Illinois"
filtData[filtData == "IN"] <- "Indiana"
filtData[filtData == "KS"] <- "Kansas"
filtData[filtData == "KY"] <- "Kentucky"
filtData[filtData == "LA"] <- "Louisiana"
filtData[filtData == "MA"] <- "Massachusetts"
filtData[filtData == "MD"] <- "MaryLand"
filtData[filtData == "ME"] <- "Maine"
filtData[filtData == "MI"] <- "Michigan"
filtData[filtData == "MN"] <- "Minnesota"
filtData[filtData == "MO"] <- "Missouri"
filtData[filtData == "MS"] <- "Mississippi"
filtData[filtData == "MT"] <- "Montana"
filtData[filtData == "NC"] <- "North Carolina"
filtData[filtData == "ND"] <- "North Dakota"
filtData[filtData == "NE"] <- "Nebraska"
filtData[filtData == "NH"] <- "New Hampshire"
filtData[filtData == "NJ"] <- "New Jersey"
filtData[filtData == "NM"] <- "New Mexico"
filtData[filtData == "NV"] <- "Nevada"
filtData[filtData == "NY"] <- "New York"
filtData[filtData == "OH"] <- "Ohio"
filtData[filtData == "OK"] <- "Oklahoma"
filtData[filtData == "OR"] <- "Orgeon"
filtData[filtData == "PA"] <- "Pennsylvania"
filtData[filtData == "RI"] <- "Rhode Island"
filtData[filtData == "SC"] <- "South Carolina"
filtData[filtData == "SD"] <- "South Dakota"
filtData[filtData == "TN"] <- "Tennesse"
filtData[filtData == "TX"] <- "Texas"
filtData[filtData == "UT"] <- "Utah"
filtData[filtData == "VA"] <- "Virginia"
filtData[filtData == "VT"] <- "Vermont"
filtData[filtData == "WA"] <- "Washington"
filtData[filtData == "WI"] <- "Wisconsin"
filtData[filtData == "WV"] <- "West Virginia"
filtData[filtData == "WY"] <- "Wyoming"



data5 <- data.frame(name = c("blank1","blank2"),
                   Source = c("None"
                   ),
                   Latitude = c(37.0902, 37.0902 ),
                   Longitude = c(-97.7129, -97.7129))

map_List <-c(
  "Esri.NatGeoWorldMap",
  "Esri.WorldStreetMap",
  "Esri.DeLorme"
  
)
states_List <- c(
  "US-TOTAL",
  "Alaska",
  "Alabama",
  "Arkansas",
  "Arizona",
  "California",
  "Colorado",
  "Connecticut",
  "District of Columbia",
  "Delaware",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Iowa",
  "Illinois",
  "Indiana",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Massachusetts",
  "MaryLand",
  "Maine",
  "Michigan",
  "Minnesota",
  "Missouri",
  "Mississippi",
  "Montana",
  "North Carolina",
  "North Dakota",
  "Nebraska",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "Nevada",
  "New York",
  "Ohio",
  "Oklahoma",
  "Orgeon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennesse",
  "Texas",
  "Utah",
  "Virginia",
  "Vermont",
  "Washington",
  "Wisconsin",
  "West Virginia",
  "Wyoming"
)
years_List <-c(
  "2000",
  "2010",
  "2018"
  
)


# Define UI for application that draws a histogram
ui<- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .selectize-input {
        height: 50px;
        width: 150px;
        font-size: 8pt;
        padding-top: 3px;
      padding-down: 3px;

      }
    ")
               
               ),
    tags$style(HTML("
      .selectize-boxInput {
        height: 50px;
        width: 150px;
        font-size: 8pt;
        padding-top: 3px;
      padding-down: 3px;

      }
    ")
               
    )
    
  ),
  dashboardPage(
    dashboardHeader(title = "CS 424 Spring 2021 Project 2"),
 
    dashboardSidebar (disable = FALSE, collapsed = FALSE,
                      
                      sidebarMenu(
                        menuItem("2018 Illinois", tabName = "2018il", icon =NULL),
                        menuItem("State Comparisons", tabName = "statecomparison", icon =icon("dashboard")),
                        menuItem("Entire US Comparison", tabName = "uscomparison", icon =icon("dashboard")),
                        menuItem("About", tabName = "about", icon = NULL)
                      )
    ),
    dashboardBody(
      tabItems(
          tabItem(tabName ="2018il",
               
                 fluidRow(
                   column(12,
                         fluidRow(box(title = "Location of all Power Plants in Illinois in 2018", solidHeader = TRUE, 
                                      status = "primary",width = 100,
                                      column(width=12,
                                             column(width=2,checkboxInput('AllEnergy','All',TRUE)),
                                             column(width=2,checkboxInput('REnergy','Renewable',FALSE)),
                                             column(width=2,checkboxInput('NREnergy','Non-Renewable',FALSE)),
                                             column(width=2,checkboxInput('CoalEnergy','Coal',FALSE)),
                                             column(width=2,checkboxInput('OilEnergy','Oil',FALSE)),
                                             column(width=2,checkboxInput('GeothermalEnergy','Geothermal',FALSE)),
                                             column(width=2,checkboxInput('GasEnergy','Gas',FALSE)),
                                             column(width=2, checkboxInput('BiomassEnergy','Biomass',FALSE)),
                                             column(width=2, checkboxInput('SolarEnergy','Solar',FALSE)),
                                             column(width=2, checkboxInput('WindEnergy','Wind',FALSE)),
                                             column(width=2, checkboxInput('NuclearEnergy','Nuclear',FALSE)),
                                             column(width=2, checkboxInput('HydroEnergy','Hydro',FALSE)),
                                             column(width=2, checkboxInput('OtherEnergy','Others',FALSE))),
                                      leafletOutput("plot1",height=650)
                                      )
                                 )
                      
                         )

                         )
                 
                  ),
          tabItem(tabName="statecomparison",
                 
       
                  fluidRow(
                    column(6,
                           fluidRow(
                             box(title = textOutput('title1'), solidHeader = TRUE, status = "primary", width =30,
                                 column(width=8,
                                        column(width=4,checkboxInput('AllEnergy1','All',TRUE)),
                                        column(width=4,checkboxInput('REnergy1','Renewable',FALSE)),
                                        column(width=4,checkboxInput('NREnergy1','Non-Renewable',FALSE)),
                                        column(width=4,checkboxInput('CoalEnergy1','Coal',FALSE)),
                                        column(width=4,checkboxInput('OilEnergy1','Oil',FALSE)),
                                        column(width=4,checkboxInput('GeothermalEnergy1','Geothermal',FALSE)),
                                        column(width=4,checkboxInput('GasEnergy1','Gas',FALSE)),
                                        column(width=4, checkboxInput('BiomassEnergy1','Biomass',FALSE)),
                                        column(width=4, checkboxInput('SolarEnergy1','Solar',FALSE)),
                                        column(width=4, checkboxInput('WindEnergy1','Wind',FALSE)),
                                        column(width=4, checkboxInput('NuclearEnergy1','Nuclear',FALSE)),
                                        column(width=4, checkboxInput('HydroEnergy1','Hydro',FALSE)),
                                        column(width=4, checkboxInput('OtherEnergy1','Others',FALSE))),
                                 column(4,selectInput('MapLayout', 'Select MapLayout:',
                                                      choices=map_List,selected = "Esri.NatGeoWorldMap")),
                                column(4,selectInput('State', 'Select State:',
                                                      choices=states_List,selected = "Illinois")),
                                column(4, selectInput('Year', 'Select Year:',
                                                      choices=years_List,selected = "2000")),
                            
                                 
                                 leafletOutput("plot2",height = 300)
                               )

                    )),
                    column(6,
                           fluidRow(
                             box(title = textOutput('title2'), solidHeader = TRUE, status = "primary", width = 30,
                                 column(width=8,
                                        column(width=4,checkboxInput('AllEnergy2','All',TRUE)),
                                        column(width=4,checkboxInput('REnergy2','Renewable',FALSE)),
                                        column(width=4,checkboxInput('NREnergy2','Non-Renewable',FALSE)),
                                        column(width=4,checkboxInput('CoalEnergy2','Coal',FALSE)),
                                        column(width=4,checkboxInput('OilEnergy2','Oil',FALSE)),
                                        column(width=4,checkboxInput('GeothermalEnergy2','Geothermal',FALSE)),
                                        column(width=4,checkboxInput('GasEnergy2','Gas',FALSE)),
                                        column(width=4, checkboxInput('BiomassEnergy2','Biomass',FALSE)),
                                        column(width=4, checkboxInput('SolarEnergy2','Solar',FALSE)),
                                        column(width=4, checkboxInput('WindEnergy2','Wind',FALSE)),
                                        column(width=4, checkboxInput('NuclearEnergy2','Nuclear',FALSE)),
                                        column(width=4, checkboxInput('HydroEnergy2','Hydro',FALSE)),
                                        column(width=4, checkboxInput('OtherEnergy2','Others',FALSE))),
                                 
                                column(4,selectInput('MapLayout1', 'Select MapLayout:',
                                                      choices=map_List,selected = "Esri.DeLorme")),
                               column(4,selectInput('State1', 'Select State:',
                                                      choices=states_List,selected = "Illinois")),
                               column(4, selectInput('Year1', 'Select Year:',
                                                      choices=years_List,selected = "2018")),
                                 leafletOutput("plot3",height = 300)
                               
                             )
                    
                    
                  )
                  
                  )),
                  fluidPage(box(
                    title = "Common Checkbox for Both Maps(Uncheck selected individual map checkboxes Above to use this set of checboxes to control both maps simultaneously)", solidHeader = TRUE, status = "primary", width = 200,  background = "light-blue",
                    column(width=12,
                          
                           column(width=3,checkboxInput('AllEnergy3','All',FALSE)),
                           column(width=3,checkboxInput('REnergy3','Renewable',FALSE)),
                           column(width=3,checkboxInput('NREnergy3','Non-Renewable',FALSE)),
                           column(width=3,checkboxInput('CoalEnergy3','Coal',FALSE)),
                           column(width=3,checkboxInput('OilEnergy3','Oil',FALSE)),
                           column(width=3,checkboxInput('GeothermalEnergy3','Geothermal',FALSE)),
                           column(width=3,checkboxInput('GasEnergy3','Gas',FALSE)),
                          column(width=3, checkboxInput('BiomassEnergy3','Biomass',FALSE)),
                           column(width=3, checkboxInput('SolarEnergy3','Solar',FALSE)),
                           column(width=3, checkboxInput('WindEnergy3','Wind',FALSE)),
                           column(width=3, checkboxInput('NuclearEnergy3','Nuclear',FALSE)),
                           column(width=3, checkboxInput('HydroEnergy3','Hydro',FALSE)),
                           column(width=3, checkboxInput('OtherEnergy3','Others',FALSE)))
                  ))
                  ),
          tabItem(tabName="uscomparison",
                  fluidRow(
                    column(12,
                           fluidRow(box(title = "Location of all Power Plants in Illinois in 2018", solidHeader = TRUE, 
                                        status = "primary",width = 100,
                                        column(width=12,
                                               column(width=2,checkboxInput('AllEnergy4','All',FALSE)),
                                               column(width=2,checkboxInput('REnergy4','Renewable',FALSE)),
                                               column(width=2,checkboxInput('NREnergy4','Non-Renewable',FALSE)),
                                               column(width=2,checkboxInput('CoalEnergy4','Coal',FALSE)),
                                               column(width=2,checkboxInput('OilEnergy4','Oil',FALSE)),
                                               column(width=2,checkboxInput('GeothermalEnergy4','Geothermal',FALSE)),
                                               column(width=2,checkboxInput('GasEnergy4','Gas',FALSE)),
                                               column(width=2, checkboxInput('BiomassEnergy4','Biomass',FALSE)),
                                               column(width=2, checkboxInput('SolarEnergy4','Solar',FALSE)),
                                               column(width=2, checkboxInput('WindEnergy4','Wind',FALSE)),
                                               column(width=2, checkboxInput('NuclearEnergy4','Nuclear',FALSE)),
                                               column(width=2, checkboxInput('HydroEnergy4','Hydro',FALSE)),
                                               column(width=2, checkboxInput('OtherEnergy4','Others',FALSE))),
                                        column(4,selectInput('MapLayout2', 'Select MapLayout:',
                                                             choices=map_List,selected = "Esri.DeLorme")),
                                        column(4,selectInput('State2', 'Select State:',
                                                             choices=states_List,selected = "US-TOTAL")),
                                        column(4, selectInput('Year2', 'Select Year:',
                                                              choices=years_List,selected = "2018")),
                                        column(4,uiOutput("slider")),
                                        
                                       
                                        leafletOutput("plot4",height=650)
                           )
                           )
                           
                    )
                    
                  )
                  
          ),
          tabItem(tabName="about",
                  
                "This project was done by AmalJosy Johnson on 3/13/2021.
               The data used for this project is from https://www.epa.gov/egrid/download-data.
               A total of three datasets is used from this website for this project.The data is from years 2000,2010 and 2018 on
               the power plant electrical power generation in the US.
               This project uses leaflet to visualize the powerplants across the USA.The leaflet shows different colore marker for each power source
               and also provides information like location,type of source,percent of renewable energy,percent of non renewable energy and power generation amount.
               The project has a custom input selection for the user to select the state,year and energy source to view and can compare
               it with another data from a different state. The user can also do a energy comparison on the whole US. The user is also given a input choice of 3 for the maptype of the leaflet"

                  
                  
          )
        
      )
        
                  )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$title1 <- renderText(paste("Location of all Power Plants in", input$State, "in",input$Year))
  output$title2 <- renderText(paste("Location of all Power Plants in", input$State1, "in",input$Year1))
  
  myReactiveFunc <- reactive(
    {
      illData1<- subset(filtData,filtData$PlantStateAbb == "Illinois")
      toReturn <- NULL
      if(input$AllEnergy){
         toReturn <-illData1
        
      }
      if(input$CoalEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="COAL"))
        
      }
      if(input$OilEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="OIL"))
        
      }
      if(input$GasEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="GAS"))
        
      }
      if(input$BiomassEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="BIOMASS"))
        
      }
      if(input$WindEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="WIND"))
        
      }
      if(input$SolarEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="SOLAR"))
        
      }
      if(input$GeothermalEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="GEOTHERMAL"))
        
      }
      if(input$NuclearEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="NUCLEAR"))
        
      }
      if(input$HydroEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="HYDRO"))
        
      }
      if(input$OtherEnergy){
        toReturn<- rbind(toReturn,subset(illData1,illData1$Source=="OTHER"))
      }
      if(input$REnergy){
        toReturn<- rbind(toReturn,subset(illData1,Source %in% c("WIND","BIOMASS","HYDRO","SOLAR","GEOTHERMAL")))
        
      }
      if(input$NREnergy){
        toReturn<- rbind(toReturn,subset(illData1,Source %in% c("COAL","OIL","GAS","NUCLEAR","OTHER")))
        
      }
    
     

     toReturn 
    }
  )

  myReactiveFunc2 <- reactive({
    if(input$Year=="2000"){
      yearData<- subset(filtData1,filtData1$PlantStateAbb==input$State)
    }
    if(input$Year=="2010"){
      yearData<- subset(filtData2,filtData2$PlantStateAbb==input$State)
    }
    if(input$Year=="2018"){
      yearData<- subset(filtData,filtData$PlantStateAbb==input$State)
    }
   
    toReturn <- NULL
    if(input$AllEnergy1||input$AllEnergy3){
      toReturn <-yearData
      
    }
    if(input$CoalEnergy1||input$CoalEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="COAL"))
      
    }
    if(input$OilEnergy1||input$OilEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="OIL"))
      
    }
    if(input$GasEnergy1||input$GasEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="GAS"))
      
    }
    if(input$BiomassEnergy1||input$BiomassEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="BIOMASS"))
      
    }
    if(input$WindEnergy1||input$WindEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="WIND"))
      
    }
    if(input$SolarEnergy1||input$SolarEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="SOLAR"))
      
    }
    if(input$GeothermalEnergy1||input$GeothermalEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="Geothermal"))
      
    }
    if(input$NuclearEnergy1||input$NuclearEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="NUCLEAR"))
      
    }
    if(input$HydroEnergy1||input$HydroEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="HYDRO"))
      
    }
    if(input$OtherEnergy1||input$OtherEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="OTHER"))
    }
    if(input$REnergy1||input$REnergy3){
      toReturn<- rbind(toReturn,subset(yearData,Source %in% c("WIND","BIOMASS","HYDRO","SOLAR","GEOTHERMAL")))
      
    }
    if(input$NREnergy1||input$NREnergy3){
      toReturn<- rbind(toReturn,subset(yearData,Source %in% c("COAL","OIL","GAS","NUCLEAR","OTHER")))
      
    }
    toReturn
  })
  myReactiveFunc3 <- reactive({
    
    if(input$Year1=="2000"){
      yearData<- subset(filtData1,filtData1$PlantStateAbb==input$State1)
    }
    if(input$Year=="2010"){
      yearData<- subset(filtData2,filtData2$PlantStateAbb==input$State1)
    }
    if(input$Year1=="2018"){
      yearData<- subset(filtData,filtData$PlantStateAbb==input$State1)
    }
    toReturn <- NULL
    if(input$AllEnergy2||input$AllEnergy3){
      toReturn <-yearData
      
    }
    if(input$CoalEnergy2||input$CoalEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="COAL"))
      
    }
    if(input$OilEnergy2||input$OilEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="OIL"))
      
    }
    if(input$GasEnergy2||input$GasEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="GAS"))
      
    }
    if(input$BiomassEnergy2||input$BiomassEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="BIOMASS"))
      
    }
    if(input$WindEnergy2||input$WindEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="WIND"))
      
    }
    if(input$SolarEnergy2||input$SolarEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="SOLAR"))
      
    }
    if(input$GeothermalEnergy2||input$GeothermalEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="GEOTHERMAL"))
      
    }
    if(input$NuclearEnergy2||input$NuclearEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="NUCLEAR"))
      
    }
    if(input$HydroEnergy2||input$HydroEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="HYDRO"))
      
    }
    if(input$OtherEnergy2||input$OtherEnergy3){
      toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="OTHER"))
    }
    if(input$REnergy2||input$REnergy3){
      toReturn<- rbind(toReturn,subset(yearData,Source %in% c("WIND","BIOMASS","HYDRO","SOLAR","GEOTHERMAL")))
      
    }
    if(input$NREnergy2||input$NREnergy3){
      toReturn<- rbind(toReturn,subset(yearData,Source %in% c("COAL","OIL","GAS","NUCLEAR","OTHER")))
      
    }
    
    toReturn
  })
  
  
  myReactiveFunc4 <- reactive(
    {
      if(input$Year2=="2000"){
        if(input$State2=="US-TOTAL"){
          
          yearData<- filtData1
        }
        else{
        yearData<- subset(filtData1,filtData1$PlantStateAbb==input$State2)
        }
      }
      if(input$Year2=="2010"){
        if(input$State2=="US-TOTAL"){
          yearData<- filtData2
        }
        else{
          yearData<- subset(filtData2,filtData2$PlantStateAbb==input$State2)
        }     
        }
      if(input$Year2=="2018"){
        if(input$State2=="US-TOTAL"){
          yearData<- filtData
        }
        else{
          yearData<- subset(filtData,filtData$PlantStateAbb==input$State2)
        }   
      }
      toReturn <- NULL
      if(!input$AllEnergy4 &&!input$CoalEnergy4&&!input$OilEnergy4&&!input$GasEnergy4&&!input$BiomassEnergy4&&!input$WindEnergy4&&!input$SolarEnergy4&&!input$GeothermalEnergy4&&!input$NuclearEnergy4
         &&!input$HydroEnergy4&&!input$OtherEnergy4&&!input$REnergy4&&!input$NREnergy4)
        {
        toReturn <-data5
      }
     
      if(input$AllEnergy4){
        toReturn <-yearData
        
      }
      
      if(input$CoalEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="COAL"&yearData$Coal>min(input$slider)&yearData$Coal<max(input$slider)))
        
      }
      if(input$OilEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="OIL"&yearData$Oil>min(input$slider)&yearData$Oil<max(input$slider)))
        
      }
      if(input$GasEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="GAS"&yearData$Gas>min(input$slider)&yearData$Gas<max(input$slider)))
        
      }
      if(input$BiomassEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="BIOMASS"&yearData$Biomass>min(input$slider)&yearData$Biomass<max(input$slider)))
        
      }
      if(input$WindEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="WIND"&yearData$Wind>min(input$slider)&yearData$Wind<max(input$slider)))
        
      }
      if(input$SolarEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="SOLAR"&yearData$Solar>min(input$slider)&yearData$Solar<max(input$slider)))
        
      }
      if(input$GeothermalEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="GEOTHERMAL"&yearData$Geothermal>min(input$slider)&yearData$Geothermal<max(input$slider)))
        
      }
      if(input$NuclearEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="NUCLEAR"&yearData$Nuclear>min(input$slider)&yearData$Nuclear<max(input$slider)))
        
      }
      if(input$HydroEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="HYDRO"&yearData$Hydro>min(input$slider)&yearData$Hydro<max(input$slider)))
        
      }
      if(input$OtherEnergy4){
        toReturn<- rbind(toReturn,subset(yearData,yearData$Source=="OTHER"&yearData$Others>min(input$slider)&yearData$Others<max(input$slider)))
      }
      if(input$REnergy4){
        toReturn<- rbind(toReturn,subset(yearData,Source %in% c("WIND","BIOMASS","HYDRO","SOLAR","GEOTHERMAL")))
        
      }
      if(input$NREnergy4){
        toReturn<- rbind(toReturn,subset(yearData,Source %in% c("COAL","OIL","GAS","NUCLEAR","OTHER")))
        
      }
     
      
      
      toReturn 
    }
  )
  


  output$plot1 <- renderLeaflet({
    

    reactiveFunc <- myReactiveFunc()
    
    leaflet(data=reactiveFunc)%>%

      addProviderTiles( "Esri.NatGeoWorldMap", group="ESRI") %>%
      #setView(lat= 37.0902 , lng= -95.7129, zoom=20)%>%
      addCircleMarkers(lng= ~Longitude, lat = ~Latitude, color = ~pal(Source),popup = paste("PlantName:", reactiveFunc$Plantname, "<br>",
                                                                                            "Source:", reactiveFunc$Source, "<br>",
                                                                                            "Location:", "Illinois", "<br>",
                                                                                            "Renewable Percent:", paste(reactiveFunc$Renewable,"%"), "<br>",
                                                                                            "Non Renewable Percent:", paste(reactiveFunc$NonRenewable,"%"), "<br>"
                                                                                          
                                                                                            
      ))%>%
      
      
      
      addResetMapButton()%>%
      addLegend("bottomright",
                pal = pal,
                values = reactiveFunc$Source,
                title = "Energy Source",
                opacity = 1)
    


      
   
})
  output$plot2 <- renderLeaflet({
    

    reactiveFunc <- myReactiveFunc2()
    
    leaflet(data=reactiveFunc)%>%

      addProviderTiles(input$MapLayout, group="ESRI") %>%
      #setView(lat= 37.0902 , lng= -95.7129, zoom=20)%>%
      addCircleMarkers(lng= ~Longitude, lat = ~Latitude, color = ~pal(Source),popup = paste("PlantName:", reactiveFunc$Plantname, "<br>",
                                                                                            "Source:", reactiveFunc$Source, "<br>",
                                                                                            "Location:", input$State, "<br>",
                                                                                            "Renewable Percent:", paste(reactiveFunc$Renewable,"%"), "<br>",
                                                                                            "Non Renewable Percent:", paste(reactiveFunc$NonRenewable,"%"), "<br>"
                                                                                            
                                                                                           
                                                                                          
                                                                                            ))%>%
                         

     
      addResetMapButton()%>%
      addLegend("bottomright",
                pal = pal,
                values = reactiveFunc$Source,
                title = "Energy Source",
                opacity = 1)
    
    
    
    
    
  })
  output$plot3 <- renderLeaflet({
    

    reactiveFunc <- myReactiveFunc3()
    
    leaflet(data=reactiveFunc)%>%
      
      addProviderTiles(input$MapLayout1, group="ESRI") %>%
      #setView(lat= 39.9331, lng=-89.3985, zoom=20)%>%
      addCircleMarkers(lng= ~Longitude, lat = ~Latitude, color = ~pal(Source),popup = paste("PlantName:", reactiveFunc$Plantname, "<br>",
                                                                                            "Source:", reactiveFunc$Source, "<br>",
                                                                                            "Location:", input$State, "<br>",
                                                                                            "Renewable Percent:", reactiveFunc$Renewable, "<br>",
                                                                                            "Non Renewable Percent:", reactiveFunc$NonRenewable, "<br>"
                                                                                            
                                                                                            
      ))%>%
      
      addResetMapButton()%>%
    addLegend("bottomright",
              pal = pal,
              values = reactiveFunc$Source,
              title = "Energy Source",
              opacity = 1)
    
    
    
    

  })
  output$plot4 <- renderLeaflet({
    
    
    reactiveFunc <- myReactiveFunc4()
    if(input$State2=="US-TOTAL"){
      myZoom<-4
    }
    else{
      myZoom<-5
    }
    
    leaflet(data=reactiveFunc,options = leafletOptions(
                                                       minZoom = 1, maxZoom = myZoom))%>%
      
      addProviderTiles(input$MapLayout2, group="ESRI") %>%

      addCircleMarkers(lng= ~Longitude, lat = ~Latitude, color = ~pal(Source),popup = paste("PlantName:", reactiveFunc$Plantname, "<br>",
                                                                                            "Source:", reactiveFunc$Source, "<br>",
                                                                                            "Location:", input$State, "<br>",
                                                                                            "Renewable Percent:", reactiveFunc$Renewable, "<br>",
                                                                                            "Non Renewable Percent:", reactiveFunc$NonRenewable, "<br>"
                                                                                            
                                                                                            
      ))%>%
      addResetMapButton()%>%
      addLegend("bottomright",
                pal = pal,
                values = reactiveFunc$Source,
                title = "Energy Source",
                opacity = 1)
   
    
    
    
    
    
  })
   
  output$slider <- renderUI({
    maxG<-0
    allmax2000<- (max(filtData1$Coal, na.rm = TRUE)+max(filtData1$Oil, na.rm = TRUE)+max(filtData1$Gas, na.rm = TRUE)+max(filtData1$Nuclear, na.rm = TRUE)+max(filtData1$Hydro, na.rm = TRUE)
                  +max(filtData1$Biomass, na.rm = TRUE)+max(filtData1$Wind, na.rm = TRUE)+max(filtData1$Solar, na.rm = TRUE)+max(filtData1$Geothermal, na.rm = TRUE)+max(filtData1$Others, na.rm = TRUE))
    allmax2010<- (max(filtData2$Coal, na.rm = TRUE)+max(filtData2$Oil, na.rm = TRUE)+max(filtData2$Gas, na.rm = TRUE)+max(filtData2$Nuclear, na.rm = TRUE)+max(filtData2$Hydro, na.rm = TRUE)
                  +max(filtData2$Biomass, na.rm = TRUE)+max(filtData2$Wind, na.rm = TRUE)+max(filtData2$Solar, na.rm = TRUE)+max(filtData2$Geothermal, na.rm = TRUE)+max(filtData2$Others, na.rm = TRUE))
    allmax2018<- (max(filtData$Coal, na.rm = TRUE)+max(filtData$Oil, na.rm = TRUE)+max(filtData$Gas, na.rm = TRUE)+max(filtData$Nuclear, na.rm = TRUE)+max(filtData$Hydro, na.rm = TRUE)
                  +max(filtData$Biomass, na.rm = TRUE)+max(filtData$Wind, na.rm = TRUE)+max(filtData$Solar, na.rm = TRUE)+max(filtData$Geothermal, na.rm = TRUE)+max(filtData$Others, na.rm = TRUE))
    if(input$Year2==2000){
      if(input$State2=="US-TOTAL"){

        if(input$AllEnergy4){
          maxG<- allmax2000
        }
        if(input$CoalEnergy4){
          maxG <- max(filtData1$Coal, na.rm = TRUE)
        }
        if(input$OilEnergy4){
          maxG <- max(filtData1$Oil, na.rm = TRUE)
        }
        if(input$GasEnergy4){
          maxG <- max(filtData1$Gas, na.rm = TRUE)
        }
        if(input$BiomassEnergy4){
          maxG <- max(filtData1$Biomass, na.rm = TRUE)
        }
        if(input$WindEnergy4){
          maxG <- max(filtData1$Wind, na.rm = TRUE)
        }
        if(input$HydroEnergy4){
          maxG <- max(filtData1$Hydro, na.rm = TRUE)
        }
        if(input$NuclearEnergy4){
          maxG <- max(filtData1$Nuclear, na.rm = TRUE)
        }
        if(input$SolarEnergy4){
          maxG <- max(filtData1$Solar, na.rm = TRUE)
        }
        if(input$GeothermalEnergy4){
          maxG <- max(filtData1$Geothermal, na.rm = TRUE)
        }
        if(input$OtherEnergy4){
          maxG <- max(filtData1$Others, na.rm = TRUE)
        }
        if(input$REnergy4){
          maxG <- allmax2000
        }
        if(input$NREnergy4){
          maxG <- allmax2000
        }

      }
      else{
        stateData<- subset(filtData1,filtData1$PlantStateAbb==input$State2)
        if(input$AllEnergy4){
          maxG<- allmax2000
        }
        if(input$CoalEnergy4){
          maxG <- max(stateData$Coal, na.rm = TRUE)
        }
        if(input$OilEnergy4){
          maxG <- max(stateData$Oil, na.rm = TRUE)
        }
        if(input$GasEnergy4){
          maxG <- max(stateData$Gas, na.rm = TRUE)
        }
        if(input$BiomassEnergy4){
          maxG <- max(stateData$Biomass, na.rm = TRUE)
        }
        if(input$WindEnergy4){
          maxG <- max(stateData$Wind, na.rm = TRUE)
        }
        if(input$HydroEnergy4){
          maxG <- max(stateData$Hydro, na.rm = TRUE)
        }
        if(input$NuclearEnergy4){
          maxG <- max(stateData$Nuclear, na.rm = TRUE)
        }
        if(input$SolarEnergy4){
          maxG <- max(stateData$Solar, na.rm = TRUE)
        }
        if(input$GeothermalEnergy4){
          maxG <- max(stateData$Geothermal, na.rm = TRUE)
        }
        if(input$OtherEnergy4){
          maxG <- max(stateData$Others, na.rm = TRUE)
        }
        if(input$REnergy4){
          maxG <- allmax2000
        }
        if(input$NREnergy4){
          maxG <- allmax2000
        }

      }
    }
    if(input$Year2==2018){
      if(input$State2=="US-TOTAL"){

        if(input$AllEnergy4){
          maxG<- allmax2018
        }
        if(input$CoalEnergy4){
          maxG <- max(filtData$Coal, na.rm = TRUE)
        }
        if(input$OilEnergy4){
          maxG <- max(filtData$Oil, na.rm = TRUE)
        }
        if(input$GasEnergy4){
          maxG <- max(filtData$Gas, na.rm = TRUE)
        }
        if(input$BiomassEnergy4){
          maxG <- max(filtData$Biomass, na.rm = TRUE)
        }
        if(input$WindEnergy4){
          maxG <- max(filtData$Wind, na.rm = TRUE)
        }
        if(input$HydroEnergy4){
          maxG <- max(filtData$Hydro, na.rm = TRUE)
        }
        if(input$NuclearEnergy4){
          maxG <- max(filtData$Nuclear, na.rm = TRUE)
        }
        if(input$SolarEnergy4){
          maxG <- max(filtData$Solar, na.rm = TRUE)
        }
        if(input$GeothermalEnergy4){
          maxG <- max(filtData$Geothermal, na.rm = TRUE)
        }
        if(input$OtherEnergy4){
          maxG <- max(filtData$Others, na.rm = TRUE)
        }

      }
      else{
        stateData<- subset(filtData,filtData$PlantStateAbb==input$State2)
        if(input$AllEnergy4){
          maxG<- allmax2000
        }
        if(input$CoalEnergy4){
          maxG <- max(stateData$Coal, na.rm = TRUE)
        }
        if(input$OilEnergy4){
          maxG <- max(stateData$Oil, na.rm = TRUE)
        }
        if(input$GasEnergy4){
          maxG <- max(stateData$Gas, na.rm = TRUE)
        }
        if(input$BiomassEnergy4){
          maxG <- max(stateData$Biomass, na.rm = TRUE)
        }
        if(input$WindEnergy4){
          maxG <- max(stateData$Wind, na.rm = TRUE)
        }
        if(input$HydroEnergy4){
          maxG <- max(stateData$Hydro, na.rm = TRUE)
        }
        if(input$NuclearEnergy4){
          maxG <- max(stateData$Nuclear, na.rm = TRUE)
        }
        if(input$SolarEnergy4){
          maxG <- max(stateData$Solar, na.rm = TRUE)
        }
        if(input$GeothermalEnergy4){
          maxG <- max(stateData$Geothermal, na.rm = TRUE)
        }
        if(input$OtherEnergy4){
          maxG <- max(stateData$Others, na.rm = TRUE)
        }
        if(input$REnergy4){
          maxG <- allmax2018
        }
        if(input$NREnergy4){
          maxG <- allmax2018
        }

      }
    }
    if(input$Year2==2010){
      if(input$State2=="US-TOTAL"){

        if(input$AllEnergy4){
          maxG<- allmax2010
        }
        if(input$CoalEnergy4){
          maxG <- max(filtData2$Coal, na.rm = TRUE)
        }
        if(input$OilEnergy4){
          maxG <- max(filtData2$Oil, na.rm = TRUE)
        }
        if(input$GasEnergy4){
          maxG <- max(filtData2$Gas, na.rm = TRUE)
        }
        if(input$BiomassEnergy4){
          maxG <- max(filtData2$Biomass, na.rm = TRUE)
        }
        if(input$WindEnergy4){
          maxG <- max(filtData2$Wind, na.rm = TRUE)
        }
        if(input$HydroEnergy4){
          maxG <- max(filtData2$Hydro, na.rm = TRUE)
        }
        if(input$NuclearEnergy4){
          maxG <- max(filtData2$Nuclear, na.rm = TRUE)
        }
        if(input$SolarEnergy4){
          maxG <- max(filtData2$Solar, na.rm = TRUE)
        }
        if(input$GeothermalEnergy4){
          maxG <- max(filtData2$Geothermal, na.rm = TRUE)
        }
        if(input$OtherEnergy4){
          maxG <- max(filtData2$Others, na.rm = TRUE)
        }

      }
      else{
        stateData<- subset(filtData2,filtData2$PlantStateAbb==input$State2)
        if(input$AllEnergy4){
          maxG<- allmax2010
        }
        if(input$CoalEnergy4){
          maxG <- max(stateData$Coal, na.rm = TRUE)
        }
        if(input$OilEnergy4){
          maxG <- max(stateData$Oil, na.rm = TRUE)
        }
        if(input$GasEnergy4){
          maxG <- max(stateData$Gas, na.rm = TRUE)
        }
        if(input$BiomassEnergy4){
          maxG <- max(stateData$Biomass, na.rm = TRUE)
        }
        if(input$WindEnergy4){
          maxG <- max(stateData$Wind, na.rm = TRUE)
        }
        if(input$HydroEnergy4){
          maxG <- max(stateData$Hydro, na.rm = TRUE)
        }
        if(input$NuclearEnergy4){
          maxG <- max(stateData$Nuclear, na.rm = TRUE)
        }
        if(input$SolarEnergy4){
          maxG <- max(stateData$Solar, na.rm = TRUE)
        }
        if(input$GeothermalEnergy4){
          maxG <- max(stateData$Geothermal, na.rm = TRUE)
        }
        if(input$OtherEnergy4){
          maxG <- max(stateData$Others, na.rm = TRUE)
        }
        if(input$REnergy4){
          maxG <- allmax2010
        }
        if(input$NREnergy4){
          maxG <- allmax2010
        }

      }
    }

   
      

    
    sliderInput("slider","", min   = 0,
                max   = maxG,
                value = c(0,maxG))
   
  })
  

}
# Run the application 
shinyApp(ui = ui, server = server)
