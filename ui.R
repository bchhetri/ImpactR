#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)
#install.packages("devtools")
#require(devtools)
#install_github('ramnathv/rCharts')
library(rCharts)
library(plyr)
library(leaflet)

dec.lab <- paste0(seq(2020, 2080, by=20), "s")
climdat<-read.csv("locationA.csv",header = T)#this the future climate file prepped

#read in disease and location name data
disease_names <- data.frame(x=c("Cryptosporidiosis","Giardiasis","Cryptosporidiosis+Giardiasis"))

shinyUI(navbarPage( "ImpactR Tool", align = "right",
##DATA UPLOAD TAB #### 
  tabPanel("1.Data Upload",
    sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',', Semicolon=';',Tab='\t'),','),
      radioButtons('quote', 'Quote', c(None='',
                     'Double Quote'='"','Single Quote'="'"),
                   '"')),
    mainPanel(
        DT::dataTableOutput('ex1')
    ))),
##EXPLORATORY DATA ANALYSIS TAB- DISEASE DATA AND WEATHER DATA####
  tabPanel("2. Historical Data Patterns",
          # Application title
          titlePanel(h5("")), 
                fluidRow(column(4, wellPanel( 
                      selectInput('disease_name', 'Disease Name', 
                      as.character(levels(disease_names$x)), selected="Cryptosporidiosis"),
                      dateRangeInput('years','Choose date range', start= "2008-01-01", 
                      end=Sys.Date(),min = "2008-01-01", max=Sys.Date() ),
                              
                #A line break to make the interface clearer 
                br(),
                sliderInput("range", label = "Smoothing (degrees of freedom):",min = 0, max = 50,value = c(0, 10)),
                br(),
                radioButtons("plotty", "Plot Type",
                               c("Weekly data" = "week","Weekly data by year" = "weeky",
                                 "Weekly data by month" = "mon"), selected="week"))),
                      # The second column houses the plot(s) of the data that was selected.  
                     #These plots are defined in the server.R file.
                            column(8, plotOutput('plot1'),
                              plotOutput('plotweather'),
                              plotOutput('plotturbidity'),
                              plotOutput('plotcdd')))
                              ),
### EXPLORATORY DATA ANALYSIS TAB- WATER QUALITY####
  tabPanel("3. Outcome-Exposure Relationships",
      # Application title
        titlePanel(h5("Choose variables and Lags")), 
        fluidRow(column(4, wellPanel( 
                      selectInput('var1', 'Variable 1: X-axis', 
                      c("Turbidity","Cysts/Oocyst counts","Precipitation"), selected="Precipitation"),
                      selectInput('var2', 'Variable 2: Y-axis', 
                      c("Turbidity","Cysts/Oocyst counts","Precipitation"), selected="Turbidity"),
                      dateRangeInput('yearsq','Choose date range', start= "2008-01-01", 
                      end=Sys.Date(),min = "2008-01-01", max=Sys.Date() ),
                #A line break to make the interface clearer 
                  br(),
                radioButtons("plottyq", "Lags",
                               c("Same week" = "A","One week" = "B","Two weeks" = "C","Three weeks" = "D","Four weeks" = "E","Five weeks"="F","Six Weeks"="G"), selected="A"))),
                      # The second column houses the plot(s) of the data that was selected.  
                     #These plots are defined in the server.R file.
                            column(8, plotOutput('plotturbidity1'),
                              plotOutput('plotdlnm'),
                              plotOutput('plotimpact')
                              ))
                              ),
  
  #######  
### CLIMATE DATA VISUALIZATION TAB####
  tabPanel("4. Future Climate Data Patterns",
      # Application title
        titlePanel(""), 
        fluidRow(
				column(4, selectInput("variable", "Climate Variable", 
				  c("Temperature", "Precipitation"), selected="Precipitation", multiple=F, width="100%")),
				column(4, selectInput("dec", "Decades", dec.lab, selected=dec.lab[c(1,2,3,4)], 
				  multiple=TRUE, width="100%"))
			,
				column(4, selectInput("rcp", "RCP", c("4.5 (low)", "6.0 (medium)", "8.5 (high)"),
				  selected="8.5 (high)", multiple=F, width="100%"))),
  #fluidRow(
    #column(8, leafletOutput("Map"))),
    fluidRow(
	column(8,
		showOutput("Chart1", "highcharts"),
	  showOutput("Chart2", "highcharts"),
		HTML('<style>.rChart {width: 100%; height: "auto"}</style>')
	)
	)),
  
#####Impacts Tab####
  tabPanel("5. Future Impacts",
      # Application title
        titlePanel(h5("The impact of future precipitation on the burden of Cryptosporidiosis and Giardiasis - based on RCP 8.5 ")), 
                  fluidRow(column(4, selectInput("dec", "Decades", dec.lab, selected=dec.lab[c(4)], 
				  multiple=TRUE, width="100%")),
                    column(4, selectInput("rcp", "RCP", c("4.5 (low)", "6.0 (medium)", "8.5 (high)"),
				  selected="8.5 (high)", multiple=F, width="100%"))),
	   column(8, plotOutput('pif')
                              ))
                              ,
  ######

  tabPanel("More Information",   # Information about data collection.
          "This Shiny R Tool called ImpactR was developed for and submitted to the PHAC. Please refer to help pages",a("here ", href="http://"),"to learn about how to use this tool. An example dataset and walkthrough is packaged with this tool.",
            br(),
            br(),
            "Please visit", 
                            a("this site ", href="http://"),
                            "and links to project publications therein to learn more about the underlying methods to evaluate the relationship between climate change and waterborne illness. ",
                            br(),
                            br(),
                            a("See the R code used to create this tool", href="https://"),
                            br(),
                            br(),
                            "Any questions or comments can be sent to",
                            br(),
                            "Bimal Chhetri: " ,
                            a("bkchhetr@sfu.ca", href="mailto:bkchhetr@sfu.ca"),
    br(),
    br(),
    "Team Members:",
    br(),
    "ABC",
    br(),
    "DEF",
    br(),
    "GHI",
    br(),
    "JKL",
    br(),
    br(),
    column(4,
   img(src='SFU.gif', align = "left",width="90%")),
      column(8,
   img(src='logo.jpg', align = "right",width="30%"))
  ### the rest of your code
  )
    
                            
                   
))