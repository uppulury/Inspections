library(shiny)
library(ggplot2)
library(dplyr)
library(ggmap)

FI <- read.csv("Food_Inspections.csv",stringsAsFactors = FALSE)
FI$Year = format(as.Date(FI[,11],"%m/%d/%Y"),"%Y")

Z_Low = FI[FI$Risk=='Risk 3 (Low)',]
Z_Medium = FI[FI$Risk=='Risk 2 (Medium)',]
Z_High = FI[FI$Risk=='Risk 1 (High)',]

ui <- fluidPage(
  titlePanel("Food Inspection Results"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("InspectionIDInput", "InspectionID", 0, 2312647, c(44247,2312647), pre = "$"),
      radioButtons("riskInput", "Risk Meter",
                   choices = c("Risk 1 (High)","Risk 2 (Medium)","Risk 3 (Low)","All","Year"),
                   selected = "Risk 1 (High)"),
      radioButtons("YearInput", "Year Of Inspection",
                   choices = c(2010:2019)),
      radioButtons("ResultInput","Final Result",
                   choices = c("Pass","Fail","Pass w/ conditions","Not Ready", "Out of Business", 
                               "No Entry","Business Not Located")),
      radioButtons("FacilityInput","Facility Type", choices = c("Restaurant","Grocery Store","School"))
    ),
    mainPanel(
      plotOutput("Figure1"),
      br(), br(),
      tabsetPanel(
        tabPanel(DT::dataTableOutput("Table1"))
      ),
      br(),br(),
      plotOutput("Figure2"),
      br(), br(),
      tabsetPanel(
        tabPanel(DT::dataTableOutput("Table2"))
      ),
      
      # plotOutput("Figure3"),
      br(), br(),
      
      plotOutput("Figure4"),
      br(), br(),
      
      plotOutput("Figure5")
      
    )
  )
)