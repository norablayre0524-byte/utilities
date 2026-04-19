setwd("C:/Users/norab/repos/utilities")
library(dplyr)
library(ggplot2)
library(readr)

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))
# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail

# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail

# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail


fall2025<- fall2025%>%
  mutate('gal_per_capita'=gallons/occupancy)
dorm2025<-fall2025%>%
  filter(building %in% c('Ayres Hall','Benedict Hall','Cannon Hall','Cleveland Hall','Elliott Hall','Gorgas Hall','Hodgson Hall','Hoffman Hall',
                         'Humphreys Hall','Johnson Hall','Phillips Hall','Quintard Hall','Smith Hall','Trezevant Hall','Tuckaway Hall'))

gender_dorm2025<-dorm2025%>%
  group_by(coed)

library(shiny)

######################################
######################################
######################################

ui <- fluidPage(

    titlePanel("Water Use By Dorm Gender"),
    br(),
    
    tabsetPanel(
      tabPanel('Dorms',
    
    fluidRow(column(4, sliderInput(inputId='months',
                                   label='select months by number',
                                   min= min(dorm2025$month),
                                   max= max(dorm2025$month),
                                   value= range(dorm2025$month))),
             column(4, selectInput(inputId='gender',
                                   label= 'select the dorms by gender',
                                   choices= unique(dorm2025$coed),
                                   selected= 'Female')),
             column(4, 'water,costs,etc go here')),
    br(),
    br(),
    
    fluidRow(column(10, plotOutput("dorm2025plot")))
    
      ),
    tabPanel('Academic Buildings',
             fluidRow(column(12, 'big data table')))
    )
    


)

######################################
######################################
######################################

server <- function(input, output) {

    output$dorm2025plot <- renderPlot({
       
      ggplot(gender_dorm2025,aes(
        x=coed,
        y=gal_per_capita,
        fill=coed
      ))+
        geom_col()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
