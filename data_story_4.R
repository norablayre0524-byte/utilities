setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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

View(fall2025)

fall2025<- fall2025%>%
  mutate('gal_per_capita'=gallons/occupancy)
dorm2025<-fall2025%>%
  filter(building %in% c('Ayres Hall','Benedict Hall','Cannon Hall','Cleveland Hall','Elliott Hall','Gorgas Hall','Hodgson Hall','Hoffman Hall',
  'Humphreys Hall','Johnson Hall','Phillips Hall','Quintard Hall','Smith Hall','Trezevant Hall','Tuckaway Hall'))
View(dorm2025)

gender_dorm2025<-dorm2025%>%
  group_by(coed)

ggplot(gender_dorm2025,aes(
  x=coed,
  y=gal_per_capita,
  fill=coed
))+
  geom_col()

View(split_creek)
View(sewanee_temp)

ggplot(split_creek, aes(
  x=air_temp,
  y=rain_in
))+
  geom_path()

# Sidebar with a slider input for number of bins 
sidebarLayout(
  sidebarPanel(
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}
