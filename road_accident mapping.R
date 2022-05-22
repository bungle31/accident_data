library(readr)
library(leaflet)
library(tidyverse)
library(lubridate)
library(DT)

if(exists("accident_data")&&is.data.frame(get("accident_data"))==FALSE){
accident_data <- read_csv("C:/Users/danha/OneDrive/R Studio Libraries/road accidents/data/dft-road-casualty-statistics-accident-last-5-years.csv",
                          col_types = list(date = col_date(format="%d/%m/%Y")))
}


year <- 2020
start_date <- "2020-01-01"
end_date <- "2020-12-31"
region <- "Norfolk"
region_type <- "police_force"

if(exists("reference_data")&&is.data.frame(get("reference_data"))==FALSE){
reference_data <- read_csv("C:/Users/danha/OneDrive/R Studio Libraries/road accidents/data/Road-Safety-Open-Dataset-Data-Guide.csv")
}


county_id <- reference_data$`code/format`[reference_data$`field name` == region_type & reference_data$label == region]
accident_severity <- reference_data$label[reference_data$`field name` == 'accident_severity']
accident_weather <- reference_data$label[reference_data$`field name` == 'weather_conditions']

accident_data_slice <- accident_data %>% 
  filter(police_force == county_id) %>% 
  #filter(accident_year == year)
  filter(date > start_date) %>% 
  filter(date < end_date)

#rm(accident_data)

#casualty_data <- read.csv("C:/Users/danha/OneDrive/R Studio Libraries/road accidents/data/dft-road-casualty-statistics-casualty-last-5-years.csv")

colours <- c('red','orange','grey')

fatal_accidents <- accident_data_slice %>% 
  filter(accident_severity == 1)
serious_accidents <- accident_data_slice %>% 
  filter(accident_severity == 2)
slight_accidents <- accident_data_slice %>% 
  filter(accident_severity == 3)

map_legend_labels <- c(
  paste0(accident_severity[1]," accidents : ",nrow(fatal_accidents)),
  paste0(accident_severity[2]," accidents : ",nrow(serious_accidents)),
  paste0(accident_severity[3]," accidents : ",nrow(slight_accidents)))

library(htmlwidgets)
library(htmltools)

map_title <- tags$div(
  HTML(paste0('<h2>',"Road Traffic Accident Summary for ",region," between ",start_date," & ",end_date,"</h2>"))
)  

map_reference <- tags$div(
  HTML("<p>Dot Size = Causualties x Severity</p><br><p>Data from <a link='https://www.gov.uk/government/collections/road-accidents-and-safety-statistics'>UK Gov Data Source</p>")
)  

map_popup <- paste0(
  'Date: ', accident_data_slice$date,'<br>',
  "Time of Day: ",accident_data_slice$time, '<br>', 
  "Severity: ", accident_severity[accident_data_slice$accident_severity],'<br>',
  "Casualties: ",accident_data_slice$number_of_casualties,'<br>',
  "Number of Vehicles: ", accident_data_slice$number_of_vehicles,'<br>',
  "Road Number: ",accident_data_slice$first_road_number,'<br>',
  "Speed Limit: ", accident_data_slice$speed_limit,'<br>',
  "Weather Conditions: ",accident_weather[accident_data_slice$weather_conditions]
  )

map <- leaflet() %>% 
  
  #addTiles() %>% 
  
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  #addProviderTiles(providers$Stamen.Toner) %>% 
  addCircleMarkers(lng = as.numeric(accident_data_slice$longitude[accident_data_slice$accident_severity == 3]),
                   lat = as.numeric(accident_data_slice$latitude[accident_data_slice$accident_severity == 3]),
                   color = colours[accident_data_slice$accident_severity],
                   popup = map_popup,
                   radius = (-1+(accident_data_slice$number_of_casualties*(accident_data_slice$accident_severity^-1))*5)) %>% 
  
  addCircleMarkers(lng = as.numeric(accident_data_slice$longitude[accident_data_slice$accident_severity == 2]),
                   lat = as.numeric(accident_data_slice$latitude[accident_data_slice$accident_severity == 2]),
                   color = colours[accident_data_slice$accident_severity],
                   popup = map_popup,
                   radius = (-1+(accident_data_slice$number_of_casualties*(accident_data_slice$accident_severity^-1))*5)) %>% 
  
  addCircleMarkers(lng = as.numeric(accident_data_slice$longitude[accident_data_slice$accident_severity == 1]),
                   lat = as.numeric(accident_data_slice$latitude[accident_data_slice$accident_severity == 1]),
                   color = colours[accident_data_slice$accident_severity],
                   popup = map_popup,
                   radius = (-1+(accident_data_slice$number_of_casualties*(accident_data_slice$accident_severity^-1))*5)) %>% 
  
  
  addLegend("topright",colors = colours,labels = map_legend_labels,title = paste0('<em>',"Accident Severity & Count","</em>"),) %>% 
  addControl(map_title, position = "bottomleft") %>% 
  addControl(map_reference,position = "bottomright")

map

#by_day_of_week <- accident_data_slice %>% 
#  group_by(day_of_week,accident_severity) %>% 
#  summarise(count = n())
# 
# ggplot(data = by_day_of_week,aes(x=day_of_week, y=count))+
#   geom_point(aes(color=accident_severity))
