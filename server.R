

source("dataprocessing.R")

source("model.R")



#FINAL COMPLETE SHINY APP
library(shiny)
library(leaflet)
library(dplyr)
library(caret)



ui <- navbarPage("Traffic Accident Prediction and Visualization",
                 
                 tabPanel("Heatmap",
                          sidebarLayout(
                            sidebarPanel(
                              p("This interactive heatmap displays days and times of accidents that occurred
                                within Washtenaw County in the year 2023. Accidents can be 
                                filtered by the available variables below."),
                              br(), 
                              h4("Heatmap:"),
                              selectInput("heatmap_var", "Select Variable:",
                                          choices = c(
                                            "Older Driver (>75)" = "Older_Driver",
                                            "Young Driver (<18)" = "Young_Driver",
                                            "Drinking" = "Drinking",
                                            "Drug Use" = "Drug_Use",
                                            "Pedestrian Involved" = "Pedestrian"
                                          )),
                              
                              selectInput("heatmap_value", "Select Value:",
                                          choices = c(1, 0))
                            ),
                            mainPanel(
                              plotOutput("heatmap_plot")
                            )
                          )
                 ),
                 #interactive map UI
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              p("This interactive map displays accident
                                locations within Washtenaw County that
                                occurred in the year 2023. Accidents can be 
                                filtered by the available variables below."),
                              br(), 
                              h4("Interactive Map:"),
                              
                              selectInput("interactive_map_var", "Select Variable:",
                                          choices = c(
                                            "Older Driver (>75)" = "Older_Driver",
                                            "Young Driver (<18)" = "Young_Driver",
                                            "Drinking" = "Drinking",
                                            "Drug Use" = "Drug_Use",
                                            "Pedestrian Involved" = "Pedestrian"
                                          )),
                              selectInput("interactive_map_value", "Select Value:",
                                          choices = c(1, 0))
                            ),
                            mainPanel(
                              leafletOutput("interactive_map")
                            )
                          )
                 ),
                 
                 #unsafe drier accident prediction UI
                 tabPanel("Unsafe Driver Accident Predictions", 
                          sidebarLayout(
                            sidebarPanel(
                              
                              p("This map displays predicted likelihoods of an 
                                accident being caused by an unsafe driver based 
                                on time of day, day of week, approximate location,
                                and worst injury in crash. This map has a low predictive value and an AUC of only 0.603."),
                              br(), 
                              h4("Prediction Map:")
                            ),
                            mainPanel(
                              leafletOutput("glm_map")
                            )
                          )
                 )
                
                 
)

#------------------------------------------------
# server logic
server <- function(input, output, session) {
  
  # reactive data for the heatmap page
  reactive_data <- reactive({
   #filtering reactive
    heatmap_data <- heatmap_subdf %>%
      filter(.data[[input$heatmap_var]] == input$heatmap_value)
    
  
    heatmap_agg <- as.data.frame(table(heatmap_data$Time_of_Day, heatmap_data$Day_of_Week))
    colnames(heatmap_agg) <- c("Time_of_Day", "Day_of_Week", "Count")
  
    time_order <- c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00 AM", 
                    "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", 
                    "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", 
                    "9:00 PM", "10:00 PM", "11:00 PM")
    heatmap_agg$Time_of_Day <- factor(heatmap_agg$Time_of_Day, levels = time_order)
    
    day_order <- c("Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday", "Sunday")
    heatmap_agg$Day_of_Week <- factor(heatmap_agg$Day_of_Week, levels = day_order)
    
    return(heatmap_agg)
  })
  
  # Reactive data for the interactive map 
  reactive_data_map <- reactive({
   
    interactive_map_data <- heatmap_subdf %>%
      filter(.data[[input$interactive_map_var]] == input$interactive_map_value)
    
    interactive_map_data %>%
      filter(Crash_Latitude >= lat_min & Crash_Latitude <= lat_max,
             Crash_Longitude >= lon_min & Crash_Longitude <= lon_max)
    
  })
  
  # Render the heatmap plot
  output$heatmap_plot <- renderPlot({
    heatmap_agg <- reactive_data()
    
    ggplot(heatmap_agg, aes(x = Time_of_Day, y = Day_of_Week, fill = Count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(
        title = paste("Heatmap for", input$heatmap_var, "=", input$heatmap_value),
        x = "Time of Day",
        y = "Day of Week",
        fill = "Count"
      ) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  })
  
  
  # Render the map
  output$interactive_map <- renderLeaflet({
    map_data_df <- reactive_data_map()
    
    leaflet(map_data_df) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      addCircleMarkers(~Crash_Longitude, ~Crash_Latitude, radius = 5, color = "red", opacity = 0.7,
                       popup = ~paste("Time: ", Time_of_Day, "<br>Location: ", Day_of_Week))  
  })
  
  # Render the map with predicted accident likelihoods using glm
  output$glm_map <- renderLeaflet({
    map_obj <- leaflet(map_subdf_2) %>%
      addTiles() %>%  
      addCircleMarkers(
        ~Crash_Longitude, ~Crash_Latitude,  
        color = ~colorNumeric(palette = "YlOrRd", domain = map_subdf_2$Unsafe_Driver_Likelihood)(Unsafe_Driver_Likelihood),
        radius = 5,             
        fillOpacity = 0.6,      
        popup = ~paste("Likelihood: ", round(Unsafe_Driver_Likelihood, 2))  
      )
    
    map_obj
  })
  


}


shinyApp(ui, server)
