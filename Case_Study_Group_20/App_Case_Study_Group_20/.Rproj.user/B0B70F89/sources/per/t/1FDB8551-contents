#### Importing the required packages ####

if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}

if(!require(magrittr)){
  install.packages("magrittr")
  require(magrittr)
}

if(!require(scales)){
  install.packages("scales")
  require(scales)
}

if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

if(!require(leaflet)){
  install.packages("leaflet")
  require(leaflet)
}

if(!require(leaflet.extras)){
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}

#### Loading the final RData file ####
# May return an error when run manually so click run app button
load("Final_Data_Group_20_original.RData")

# Preparing the icons for the heatmap  

car_icon <- makeIcon(
  iconUrl = "Additional_Files_Group_20/car.png",
  iconWidth = 24, 
  iconHeight = 24,
  iconAnchorX = 12, 
  iconAnchorY =12
)

#### Writing the UI function ####
#Reihenfolge etc. ändern!!!
ui <- fluidPage(

  fluidRow(
    
    titlePanel("Heatmap: Ratio of diesel cars in each municipality"),
    
    fluidRow( 
      
      column(12,
             
             # This will create a space for us to display our map
             leafletOutput(outputId = "heatmap", height = 600)
             
      )
    ),
    
    # Including a date range input button to allow users to define time ranges.
    column(4,
           dateInput(
             "banDate", 
             "Ban date: ", 
             min = "2009-01-01", 
             max = "2016-12-31", #besser dynamisch als maximum und minumum werte im Datensatz
             format = "dd/mm/yyyy", 
             startview = "year",
             value = "2012-01-31")
    ),
    
    # Including a select input button to allow users to filter by zip code.
    column(4,
           selectInput("zipCode", 
                       "Select a zip-code: ", 
                       choices = sort((unique(complete_information$Postleitzahl))))
    ),
    
    column(4,
           selectInput("regionCode", 
                       "Select a region-code: ", 
                       choices = append("NONE", sort(unique(complete_information$Region))))
    
    )
  ),
  
  fluidRow(
    br(),
    
    # Displaying the vehicles whose owners need are affected by the ban
    column(12, 
           plotOutput('affected')
    )
  ),
  
  fluidRow(
    
    br(),
    
    # Displaying the table of affected cars
    column(12, 
           h4(strong("Available data on cars in all municipalities"), align = "center"),
           dataTableOutput('table_diesel_vehicles'))
  ),
  
  br(), 
  hr(), 
  br(),

  plotOutput(outputId = "plot")
)

#### Writing the server function ####
server <- function(input, output) {
  
  # Filtering the data based on input (reactive)
  filter_data <- reactive({
  
    #Either filter by zip code or region
  if(input$regionCode == "NONE"){ #No Region chosen
    complete_information %>%
      filter(Zulassung < input$banDate) %>%
        filter(!(Postleitzahl == input$zipCode & ID_Motor == "Diesel")) %>%
          mutate(Anteil_Diesel = sum(ID_Motor == "Diesel")/NROW(ID_Motor), Gesamt_Autos = NROW(ID_Motor))
  }
    else {
      complete_information %>%
        ungroup() %>%
          group_by(Region) %>%
            filter(Zulassung < input$banDate) %>%
              filter(!(Region == input$regionCode & ID_Motor == "Diesel")) %>%
                mutate(Anteil_Diesel = sum(ID_Motor == "Diesel")/NROW(ID_Motor), Gesamt_Autos = NROW(ID_Motor))
     }
  })
  
  # Render plot -> ändern so dass der Graph mit betroffenen Personen angezeigt wird!
  output$plot <- renderPlot({
    
    # Call reactive function to filter data based on input
    select_data <- filter_data()
    
    # Displaying a bar plot because our only variable, x(date), is discrete
    ggplot(select_data, aes(x = Zulassung)) + 
      geom_bar(color = "indianred1", fill = "indianred1") + 
      
      # Labelling x and y axes
      xlab("Date") +
      ylab("Number of Diesel vehicles") +
      
      # Adding a plot title
      ggtitle("Registration of diesel cars in chosen municipality: ", input$zipCode) +
        theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
      
      # Formatting date labels on x axis
      scale_x_date( 
        date_breaks = "1 month",
        date_labels = "%b %y",
        date_minor_breaks = "1 week"
      ) +
      
      # Adjusting the y axis breaks to show only integers since y is a count
      scale_y_continuous(
        breaks = pretty_breaks()
      ) +
      
      # Rotating the x axis labels so they're not too crowded
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Creating the table of data about all diesel vehicles
  output$table_diesel_vehicles <- renderDataTable(complete_information)
  
  # Create the heat map
  # Filter aut the data from filter_data (Everything after the ban date + 
  #everything before the ban date in the ban location that is a diesel)
  
  output$heatmap <- renderLeaflet({
    
      Heatmap_Data <- filter_data()[match(
      unique(filter_data()$Gemeinden), 
      filter_data()$Gemeinden),]
    
    leaflet(data = Heatmap_Data) %>% 
      
      # Setting the view over Germany
      setView(lng = 10.3, lat = 51.01, zoom = 6) %>% 
      
      addTiles() %>%
      
      # Setting up the heatmap with intensity according to ratio of diesel to total cars
      addHeatmap(
        lng = Heatmap_Data$Laengengrad,
        group ='heatmap', 
        lat = Heatmap_Data$Breitengrad, 
        intensity = Heatmap_Data$Anteil_Diesel, #Hier den Anteil Benziner/Diesel anzeigen
        blur =  10, 
        max = 10, 
        radius = 18
      ) %>%
      
      # Adding popups for each municipality with details of the registered cars in that city
      addMarkers(
        leaflet(data = Heatmap_Data),
        group = 'detail',
        lng = Heatmap_Data$Laengengrad,
        lat = Heatmap_Data$Breitengrad,
        popup = paste(
          "<b>Municipality: ",
          Heatmap_Data$Gemeinden,
          '</b><br/>',
          "Number of petrol cars in municipality: ",
          round((1 - Heatmap_Data$Anteil_Diesel)*Heatmap_Data$Gesamt_Autos, 0),
        '</b><br/>',
        "Number of diesel cars in municipality: ",
        round(Heatmap_Data$Anteil_Diesel*Heatmap_Data$Gesamt_Autos, 0)),
        icon = car_icon
        ) %>%
      
      groupOptions("detail", zoomLevels = 8:18) %>%
      groupOptions("heatmap", zoomLevels = 5:7)
  })
  
  output$affected <- renderPlot({

    plot_data <- complete_information %>%
        ungroup() %>%
          group_by(Region)%>%
            filter(Zulassung <= input$banDate)%>%
              filter(ID_Motor == "Diesel")%>%
                filter(Region == input$regionCode)%>%
                  ggplot(aes(x = Region)) + geom_bar() + ggtitle("Affected car owners in ban region: ", input$regionCode)
    
    plot_data
    
  })
}

#### Run Shiny App ####
shinyApp(ui, server)
