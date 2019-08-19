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
load("Final_Data_Group_20_original.RData")

#Car-Icons as a symbol for the municipalities
car_icon <- makeIcon(
  iconUrl = "Additional_Files_Group_20/car.png",
  iconWidth = 24, 
  iconHeight = 24,
  iconAnchorX = 12, 
  iconAnchorY =12
)

#### Create Layout #### 
ui <- fluidPage(

  fluidRow(
    
    titlePanel("Heatmap: Ratio of diesel cars in each municipality"),
    
    fluidRow( 
      
      #Output space in which to plot the map
      column(12, #Size
             leafletOutput(outputId = "heatmap", height = 600)
      )
    ),
    
    #Include an input field to let users define the banDate
    column(4,
           dateInput(
             "banDate", 
             "Ban date: ", 
             min = min(complete_information$Zulassung), 
             max = max(complete_information$Zulassung), #besser dynamisch als maximum und minumum werte im Datensatz
             format = "dd/mm/yyyy", 
             startview = "year",
             value = "2012-01-31")
    ),
    
    # Include a selector to let users to chose the ban zip code
    column(4,
           selectInput("zipCode", 
                       "Select a zip-code: ", 
                       choices = sort((unique(complete_information$Postleitzahl))))
    ),
    
    # Include a selector to let users to chose the ban region
    column(4,
           selectInput("regionCode", 
                       "Select a region-code: ", 
                       choices = append("NONE", sort(unique(complete_information$Region))),
                       selected = "01")
    )
  ),
  
  fluidRow(
    br(),
    
    # Output space for the affected car owner plot
    column(12, 
           plotOutput('affected')
    )
  ),
  
  fluidRow(
    
    br(),
    
    # Output space for the complete information table
    column(12, 
           h4(strong("Available data on cars in all municipalities"), align = "center"),
           dataTableOutput('table_diesel_vehicles'))
  ),
  
  br(), 
  hr(), 
  br(),

  fluidRow(
  
  #Output space for the diesel registration plot
  plotOutput(outputId = "registration_plot")
  )
)

#### Server function - calculations, plot creation, etc ####
server <- function(input, output) {
  
  # Filtering the data based on input (reactive)
  filter_data <- reactive({
  
    #Either filter by zip code or region
  if(input$regionCode == "NONE"){ #No Region chosen
    complete_information %>%
      filter(Zulassung < input$banDate) %>%
        filter(!(Postleitzahl == input$zipCode & ID_Motor == "Diesel")) %>%
      #Add columns for the total number of cars and the percentage of diesel cars in region
          mutate(Anteil_Diesel = sum(ID_Motor == "Diesel")/NROW(ID_Motor), Gesamt_Autos = NROW(ID_Motor))
  }
    else {
      complete_information %>%
        ungroup() %>%
          group_by(Region) %>%
            filter(Zulassung < input$banDate) %>%
              filter(!(Region == input$regionCode & ID_Motor == "Diesel")) %>%
        #Add columns for the total number of cars and the percentage of diesel cars in region
                mutate(Anteil_Diesel = sum(ID_Motor == "Diesel")/NROW(ID_Motor), Gesamt_Autos = NROW(ID_Motor))
     }
  })
  
  # Render plot -> ändern so dass der Graph mit betroffenen Personen angezeigt wird!
  output$registration_plot <- renderPlot({
    
    #rename for simplicity reason
    select_data <- filter_data()
    
    # Displaying a bar plot the discrete variable date
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
      
      # Rotating the x axis labels so they're not too crowded for large date ranges
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Creating the table of data about all diesel vehicles
  output$table_diesel_vehicles <- renderDataTable(complete_information)
  
  # Create the heat map
  output$heatmap <- renderLeaflet({
    
    #Reduce data to one entry per municipality, since all relevant column entries are equal for the same municipality
      Heatmap_Data <- filter_data()[match(
      unique(filter_data()$Gemeinden), 
      filter_data()$Gemeinden),]
    
    leaflet(data = Heatmap_Data) %>% 
      
      # Setting the default view over Germany
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
      
      # Adding popups for each municipality with details about the registered cars in that city
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
      
      groupOptions("detail", zoomLevels = 8:18) %>% #shows the markes on this zoom level
      groupOptions("heatmap", zoomLevels = 6:7) #shows the heatmap on this zoom level
  })
  
  #Plot of the affected car owners - Sollte abgeändert/ergänzt werden, Verhältnisplots etc. wie besprochen
  output$affected <- renderPlot({

    #Filter the diesel cars in the ban region
    
    region_cars <- complete_information %>%
      ungroup() %>%
        group_by(Region)%>%
          filter(Zulassung <= input$banDate)%>%
              filter(Region == input$regionCode)%>%
                mutate(Anzahl_Diesel = sum(ID_Motor == "Diesel"), Anzahl_Benzin = sum(ID_Motor == "Benzin"))%>%
                  select(Anzahl_Diesel, Anzahl_Benzin)
    
    small_cars_df <- data.frame(
      Motor = c("Diesel", "Benzin"),
      Number = c(region_cars$Anzahl_Diesel[1], region_cars$Anzahl_Benzin[1])) %>%
        mutate(partial= Number / sum(Number)) %>%
          mutate(labels= paste(percent(partial)," (",Number, ")", sep = ""))

    small_cars_bp<- ggplot(small_cars_df, aes(x="", y=Number, fill=Motor)) +
      geom_bar(width = 0.9, stat = "identity", color= "white")
    
    pie <- small_cars_bp + coord_polar("y", start=0)
    pie +
      scale_fill_brewer(palette="Greens") + 
      geom_text(aes(y = Number/2 + c(0, cumsum(Number)[-length(Number)]),
                    label = labels), size=4) + 
      ggtitle("Affected car owners in ban region: ", region_cars$Anzahl_Diesel[1]) + 
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold", size = 16))


    #Prozentualer Anteil der Betroffenen -> muss anders beschriftet werden!
    #Am besten mehere solcher Plots erstellen und kombinieren
    #Nachschauen, ob man solche plots einfach durch die Angaben der Prozente erstellen kann 
    
    # plot_data <- complete_information %>%
    #     ungroup() %>%
    #       filter(Zulassung <= input$banDate)%>%
    #       filter(Region == input$regionCode)%>%
    #       group_by(ID_Motor)%>%
    #         ggplot(aes(x = Region, fill = ID_Motor)) + geom_bar(position = "fill") + ggtitle("Affected car owners in ban region: ", input$regionCode) +
    #         coord_polar("y")
    # 
    # plot_data
    
  })
}

#### Run Shiny App ####
shinyApp(ui, server)
