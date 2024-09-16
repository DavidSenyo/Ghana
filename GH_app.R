



source("GH_dataRead.R")

##############################################################
### Shiny App for Ghana map app
###############################################################

# Define UI
ui <- fluidPage(
  useShinyjs(), 
  theme = shinytheme("yeti"),
  
  h1(id="big-heading", tags$strong(HTML(paste0("Ghana - West Africa"))), align="center"),
  tags$style(HTML("#big-heading{color: #903a1e; background: #6ff591;}")), hr(), hr(),  
  em("Author: D. Ackuaku (davidackuaku@gmail.com)"),
  hr(), hr(),
  
sidebarLayout(
       sidebarPanel(
                    pickerInput(
                                inputId = "features",
                                label = "Select features to display:",
                                choices = c("Educational Institutions", 
                                            "Cultural Sites", 
                                            "capitals",
                                            "tourist_attractions",
                                            "regional_hospitals",
                                            "waterfalls",
                                            "major_towns",
                                            "infrastructure_projects",
                                            "rivers_and_lakes",
                                            "major_roads"),
                                selected=c("capitals"),
                                multiple = F
                               ),
                       #actionButton("reset", "Reset Filters (to Capitals)" ), br(),
                       h6("this filter is only for the first tab, first subtab"),
                       tags$img(src = "ghana.jpg", style = "margin-right: 50px;", height='75px', width='75px'),
                       tags$img(src="filter.png", style = "margin-left", height='75px', width='75px'), hr(), width=3
                ),
      mainPanel(
                tabsetPanel(
                      tabPanel("Ghana", br(),hr(),
                                     
                            tabsetPanel(         
                            tabPanel("Sites in Ghana", hr(),
                                     div(id = "loading-message", h1("Loading map, please wait...")),
                                     leafletOutput("ghana_map", width = "100%", height = 600), br(), hr(), hr() ),
                            tabPanel("MPs in Ghana (8th Parl.)", hr(),
                                     div(id = "loading-message1", h1(" ...MP Counts, a minute...")),
                                     leafletOutput("ghana_mapP", width = "100%", height = 600), br(), hr(), hr() )
                                     )),
                            
                      tabPanel("Ghana Elections", 
                                em("NB: the election data is from wikipedia for all elections in the 4th Republic"),br(),      
                            tabsetPanel( 
                            tabPanel("MPs in Ghana(By Parties)", hr(), 
                                     column(3, 
                                            pickerInput (
                                              inputId = "region",
                                              label = "Select Region",
                                              multiple = TRUE,
                                              choices = sort(unique(MPs_GH$Region)),
                                              selected = "Volta"
                                              #options = list('actions-box' = TRUE) 
                                              ) ),
                                     column(3,
                                            selectInput(inputId = "pt",
                                                        label = "Select Party",
                                                        choices = sort(unique(MPs_GH$`Elected Party`)), multiple = T) 
                                            ), 
                                     column(3,
                                            checkboxGroupInput (inputId = "vpt",
                                                                label = "Pick Prev. Party Elected",
                                                                choices = sort(unique(mps_2008$`Elected Party`)),
                                                                selected = c('NDC', 'NPP') ) 
                                         ),
                                     column(3,
                                            checkboxGroupInput (inputId = "eyr",
                                                                label = "Pick the 4th Repu. Parl. Year",
                                                                choices = sort(unique(MPs_GH$Year)),
                                                                selected = '2020' ) 
                                     ),
                                     br(), br(),hr(), 
                                     strong('Summary of MPs for each Party by Regions'), tableOutput("mp_count"),hr(), br(),
                                     strong("MPs for Each Constituency by Elected Party and Regions"),  dataTableOutput("MPs_Regions"), br(), hr(), hr() ),
                            tabPanel("MPs(By Parties)", hr(),
                                     column(6, 
                                            pickerInput (
                                              inputId = "region1",
                                              label = "Select Region",
                                              multiple = TRUE,
                                              choices = sort(unique(MPs_GH$Region)),
                                              selected = c("Volta", "Central")
                                               
                                            ) ),
                                     column(6,
                                            pickerInput (inputId = "eyr1",
                                                                label = "Pick Election Year",
                                                                choices = sort(unique(MPs_GH$Year)),
                                                                selected = '2020' ) 
                                        ), 
                                     actionButton("refresh", "Reset Filters(Region to New Regions)" ), br(), hr(), hr(), 
                                     strong('Seats Won for each Party by Regions'), plotlyOutput("circlePackingPlot", height = "400px"), br(), 
                                     strong('Seats in Parliament'), plotOutput("parliamentPlot"),  hr(), hr() )
                            
                                 )), 
                            
                      tabPanel("Ghana SocioEconomy", 
                               em("NB: the socioeconomic data was downloaded from the world bank website"), 
                                     
                            tabsetPanel(
                            tabPanel("GH Economic Metrics", hr(), 
                                     pickerInput(inputId = "yr",
                                                 label = "Choose Year(s)",
                                                 multiple = T,
                                                 choices = sort(GH_EcoData$Year),
                                                 selected = c("2019", "2020", "2021", "2022", "2023")
                                                 ) , 
                                     plotlyOutput("plot1"), br(), hr() ,
                                     plotlyOutput("plot2"), br(), hr(),
                                     plotlyOutput("plot3"), br(), hr(), hr() ),
                            tabPanel("GH Population", hr(), 
                                      pickerInput(inputId = "yr1",
                                                 label = "Choose Year(s)",
                                                 multiple = T,
                                                 choices = sort(GH_SocioEco$Year),
                                                 selected = c("2019", "2020", "2021", "2022", "2023")
                                                 ) ,
                                     plotlyOutput("plot4"), br(), hr(),
                                     plotlyOutput("plot5"), br(), hr(), hr() ) 
                                     ))
                          
                          )
                        )
      
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$region, {
    updateSelectInput(session, 'pt', selected = MPs_GH$`Elected Party`, choices = sort(unique(MPs_GH$`Elected Party`[MPs_GH$Region == input$region])))
  })

###############    
### Tab 1 
#############  
# observeEvent(input$features, {
#     updatePickerInput(session, "features", selected = c("Capital")  )
#   })
  
output$ghana_map <- renderLeaflet({
                                   leaflet() %>%
                                                addProviderTiles(providers$OpenStreetMap) %>%
                                                addPolygons(data = ghana_shapefile, 
                                                            fillColor = ~pal(NAME_1), 
                                                            fillOpacity = 0.5, 
                                                            color = "black", 
                                                            weight = 1,
                                                            label = ~NAME_1)
  })
  
observe({
              proxy <- leafletProxy("ghana_map")

    
              if ("Educational Institutions" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = educational_institutions, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'graduation-cap', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = 'green'
                                            ),
                                            popup = ~paste("<b>Educational Institution:</b> ", name),
                                            label = ~name)
              }
              
              if ("Cultural Sites" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = cultural_sites, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'university', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = 'purple'
                                            ),
                                            popup = ~paste("<b>Cultural Site:</b> ", name),
                                            label = ~name)
              }
              
              if ("capitals" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = capitals, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'home', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = '#a58ea7'
                                            ),
                                            popup = ~paste("<b>Capital:</b> ", capital, 
                                                           "<br><b>Region:</b> ", region,
                                                           "<br><b>Population Density:</b> ", population_density, " people/sq km"))
              }
              
              if ("tourist_attractions" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = tourist_attractions, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'star', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = 'blue'
                                            ),
                                            popup = ~paste("<b>Tourist Attraction</b>", 
                                                           "<br><b>Attraction:</b> ", name))
              }
              
              if ("regional_hospitals" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = regional_hospitals, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'hospital-o', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = 'green'
                                            ),
                                            popup = ~paste("<b>Regional Hospitals</b>", 
                                                           "<br>Region:</b>", region,
                                                           "<br><b>Hospital:</b> ", name))
              }
              
              if ("waterfalls" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = waterfalls, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'tint', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = '#9999ff'
                                            ),
                                            popup = ~paste("<b>Tourist Attraction</b>", 
                                                           "<br><b>Waterfall:</b>", name))
              }
              
              if ("major_towns" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = major_towns, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'building', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = '#b266ff'
                                            ),
                                            popup = ~paste("<b>Town:</b>",
                                                           "<br><b>Population Density:</b> ", 
                                                           population_density, " people/sq km"))
              }
              
              if ("infrastructure_projects" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = infrastructure_projects, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'wrench', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = 'orange'
                                            ),
                                            popup = ~paste("<b>Infrastructure Project:</b> ", name),
                                            label = ~name)
              }
              
              if ("rivers_and_lakes" %in% input$features) {
                proxy %>% addAwesomeMarkers(data = rivers_and_lakes, 
                                            lat = ~lat, 
                                            lng = ~lon, 
                                            icon = awesomeIcons(
                                              icon = 'tint', 
                                              iconColor = 'white', 
                                              library = 'fa', 
                                              markerColor = '#ff8000'
                                            ),
                                            popup = ~paste("<b>River/Lake:</b> ", name),
                                            label = ~name)
              }
              
              if ("major_roads" %in% input$features) {
                proxy %>%
                  addAwesomeMarkers(data = major_roads, 
                                    lat = ~lat, 
                                    lng = ~lon, 
                                    icon = awesomeIcons(
                                      icon = 'road', 
                                      iconColor = 'white', 
                                      library = 'fa', 
                                      markerColor = '#b2b2b2'
                                    ),
                                    popup = ~paste("<b>Major Road/Highway:</b> ", name),
                                    label = ~name) %>%
                  addPolylines(lng = sapply(road_path_N1, "[[", 2), lat = sapply(road_path_N1, "[[", 1), color = "black", weight = 4, group = "N1 Highway") %>%
                  addPolylines(lng = sapply(road_path_N6, "[[", 2), lat = sapply(road_path_N6, "[[", 1), color = "black", weight = 4, group = "N6 Highway") %>%
                  addPolylines(lng = sapply(road_path_N2, "[[", 2), lat = sapply(road_path_N2, "[[", 1), color = "black", weight = 4, group = "N2 Highway") %>%
                  addLayersControl(
                    baseGroups = c("Major Roads"),
                    overlayGroups = c("N1 Highway", "N6 Highway", "N2 Highway"),
                    options = layersControlOptions(collapsed = FALSE)
                  ) %>%
                  hideGroup(c("N1 Highway", "N6 Highway", "N2 Highway"))
              }
              
    # Dynamically add or remove layer control for routes
observeEvent(input$features, {
                              proxy <- leafletProxy("ghana_map")
      
                              if ("major_roads" %in% input$features) {
                                proxy %>%
                                  showGroup(c("N1 Highway", "N6 Highway", "N2 Highway"))
                              } else {
                                proxy %>%
                                  hideGroup(c("N1 Highway", "N6 Highway", "N2 Highway"))
                              }
                              
                              # Hide the loading message once the map is rendered
                              shinyjs::hide("loading-message")
                            })

  })
  
###########################
### Tab 2
##########################  
output$ghana_mapP <- renderLeaflet({
                                    pal <- colorFactor(rainbow(length(unique(ghana_shapefile$NAME_1))), ghana_shapefile$NAME_1)
                                    
                                    leaflet() %>%
                                      addProviderTiles(providers$OpenStreetMap) %>%
                                      addPolygons(data = ghana_shapefile, 
                                                  fillColor = ~pal(NAME_1), 
                                                  fillOpacity = 0.5, 
                                                  color = "black", 
                                                  weight = 1,
                                                  label = ~paste(NAME_1)) %>%
                                      addAwesomeMarkers(data = Political_parties, 
                                                        icon = ~awesomeIcons(
                                                          icon = 'home', 
                                                          iconColor = '#d8a173', 
                                                          library = 'fa', 
                                                          markerColor = '#a58ea7'
                                                        ),
                                                        popup = ~paste("<b>Region:</b>", Region, "<br>",
                                                                       "<b>IND:</b>", IND, "<br>",
                                                                       "<b>NDC:</b>", NDC, "<br>",
                                                                       "<b>NPP:</b>", NPP),
                                                        group = ~Region) %>%
                                      addLayersControl(
                                        overlayGroups = unique(Political_parties$Region),
                                        options = layersControlOptions(collapsed = F)
                                      )
                                    
    
    
  })
  
observeEvent(input$features, {
                              shinyjs::hide("loading-message1")
                             })

###########################
## Tab 3
##########################
  
  
output$MPs_Regions <- renderDataTable({
    datatable( 
              MPs_GH %>% filter(Region %in% input$region & `Elected Party` %in% input$pt & `Previous Party` %in% input$vpt &
                                Year %in% input$eyr), 
              
              rownames = F, filter = 'top', options = list(pageLength = 5, autoWidth = TRUE) 
              )
  })
  
  
output$mp_count<- function()({
    
    MP_Count1 <- MPs_GH %>% 
                filter(Region %in% input$region & `Elected Party` %in% input$pt & `Previous Party` %in% input$vpt &
                       Year %in% input$eyr) %>%
                group_by(Year, Region, `Elected Party`) %>%
                mutate(count = n()   ) %>%
                select(Year, Region, `Elected Party`, count) %>%
                distinct() %>% 
                ungroup() %>% 
                group_by(Year, Region) %>%
                mutate('Prop (%)' = round((count/sum(count))*100, digits = 2)) %>%
                adorn_totals(name = 'Total MPs')
    
    
               MP_Count1 %>%
                        kbl()%>% 
                        kable_styling(bootstrap_options = c("striped", "hover")  ) %>%
                        column_spec(1, color = '#660a38', background = '#d1e8ff') %>%
                        column_spec(2, color = '#660a38', background = '#75baff') %>%
                        column_spec(3, color = '#33051c', background = '#e8f3ff') %>%
                        column_spec(4, color = '#33051c', background = '#8cc6ff') %>%
                        column_spec(5, color = '#33051c', background = '#e8f3ff')
               
               
  })  
  
  
  ###########################
  ## Tab 4
  ##########################  
observeEvent(input$refresh, {
    updateSelectInput(session, "region1", selected = c("Oti", "Bono", "Savannah", "Western North", "Bono East", "North East")  )
    updateSelectInput(session, "eyr1", selected = "2020")
  })
  
output$circlePackingPlot <- renderPlotly({
    MP_Count <- MPs_GH %>% 
      filter(Region %in% input$region1 & Year %in% input$eyr1) %>% 
      group_by(Year, Region, `Elected Party`) %>%
      mutate(count = n()   ) %>%
      select(Year, Region, `Elected Party`, count) %>%
      distinct()
    
    # Combine Region and Elected_Party for unique identification
    MP_Count$ID <- paste(MP_Count$Region, MP_Count$`Elected Party`, sep = "_")
    
    # Generate circle packing layout
    packing <- circleProgressiveLayout(MP_Count$count, sizetype = "area")
    MP_Count <- cbind(MP_Count, packing)
    
    # Calculate vertices for plotting with ggplot2
    MP_Count_gg <- circleLayoutVertices(packing, npoints = 50)
    
    MP_Count_gg <- MP_Count_gg %>%
      mutate(Region = MP_Count$Region[id])
    
    # Create the ggplot
    p <- ggplot() +
      geom_polygon(data = MP_Count_gg, aes(x, y, group = id, fill = Region, 
                                           text = paste("Region:", MP_Count$Region[id], "<br>",
                                                        "Party:", MP_Count$`Elected Party`[id], "<br>",
                                                        "Count:", MP_Count$count[id])),
                   color = "black", alpha = 0.6) +
      geom_text(data = MP_Count, aes(x, y, label = paste(Region, `Elected Party`, sep = "\n")), 
                size = 3, color = "black") +
      theme_void() +
      theme(legend.position = "none") +
      coord_equal() +
      scale_fill_viridis_d()
    
    # Convert the ggplot to a plotly object for interactivity
    ggplotly(p, tooltip = "text")
    
    
  })
  
##
output$parliamentPlot <- renderPlot({
    MPCount <- MPs_GH %>% 
               group_by(Year, Region, `Elected Party`) %>%
               mutate(count = n()   ) %>%
               distinct(Year, `Elected Party`, count) %>% 
               ungroup() %>% 
               group_by(Year, `Elected Party`) %>%
               mutate(Seats = sum(count)) %>%
               ungroup() %>%
               select(Year, `Elected Party`, count, Seats) %>%
               distinct(Year, `Elected Party`, Seats) %>%
               mutate(`Elected Party` = 
                                       ifelse(Year == '2012' & `Elected Party` %notin% c('NDC', 'NPP'),  'IND', `Elected Party`)) %>%
               distinct(Year, `Elected Party`, .keep_all = T) %>%
               filter(Year %in% input$eyr1)
    
    
    colors <- c("#99cc99", "#ffa3a3", "#b2b2ff", "#d8b2ff", "#ffe5cc", "grey")
    
    # Call the parlDiag function
    #plot <- parlDiag(parties, seats, cols = colors, repr = "absolute")
    plot <- parlDiag(MPCount$`Elected Party`, MPCount$Seats, cols = colors, repr = "absolute")
    
    # Return the plot
    print(plot)
  })
  
###########################
## Tab 5
##########################    
df_GH_EcoData <- reactive({GH_EcoData %>% filter(Year %in% input$yr)})

  output$plot1 <- renderPlotly({
    ggplotly(
      ggplot(df_GH_EcoData(), aes(Year)) + 
        #geom_line(aes(y = `Inflation (annual %)`, colour = "`Inflation (annual %)`")) + 
        geom_line(aes(y = `GDP (current US$)`, colour = "`GDP (current US$)`")) +
        geom_point(aes(y = `GDP (current US$)`, colour = "`GDP (current US$)`")) +
        geom_line(aes(y = `GDP (constant 2015 US$)`, colour = "`GDP (constant 2015 US$)`")) +
        geom_point(aes(y = `GDP (constant 2015 US$)`, colour = "`GDP (constant 2015 US$)`")) +
        ggtitle("GDP (Current USD and Constant 2015 USD)") +
        ylab("Metric Value") +
        xlab("Year") +
        theme(legend.position = "none")
    ) %>%
      layout(hovermode = "x")
  })
  
output$plot2 <- renderPlotly({
    ggplotly(
      ggplot(df_GH_EcoData(), aes(Year)) + 
        geom_line(aes(y = `GDP per capita (current US$)`, colour = "`GDP per capita (current US$)`")) +
        geom_point(aes(y = `GDP per capita (current US$)`, colour = "`GDP per capita (current US$)`"))+
        geom_line(aes(y = `GDP per capita (constant 2015 US$)`, colour = "`GDP per capita (constant 2015 US$)`")) +
        geom_point(aes(y = `GDP per capita (constant 2015 US$)`, colour = "`GDP per capita (constant 2015 US$)`")) +
        ggtitle("Per Capita GDP (Current USD and Constant 2015 USD)") +
        ylab("Metric Value") +
        xlab("Year") +
        theme(legend.position = "none")
    ) %>%
      layout(hovermode = "x")
  })
  
output$plot3 <- renderPlotly({
    ggplotly(
      ggplot(df_GH_EcoData(), aes(Year)) + 
        geom_line(aes(y = `Inflation (annual %)`, colour = "`Inflation (annual %)`")) + 
        geom_point(aes(y = `Inflation (annual %)`, colour = "`Inflation (annual %)`"))+
        geom_line(aes(y = `Agric, value added (% of GDP)`, colour = "`Agric, value added (% of GDP)`")) +
        geom_point(aes(y = `Agric, value added (% of GDP)`, colour = "`Agric, value added (% of GDP)`")) +
        geom_line(aes(y = `Imports  (% of GDP)`, colour = "`Imports  (% of GDP)`")) +
        geom_point(aes(y = `Imports  (% of GDP)`, colour = "`Imports  (% of GDP)`")) +
        ggtitle("Inflation and Agriculture") +
        ylab("Value") +
        xlab("Year") +
        theme(legend.position = "none")
    ) %>%
      layout(hovermode = "x")
  })

  
  
###########################
## Tab 6
##########################    
df_GH_SocioEco <- reactive({GH_SocioEco %>% filter(Year %in% input$yr1)})
  
  output$plot4 <- renderPlotly({
    ggplotly(
      ggplot(df_GH_SocioEco(), aes(Year)) + 
        geom_line(aes(y = `Population, male`, colour = "`Population, male`")) + 
        geom_point(aes(y = `Population, male`, colour = "`Population, male`"))+
        geom_line(aes(y = `Population, female`, colour = "`Population, female`")) +
        geom_point(aes(y = `Population, female`, colour = "`Population, female`")) +
        geom_line(aes(y = `Population, total` , colour = "`Population, total`")) +
        geom_point(aes(y = `Population, total`, colour = "`Population, total`")) +
        ggtitle("Population") +
        ylab("Value") +
        xlab("Year") +
        theme(legend.position = "none")
    ) %>%
      layout(hovermode = "x")
  })

  
output$plot5 <- renderPlotly({
ggplotly(
      ggplot(df_GH_SocioEco(), aes(Year)) + 
        geom_line(aes(y = `Unemployment, male (% of male labor force)`, colour = "`Unemployment, male (% of male labor force)`")) + 
        geom_point(aes(y = `Unemployment, male (% of male labor force)`, colour = "`Unemployment, male (% of male labor force)`"))+
        geom_line(aes(y = `Unemployment, female (% of female labor force)`, colour = "`Unemployment, female (% of female labor force)`")) +
        geom_point(aes(y = `Unemployment, female (% of female labor force)`, colour = "`Unemployment, female (% of female labor force)`")) +
        ggtitle("Unemployment Rates") +
        ylab("Rate") +
        xlab("Year") +
        theme(legend.position = "none")
    ) %>%
      layout(hovermode = "x")
  })
  
  
  
  
}



# Run the complete app
shinyApp(ui = ui, server = server)



