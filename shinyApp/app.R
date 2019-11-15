#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#==
#    http://shiny.rstudio.com/
#

packages = c('devtools', 'tidyverse', 'ggridges','readxl','dplyr',
             'plotly','shiny','shiny.semantic','semantic.dashboard','ggplot2', 
             'DT', 'scales', 'rgdal', 'leaflet', 'RColorBrewer','png','base64enc')

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
} 

data1 <- read_excel("data/Happiness Index.xlsx", sheet = 1)
data2 <- read_excel("data/Happiness Index.xlsx", sheet = 2)
HWlogo <- base64enc::dataURI(file="img/HW_Logo.png", mime="image/png")

world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/data/shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
data1 <- merge(
  data1 %>%
    group_by(Year) %>%
    summarise(average_happiness = mean(`Life Ladder`)), 
  data1, by="Year")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Happiness Watch", inverted = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "home", "Home"),
      menuItem(tabName = "overview", "Global Overview"),
      menuItem(tabName = "comparison", "Country Comparison"),
      menuItem(tabName = "country", "Country Time-Series")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 16,
            align = "center",
            box(
              div(style="padding: 60px; height: 700px",
                list(img(src=HWlogo,height='300px')),
                fluidRow(
                  div(style="font-size: 25px; margin-bottom: 25px; margin-top: 50px",
                    "Traditionally, a country’s well-being has been measured on economic variables like GDP or unemployment rate. However, no institution, nation or group of people can really be properly understood without also factoring in a number of other elements."
                  ),
                  div(style="font-size: 25px",
                    "One of these key elements is happiness. What contributes to a country’s happiness? Why are some countries happier than others? Are there any trends or patterns we can discern from the available data? With reference to the World Happiness Report, we attempt to visualize the factors that contribute to a country’s happiness on a global scale."
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "overview",
        fluidRow(
          column(width = 9,
                 box(
                     style="height:750px",
                   title = "World Happiness Index Rankings",
                   color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                   fluidRow(
                     div(style="display: inline-block; width: 170px",
                       selectInput(
                         inputId = "stackedAndSlopeToggle", 
                         label = "View as:",
                         c("Stacked Bar Chart" = "barchart",
                           "Scatterplot" = "scatter"),
                         selected = "barchart"  
                       )
                     ),
                     div(style="display: inline-block; width: 150px; margin-left: 20px",
                         conditionalPanel(
                           condition = "input.stackedAndSlopeToggle == 'barchart'",
                           selectInput(inputId = "stackedBC_sortby", label = "Sort by:",
                                       choices = c("Happiness Index" = "HappinessScore",
                                                   "GDP", 
                                                   "Social Support" = "SocialSupport", 
                                                   "Life Expectancy" = "LifeExpectancy" ,
                                                   "Freedom", "Generosity", 
                                                   "Corruption" = "PerceptionsOfCorruption", 
                                                   "Dystopia"),
                                       selected = 2018
                           )
                         ),
                         conditionalPanel(
                           condition = "input.stackedAndSlopeToggle == 'scatter'",
                           selectInput(inputId = "start_year", label = "From:",
                                       choices = c(2005:2018),
                                       selected = 2005
                           )
                         )
                      ),
                     div(style="display: inline-block; width: 150px; margin-left: 20px",
                         conditionalPanel(
                           condition = "input.stackedAndSlopeToggle == 'scatter'",
                           selectInput(inputId = "end_year", label = "To:",
                                       choices = c(2005:2018),
                                       selected = 2018
                           )
                         )
                        )
                    ),
                     
                     conditionalPanel(
                       condition = "input.stackedAndSlopeToggle == 'barchart'",
                       tags$hr(),
                       color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                       plotlyOutput("barchart")
                       
                     ),
                     
                     conditionalPanel(
                       condition = "input.stackedAndSlopeToggle == 'scatter'",
                       tags$hr(),
                       fluidRow(
                         plotlyOutput("scatterplot")
                       )
                     )
                     
                     
                 )
          ),
          column(width = 7,
                   box(
                       style= "height: 400px",
                       title = "World Happiness Index 2019 Choropleth Plot",
                       color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                           leafletOutput("choroplethplot")
                       
                   ),
                   div(style="padding-top:25px",
                     box(
                         style= "height: 275px",
                         title = "World Happiness Index 2019 Component Distribution",
                         color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                         plotOutput("ridgeplot", height = 270)
                     )
                   )
                 
          )
        )
      ),
      tabItem(
        tabName = "comparison",
        fluidRow(
          selectInput(inputId = "comparison_year", label = "Select year:", 
                      choices = c(2005:2018), selected = 2018),
          div(style="display: inline-block; width: 150px; margin-left: 20px",
            selectInput(inputId = "first_country", label = "Select country:", 
                        choices = levels(as.factor(data2$Country)), selected = "Finland")
          ),
          div(style="display: inline-block; width: 150px; margin-left: 20px",
            selectInput(inputId = "second_country", label = "Select country:",
                        choices = levels(as.factor(data2$Country)), selected = "Afghanistan")
          )
        ),
        fluidRow(
          box(width = 16,
              style= "height: 350px",
              title = "Happiness Index Component Breakdown",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              div(style="display: inline-block; width: 150px",
                  selectInput(
                    inputId = "barAndRadarToggle", 
                    label = "View as:",
                    c("Grouped Bar" = "groupedbar",
                      "Radar" = "radar"),
                    selected = "groupedbar"  
                  )
              ),
              conditionalPanel(
                condition = "input.barAndRadarToggle == 'groupedbar'",
                tags$hr(),
                color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                plotlyOutput("country_barchart", height = 250)
              ),
              
              conditionalPanel(
                condition = "input.barAndRadarToggle == 'radar'",
                tags$hr(),
                plotlyOutput("country_radar", height = 250)
              )
          )
        ),
        fluidRow(
          #First Country 
          box(width = 8,
              style= "height: 250px",
              title = "First Country",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              plotOutput("countryAridge", height = 240)
          ),
          box(width = 8,
              style= "height: 250px",
              title = "Second Country",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              plotOutput("countryBridge", height = 240)
          )
        )
      ),
      tabItem(
        tabName = "country",
        fluidRow(
          selectInput(inputId = "country_tab_selected", label = "Select country:",
                      choices = levels(as.factor(data2$Country)), selected = "Afghanistan")
        ),
        fluidRow(
          box(width = 16,
              style="height:210px",
              title = "Happiness Index 2005 - 2018",
              color = "teal", ribbon = FALSE, title_side = "top", Collapsible = FALSE,
              plotlyOutput("happiness_timeseries"))
        ),
        fluidRow(
          box(width = 16,
              style="height:340px",
              title = "Well-being Measures 2005 - 2018",
              color = "teal", ribbon = FALSE, title_side = "top", Collapsible = FALSE,
              plotlyOutput("measures_timeseries"))
        )
      )
    )
  ), theme = "cosmo"
)


# Define server logic 
server <- function(input, output) {
  
  RidgePlot_and_Chloro <- data2 %>% 
    rename("GDP"="Explained by: GDP per capita",
           "SocialSupport"="Explained by: Social support",
           "LifeExpectancy"="Explained by: Healthy life expectancy",
           "Freedom"="Explained by: Freedom to make life choices",
           "Generosity"="Explained by: Generosity",
           "PerceptionsOfCorruption"="Explained by: Perceptions of corruption",
           "HappinessScore"="Happiness score") %>%
    mutate(GDP_Percent = `GDP`/`HappinessScore`,
           SocialSupport_Percent = `SocialSupport`/`HappinessScore`,
           LifeExpectancy_Percent = `LifeExpectancy`/`HappinessScore`,
           Freedom_Percent = `Freedom`/`HappinessScore`,
           Generosity_Percent = `Generosity`/`HappinessScore`,
           PerceptionsOfCorruption_Percent = `PerceptionsOfCorruption`/`HappinessScore`) %>%
    gather( key = "Factor", value = "Value", `GDP`:`PerceptionsOfCorruption`, na.rm = FALSE, convert = FALSE, factor_key = FALSE) %>%
    gather( key = "PercentFactor", value = "PercentValue", `GDP_Percent`:`PerceptionsOfCorruption_Percent`, na.rm = FALSE, convert = FALSE, factor_key = FALSE)
  
  data2_sorted <- RidgePlot_and_Chloro[order(RidgePlot_and_Chloro$"HappinessScore"),]
  shape.data <- world_spdf@data
  country.data <- select(RidgePlot_and_Chloro, Country, HappinessScore)
  shape.data <- merge(country.data, shape.data, by.x=c("Country"), by.y=c("NAME") )
  world_spdf@data <- shape.data
  
  
  output$ridgeplot <- renderPlot({
    ggplot(RidgePlot_and_Chloro, aes(y=1,x=PercentValue,fill=PercentFactor)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      scale_fill_discrete(name = "", labels = c("Freedom", "Generosity", "Corruption","GDP","Life Expectancy","Social Support")) + 
      theme(axis.text=element_text(size=10), legend.position = "bottom")
  })
  
  #World Stacked Bar Chart
  
  stackedBC_data <- data2 %>% 
    rename("GDP"="Explained by: GDP per capita",
           "SocialSupport"="Explained by: Social support",
           "LifeExpectancy"="Explained by: Healthy life expectancy",
           "Freedom"="Explained by: Freedom to make life choices",
           "Generosity"="Explained by: Generosity",
           "PerceptionsOfCorruption"="Explained by: Perceptions of corruption",
           "Dystopia"="Dystopia (1.88) + residual",
           "HappinessScore"="Happiness score") %>%
    select(Country, HappinessScore, GDP, SocialSupport, LifeExpectancy,
           Freedom, Generosity, PerceptionsOfCorruption, Dystopia)
  
  headers <- reactive({
    # Storing datatable's headers as list
    headers <- (colnames(stackedBC_data))
    
    # Function (Testing for user's input)
    col <- input$stackedBC_sortby
    
    # Don't need to change position of HappinessScore
    if(col != "HappinessScore"){
      temp <- headers[3] #replacing this index 
      indexOfCol <- grep(col, headers)
      
      headers[3] <- col
      headers[indexOfCol] <- temp
    }
    headers
  })
  
  sorted_data <- reactive({
    sorted_data <- stackedBC_data[order(stackedBC_data[[input$stackedBC_sortby]]),] 
  })
  
  output$barchart <-renderPlotly({
    #Plotting Stacked bar chart
    p <- plot_ly(sorted_data(), type='bar', height = 670, width=600)
    print(headers())
    
    for(col in headers()) {
      if(col == "GDP"){
        p <- add_trace(p,x = ~GDP, y=~Country,  name = 'GDP', marker= list(color='rgba(109, 166, 167, 1)')) 
      }else if(col == "SocialSupport"){
        p <- add_trace(p,x = ~SocialSupport,
                       y=~Country, type='bar', name = 'Social Support', marker= list(color='rgba(212, 154, 203, 1)'))
      }else if(col == "LifeExpectancy"){
        p <- add_trace(p,x = ~LifeExpectancy, 
                       y=~Country, type='bar', name = 'Life Expectancy', marker= list(color='rgba(173, 201, 249, 1)'))
      }else if(col == "Freedom"){
        p <- add_trace(p,x = ~Freedom, 
                       y=~Country, type='bar', name = 'Freedom', marker= list(color='rgba(255, 188, 183, 1)'))
      }else if(col == "Generosity"){
        p <- add_trace(p,x = ~Generosity, 
                       y=~Country, type='bar', name = 'Generosity', marker= list(color='rgba(213, 200, 140, 1)'))
      }else if(col == "PerceptionsOfCorruption"){
        p <- add_trace(p,x = ~PerceptionsOfCorruption, 
                       y=~Country, name = 'Corruption', marker= list(color='rgba(130, 191, 141, 1)'))
      }else if(col == "Dystopia"){
        p <- add_trace(p,x = ~Dystopia, 
                       y=~Country, name = 'Dystopia', marker= list(color='rgba(174, 176, 183, 1)'))
      }
    }
    
    p <-layout(p,
               width = 550,
               yaxis = list(
                 categoryorder = "array",
                 categoryarray = ~Country,
                 showticklabels= FALSE
               ),
               barmode="stack",
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             traceorder = "normal",
                             x = 0.5)
    )
  })
  
  #new choropleth
  # Create a color palette with handmade bins.
  mybins <- c(0,2,4,6,8)
  mypalette <- colorBin( palette="PuBuGn", domain=world_spdf@data$HappinessScore, na.color="white", bins=mybins)
  
  # Prepare the text for tooltips:
  mytext <- paste(
    "Country: ", world_spdf@data$Country,"<br/>", 
    "Happiness Index: ", round(world_spdf@data$HappinessScore, 2), 
    sep="") %>%
    lapply(htmltools::HTML)
  
  # Final Map
  output$choroplethplot <- renderLeaflet({
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom = 0.5) %>%
      addPolygons( 
        fillColor = ~mypalette(HappinessScore), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~HappinessScore, opacity=0.8, title = "Happiness Index", position = "bottomleft" )
    
  })
  
  
  # Scatter Plot Chart 
  scatterplot_data <- reactive({
    scatterplot_data <- data1 %>%
      select("Country name", Year, "Life Ladder") %>%
      spread(Year, "Life Ladder") %>% 
      drop_na(input$start_year, input$end_year) %>% 
      rename("country"="Country name",
             "year1"=paste(input$start_year),
             "year2"=paste(input$end_year)) %>%
      select(country,year1,year2) %>%
      mutate(change = round((year2 - year1)/year1, digits = 2))
  })
  
  geo <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  output$scatterplot <- renderPlotly({
    plot_ly(scatterplot_data(), name=~country, x = ~year2, y = ~year1, 
            type = 'scatter', mode = 'markers', text = ~paste("Country :", country, "<br>Change: ", change),
            color= ~change, colors = c('#F96997','#2EB49E'))%>%
      layout(xaxis = list(title = input$end_year), yaxis = list(title = input$start_year), showlegend = FALSE,geo = geo, paper_bgcolor='transparent') 
  })
  
  # MinMax Scaler
  MMScaler <- function(x){(x-min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE)-min(x,na.rm = TRUE))}
  
  # Comparison Tab Data
  comparison_data <- data1 %>% 
    rename("Country" = "Country name",
           "HappinessIndex" = "Life Ladder",
           "GDP" = "Log GDP per capita",
           "SocialSupport" = "Social support",
           "LifeExpectancy" = "Healthy life expectancy at birth",
           "Freedom" = "Freedom to make life choices",
           "Corruption" = "Perceptions of corruption") %>%
    mutate(MinMaxGDP = MMScaler(GDP),
           MinMaxSS = MMScaler(SocialSupport),
           MinMaxLE = MMScaler(LifeExpectancy),
           MinMaxFreedom = MMScaler(Freedom),
           MinMaxCorruption = 1 - MMScaler(Corruption),
           MinMaxGenerosity = MMScaler(Generosity)) 
  
  # Comparison Tab - Combined Bar
  combined_data <- reactive({
    combined_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter((Country == input$first_country | Country == input$second_country) & Year == input$comparison_year) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$country_barchart <- renderPlotly({
    plot_ly(combined_data(), x = ~Category, y = ~Value, color = ~Country, type = "bar", position = "dodge") 
  })
  
  # Comparison Tab - Country A Radar
  countryA_data <- reactive({
    countryA_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$first_country & Year == input$comparison_year) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  # Comparison Tab - Country B Radar
  countryB_data <- reactive({
    countryB_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$second_country & Year == input$comparison_year) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  # Combined Radar
  output$country_radar <- renderPlotly({
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        data = countryA_data(),
        r = ~Value,
        theta = ~Category,
        name = input$first_country
      ) %>%
      add_trace(
        data = countryB_data(),
        r = ~Value,
        theta = ~Category,
        name = input$second_country
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        )
      )
  })
  
  # Comparison Tab - Country A Ridge
  countryAridge_data <- reactive({
    countryAridge_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$first_country) %>% 
      gather(key = "Category", value = "Value", MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$countryAridge <- renderPlot({
    ggplot(countryAridge_data(), aes(y=1,x=Value,fill=Category)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      scale_fill_discrete(name = "", labels = c("Corruption", "Freedom", "GDP","Generosity","Life Expectancy","Social Support")) + 
      theme(axis.text=element_text(size=10,), legend.position = "bottom") +
      labs(title = paste(input$first_country, "Component Distribution"))
  })
  
  # Comparison Tab - Country B Ridge
  countryBridge_data <- reactive({
    countryBridge_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$second_country) %>% 
      gather(key = "Category", value = "Value", MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$countryBridge <- renderPlot({
    ggplot(countryBridge_data(), aes(y=1,x=Value,fill=Category)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      scale_fill_discrete(name = "", labels = c("Corruption", "Freedom", "GDP","Generosity","Life Expectancy","Social Support")) + 
      theme(axis.text=element_text(size=10), legend.position = "bottom") +
      labs(title = paste(input$second_country, "Component Distribution"))
  })
  
  
  #Country Tab
  country_data_selected <- reactive({
    data1 %>%
      rename("Country" = "Country name",
             "HappinessIndex" = "Life Ladder") %>%
      select(Country, Year, HappinessIndex, average_happiness) %>% 
      filter(Country == input$country_tab_selected)
  })
  
  
  country_data_selected_measurements <- reactive({
    comparison_data %>%
      select(Country, Year, HappinessIndex, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity, MinMaxGDP) %>% 
      filter(Country == input$country_tab_selected)
  })
  
  output$happiness_timeseries <- renderPlotly({
    plot_ly(country_data_selected(), x = ~Year, y = ~HappinessIndex, mode = 'lines+markers') %>%
      add_trace(y = ~HappinessIndex, name = "Happiness Index", mode = 'lines+markers')%>%
      add_trace(y = ~average_happiness, name = "Average Happiness Index", mode = 'lines', line = list(color = 'rgb(169,169, 169)', dash = 'dot'))%>% 
      layout(
        height = 200,
        yaxis = list(title = "Happiness Index"
        ))
  })
  
  output$measures_timeseries <- renderPlotly({
    plot_ly(country_data_selected_measurements(), x = ~Year, y = ~MinMaxGDP, mode = 'lines+markers', name = "GDP") %>%
      add_trace(y = ~MinMaxGDP, name = "GDP", mode = 'lines+markers')%>%
      add_trace(y = ~MinMaxSS, name = "Social Support", mode = 'lines+markers')%>%
      add_trace(y = ~MinMaxLE, name = "Life Expectancy", mode = 'lines+markers')%>%
      add_trace(y = ~MinMaxFreedom, name = "Freedom to Make Life Choices", mode = 'lines+markers')%>%
      add_trace(y = ~MinMaxCorruption, name = "Perceptions of Corruptions", mode = 'lines+markers')%>%
      add_trace(y = ~MinMaxGenerosity, name = "Generosity", mode = 'lines+markers') %>%
      layout(
        height = 350,
        yaxis = list(title = "Normalized Value"
        ))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
