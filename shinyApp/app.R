#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#==
#    http://shiny.rstudio.com/
#

packages = c('devtools', 'tidyverse', 'ggridges','readxl','dplyr',
             'plotly','shiny','shiny.semantic','semantic.dashboard','ggplot2', 'DT', 'scales', 'rgdal', 'leaflet', 'RColorBrewer')

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
} 

data1 <- read_excel("data/Happiness Index.xlsx", sheet = 1)
data2 <- read_excel("data/Happiness Index.xlsx", sheet = 2)

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
      menuItem(tabName = "overview", "Overview"),
      menuItem(tabName = "comparison", "Country Comparison"),
      menuItem(tabName = "comparison2", "Country Comparison 2"),
      menuItem(tabName = "country", "Country Time-Series Dashboard")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "overview",
        fluidRow(
          column(width = 9,
                 box(
                     style="height:850px",
                   title = "World Happiness Index 2019 Bar Chart (Descending Order)",
                   color = "teal", ribbon = FALSE, title_side = "top", Collapsible = FALSE,
                   fluidRow(
                     div(style="display: inline-block; width: 150px",
                       selectInput(
                         inputId = "stackedAndSlopeToggle", 
                         label = "View as:",
                         c("Barchart" = "barchart",
                           "Slope" = "slope"),
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
                           condition = "input.stackedAndSlopeToggle == 'slope'",
                           selectInput(inputId = "start_year", label = "From:",
                                       choices = c(2005:2018),
                                       selected = 2005
                           )
                         )
                      ),
                     div(style="display: inline-block; width: 150px; margin-left: 20px",
                         conditionalPanel(
                           condition = "input.stackedAndSlopeToggle == 'slope'",
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
                       condition = "input.stackedAndSlopeToggle == 'slope'",
                       tags$hr(),
                       fluidRow(
                         plotlyOutput("scatterplot")
                       )
                     )
                     
                     
                 )
          ),
          column(width = 7,
                   box(
                       title = "World Happiness Index 2019 Choropleth Plot",
                       color = "teal", ribbon = FALSE, title_side = "top", Collapsible = FALSE,
                           leafletOutput("choroplethplot")
                       
                   ),
                   box(
                       title = "Happiness Score Distribution",
                       color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                       column(
                         width = 9,
                         radioButtons(
                           inputId = "ridge23toggle", 
                           label = "View as:",
                           c("Percentile" = "percentile",
                             "Value" = "value"),
                           selected = "percentile",
                           inline = TRUE
                         )
                       ),
                       column(width = 9,
                              conditionalPanel(
                                condition = "input.ridge23toggle == 'value'",
                                plotOutput("ridgeplot2", height = 200)
                              )
                       ),
                       column(width = 9,
                              conditionalPanel(
                                condition = "input.ridge23toggle == 'percentile'",
                                plotOutput("ridgeplot3", height = 200)
                              )
                       )
                   )
                 
          )
        )
      ),
      tabItem(
        tabName = "comparison",
        fluidRow(
          selectInput(inputId = "comparison_year", label = "Select year:", 
                      choices = c(2005:2018), selected = 2018)
        ),
        fluidRow(
          #First Country 
          box(width = 8,
              title = "Country A",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              selectInput(inputId = "first_country", label = "Select country:", 
                          choices = levels(as.factor(data2$Country)), selected = "Afghanistan"),
              plotlyOutput("countryA_barchart"),
              plotOutput("countryAridge")
          ),
          box(width = 8,
              title = "Country B",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              selectInput(inputId = "second_country", label = "Select country:",
                          choices = levels(as.factor(data2$Country)), selected = "Afghanistan"),
              plotlyOutput("countryB_barchart"),
              plotOutput("countryBridge")
          )
        )
      ),
      tabItem(
        tabName = "comparison2",
        fluidRow(
          selectInput(inputId = "comparison_year2", label = "Select year:", 
                      choices = c(2005:2018), selected = 2018)
        ),
        fluidRow(
          #First Country 
          box(width = 8,
              title = "Country A",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              selectInput(inputId = "first_country2", label = "Select country:", 
                          choices = levels(as.factor(data2$Country)), selected = "Afghanistan"),
              plotlyOutput("countryA_radar")
          ),
          box(width = 8,
              title = "Country B",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              selectInput(inputId = "second_country2", label = "Select country:",
                          choices = levels(as.factor(data2$Country)), selected = "Afghanistan"),
              plotlyOutput("countryB_radar")
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
  
  output$ridgeplot1 <- renderPlot({
    filtered_data <- subset(data1,
                            Year %in% input$ridge1years)
    ggplot(filtered_data, aes(y=as.factor(Year),
                              x=`Life Ladder`)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      theme(axis.text=element_text(size=10))
  })
  
  output$ridgeplot2 <- renderPlot({
    ggplot(RidgePlot_and_Chloro, aes(y=1,x=Value,fill=Factor)) +
      geom_density_ridges(alpha=0.5) +
      
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      theme(axis.text=element_text(size=10), legend.position = "bottom")
  })
  
  output$ridgeplot3 <- renderPlot({
    ggplot(RidgePlot_and_Chloro, aes(y=1,x=PercentValue,fill=PercentFactor)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
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
    p <- plot_ly(sorted_data(), type='bar', height = 780, width=600)
    print(headers())
    
    for(col in headers()) {
      if(col == "GDP"){
        p <- add_trace(p,x = ~GDP, y=~Country,  name = 'GDP', marker= list(color="orange")) 
      }else if(col == "SocialSupport"){
        p <- add_trace(p,x = ~SocialSupport,
                       y=~Country, type='bar', name = 'Social Support', marker= list(color="green"))
      }else if(col == "LifeExpectancy"){
        p <- add_trace(p,x = ~LifeExpectancy, 
                       y=~Country, type='bar', name = 'Life Expectancy', marker= list(color="red"))
      }else if(col == "Freedom"){
        p <- add_trace(p,x = ~Freedom, 
                       y=~Country, type='bar', name = 'Freedom', marker= list(color="yellow"))
      }else if(col == "Generosity"){
        p <- add_trace(p,x = ~Generosity, 
                       y=~Country, type='bar', name = 'Generosity', marker= list(color="blue"))
      }else if(col == "PerceptionsOfCorruption"){
        p <- add_trace(p,x = ~PerceptionsOfCorruption, 
                       y=~Country, name = 'Corruption', marker= list(color="grey"))
      }else if(col == "Dystopia"){
        p <- add_trace(p,x = ~Dystopia, 
                       y=~Country, name = 'Dystopia', marker= list(color="purple"))
      }
    }
    
    p <-layout(p,
               width = 550,
               yaxis = list(
                 categoryorder = "array",
                 categoryarray = ~Country
               ),
               barmode="stack",
               legend=list(traceorder = "normal"))
  })
  
  #new choropleth
  # Create a color palette with handmade bins.
  mybins <- c(0,2,4,6,8)
  mypalette <- colorBin( palette="YlOrRd", domain=world_spdf@data$HappinessScore, na.color="white", bins=mybins)
  
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
            color= ~change, colors = c('red','green'))%>%
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
           MinMaxCorruption = MMScaler(Corruption),
           MinMaxGenerosity = MMScaler(Generosity)) 
  
  # Comparison Tab - Country A Bar
  countryA_data <- reactive({
    countryA_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$first_country & Year == input$comparison_year) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$countryA_barchart <- renderPlotly({
    plot_ly(countryA_data(), x = ~Category, y = ~Value, type = "bar") 
  })
  
  
  # Comparison Tab - Country B Bar
  countryB_data <- reactive({
    countryB_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$second_country & Year == input$comparison_year) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$countryB_barchart <- renderPlotly({
    plot_ly(countryB_data(), x = ~Category, y = ~Value, type = "bar") 
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
      theme(axis.text=element_text(size=10), legend.position = "bottom")
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
      theme(axis.text=element_text(size=10), legend.position = "bottom")
  })
  
  # Comparison Tab - Country A Radar
  countryA_data <- reactive({
    countryA_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$first_country2 & Year == input$comparison_year2) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$countryA_radar <- renderPlotly({
    plot_ly(countryA_data(),
            type = 'scatterpolar',
            r = ~Value,
            theta = ~Category,
            fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        ),
        showlegend = F
      )
  })
  
  
  # Comparison Tab - Country B Bar
  countryB_data <- reactive({
    countryB_data <- comparison_data %>% 
      select(Country, Year, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity) %>% 
      filter(Country == input$second_country2 & Year == input$comparison_year2) %>% 
      gather(Category, Value, MinMaxGDP, MinMaxSS, MinMaxLE, MinMaxFreedom, MinMaxCorruption, MinMaxGenerosity)
  })
  
  output$countryB_radar <- renderPlotly({
    plot_ly(countryB_data(),
            type = 'scatterpolar',
            r = ~Value,
            theta = ~Category,
            fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        ),
        showlegend = F
      ) 
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
