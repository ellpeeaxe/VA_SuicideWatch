#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#==
#    http://shiny.rstudio.com/
#

packages = c('devtools', 'tidyverse', 'ggridges','readxl','dplyr',
             'plotly','shiny','shiny.semantic','semantic.dashboard','ggplot2', 'DT', 'scales', 'rgdal')

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
} 

data1 <- read_excel("data/Happiness Index.xlsx", sheet = 1)
data2 <- read_excel("data/Happiness Index.xlsx", sheet = 2)

data3_geocode <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
colnames(data3_geocode)[colnames(data3_geocode)=="COUNTRY"] <- "Country"
data2 <- merge(data2, data3_geocode[, c("Country","CODE")], by="Country")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Happiness Watch", inverted = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "overview", "Overview"),
      menuItem(tabName = "comparison", "Country Comparison")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "overview",
        fluidRow(
          box(width = 7,
            title = "World Happiness Index 2019 Bar Chart (Descending Order)",
            color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
            column(width= 5, 
                   div(style="height:300px; overflow-y: scroll",
                       color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                         plotlyOutput("barchart")
                   )
            )
          ),
          box(width = 9,
              title = "World Happiness Index 2019 Choropleth Plot",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              column(width = 10,
                     div(style="height:300px",
                         plotlyOutput("choroplethplot")
                    )
              )
          )
        ),
        fluidRow(
          box(width = 7,
              style="height:450px",
              title = "World Happiness Index Scatter Plot",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              fluidRow(
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput(inputId = "start_year", label = "From:",
                                   choices = c(2005:2018),
                                   selected = 2005
                       )
                ),
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput(inputId = "end_year", label = "To:",
                                   choices = c(2005:2018),
                                   selected = 2018
                       )
                )
              ),
              column(width = 5,
                     plotlyOutput("scatterplot")
              )
              
          ),
          box(width = 9,
              style="height:450px",
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
                       plotOutput("ridgeplot2")
                     )
              ),
              column(width = 9,
                     conditionalPanel(
                       condition = "input.ridge23toggle == 'percentile'",
                       plotOutput("ridgeplot3")
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
          box(width = 7,
              title = "Country A",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              selectInput(inputId = "first_country", label = "Select country:", 
                         choices = levels(as.factor(data2$Country)), selected = "Afghanistan"),
              plotlyOutput("countryA_barchart")
          ),
          box(width = 7,
              title = "Country B",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              selectInput(inputId = "second_country", label = "Select country:",
                          choices = levels(as.factor(data2$Country)), selected = "Afghanistan"),
              plotlyOutput("countryB_barchart")
          )
        )
      )
    )
  ), theme = "cosmo"
)


# Define server logic 
server <- function(input, output) {
  
  data2 <- data2 %>% 
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
  
  data2_sorted <- data2[order(data2$"HappinessScore"),]
  
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
    ggplot(data2, aes(y=1,x=Value,fill=Factor)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      theme(axis.text=element_text(size=10), legend.position = "bottom")
  })
  
  output$ridgeplot3 <- renderPlot({
    ggplot(data2, aes(y=1,x=PercentValue,fill=PercentFactor)) +
      geom_density_ridges(alpha=0.5) +
      scale_y_discrete(expand = c(0.01, 0)) +  
      scale_x_continuous(expand = c(0, 0))+
      theme(axis.text=element_text(size=10), legend.position = "bottom")
  })
  
  #World Bar Chart
  output$barchart <-renderPlotly({
    plot_ly(data2_sorted, y=~Country, x=~HappinessScore, type = 'bar', orientation = 'h', height = 3000) %>%
      layout(
        yaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~HappinessScore)
      )
  })
  
  # World Choropleth Chart
  l <- list(color = toRGB("grey"), width = 0.5)
  geo <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )

  output$choroplethplot <- renderPlotly({
    plot_geo(data2) %>%
      add_trace(
        z = ~HappinessScore, color = ~HappinessScore, colors = 'Blues',
        text = ~Country, locations = ~CODE, marker = list(line = l)
      ) %>%
      colorbar(title = 'Happiness') %>%
      layout(
        geo = geo, paper_bgcolor='transparent')
    
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
      mutate(change = percent(round((year2 - year1)/year1, digits = 2)))
  })

  
  output$scatterplot <- renderPlotly({
    plot_ly(scatterplot_data(), name=~country, x = ~year1, y = ~year2, 
            type = 'scatter', mode = 'markers', text = ~paste("Change: ", change),
            color= ~change, colors = c('red','green'))%>%
      layout(xaxis = list(title = input$start_year), yaxis = list(title = input$end_year), showlegend = FALSE,geo = geo, paper_bgcolor='transparent') 
  })
  
  # Comparison Tab - Country A
  countryA_data <- reactive({
    countryA_data <- data1 %>% 
      rename("Country" = "Country name",
           "HappinessIndex" = "Life Ladder",
           "GDP" = "Log GDP per capita",
           "SocialSupport" = "Social support",
           "LifeExpectancy" = "Healthy life expectancy at birth",
           "Freedom" = "Freedom to make life choices",
           "Corruption" = "Perceptions of corruption") %>%
    select(Country, Year, HappinessIndex, GDP, SocialSupport, 
           LifeExpectancy, Freedom, Corruption) %>% 
    filter(Country == input$first_country & Year == input$comparison_year) %>% 
    gather(Category, Value, HappinessIndex, GDP, SocialSupport, LifeExpectancy, Freedom, Corruption)
  })
  
  output$countryA_barchart <- renderPlotly({
    plot_ly(countryA_data(), x = ~Category, y = ~Value, type = "bar") 
  })
  
  
  # Comparison Tab - Country B
  countryB_data <- reactive({
    countryB_data <- data1 %>% 
      rename("Country" = "Country name",
             "HappinessIndex" = "Life Ladder",
             "GDP" = "Log GDP per capita",
             "SocialSupport" = "Social support",
             "LifeExpectancy" = "Healthy life expectancy at birth",
             "Freedom" = "Freedom to make life choices",
             "Corruption" = "Perceptions of corruption") %>%
      select(Country, Year, HappinessIndex, GDP, SocialSupport, 
             LifeExpectancy, Freedom, Corruption) %>% 
      filter(Country == input$second_country & Year == input$comparison_year) %>% 
      gather(Category, Value, HappinessIndex, GDP, SocialSupport, LifeExpectancy, Freedom, Corruption)
  })
  
  output$countryB_barchart <- renderPlotly({
    plot_ly(countryB_data(), x = ~Category, y = ~Value, type = "bar") 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
