#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('devtools', 'tidyverse', 'ggridges','readxl','dplyr',
             'plotly','shiny','shiny.semantic','semantic.dashboard','ggplot2')

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
      menuItem(tabName = "breakdown", "Breakdown")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "overview",
        fluidRow(
          box(width = 6,
            title = "World Happiness Index 2019 Bar Chart (Descending Order)",
            color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
            column(width= 5,
                   div(style="max-height:500px; overflow-y: scroll",
                       color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                         plotlyOutput("barchart")
                   )
            )
          ),
          box(width = 10,
              title = "World Happiness Index 2019 Choropleth Plot",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              column(width = 10,
                     plotlyOutput("choroplethplot")
              )
          )
        )
      ),
      tabItem(
        tabName = "breakdown",
        fluidRow(
          box(width = 15,
              title = "Happiness Score Distribution",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              column(width = 15,
                     selectInput("ridge1years", "Years",
                                 width = "100%",
                                 choices = levels(as.factor(data1$Year)),
                                 multiple = TRUE,
                                 selected = levels(as.factor(data1$Year)))
              ),
              column(width = 15,
                     plotOutput("ridgeplot1")
              )
          )
        ),
        fluidRow(
          box(width = 15,
              title = "Happiness Score Distribution",
              color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
              column(
                  width = 15,
                  radioButtons(
                      inputId = "ridge23toggle", 
                      label = "View as:",
                      c("Percentile" = "percentile",
                        "Value" = "value"),
                      selected = "percentile",
                      inline = TRUE
                  )
              ),
              column(width = 15,
                     conditionalPanel(
                         condition = "input.ridge23toggle == 'value'",
                         plotOutput("ridgeplot2")
                     )
              ),
              column(width = 15,
                     conditionalPanel(
                         condition = "input.ridge23toggle == 'percentile'",
                        plotOutput("ridgeplot3")
                     )
              )
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
      colorbar(title = 'Happiness Score') %>%
      layout(
        geo = geo)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
