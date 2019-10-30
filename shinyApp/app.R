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
                    box(width = 15,
                        title = "Happiness Score Distribution",
                        color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                        column(width = 15,
                               plotOutput("ridgeplot1")
                        )
                    )
                ),
                fluidRow(
                    box(width = 15,
                        title = "Happiness Score Distribution",
                        color = "teal", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                        column(width = 15,
                            plotOutput("ridgeplot3")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "breakdown"
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
               "PerceptionsOfCorruption"="Explained by: Perceptions of corruption") %>%
        mutate(GDP_Percent = `GDP`/`Happiness score`,
               SocialSupport_Percent = `SocialSupport`/`Happiness score`,
               LifeExpectancy_Percent = `LifeExpectancy`/`Happiness score`,
               Freedom_Percent = `Freedom`/`Happiness score`,
               Generosity_Percent = `Generosity`/`Happiness score`,
               PerceptionsOfCorruption_Percent = `PerceptionsOfCorruption`/`Happiness score`) %>%
        gather( key = "Factor", value = "Value", `GDP`:`PerceptionsOfCorruption`, na.rm = FALSE, convert = FALSE, factor_key = FALSE) %>%
        gather( key = "PercentFactor", value = "PercentValue", `GDP_Percent`:`PerceptionsOfCorruption_Percent`, na.rm = FALSE, convert = FALSE, factor_key = FALSE) 
    
    output$ridgeplot1 <- renderPlot({
        ggplot(data1, aes(y=as.factor(Year),
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
            theme(axis.text=element_text(size=10))
    })
    
    output$ridgeplot3 <- renderPlot({
        ggplot(data2, aes(y=1,x=PercentValue,fill=PercentFactor)) +
            geom_density_ridges(alpha=0.5) +
            scale_y_discrete(expand = c(0.01, 0)) +  
            scale_x_continuous(expand = c(0, 0))+
            theme(axis.text=element_text(size=10))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
