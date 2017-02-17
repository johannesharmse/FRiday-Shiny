#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(southafricastats)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)


mortality = mortality_zaf %>% filter(indicator != "All causes")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("South Africa Stats"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "province",
                     label = "Choose a province:",
                     choices = unique(mortality_zaf$province), 
                     selected = unique(mortality_zaf$province)[[5]], 
                     multiple = TRUE), 
         checkboxInput(inputId = "showtable", 
                       label = "Show table?", 
                       value = FALSE)
         
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("LinePlot"), 
         dataTableOutput("mortalityTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$LinePlot <- renderPlot({
      mortality %>% filter(province %in% input$province) %>% 
       ggplot(aes(year, deaths, color = indicator)) + 
       facet_wrap(~province) + 
       geom_line(alpha = 0.8, size = 2) + theme_minimal(base_size = 18)
     
   })
   
   output$mortalityTable = renderDataTable({
     if (input$showtable) {DT::datatable(mortality %>% filter(province %in% input$province))}
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

