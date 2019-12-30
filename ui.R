
library(shiny)

# Define style of application and title
ui <- navbarPage("Health Care Resources in G20 Countries ",
                 tabPanel("Description", h4('Analysing health care resources in G20 counties'), textOutput("text"), hr()),
                 
# Create tab for map
      tabPanel('Map', titlePanel(title="Doctors per 1000 inhabitants for G20 countries-latest data(2017)"),
                            plotOutput('map')),
# Create tab for plot
      navbarMenu("Plots",
           tabPanel("Doctors Per 1000 Inhabitants in Each Year", sidebarPanel(
                      sliderInput("y", "Year:", min = 1997, max = 2017, value = 100,
                                  step = 1)),
            mainPanel(
                plotOutput("p1")
              )
                     
               ),
            tabPanel("Health Care Expenditure (USD/capita)", sidebarPanel(
                      selectInput("country", label = "Select a country in G20",
                                  choices = c("AUS", "BRA", "CAN", "CHN", "DEU", "FRA", "GBR", "IDN", "IND", "ITA", "JPN", "KOR" ,"MEX", "RUS","TUR","USA","ZAF"), selected = "USA")),
                mainPanel(
                  plotOutput("p2")
               )
                 
                 )),
                     
# Create tab for data table
tabPanel('Table', 
         sidebarPanel(
           selectInput("subject", label = "Category",
                       choices = c("Government"="COMPULSORY","Total"="TOT","Voluntary"="VOLUNTARY"), selected = "TOT")),
         mainPanel(
           tableOutput('table'))
         )
)

                 
                 
  