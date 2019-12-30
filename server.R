library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

shinyServer(function(input, output) {
# Create text output for Description tab
    output$text <- renderText({
        
        "This application analyses Health Care Resources Database from OECD (https://data.oecd.org/healthres/health-spending.htm#indicator-chart). The database includes the health expenditure, the number of doctors and other indicators since 1961 from around the world. This application focuses on health expenditure and doctors, these two indicators for G20 countries only. This application shows the difference in health care resourses in G20 countries and allows the users to do the comparisons and analysis."
    })

# Map (Non-interactive)
    output$map<-renderPlot({
      
# Read the data
      map_data<-read.csv("https://gwu-workshop-2019-wxs.s3.us-east-2.amazonaws.com/Geographical_position.csv")
      doctors<-read.csv("https://gwu-workshop-2019-wxs.s3.us-east-2.amazonaws.com/doctors.csv")
      
# Manipulate the data  
      map_data$Code<-as.character(map_data$Code)
      colnames(doctors) <- c('Code', 'indicator', 'subject', 'measure', 'frequency', 'time', 'value','other')
      doctors <- doctors %>% select('Code','time','value')
      G20<-c("AUS", "BRA", "CAN", "CHN", "DEU", "FRA", "GBR", "IDN", "IND", "ITA", "JPN", "KOR" ,"MEX", "RUS","TUR","USA","ZAF","EU","SAU","ARG")
      doctors<-subset(doctors,Code %in% G20)
      doctors$Code<-as.character(doctors$Code)
      
# Join the two data sets
      total<-left_join(map_data, doctors, by = "Code")
      total<-total %>% filter(time==2017) #only show the latest data
      
# Create the map
      mapworld<-borders("world",colour = "gray50",fill="white") 
      mp<-ggplot(total)+mapworld+ylim(-60,90)
      mp = mp + geom_point(aes(x=Longitude,y=Latitude,size=value,color=value))+scale_colour_gradient(low="green",high="red")+scale_size(range=c(1,8))
# Plot     
      mp
    })
      
# Plot 1-interactive
    output$p1 <- renderPlot({
# Read input file
        doctors<-read.csv("https://gwu-workshop-2019-wxs.s3.us-east-2.amazonaws.com/doctors.csv")

# Manipulate input file
colnames(doctors) <- c('country', 'indicator', 'subject', 'measure', 'frequency', 'time', 'value','code')
doctors <- doctors %>% select('country','time','value')
G20<-c("AUS", "BRA", "CAN", "CHN", "DEU", "FRA", "GBR", "IDN", "IND", "ITA", "JPN", "KOR" ,"MEX", "RUS","TUR","USA","ZAF","EU","SAU","ARG")
doctors<-subset(doctors,country %in% G20)
doctors$bubblesize<- 10*sqrt(doctors$value/pi)

# Plot
doctors<-doctors %>% filter(time==input$y)
p <-ggplot(doctors)
p +aes(x=country,y=value,color=country)+
    geom_point(stat= "identity",aes(size=bubblesize),alpha=0.7,show.legend = TRUE)+ 
    guides(color=guide_legend(title="country"))+
    scale_size(range = c(1, 20),guide=FALSE)+
    labs(x='country',y='Doctors per 100 habitants',title=paste('G20 Countries Health Care Resource--Doctors in',input$y))+
    geom_text(aes(y=value,label=value,hjust=0.5), size=3,color="black",position = position_dodge(width=0.00),check_overlap = FALSE) 
})

# Plot2-Interactive
      output$p2<- renderPlot({
        
# Read input file
expense<-read.csv("https://gwu-workshop-2019-wxs.s3.us-east-2.amazonaws.com/Health_spending.csv")

# Manipute the input file
colnames(expense) <- c('country', 'indicator', 'subject', 'measure', 'frequency', 'time', 'value','code')
G20<-c("AUS", "BRA", "CAN", "CHN", "DEU", "FRA", "GBR", "IDN", "IND", "ITA", "JPN", "KOR" ,"MEX", "RUS","TUR","USA","ZAF","EU","SAU","ARG")
expense<-subset(expense,country %in% G20)
expense <- expense %>% select('country','time','value','subject')

# Plot
expense<-expense %>% filter(country==input$country&(subject=="VOLUNTARY"|subject=="COMPULSORY"))
p<-ggplot(expense)
p+aes(x=time,y=value,fill=subject)+geom_bar(stat="identity")+labs(x="Year",y='Health spending in USD/capita',title=paste("Health care resource-spending in",input$country))+ guides(fill=guide_legend(title="subject"))
})


# Create data table output--interactive
    output$table<- renderTable({
    
# Read input file
    expense<-read.csv("https://gwu-workshop-2019-wxs.s3.us-east-2.amazonaws.com/Health_spending.csv")
    map_data<-read.csv("https://gwu-workshop-2019-wxs.s3.us-east-2.amazonaws.com/Geographical_position.csv")
    
# manipute the input file
    map_data$Code<-as.character(map_data$Code)
    colnames(expense) <- c('Code', 'Indicator', 'Subject', 'Measure', 'Frequency', 'Year', 'Expenditure(USD/capita)','code')
    G20<-c("AUS", "BRA", "CAN", "CHN", "DEU", "FRA", "GBR", "IDN", "IND", "ITA", "JPN", "KOR" ,"MEX", "RUS","TUR","USA","ZAF","EU","SAU","ARG")
    expense<-subset(expense,Code %in% G20)
    expense$Subject=factor(expense$Subject)
    expense$Code<-as.character(expense$Code)
    
# Join the data
    total2<-left_join(map_data, expense, by = "Code")
    
# Filter the data for different states selected by user input
    table<- total2 %>% filter(Subject==input$subject) %>%select('Country','Year','Expenditure(USD/capita)')

# Return table
    return(table)
   })
})





