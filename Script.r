
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)


# Data Preparation Steps

data <- read.csv("Dec-2017.csv")

data$Date <- strptime(as.character(data$Date.yyyy.MM.dd.),format="%m/%d/%Y")
data$Date <- as.POSIXct(data$Date)

data$DateTime <- strptime(as.character(data$DateTime),format="%m/%d/%Y %H:%M")
data$DateTime <- as.POSIXct(data$DateTime)

data$Day <- as.numeric(as.character(strftime(data$DateTime,format="%d")))
data$Hour <- as.numeric(as.character(strftime(data$DateTime,format="%H")))

data <- data %>% filter(BC6!=0)





ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny - First Interactive Visualization Example"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  selected = "Blue",multiple = F),
      
      radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
      
      selectInput(inputId="channel1",label="Choose Channel",choices = c("BC1"="BC1",
                                                                        "BC2"="BC2",
                                                                        "BC3"="BC3",
                                                                        "BC4"="BC4",
                                                                        "BC5"="BC5",
                                                                        "BC6"="BC6",
                                                                        "BC7"="BC7"),
                  selected = "BC6",multiple = F),
      
      sliderInput(inputId = "bins1xz",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      sliderInput(inputId = "range1",
                  label = "Data Range",
                  min = 1,
                  max = 31,
                  value = c(1,31))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "distPlot1"),
      plotOutput(outputId = "distPlot2")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output){
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    p2 <- data %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot()
    if(input$channel1 == "BC1"){
      p2 <- p2 + geom_histogram(aes(x=BC1),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC2"){
      p2 <- p2 + geom_histogram(aes(x=BC2),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC3"){
      p2 <- p2 + geom_histogram(aes(x=BC3),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC4"){
      p2 <- p2 + geom_histogram(aes(x=BC4),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC5"){
      p2 <- p2 + geom_histogram(aes(x=BC5),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC6"){
      p2 <- p2 + geom_histogram(aes(x=BC6),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "BC7"){
      p2 <- p2 + geom_histogram(aes(x=BC7),bins = input$bins1xz,col=input$border1,fill=sColor)
    }
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Black Carbon (ng/m3)",y="Count",title=paste("Black Carbon Concentration Histogram",input$channel1,sep = " "))
    
    p2
    #hist(x, breaks = bins, col = sColor, border = input$border1,
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
  })
  
  output$distPlot1 <- renderPlot({
   
    p1 <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot(aes(x=DateTime))
    if(input$channel1 == "BC1"){
      p1 <- p1 + geom_line(aes(y=BC1,col="BC1"),size=0.5)
    }else
    if(input$channel1 == "BC2"){
      p1 <- p1 + geom_line(aes(y=BC2,col="BC2"),size=0.5)
    }else
    if(input$channel1 == "BC3"){
      p1 <- p1 + geom_line(aes(y=BC3,col="BC3"),size=0.5)
    }else
    if(input$channel1 == "BC4"){
      p1 <- p1 + geom_line(aes(y=BC4,col="BC4"),size=0.5)
    }else
    if(input$channel1 == "BC5"){
      p1 <- p1 + geom_line(aes(y=BC5,col="BC5"),size=0.5)
    }else
    if(input$channel1 == "BC6"){
      p1 <- p1 + geom_line(aes(y=BC6,col="BC6"),size=0.5)
    }else
    if(input$channel1 == "BC7"){
      p1 <- p1 + geom_line(aes(y=BC7,col="BC7"),size=0.5)
    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Time",y="Black Carbon (ng/m3)",title="Black Carbon Concentration in Air - Dec, 2017",colour="Channel")

    p1
    
  })
  
  output$distPlot2 <- renderPlot({
    d <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2])
    
    d <- ddply(d, .variables = c("Hour"),function(x){
      
      BC1avg <- mean(x$BC1,na.rm = T)
      BC2avg <- mean(x$BC2,na.rm = T)
      BC3avg <- mean(x$BC3,na.rm = T)
      BC4avg <- mean(x$BC4,na.rm = T)
      BC5avg <- mean(x$BC5,na.rm = T)
      BC6avg <- mean(x$BC6,na.rm = T)
      BC7avg <- mean(x$BC7,na.rm = T)
      
      data.frame(BC1avg,BC2avg,BC3avg,BC4avg,BC5avg,BC6avg,BC7avg)
    })
    
    p1 <- d %>% ggplot(aes(x=Hour))
    if(input$channel1 == "BC1"){
      p1 <- p1 + geom_line(aes(y=BC1avg,col="BC1"),size=1)
      p1 <- p1 + geom_point(aes(y=BC1avg))
    }else if(input$channel1 == "BC2"){
      p1 <- p1 + geom_line(aes(y=BC2avg,col="BC2"),size=1)
      p1 <- p1 + geom_point(aes(y=BC2avg))
    }else if(input$channel1 == "BC3"){
      p1 <- p1 + geom_line(aes(y=BC3avg,col="BC3"),size=1)
      p1 <- p1 + geom_point(aes(y=BC3avg))
    }else if(input$channel1 == "BC4"){
      p1 <- p1 + geom_line(aes(y=BC4avg,col="BC4"),size=1)
      p1 <- p1 + geom_point(aes(y=BC4avg))
    }else if(input$channel1 == "BC5"){
      p1 <- p1 + geom_line(aes(y=BC5avg,col="BC5"),size=1)
      p1 <- p1 + geom_point(aes(y=BC5avg))
    }else if(input$channel1 == "BC6"){
      p1 <- p1 + geom_line(aes(y=BC6avg,col="BC6"),size=1)
      p1 <- p1 + geom_point(aes(y=BC6avg))
    }else if(input$channel1 == "BC7"){
      p1 <- p1 + geom_line(aes(y=BC7avg,col="BC7"),size=1)
      p1 <- p1 + geom_point(aes(y=BC7avg))
    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Time",y="Black Carbon (ng/m3)",title="Black Carbon Concentration in Air - Average Diurnal Variation - Dec, 2017",colour="Channel")
    
    p1
    
  })
}

shinyApp(ui = ui, server = server)
