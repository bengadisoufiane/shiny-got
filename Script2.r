
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(sf)
library(tidyr)
library(leaflet)

library(readr)
# Data Preparation Steps
characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")

locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)

lakes=st_read("data/GoTRelease/Lakes.shp",crs=4326)

conts=st_read("./data/GoTRelease/Continents.shp",crs=4326)
land=st_read("./data/GoTRelease/Land.shp",crs=4326)
wall=st_read("./data/GoTRelease/Wall.shp",crs=4326)
islands=st_read("./data/GoTRelease/Islands.shp",crs=4326)
kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326)
landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326)
roads=st_read("./data/GoTRelease/Roads.shp",crs=4326)
rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326)

main_char= c("Jon Snow", "Tyrion Lannister","Daenerys Targaryen","Sansa Stark","Cersei Lannister","Arya Stark")
scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)
season=c("1","2","3","4","5","6","7","8")
main_char= c("Jon Snow", "Tyrion Lannister","Daenerys Targaryen","Sansa Stark","Cersei Lannister","Arya Stark")
landpol = st_union(st_geometry(land)) 
islandpol = st_union(st_geometry(islands))
backpol=st_union(landpol,islandpol)


loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
loc_time_mc = scenes_locations %>% left_join(loc_time)
background = st_as_sf(data.frame(name=main_char,geometry=rep(backpol,6)))

loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
loc_time_mc = scenes_locations %>% left_join(loc_time)
colforest="#c0d7c2"
colriver="#7ec9dc"
colriver="#87cdde"
colland="ivory"
borderland = "ivory3"  


ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny - First Interactive Visualization Example"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      
      checkboxGroupInput(inputId = "season",label = "season",choices =season ,selected =season),
      
      selectInput(inputId="personnage",label="personnage",choices =main_char,
                  selected = "BC6",multiple = F),
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('statistic',plotOutput('stat'),plotOutput('stat1'),plotOutput('stat2')),
        tabPanel('map',plotOutput('map')),
        tabPanel('hist',plotOutput("hist")),
        tabPanel('characters',DT::DTOutput('characters')),
        tabPanel('episodes',DT::DTOutput('episodes')),
        tabPanel('scenes',DT::DTOutput('scenes')),
        tabPanel('appearances',DT::DTOutput('appearances'))
      )
    )
    
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output){
  #mise a jour des donnees avec une fonction reactive 
  characters_ <- reactive(characters)
  episodes_ <- reactive(episodes)
  scenes_ <- reactive(scenes)
  appearances_ <- reactive(appearances_)
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$stat <- renderPlot({

    
    scenes_stats=scenes %>% left_join(episodes) %>% 
      group_by(episodeTitle,seasonNum) %>% filter(seasonNum %in% input$season) %>% 
      summarize(nb_scenes=n(),duration_max=max(duration),nbdeath=sum(nbdeath))
    
    labels = scenes_stats %>% filter(duration_max>400|nb_scenes>200|seasonNum==1)
    ggplot(scenes_stats,aes(x=nb_scenes,y=duration_max,col=factor(seasonNum)))+
      geom_point(aes(size=nbdeath))+
      geom_text(data=labels,aes(label=episodeTitle),vjust=-0.6)+
      scale_x_continuous("Nombre de scène",limits = c(0,280))+
      scale_y_continuous("Durée de la scène la plus longue",limits = c(100,300))+
      scale_color_brewer("Saison",palette ="Spectral")+
      guides(colour = "legend", size = "legend")+
      ggtitle("informations sur les saisons et le nombre de morts ")+
      theme_bw()
  })
  output$stat1 <- renderPlot({
    
    
    screenTimePerSeasons = appearances %>% left_join(scenes) %>% 
      left_join(episodes) %>% 
      group_by(name,seasonNum) %>%  filter(name %in% main_char)%>%  filter(seasonNum %in% input$season)%>% 
      summarise(screenTime=sum(duration)) %>% 
      arrange(desc(screenTime)) 
    screenTimeTotal = screenTimePerSeasons %>% 
      group_by(name) %>% 
      summarise(screenTimeTotal=sum(screenTime))
    mainCharacters = screenTimeTotal %>% 
      filter(screenTimeTotal>60*60) %>% 
      arrange(screenTimeTotal) %>% 
      mutate(nameF=factor(name,levels = name))
    data = screenTimePerSeasons %>% left_join(mainCharacters) %>% filter(!is.na(nameF))
    ggplot(data)+
      geom_bar(aes(y=nameF,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=mainCharacters,aes(y=nameF,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")+ggtitle("Temps d'apparition cumulé par personnage et saison")
  })
  output$stat2 <- renderPlot({
    
    labels = scenes %>% filter(duration>400)
    ggplot(scenes %>% left_join(episodes)%>% filter(seasonNum %in% input$season))+
      geom_boxplot(aes(x=factor(episodeId),y=duration,fill=factor(seasonNum)))+
      geom_text(data=labels ,aes(x=factor(episodeId),y=duration,label=subLocation),hjust = "right",vjust="top")+
      scale_x_discrete("N° épisode",as.character(seq(1,73, by=5)))+
      scale_fill_brewer(palette="Spectral",guide="none")+
      ylab("Durée des scènes (min)")+
      ggtitle("Répartition des durées des scènes par épisodes")+
      theme_bw()
  })
  
  output$map <- renderPlot({
    
    
    ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
      geom_sf(data=loc_time_mc%>% filter(!is.na(duration))%>% filter(name==input$personnage)
              ,aes(size=duration/60,color=input$personnage))+
      geom_sf_text(data=loc_time_mc%>% filter(!is.na(duration))%>% filter(name==input$personnage)
                   ,aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
      coord_sf(expand = 0,ndiscr = 0)+
      scale_color_discrete(guide="none")+
      scale_size_area("Durée (min) :",max_size = 12,breaks=c(30,60,120,240))+
      theme(panel.background = element_rect(fill = colriver,color=NA),
            text = element_text(family="Palatino",face = "bold",size = 14),
            legend.key = element_rect(fill="#ffffff"),
      ) +
      labs(title = paste("Temps de présence de : ",  input$personnage),x="",y="")

    
  })
  
  output$hist <- renderPlot({
    

    ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
      geom_sf(data=islands,fill=colland,col="ivory3")+
      geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
      geom_sf(data=rivers,col=colriver)+
      geom_sf(data=lakes,col=colriver,fill=colriver)+
      geom_sf(data=wall,col="black",size=1)+
      geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
      theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
      theme(panel.background = element_rect(fill = colriver,color=NA)) +
      labs(title = "fond de carte de l’univers GoT avec les lacs, rivières et forêts ainsi que les noms des principales villes.",x="",y="")
    
  })
  #un tableau interactif
  output$characters <- DT::renderDT(
    characters_()
  )
  output$episodes <- DT::renderDT(
    episodes_()
  )
  output$scenes <- DT::renderDT(
    scenes_()
  )
  output$appearances <- DT::renderDT(
    appearances_()
  )
}

shinyApp(ui = ui, server = server)
