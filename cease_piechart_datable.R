library(shiny)
library(tidyverse)
library(BSDA)
library(bslib)
library(scales)
library(shinythemes)

df_cease <- read_csv("C:/Users/chris/Downloads/savant_data.csv")

ui = fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Dylan Cease No Hitter"),
  fluidRow(
    column(4,
           selectInput("PT",
                       "Pitch Type: ",
                       c("All",
                         unique(as.character(df_cease$pitch_type)))
           )
           ),
    column(4,
           radioButtons("inning",
                        "Inning",
                        choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        selected = 1)),
    column(4,
           selectInput("outs",
                       "Number of Outs",
                       c(0, 1, 2),
                       selected = 0),
           plotOutput("PieOuts"))
  ),
  hr(),
  
  fluidRow(
    column(6,
           DT::dataTableOutput("table")
           
  )
    
 ),
 hr(),
 
 fluidRow(
   column(4,
          selectInput("LR", "Handedness of Batter: ", choices =  c("L", "R"),
                      selected = "L")),
   column(4,
          selectInput("PTLR", "Select Pitch Type", choices = c(unique(as.character(df_cease$pitch_type))))),
   column(6,
          DT::dataTableOutput("tableLR"))
 )
)
  
  
  
  

server = function(input,output){
  
  output$table = DT::renderDataTable({
    data = df_cease %>% 
      select(pitch_type, events, inning, outs_when_up, 
             effective_speed, launch_angle, launch_speed) %>% 
      filter(inning == input$inning)
    if (input$PT != "All"){
      data = data[data$pitch_type == input$PT, ]
    }
    data
  })
  
  output$PieOuts = renderPlot({
    pie_data = df_cease %>% 
      group_by(outs_when_up, pitch_type) %>% 
      summarise(Total_Pitches = n()) %>% 
      filter(outs_when_up == input$outs)
    
    ggplot(pie_data, aes(x = "", y = Total_Pitches, fill = pitch_type)) +
      geom_col()+
      coord_polar(theta = "y")+
      geom_text(aes(label = Total_Pitches), position = position_stack(vjust = 0.5)) +
      theme_bw() 
      
  })
  
  output$tableLR = DT::renderDataTable({
    dataLR = df_cease %>% 
      group_by(events, stand) %>% 
      select(pitch_type, events, stand, description) %>% 
      filter(pitch_type == input$PTLR)
    if(input$LR != "All"){
      dataLR = dataLR[dataLR$stand == input$LR, ]
    }
    
    dataLR
  })
  
}



shinyApp(ui = ui, server = server)

