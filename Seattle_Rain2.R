

library(shiny)  


# Shiny App

ui <- fluidPage(
  titlePanel('It is going to rain today in Seattle?'),
  sidebarPanel(
    sliderInput(inputId = "Month", label = "What month are we in?", value = 1, min = 1, max = 12, step=1),
    numericInput(inputId="Rain_yesterday", label="Did it rain yesterday?", value=0,0,1),
    sliderInput(inputId = "Amount_Rain_yesterday", label = "How much did it rain yesterday?", value = 0, min = 0, max = 25),
    sliderInput(inputId = "Tmax_yesterday", label = "What was the maximum temperature yesterday?", value = 50, min = 0, max = 100),
    sliderInput(inputId = "Tmin_yesterday", label = "What was the minimum temperature yesterday?", value = 40, min = 0, max = 100)
  ),
  mainPanel(
    span(textOutput("RainSun"), style="color:red; 
                                      font-size:50px;
                                      font-style:italic;
                                      font-style:bold")
))

server <- function(input, output) {
  
  output$RainSun <- renderText({
    month_effect=ifelse(input$Month==1, -0.613214867,
                        ifelse(input$Month==2, -0.613214867-0.077224113,
                               ifelse(input$Month==3, -0.613214867-0.046561824,
                                      ifelse(input$Month==4, -0.613214867-0.222228005, 
                                             ifelse(input$Month==5, -0.613214867-0.673810812,
                                                    ifelse(input$Month==6, -0.613214867-0.908368030,
                                                           ifelse(input$Month==7, -0.613214867-1.615643143,
                                                                  ifelse(input$Month==8, -0.613214867-1.458264173,
                                                                         ifelse(input$Month==9, -0.613214867-1.027356382,
                                                                                ifelse(input$Month==10,-0.613214867-0.575591607,
                                                                                       ifelse(input$Month==11, -0.613214867-0.073702207, -0.613214867-0.002629794)))))))))))
    rain_today=month_effect+1.339804930*input$Amount_Rain_yesterday-0.036281442*input$Tmax_yesterday+
      0.054275530*input$Tmin_yesterday+0.988534351*input$Rain_yesterday
  
#   return(rain_today)
    
    if (round(rain_today, digits=0)<0.5){
     return("Finally! No rain today :)")
    
   } else {
       return("Sorry... another rainy day :(")
    }

   
})

}
shinyApp(ui = ui, server = server)  