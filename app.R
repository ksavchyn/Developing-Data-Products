ui <- fluidPage(
  headerPanel("What's Your Astrological Sign?"),
  
    sidebarPanel(
    dateInput("date", "Enter Your Birthdate")),

    fluidRow(
      
      h3("You were born on"),
      verbatimTextOutput("dob"),
      h3("Your astrological sign is"),
      verbatimTextOutput("theirsign"),
      h3("Your element is"),
      verbatimTextOutput("theirelement"),
      verbatimTextOutput("theirpersonality"),
      column(9, offset = -20,
             img(src="elements.jpg", height = 300, width = 800))
    )
   )

server <- function(input, output){
    output$dob <- renderPrint({input$date})
     
    month <- reactive({as.numeric(format(input$date, "%m"))})
    day <- reactive({as.numeric(format(input$date, "%d"))})
    
    output$theirsign <- renderPrint({
      
        ifelse((month()==3 & day() %in% 21:31) | (month()==4 & day() %in% 1:19), "Aries", 
        ifelse((month()==4 & day() %in% 20:31) | (month()==5 & day() %in% 1:20), "Taurus",
        ifelse((month()==5 & day() %in% 21:31) | (month()==6 & day() %in% 1:21), "Gemini",
        ifelse((month()==6 & day() %in% 22:31) | (month()==7 & day() %in% 1:22), "Cancer",
        ifelse((month()==7 & day() %in% 23:31) | (month()==8 & day() %in% 1:22), "Leo", 
        ifelse((month()==8 & day() %in% 23:31) | (month()==9 & day() %in% 1:22), "Virgo", 
        ifelse((month()==9 & day() %in% 23:31) | (month()==10 & day() %in% 1:23), "Libra", 
        ifelse((month()==10 & day() %in% 24:31)| (month()==11 & day() %in% 1:20), "Scorpio", 
        ifelse((month()==11 & day() %in% 21:31)| (month()==12 & day() %in% 1:21), "Saggitarius", 
        ifelse((month()==12 & day() %in% 22:31)| (month()==1 & day() %in% 1:20), "Capricorn", 
        ifelse((month()==1 & day() %in% 21:31) | (month()==2 & day() %in% 1:21), "Aquarius", 
        ifelse((month()==2 & day() %in% 22:31) | (month()==3 & day() %in% 1:20), "Pisces",
        "I don't know"))))))))))))
            
            })
    
    output$theirelement <- renderPrint({
      ifelse((month()==3 & day() %in% 21:31) | (month()==4 & day() %in% 1:19) 
           | (month()==7 & day() %in% 23:31) | (month()==8 & day() %in% 1:22)
           | (month()==11 & day() %in% 21:31)| (month()==12 & day() %in% 1:21), "FIRE",
      ifelse((month()==4 & day() %in% 20:31) | (month()==5 & day() %in% 1:20)
           | (month()==8 & day() %in% 23:31) | (month()==9 & day() %in% 1:22)
           | (month()==12 & day() %in% 22:31)| (month()==1 & day() %in% 1:20), "EARTH", 
      ifelse((month()==5 & day() %in% 21:31) | (month()==6 & day() %in% 1:21)
           | (month()==9 & day() %in% 23:31) | (month()==10 & day() %in% 1:23)
           | (month()==1 & day() %in% 21:31) | (month()==2 & day() %in% 1:21), "AIR", 
      ifelse((month()==6 & day() %in% 22:31) | (month()==7 & day() %in% 1:22)
           | (month()==10 & day() %in% 24:31)| (month()==11 & day() %in% 1:20)
           |(month()==2 & day() %in% 22:31) | (month()==3 & day() %in% 1:20), "WATER",
      "I don't know"))))
        
            })
    
    output$theirpersonality <- renderText({
      
      ifelse((month()==3 & day() %in% 21:31) | (month()==4 & day() %in% 1:19), "You are known to be adventurous, courageous, and versatile, 
but also can tend to be arrogant, stubborn, and impulsive.", 
      ifelse((month()==4 & day() %in% 20:31) | (month()==5 & day() %in% 1:20), "Yu are known to be generous, dependable, and down to earth,
but also can tend to be self-indulgent, lazy, and materialistic.",
      ifelse((month()==5 & day() %in% 21:31) | (month()==6 & day() %in% 1:21), "You are known to be flexible, enthusiastic, and witty,
but also can tend to be inconsistent, superficial, and indecisive. ",
      ifelse((month()==6 & day() %in% 22:31) | (month()==7 & day() %in% 1:22), "You are known to be creative, spontaneous, and faithful, 
but also can tend to be moody, pessimistic, and clingy.",
      ifelse((month()==7 & day() %in% 23:31) | (month()==8 & day() %in% 1:22), "You are known to be kind, energetic, and optimistic, 
but also can tend to be headstrong, egoistic, and possessive.", 
      ifelse((month()==8 & day() %in% 23:31) | (month()==9 & day() %in% 1:22), "You are known to be watchful, intelligent and practical,
but also can tend to be overcritical, fussy, and fastidious.", 
      ifelse((month()==9 & day() %in% 23:31) | (month()==10 & day() %in% 1:23), "You are known to be tactful, romantic, and charming,
but also can tend to be detached, unreliable, and superficial.", 
      ifelse((month()==10 & day() %in% 24:31)| (month()==11 & day() %in% 1:20), "You are known to be focused, brave, and balanced,
but also can tend to be jealous, secretive, and resentful.", 
      ifelse((month()==11 & day() %in% 21:31)| (month()==12 & day() %in% 1:21), "You are known to be straightforward, philosophical, and big-hearted,
but also can tend to be careless, impatient, and over-confident.", 
      ifelse((month()==12 & day() %in% 22:31)| (month()==1 & day() %in% 1:20), "You are known to be practical, ambitious, and wise, 
but also can tend to be pessimistic, stubborn, and shy.", 
      ifelse((month()==1 & day() %in% 21:31) | (month()==2 & day() %in% 1:21), "You are known to be friendly, humanitarian, and independent,
but also can tend to be unpredictable, aloof, and extremist.", 
      ifelse((month()==2 & day() %in% 22:31) | (month()==3 & day() %in% 1:20), "You are known to be imaginative, kind, and compassionate,
but also can tend to be escapist, idealistic, and over-sensitive",
     "I don't know"))))))))))))

  })
    
  
}    
  
shinyApp(ui=ui, server=server)