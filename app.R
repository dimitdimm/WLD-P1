
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(auth0)

library(tidyverse)
library(readxl)
#library(officer)

library(thematic)
library(scales)
library(DT)
library(shinysurveys)


#thematic_shiny()

###############
##Global Functions 
###############


##- Authintication 


##- Forms / Funtions 


##- Vizualizaition 


##- Reports & Analysis 




###############
##1 - Data load 
###############

df_dma_q <- read_xlsx('C:/Users/ddimitrov8/OneDrive - DXC Production/Documents/GitHub/WLD-P1/dma.xlsx', sheet = 'dma')
#df_dma_q <- read_csv('C:/Users/ddimitrov8/OneDrive - DXC Production/Documents/GitHub/New/dma.xlsx')

###############
# Layout 
###############


assessments = c("Digital Maturity","Data maturity","NIS2")

sidebar_content <-
  list(
    
    title = "Select Assessment Module",
    
    pickerInput(
      inputId = "assessment_seleted",
      label = tags$span(class = "bold-label", "Modules :"), 
      choices = assessments,
      options = list(
        `actions-box` = TRUE),
      selected = "",
      multiple = FALSE
    ),
    
    
   
    
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(), 
    tags$br(),
    tags$br(),

    
    "© WLD Company"
  )



###############
## UI Definition
###############

ui <- page_sidebar(
  
  
  
  tags$style(HTML(
    "body {
      font-family: 'Arial', sans-serif;
      background-color: #f8f8f8;
      }"
  )),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  theme = bs_theme(
    bootswatch = "yeti",
    base_font = "Arial",
    heading_font = font_face(
      family = "Arial",
      src = "url('../arial.ttf') format('truetype')"
    ) 
  ),
  
  title = "WLD Assessment Center",
  
  
  sidebar = sidebar(
    HTML('<img src="logo_text11.png" width="100%" height="auto">'),
    HTML('<div style="height: 25px;"></div>'), # Adjusting padding
    
    sidebar_content # Sidebar content
  ),
  
  
  navset_card_tab(
    
  
    
    nav_panel(
      
      title = "Assment",
      fluid = FALSE,
      
      
      
      shinysurveys::surveyOutput(df = df_dma_q,
                 survey_title = "Data Maturity Assessment",
                 survey_description = "A minimal description",
                 theme = "#f8f8f8" # modifies entire BSLIB theme Body bg-color !!!!!!!! 
                 )
      
      
    ),
      
      
      
    nav_panel(
          
          title = "Results",
          fluid = TRUE,
          layout_columns(  
            
            column(12, h5("1st Column"),
                   card(
                     full_screen = TRUE,
                     class = "bold-cards",
                     card_header("One One")
                     ),
                   value_box(
                     title = tags$p("Value Box"),
                     showcase_layout = c("left center"),
                     textOutput("VB1"),
                     value = p("VB1") ,
                     p("Overall result:"),
                     p("Drill down 1: "),
                     p("Drill down2: ")
                   ),
                   
                   ),
            column(12, h5("2nd Column"),
                   card(
                     full_screen = TRUE,
                     class = "bold-cards",
                     card_header("Two Two")
                    ),
                   value_box(
                     title = tags$p("Value Box2"),
                     showcase_layout = c("left center"),
                     textOutput("VB2"),
                     value = p("VB2") ,
                     p("Overall result:"),
                     p("Drill down 1: "),
                     p("Drill down2: ")
                   )
                   )
            
            )
          )
        
      )
    
)
  


###############
## Server logic 
###############

server <- function(input, output, session) {
  #bs_themer() 
  
  shinysurveys::renderSurvey()
  
  
  observeEvent(input$submit, {
   
     showModal(modalDialog(
      title = "Congrats, you completed Data Maturity Survey!",
      "You can customize what actions happen when a user finishes a survey using input$submit."
    ))
    
    response_data <- getSurveyData()
    print(response_data)
  })
}


###############
## Run the application 
###############

shinyApp(ui, server)

