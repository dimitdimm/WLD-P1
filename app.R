
library(shiny)
library(shinyjs)
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

#- Digital Maturity Assessment
df_dma_q <- read_xlsx('C:/Users/ddimitrov8/OneDrive - DXC Production/Documents/GitHub/WLD-P1/dma.xlsx', sheet = 'dig_ma') 
#- Data Maturity Assessment
df_datam_q <- read_xlsx('C:/Users/ddimitrov8/OneDrive - DXC Production/Documents/GitHub/WLD-P1/dma.xlsx', sheet = 'data_ma')
#- NIS2 Assessment
df_nis_q <- read_xlsx('C:/Users/ddimitrov8/OneDrive - DXC Production/Documents/GitHub/WLD-P1/dma.xlsx', sheet = 'nis2')




surveys <- list(
  "survey_dma" = df_dma_q,
  "survey_datam" = df_datam_q,
  "survey_nis" = df_nis_q
)

###############
# Layout 
###############


assessments = c("","Digital Maturity","Data Maturity","NIS2")

sidebar_content <-
  list(
    
    title = "Select Assessment Module",
    
    pickerInput(
      inputId = "assessment_seleted",
      label = tags$span(class = "bold-label", "Modules :"), 
      choices = assessments,
      options = list(
        `actions-box` = TRUE),
      selected = NULL,
      multiple = FALSE
    ),
    actionButton("confirm_btn", "Confirm Selection", class = "btn-primary"),
    
    
    
    
    
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
      title = "Assessment",
      fluid = FALSE,
      mainPanel(
        useShinyjs(),
        uiOutput("survey_ui")
        
        
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
  
 
  # Store the selected survey
  selectedSurvey <- reactiveVal(NULL)
  
  # Observe confirmation button
  observeEvent(input$confirm_btn, {
    if (is.null(input$assessment_seleted) || input$assessment_seleted == "") {
      showModal(modalDialog(
        title = "Error",
        "Please select a survey from the dropdown menu.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Confirmation",
        paste("Do you want to proceed with", input$assessment_seleted, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_confirmation", "OK")
        )
      ))
    }
  })
  
  # Handle modal confirmation
  observeEvent(input$ok_confirmation, {
    selectedSurvey(input$assessment_seleted)
    removeModal()
    
  })
  
  # Reactive expression to render the survey after selection
  output$survey_ui <- renderUI({
    req(selectedSurvey())
    survey_questions <- switch(selectedSurvey(),
                               "Digital Maturity" = surveys[["survey_dma"]],
                               "Data Maturity" = surveys[["survey_datam"]],
                               "NIS2" = surveys[["survey_nis"]])
    
    print("Selected Survey:")# Debug print
    print(survey_questions) # Debug print
    
    shinysurveys::surveyOutput(
      df = survey_questions,
      survey_title = paste(selectedSurvey(), "Survey"),
      survey_description = "Please fill out the survey below."
    )
  })
  
 
   # Validate survey responses
  validateSurveyResponses <- function(responses, survey_questions) {
    # Extract unique question IDs
    unique_questions <- unique(survey_questions$input_id)
    print("Unique questions:")
    print(unique_questions) # Debug print
    
    print("Responses received:")
    print(responses) # Debug print
    
    # Check if at least one answer is provided for each question ID
    missing_responses <- sapply(unique_questions, function(question) {
      # Filter responses for the current question ID
      relevant_responses <- responses[responses$question_id == question, "response"]
      # Check if all responses for the question are empty
      all(relevant_responses == "")
    })
    print("Missing responses array:")
    print(missing_responses) # Debug print
    
    return(all(!missing_responses))
  }
  
  # Reactive expression to check if all required questions are answered
  allQuestionsAnswered <- reactive({
    req(selectedSurvey())
    response_data <- shinysurveys::getSurveyData()
    print("Response data:")
    print(response_data) # Debug print
    
    survey_questions <- switch(selectedSurvey(),
                               "Digital Maturity" = surveys[["survey_dma"]],
                               "Data Maturity" = surveys[["survey_datam"]],
                               "NIS2" = surveys[["survey_nis"]])
    print("Survey questions:")
    print(survey_questions) # Debug print
    
    validateSurveyResponses(response_data, survey_questions)
  })
  
  
  # Handle survey submission
  observeEvent(input$submit, {
    response_data <- shinysurveys::getSurveyData()
    print("Response data at submission:")
    print(response_data) # Debug print
    
    survey_questions <- switch(selectedSurvey(),
                               "Digital Maturity" = surveys[["survey_dma"]],
                               "Data Maturity" = surveys[["survey_datam"]],
                               "NIS2" = surveys[["survey_nis"]])
    print("Survey questions at submission:")
    print(survey_questions) # Debug print
    
    if (validateSurveyResponses(response_data, survey_questions)) {
      showModal(modalDialog(
        title = paste("Congrats, you completed the", selectedSurvey(), "Survey!"),
        "You can customize what actions happen when a user finishes a survey using input$submit."
      ))
      print(response_data)
    } else {
      showModal(modalDialog(
        title = "Incomplete Survey",
        "Please complete all mandatory questions before submitting.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
}


#   shinysurveys::renderSurvey()
#   
#   
#   observeEvent(input$submit, {
#    
#      showModal(modalDialog(
#       title = "Congrats, you completed Data Maturity Survey!",
#       "You can customize what actions happen when a user finishes a survey using input$submit."
#     ))
#     
#     response_data <- getSurveyData()
#     print(response_data)
#   })
# }


###############
## Run the application 
###############

shinyApp(ui, server)

