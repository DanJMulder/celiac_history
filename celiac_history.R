## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for Quickly Summarizing a Celiac Patient's History                            ##
##   Written by Daniel Mulder, July 2022                                                     ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Preamble
# This script does not constitute medical advice and is only to be used for the purposes of learning or preparing personal templates
# This script contains no real medical information, any information contained within is fictional example information

# Load required packages ----
library(shiny) # for interactive web application framework
library(tidyverse) # for basic data organization
library(glue) # for gluing together text
library(lubridate) # for creating/parsing date objects

# Columns from the database spreadsheet
celiac_history_columns <- c("name",
                         "date_of_birth",
                         "sex",
                         "cr",
                         "diagnosis",
                         "presentation",
                         "diagnosis_date",
                         "endoscopic_findings",
                         "histologic_findings",
                         "gluten_free_diet_date",
                         "initial_ttg",
                         "ttg_normalized_date",
                         "most_recent_ttg",
                         "most_recent_ema",
                         "gi_symptoms",
                         "non_gi_symptoms",
                         "nurtitional_concerns",
                         "discussion_items_not_yet_covered")

celiac_diagnoses <- c("biopsy-proven celiac disease", "possible celiac disease", "probable celiac disease", "celiac disease")

#Source in my previous patient data
celiac_database <- read_csv("celiac_database.csv",
                                col_types = list(col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                                  col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                                  col_character(), col_character(), col_character(), col_character()))


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cyborg"),
  br(),
  tabsetPanel(type = "tabs",
              tabPanel("Input",
                       br(),
                       tags$b("Celiac History:"),
                       br(),
                       br(),
                       numericInput("cr_number", width = '50%', "Record Number:", value = 1234567, min = 0, max = 999999999),
                       # load demographic data if patient already known to me
                       actionButton("load_demographics", "Load demographics (for known patients)", icon("download")),
                       br(),
                       br(),
                       textInput("name", width = '50%', "Patient name (First Last):", value = ""),
                       textInput("date_of_birth", width = '50%', "Date of Birth (YYYY-MM-DD):", value = "2010-01-01"),
                       textOutput("current_age"),
                       br(),
                       selectInput("sex", width = '50%', "Sex:", choices = c("female", "male", "nonbinary person")),
                       selectInput("diagnosis", width = '50%', "Diagnosis - ", choices = celiac_diagnoses),
                       textAreaInput("presentation", width = '50%', "Presentation - ", value = ""),
                       textInput("diagnosis_date", width = '50%', "Date of Diagnosis (YYYY-MM-DD):", value = "2021-01-01"),
                       textInput("endoscopic_findings", width = '50%', "Endoscopic Findings - ", value = ""),
                       textInput("histologic_findings", width = '50%', "Histologic Findings - ", value = ""),
                       textInput("gluten_free_diet_date", width = '50%', "Date of Gluten Free Diet (YYYY-MM-DD):", value = "2021-01-01"),
                       textInput("initial_ttg", width = '50%', "Initial tTG - ", value = ""),
                       textInput("ttg_normalized_date", width = '50%', "Date of tTG Normalization (YYYY-MM-DD):", value = "2021-01-01"),
                       textInput("most_recent_ttg", width = '50%', "Most Recent tTG - ", value = "2021-01-01"),
                       textInput("most_recent_ema", width = '50%', "Most Recent EMA - ", value = "2021-01-01"),
                       textInput("gi_symptoms", width = '50%', "GI Symptoms - ", value = "2021-01-01"),
                       textInput("non_gi_symptoms", width = '50%', "Non-GI Symptoms - ", value = "2021-01-01"),
                       textInput("nurtitional_concerns", width = '50%', "Nurtitional Concerns - ", value = "2021-01-01"),
                       textInput("discussion_items_not_yet_covered", width = '50%', "Discussion Items not yet Covered - ", value = "2021-01-01"),
                       br(),
                       actionButton("save", "Update Database", icon("database")),
                       br(),
                       br()
              ),
              
              tabPanel("Text Output (for copy/paste)",
                       
                       # Tab 2: Output (text note) Preview ----
                       br(),
                       tagAppendAttributes(textOutput("full_note"), style = "white-space:pre-wrap;"),
                       br(),
                       actionButton("save", "Update Database", icon("database")),
                       br(),
                       br()
              )
  )
)


server <- function(input, output, session) {
  
  # When the load demographics button is pressed, autofill the previous info...
  # by first running the "loadDemographics function and then updating the info using the "updateText" etc functions
  
  loadDemographics <- reactive({
    if (input$cr_number %in% celiac_database$cr) {
      is_known_patient <<- TRUE
      print("Matching record found, loading prior information...")
      known_patient <<- filter(celiac_database, celiac_database$cr == input$cr_number)
      knonw_patient_cr <<- known_patient[[4]]
      known_patient_name <<- known_patient[[1]][1]
      known_patient_dob <<- known_patient[[2]][1]
      known_patient_sex <<- known_patient[[3]][1]
      known_patient_diagnosis <<- known_patient[[5]][1]
      known_patient_presentation <<- known_patient[[6]][1]
      known_patient_diagnosis_date <<- known_patient[[7]][1]
      known_patient_endoscopic_findings <<- known_patient[[8]][1]
      known_patient_histologic_findings <<- known_patient[[9]][1]
      known_patient_gluten_free_diet_date <<- known_patient[[10]][1]
      known_patient_initial_ttg <<- known_patient[[11]][1]
      known_patient_ttg_normalized_date <<- known_patient[[12]][1]
      known_patient_most_recent_ttg <<- known_patient[[13]][1]
      known_patient_most_recent_ema <<- known_patient[[14]][1]
      known_patient_gi_symptoms <<- known_patient[[15]][1]
      known_patient_non_gi_symptoms <<- known_patient[[16]][1]
      known_patient_nurtitional_concerns <<- known_patient[[17]][1]
      known_patient_discussion_items_not_yet_covered <<- known_patient[[18]][1]
      print(paste0(known_patient))
    } else {
      is_known_patient <<- FALSE
      print("No prior matching records found")
    }
  })
  
  observeEvent(input$load_demographics, {
    loadDemographics()
    updateTextInput(session, "name",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_name)
                      })
    updateTextInput(session, "date_of_birth",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_dob)
                      })
    updateTextInput(session, "sex",
                      value =
                        if (is_known_patient == TRUE) {
                          paste(known_patient_sex)
                        })
    updateTextInput(session, "diagnosis",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_diagnosis)
                      })
    updateTextInput(session, "presentation",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_presentation)
                      })
    updateTextInput(session, "diagnosis_date",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_diagnosis_date)
                      })
    updateTextInput(session, "endoscopic_findings",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_endoscopic_findings)
                      })
    updateTextInput(session, "histologic_findings",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_histologic_findings)
                      })
    updateTextInput(session, "gluten_free_diet_date",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_gluten_free_diet_date)
                      })
    updateTextInput(session, "initial_ttg",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_initial_ttg)
                      })
    updateTextInput(session, "ttg_normalized_date",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_ttg_normalized_date)
                      })
    updateTextInput(session, "most_recent_ttg",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_most_recent_ttg)
                      })
    updateTextInput(session, "most_recent_ema",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_most_recent_ema)
                      })
    updateTextInput(session, "gi_symptoms",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_gi_symptoms)
                      })
    updateTextInput(session, "non_gi_symptoms",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_non_gi_symptoms)
                      })
    updateTextInput(session, "nurtitional_concerns",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_nurtitional_concerns)
                      })
    updateTextInput(session, "discussion_items_not_yet_covered",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_discussion_items_not_yet_covered)
                      })
  })
  
  output$current_age <- renderText({
    
    age_raw <-
      if (is.na(input$date_of_birth)) {
        paste("Unable to calculate current age")
      } else {
        as.period(interval(start = input$date_of_birth, end = today()))
      }

    age_text <-
      if(age_raw$year < 2){
        if(age_raw$month <1){
          paste(age_raw$day, "day old")
        } else {
            paste(age_raw$month + (12*age_raw$year), "month old")}
      } else {
          paste(age_raw$year, "year", age_raw$month, "month old")}

    return(paste("Current age:", age_text))
  })
  
  # Updating Database ----
  
  # when the save button is pressed, the function below will load the celiac_database spreadsheet, update it, and save the new info to it
  # if there is no "encounter_data.csv" file in the working directory then the code below will create one
  # if there already is a "encounter_data.csv" file, then the code below will add a line to it
  
  observeEvent(input$save, {
    
    # below are a series of objects created from the textbox information input into the app
    
    responses_database <- read_csv("celiac_database.csv",
                                   col_types = list(col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                                    col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
                                                    col_character(), col_character(), col_character(), col_character()))
  
    this_patient <- t(as.data.frame(c(input$name,
                                      input$date_of_birth,
                                      input$sex,
                                      input$cr_number,
                                      input$diagnosis,
                                      input$presentation,
                                      input$endoscopic_findings,
                                      input$histologic_findings,
                                      input$gluten_free_diet_date,
                                      input$initial_ttg,
                                      input$ttg_normalized_date,
                                      input$most_recent_ttg,
                                      input$most_recent_ema,
                                      input$gi_symptoms,
                                      input$non_gi_symptoms,
                                      input$nurtitional_concerns,
                                      input$discussion_items_not_yet_covered)))
    colnames(this_patient) <- c("name",
                                "date_of_birth",
                                "sex",
                                "cr",
                                "diagnosis",
                                "presentation",
                                "diagnosis_date",
                                "endoscopic_findings",
                                "histologic_findings",
                                "gluten_free_diet_date",
                                "initial_ttg",
                                "ttg_normalized_date",
                                "most_recent_ttg",
                                "most_recent_ema",
                                "gi_symptoms",
                                "non_gi_symptoms",
                                "nurtitional_concerns",
                                "discussion_items_not_yet_covered")
    filtered_db <- responses_database %>%
      filter(input$cr_number != cr)
    updated_db <- rbind(filtered_db, this_patient)
    write_csv(updated_db, "celiac_database_2.csv")
    
  })
  
  # When the save button is clicked, the function below will save the text as a .docx file
  # (also saves as a txt file in case user does not use office)
  # In order to create the filename it requires the arguments: patient_name, cr and visit_type
  # the rest of the note is created from the formData() function above
  
  formData <- reactive({
    return(glue("Celiac Disease History:",
                "\n",
                paste("Diagnosis -", input$diagnosis),
                "\n",
                paste("Presentation -", input$presentation),
                "\n",
                paste("Diagnosis Date -", ymd(input$diagnosis_date)),
                "\n",
                paste("Endoscopic Findings -", input$endoscopic_findings),
                "\n",
                paste("Histologic Findings -", input$histologic_findings),
                "\n",
                paste("Gluten Free Diet Start Date -", input$gluten_free_diet_date),
                "\n",
                paste("Initial tTG-IgA -", input$initial_ttg),
                "\n",
                paste("Date tTG Normalized -", input$ttg_normalized_date),
                "\n",
                paste("Most Recent tTG -", input$most_recent_ttg),
                "\n",
                paste("Most Recent EMA -", input$most_recent_ema),
                "\n",
                paste("GI Symptoms -", input$gi_symptoms),
                "\n",
                paste("Non-GI Symptoms -", input$non_gi_symptoms),
                "\n",
                paste("Nutritional Concerns -", input$nurtitional_concerns),
                "\n",
                paste("Discussion Items Not Yet Covered -", input$discussion_items_not_yet_covered)
    )
    )
  })
  
  output$full_note <- renderPrint(formData())
  
}

shinyApp(ui, server)
