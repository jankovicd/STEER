
about_you_ques <- p(

  # add any questions of interest using the standard Shiny widgets.
  # see details at https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
    
  strong("1. Please select your role from below:"),
  radioButtons("about_you_1"," ",c(
    "Medical consultant" = 1,
    "Microbiologist" = 2,
    "Pharmacist" = 3,
    "Other (please specify below)" = 4), selected=integer(0)),
  textInput("about_you_other_1","",value=""),
  br(), br(),
  
  strong("2. Please select the setting where you work from below:"),
  checkboxGroupInput("about_you_2"," ",c(
    "Primary care" = 1,
    "Secondary care" = 2,
    "Social care" = 3,
    "Other (please specify below)" = 4), selected=integer(0)),
  textInput("about_you_other_2","",value=""), br(), br(),
  
)


