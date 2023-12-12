
function(request){

  fluidPage(

    mainPanel(

      tabsetPanel(id = "top_tabs",

                  tabPanel("Introduction",
                           tagList(div(
                             uiOutput("tab_home")
                           ))
                  ), #close tab

                  tabPanel("Instructions",
                           tagList(div(
                             uiOutput("tab_instructions")
                           ))
                  ), #close tab

                  tabPanel("Background information",
                           tagList(div(
                             uiOutput("tab_background_info")
                           ))
                  ), #close tab

                  tabPanel("Questions",
                           tagList(div(
                             uiOutput("tab_questions")
                           ))
                  )  #close tab

      ) #close tabsetPanel

    ) #close main panel

  )#close fluidPage

}
