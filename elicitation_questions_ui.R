
f_chips_and_bins <- function(que_no,
                             elici_minis,
                             elici_maxis,
                             chips_nchip,
                             chips_chips,
                             chips_lbins,
                             chips_rbins,
                             show_plot,
                             enter_plot,
                             comment)

(p(
  br(), br(),
  strong(eli_que_text[que_no]), br(),
  ifelse(!is.na(quant_limit_lower[que_no]) & !is.na(quant_limit_upper[que_no]),
         tagList(div(em("Note that the values should be between ", quant_limit_lower[que_no]," and ", quant_limit_upper[que_no],"."))),
         ifelse(is.na(quant_limit_lower[que_no]) & is.na(quant_limit_upper[que_no]),
                "",
                ifelse(is.na(quant_limit_lower[que_no]),
                       tagList(div(em("Note that both values should be less than ", quant_limit_upper[que_no],"."))),
                       tagList(div(em("Note that both values should be greater than ", quant_limit_lower[que_no],".")))))),
  br(),br(),
  p("I believe that it's very unlikely that:"), 
  tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("the ", quantity[que_no], " is less than"))),
          div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("min",que_no), NULL, elici_minis, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no])),
          div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
          div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(paste0(units[que_no],",")))),
  tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("the ", quantity[que_no], " is greater than"))),
          div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("max",que_no), NULL, elici_maxis, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no])),
          div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
          div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(paste0(units[que_no],".")))),
  br(),br(),
  ifelse(show_plot == 0,
         tagList(div(
           fluidRow(
             column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
             column(1, 
                    actionButton(paste0("show_plot_", que_no), "Continue", width = '120px', style = "background-color: lightgrey")), br()
           )
           )),
         tagList(div(
           fluidRow(
             column(9, p(style = "font-size:90%;", "You can update your range by entering new values and clicking 'Update range'.")),
             column(1, 
                    actionButton(paste0("show_plot_", que_no), "Update range", width = '120px', style="background-color: lightgrey"))
             ),
           hr(),br(),
           p("Please add", chips_nchip,"chips to the grid below to express your uncertainty. 
      The more chips you place in a particular bin the more certain you are 
      that the proportion lies in that bin."),br(), br(),
           "You can use",strong(chips_nchip-sum(chips_chips)), " more chips.",
           HTML("<div style='height: 350px;'>"),
           plotOutput(paste0("plot_",que_no), click=paste0("location_",que_no)),
           HTML("</div>"), br(),
           ifelse(enter_plot == 0,#****
                  tagList(div(
                    fluidRow(
                      column(9, "When you are happy with your answers please click on 'Enter', then scroll down."),
                      column(2, actionButton(paste0("enter_plot_", que_no), "Enter", width='120px', style="background-color: lightgrey")
                      )), br(), br(),
                    )),
                  tagList(div(
                    hr(), br(),
                    strong("Summary"),br(),br(),
                    "Your answers imply that",br(),br(),
                    div(f_text_fback_chips(chips_chips, chips_lbins, chips_rbins, quantity[que_no], units[que_no]),
                        style='width:700px; padding-left:45px;'), br(),br(),
                    "If these summary statements do not represent your beliefs you can modify the grid.",br(),
                    hr(),
                    p("If you have any additional comments about your answer, please state these here.",
                      withTags(div(
                          textarea(id = paste0("comment_", que_no),
                                   value = comment,
                                   class = "form-control shiny-bound-input",
                                   style = "width: 800px; height: 34px")
                          )), br(), br(),
                     fluidRow( 
                        column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Save', then scroll down to continue.")),
                        column(1, actionButton(paste0("next_que_", que_no), "Save", width='120px', style="background-color: lightgrey")
                        )), br()), br(),
                      "Note that you can edit your answer at any point, but remember to save your new inputs.", br(), br(), br()
                  ))
           )
           
           ))
         
         
         )
  
  
)

)


f_quartiles <- function(que_no,
                        elici_minis,
                        elici_maxis,
                        elici_q1,
                        elici_q2,
                        elici_q3,
                        enter_min_max,
                        enter_quarts,
                        comment)


  (p(
    br(), br(),
    strong(eli_que_text[que_no]), br(),
    ifelse(!is.na(quant_limit_lower[que_no]) & !is.na(quant_limit_upper[que_no]),
           tagList(div(em("Note that the values should be between ", quant_limit_lower[que_no]," and ", quant_limit_upper[que_no],"."))),
           ifelse(is.na(quant_limit_lower[que_no]) & is.na(quant_limit_upper[que_no]),
                  "",
                  ifelse(is.na(quant_limit_lower[que_no]),
                         tagList(div(em("Note that both values should be less than ", quant_limit_upper[que_no],"."))),
                         tagList(div(em("Note that both values should be greater than ", quant_limit_lower[que_no],".")))))),
    br(),br(),
    p("I believe that it's very unlikely that:"),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("the ", quantity[que_no], " is less than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("min",que_no), NULL, elici_minis, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no])),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(paste0(units[que_no],",")))),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("the ", quantity[que_no], " is greater than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("max",que_no), NULL, elici_maxis, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no])),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(paste0(units[que_no],".")))),
    br(),br(),
    ifelse(enter_min_max == 0,
           tagList(div(
             fluidRow(
               column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
               column(1, actionButton(paste0("enter_min_max_", que_no), "Continue", width = '120px', style = "background-color: lightgrey")), br()
             )
           )),
           tagList(div(
             hr(), br(),
             fluidRow(column(10,p("Can you determine a value ", strong("(your median or midpoint, M)"), " such that the proportion is equally likely to be less than or greater than this value?"),br(),
                             p("Suppose you were told that the proportion is below your assessed midpoint. Can you now provide a new value ", strong("(your lower quartile Q1)"), ", so that the proportion of patients is equally likely to be less than or greater than this value?"),br(),
                             p("Suppose you were told that the proportion is above your assessed midpoint. Can you now provide a new value ", strong("(your upper quartile Q3)"), ", so that the proportion of patients is equally likely to be less than or greater than this value?"),br()),
                      column(2,numericInput(paste0("quartile2_", que_no), NULL, elici_q2, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no]), br(), br(),
                               numericInput(paste0("quartile1_", que_no), NULL, elici_q1, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no]), br(), br(),
                               numericInput(paste0("quartile3_", que_no), NULL, elici_q3, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no]), br())
                      ), br(),
             HTML("<div style='height: 70px;width: 600px'>"),
             plotOutput(paste0("quart_fig_",que_no)),
             HTML("</div>"), br(),
             ifelse(enter_quarts==0,
                    tagList(div(
                      fluidRow(
                        column(9, p(style="font-size:90%;","When you are happy with your answers please click on 'Enter', then scroll down.")),
                        column(1, actionButton(paste0("enter_quarts_", que_no), "Enter", width='120px', style = "background-color: lightgrey"))))),
                    tagList(div(
                      fluidRow(
                        column(9, p(style="font-size:90%;","You can change any of the above values, but remember to click on 'Update values' to view updated summary statements.")),
                        column(1, actionButton(paste0("enter_quarts_", que_no), "Update values", width='120px', style = "background-color: lightgrey"))),
                      hr(), br(),
                      strong("Summary"),br(),br(),
                      "Your answers imply that", br(), br(),
                      tags$li("the proportion is", strong(paste0("equally likely to be less than and greater than ", elici_q2,","))),
                      tags$li("the proportion is", strong("equally likely to be between", elici_q1, "and", elici_q3,
                      "as it is to be outside this range.")), br(), br(),
                      "If these summary statements do not represent your beliefs you can modify your answers and click on 'Update values'.",br(),
                      hr(),
                      p("If you have any additional comments about your answer, please state these here.",
                        withTags(div(
                          textarea(id = paste0("comment_", que_no),
                                   value = comment,
                                   class = "form-control shiny-bound-input",
                                   style = "width: 800px; height: 34px")
                        )), br(), br(),
                        fluidRow(
                          column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Save', then scroll down to continue.")),
                          column(1, actionButton(paste0("next_que_", que_no), "Save", width='120px', style="background-color: lightgrey")
                                 )), br()), br(),
                      "Note that you can edit your answer at any point, but remember to save your new inputs.", br(), br(), br()
                      ))
                    )
             ))
           )
    )) #close function


f_tertiles <- function(que_no,
                       elici_minis,
                       elici_maxis,
                       elici_t1,
                       elici_t2,
                       enter_min_max,
                       enter_terts,
                       comment)
  
  
  (p(
    br(), br(),
    strong(eli_que_text[que_no]), br(),
    ifelse(!is.na(quant_limit_lower[que_no]) & !is.na(quant_limit_upper[que_no]),
           tagList(div(em("Note that the values should be between ", quant_limit_lower[que_no]," and ", quant_limit_upper[que_no],"."))),
           ifelse(is.na(quant_limit_lower[que_no]) & is.na(quant_limit_upper[que_no]),
                  "",
                  ifelse(is.na(quant_limit_lower[que_no]),
                         tagList(div(em("Note that both values should be less than ", quant_limit_upper[que_no],"."))),
                         tagList(div(em("Note that both values should be greater than ", quant_limit_lower[que_no],".")))
                         ) #close ifelse
                  )#close ifelse
           ),#close ifelse
    br(),br(),
    p("I believe that it's very unlikely that:"),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("the ", quantity[que_no], " is less than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("min",que_no), NULL, elici_minis, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no])),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(paste0(units[que_no],",")))),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("the ", quantity[que_no], " is greater than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("max",que_no), NULL, elici_maxis, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no])),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(paste0(units[que_no],".")))),
    br(),br(),
    ifelse(enter_min_max == 0,
           tagList(div(
             fluidRow(
               column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
               column(1, actionButton(paste0("enter_min_max_", que_no), "Continue", width = '120px', style = "background-color: lightgrey")), br()
             )
           )),
           tagList(div(
             hr(), br(),
             "Can you provide two values within your plausible range that divide the range of values into three equally likely intervals?", br(),br(),
             fluidRow(column(4,p("Value 1 ", strong("(lower tertile)"),":")),
                      column(3, numericInput(paste0("tertile1_", que_no), NULL, elici_t1, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no]))), br(),
             fluidRow(column(4,p("Value 2 ", strong("(upper tertile)"),":")),
                      column(3, numericInput(paste0("tertile2_", que_no), NULL, elici_t2, min = quant_limit_lower[que_no], max = quant_limit_upper[que_no]))), br(),
             HTML("<div style='height: 70px;width: 600px'>"),
             plotOutput(paste0("terts_fig_",que_no)),
             HTML("</div>"), br(),
             ifelse(enter_terts==0,
                    tagList(div(
                      fluidRow(
                        column(9, p(style="font-size:90%;","When you are happy with your answers please click on 'Enter', then scroll down.")),
                        column(1, actionButton(paste0("enter_terts_", que_no), "Enter", width='120px', style = "background-color: lightgrey")))
                      )),
                    tagList(div(
                      fluidRow(
                        column(9, p(style="font-size:90%;","You can change any of the above values, but remember to click on 'Update values' to view updated summary statements.")),
                        column(1, actionButton(paste0("enter_terts_", que_no), "Update values", width='120px', style = "background-color: lightgrey"))),
                      hr(), br(),
                      strong("Summary"),br(),br(),
                      "Your answers imply that the proportion is",
                      strong("equally likely to be less than", elici_t1),
                      "as it is to be", strong("greater than", elici_t2),
                      "or", strong("between", elici_t1,"and", elici_t2), ".", br(), br(),
                      "If these summary statements do not represent your beliefs you can modify your answers and click on 'Update values'.",br(),
                      hr(),
                      p("If you have any additional comments about your answer, please state these here.",
                        withTags(div(
                          textarea(id = paste0("comment_", que_no),
                                   value = comment,
                                   class = "form-control shiny-bound-input",
                                   style = "width: 800px; height: 34px")
                        )), br(), br(),
                        fluidRow(
                          column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Save', then scroll down to continue.")),
                          column(1, actionButton(paste0("next_que_", que_no), "Save", width='120px', style="background-color: lightgrey")
                          )), br()), br(),
                      "Note that you can edit your answer at any point, but remember to save your new inputs.", br(), br(), br()
                    ))
             )
           ))
    )
  )) #close function





