library(rdrop2)

source("manual_inputs.R", local = TRUE)
source("functions.R", local = TRUE)
source("elicitation_questions_ui.R", local = TRUE)
source("about_you.R", local = TRUE)

function (input, output, session) {

  ##### create reactive values #####

  # reactive values where experts' responses are stored
  # ($test used in practice question in "instructions" tab)

  elici_minis <- reactiveValues(test = 9) # lower limit of expert's plausible range
  elici_maxis <- reactiveValues(test = 17) # upper limit of expert's plausible range

  if(elicitation_method == "chips and bins"){

    chips_width <- reactiveValues(test = 1) # bin width
    chips_lower <- reactiveValues(test = 8) # lower limit of the plot
    chips_upper <- reactiveValues(test = 18) # upper limit of the plot
    chips_nbins <- reactiveValues(test = 10) # total number of bins
    chips_nchip <- reactiveValues(test = 20) # total number of chips
    chips_nhigh <- reactiveValues(test = 20) # height of plot
    chips_lbins <- reactiveValues(test = 8:17) # lower limit of each bin
    chips_rbins <- reactiveValues(test = 9:18) # upper limit of each bin
    chips_value <- reactiveValues(test = 9:18) # bins (equal to rbins)
    chips_chips <- reactiveValues(test = rep(0,10)) # number of chips in each bin

    show_plot <- reactiveValues(test=0) # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
    enter_plot <- reactiveValues(test=0) #indicator (>0) that experts have used all chips

  } else if (elicitation_method == "quartiles") {

    elici_q1 <- reactiveValues(test = 11) # expert's lower quartile
    elici_q2 <- reactiveValues(test = 12) # expert's median
    elici_q3 <- reactiveValues(test = 14) #expert's upper quartile

    enter_min_max <- reactiveValues(test=0) # indicator (>0) that experts have entered (or updated) their plausible range for quertiles/tertiles
    enter_quarts <- reactiveValues(test=0) # indicator (>0) that experts have entered (or updated) their quartiles

  } else if (elicitation_method == "tertiles") {

    elici_t1 <- reactiveValues(test = 12) # expert's lower tertile
    elici_t2 <- reactiveValues(test = 14) # expert's upper tertile

    enter_min_max <- reactiveValues(test=0) # indicator (>0) that experts have entered (or updated) their plausible range for quertiles/tertiles
    enter_terts <- reactiveValues(test=0) # indicator (>0) that experts have entered (or updated) their tertiles

  }

  comments <- reactiveValues(test="") # expert's comments about elicitation questions

  buttons <- reactiveValues(expert_id = 0, # expert's unique code provided by the investigator that distinguished their saved answers from others'
                            enter_unique_id = 0, # indicator (>0) that the expert has entered their unique identifier
                            next_home = 0, # current page on the home tab
                            end_home = 0, # indicator (>0) that the expert is on the last page of the home tab
                            next_que_0 = 0, # indicator (>0) that training is complete, switch to next tab
                            start_que = 0, # indicator (>0) that the expert has read background information
                            que_no = 1, # the next question the expert needs to answer
                            enter_previous_responses = 0,
                            radio_previous_responses = 0,
                            get_previous_responses = 0)

  save <- reactiveValues(about_you_all = 0, # object for saving "about you" questions
                         about_you_colnames = "expert_id", # column names for "about_you" questions
                         all_answers = 0,
                         all_answers_colnames = 0)

  if (dummy_app) {

    buttons$expert_id <- 1111
    buttons$enter_unique_id <- 1
    save$about_you_all <- 1111

  }

  for (i in eli_que_names){

    # create reactive values where experts' responses are stored

    elici_minis[[i]] <- integer(0)
    elici_maxis[[i]] <- integer(0)

    comments[[i]] <- ""

    if(elicitation_method == "chips and bins"){

      chips_width[[i]] <- 0
      chips_lower[[i]] <- 0
      chips_upper[[i]] <- 0
      chips_nbins[[i]] <- 0
      chips_nchip[[i]] <- 0
      chips_nhigh[[i]] <- 0
      chips_lbins[[i]] <- 0
      chips_rbins[[i]] <- 0
      chips_value[[i]] <- 0
      chips_chips[[i]] <- 0

      show_plot[[i]]  <- 0
      enter_plot[[i]] <- 0

    } else if (elicitation_method == "quartiles"){

      elici_q1[[i]] <- integer(0)
      elici_q2[[i]] <- integer(0)
      elici_q3[[i]] <- integer(0)

      enter_min_max[[i]] <- 0
      enter_quarts[[i]] <-0

      buttons[[paste0("conditions_",i)]] <- 0

    } else if (elicitation_method == "tertiles"){

      elici_t1[[i]] <- integer(0)
      elici_t2[[i]] <- integer(0)

      enter_min_max[[i]] <- 0
      enter_terts[[i]] <-0

      buttons[[paste0("conditions_",i)]] <- 0

    }

    save[[i]] <- 0 # saves elicitation answers
    save[[paste0(i,"_colnames")]] <- 0 # column names for elicitation answers

  }


  ##### button clicks #####

  observeEvent(input$enter_previous_responses,{

    buttons$enter_previous_responses <- 1
    buttons$radio_previous_responses <- input$radio_previous_responses

    })

  observeEvent(input$get_previous_responses, {

    file1 <- input[["load_past_answers"]]
    ext <- tools::file_ext(file1$datapath)
    req(file1)
    validate(need(ext == "csv", "Please upload a csv file"))

    previous_responses <- read.csv(file1$datapath, header = TRUE)[-1]

    buttons$expert_id <- previous_responses["expert_id"]
    buttons$enter_unique_id <- 1

    save[["all_answers"]] <- previous_responses
    save[["all_answers_colnames"]] <- colnames(previous_responses)

    if(include_about_you){

      about_you <- previous_responses[grep("about_you",colnames(previous_responses))]

      if(length(about_you)>0){

        save[["about_you_all"]] <- c(buttons$expert_id, as.numeric(previous_responses[grep("about_you_",colnames(previous_responses))]))
        save[["about_you_colnames"]] <- c("expert_id", colnames(previous_responses)[grep("about_you_",colnames(previous_responses))])
      }

    }

    # check answers relating to elicitation
    temp1 <- colnames(previous_responses)[grep("_min",colnames(previous_responses))]

    if(length(temp1)>0){

      # get the list of elicitation questions the experts had answered before
      answered_questions <- as.numeric(gsub(".*?([0-9]+).*", "\\1", temp1))

      for (i in answered_questions) {
        # reload experts previous answers into reactive values

        que_name <- eli_que_names[i]

        elici_minis[[que_name]] <- as.numeric(previous_responses[paste0(que_name,"_min")])
        elici_maxis[[que_name]] <- as.numeric(previous_responses[paste0(que_name,"_max")])

        if(paste0(que_name,"_comment")%in%colnames(previous_responses)){

          comments[[que_name]] <- as.character(previous_responses[paste0(que_name,"_comment")])

        }

        if(elicitation_method == "chips and bins"){

          chips_width[[que_name]] <- f_width(elici_minis[[que_name]], elici_maxis[[que_name]], bins)
          chips_lower[[que_name]] <- f_lower(elici_minis[[que_name]], chips_width[[que_name]], quant_limit_lower[i])
          chips_upper[[que_name]] <- f_upper(elici_maxis[[que_name]], chips_width[[que_name]], quant_limit_upper[i])
          chips_nbins[[que_name]] <- f_nbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
          chips_nchip[[que_name]] <- 2 * chips_nbins[[que_name]]
          chips_nhigh[[que_name]] <- 2 * chips_nbins[[que_name]]
          chips_lbins[[que_name]] <- f_lbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
          chips_rbins[[que_name]] <- f_rbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
          chips_value[[que_name]] <- chips_rbins[[que_name]]

          chips_chips[[que_name]] <- unlist(previous_responses[grep(paste0(que_name,"_chip"),colnames(previous_responses))])

          show_plot[[que_name]]  <- 1
          enter_plot[[que_name]] <- 1

        } else if (elicitation_method == "quartiles"){

          elici_q1[[que_name]] <- unlist(previous_responses[paste0(que_name,"_lower_quartile")])
          elici_q2[[que_name]] <- unlist(previous_responses[paste0(que_name,"_median")])
          elici_q3[[que_name]] <- unlist(previous_responses[paste0(que_name,"_upper_quartile")])

          enter_min_max[[que_name]] <- 1
          enter_quarts[[que_name]] <- 1

          buttons[[paste0("conditions_",i)]] <- 1

        } else {

          elici_t1[[que_name]] <- unlist(previous_responses[paste0(que_name,"_lower_tertile")])
          elici_t2[[que_name]] <- unlist(previous_responses[paste0(que_name,"_upper_tertile")])

          enter_min_max[[que_name]] <- 1
          enter_terts[[que_name]] <- 1

          buttons[[paste0("conditions_",i)]] <- 1

        }

      }

      buttons$next_home <- last_home_page
      buttons$end_home <- 1
      buttons$next_que <- 1
      buttons$start_que <- 1
      buttons$que_no <- answered_questions[length(answered_questions)] + 1

      updateTabsetPanel(session, "top_tabs", selected = "Questions")
      updateTabsetPanel(session, "question_tabs", selected = paste0("Question ", buttons$que_no))


    } else {

      temp2 <- colnames(previous_responses)[grep("about_you",colnames(previous_responses))]

      if (length(temp2)>0){
        # if the expert has answered "about_you" questions, skip to "Instructions" tab

        buttons$next_home <- last_home_page
        buttons$end_home <- 1

        updateTabsetPanel(session, "top_tabs",
                          selected = "Instructions"
        )

      }

    }

  })

  observeEvent(input$enter_unique_id,{

    if ("expert_id"%in%names(input)) {
      # if an expert has entered their ID, checks whether they had saved any
      # answers before, and if yes, uploads them

      if(input$expert_id%in%all_expert_ids) {
        #check that the unique id is one included in all_expert_ids

        buttons$expert_id <- input$expert_id
        save$about_you_all <- buttons$expert_id
        buttons$enter_unique_id <- 1

        if(save_method == "dropbox"){

        # load previous answers
        previous_responses <- f_load_answers(buttons$expert_id)

        if(!is.na(previous_responses[1])){

          # if there were any saved answers, check those relating to elicitation
          temp1 <- colnames(previous_responses)[grep("_min",colnames(previous_responses))]

          if(length(temp1)>0){

            # get the list of elicitation questions the experts had answered before
            answered_questions <- as.numeric(gsub(".*?([0-9]+).*", "\\1", temp1))

            for (i in answered_questions) {
              # reload experts previous answers into reactive values

              que_name <- eli_que_names[i]

              elici_minis[[que_name]] <- as.numeric(previous_responses[paste0(que_name,"_min")])
              elici_maxis[[que_name]] <- as.numeric(previous_responses[paste0(que_name,"_max")])

              if(paste0(que_name,"_comment")%in%colnames(previous_responses)){

                comments[[que_name]] <- as.character(previous_responses[paste0(que_name,"_comment")])

              }

              if(elicitation_method == "chips and bins"){

                chips_width[[que_name]] <- f_width(elici_minis[[que_name]], elici_maxis[[que_name]], bins)
                chips_lower[[que_name]] <- f_lower(elici_minis[[que_name]], chips_width[[que_name]], quant_limit_lower[i])
                chips_upper[[que_name]] <- f_upper(elici_maxis[[que_name]], chips_width[[que_name]], quant_limit_upper[i])
                chips_nbins[[que_name]] <- f_nbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
                chips_nchip[[que_name]] <- 2 * chips_nbins[[que_name]]
                chips_nhigh[[que_name]] <- 2 * chips_nbins[[que_name]]
                chips_lbins[[que_name]] <- f_lbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
                chips_rbins[[que_name]] <- f_rbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
                chips_value[[que_name]] <- chips_rbins[[que_name]]

                chips_chips[[que_name]] <- unlist(previous_responses[grep(paste0(que_name,"_chip"),colnames(previous_responses))])

                show_plot[[que_name]]  <- 1
                enter_plot[[que_name]] <- 1

              } else if (elicitation_method == "quartiles"){

                elici_q1[[que_name]] <- unlist(previous_responses[paste0(que_name,"_lower_quartile")])
                elici_q2[[que_name]] <- unlist(previous_responses[paste0(que_name,"_median")])
                elici_q3[[que_name]] <- unlist(previous_responses[paste0(que_name,"_upper_quartile")])

                enter_min_max[[que_name]] <- 1
                enter_quarts[[que_name]] <- 1

              } else {

                elici_t1[[que_name]] <- unlist(previous_responses[paste0(que_name,"_lower_tertile")])
                elici_t2[[que_name]] <- unlist(previous_responses[paste0(que_name,"_upper_tertile")])

                enter_min_max[[que_name]] <- 1
                enter_terts[[que_name]] <- 1

              }

            }

            buttons$next_home <- last_home_page
            buttons$end_home <- 1
            buttons$next_que <- 1
            buttons$start_que <- 1
            buttons$que_no <- answered_questions[length(answered_questions)] + 1

            updateTabsetPanel(session, "top_tabs", selected = "Questions")
            updateTabsetPanel(session, "question_tabs", selected = paste0("Question ", buttons$que_no))


          } else {

            temp2 <- colnames(previous_responses)[grep("about_you",colnames(previous_responses))]

            if (length(temp2)>0){
              # if the expert has answered "about_you" questions, skip to "Instructions" tab

              buttons$next_home <- last_home_page
              buttons$end_home <- 1

              updateTabsetPanel(session, "top_tabs",
                                selected = "Instructions"
              )

          }

          }

        }

        }

      } else {

        showModal(modalDialog (uiOutput ("no_unique_id"), size="l"))

      }

    } else {

      showModal(modalDialog (uiOutput ("no_unique_id"), size="l"))

    }


  })


  if(save_method == "local"){

    output[["download_about_you"]] <-

        downloadHandler(

          filename = paste0(buttons$expert_id,"_download_all.csv"),
          content = function(con) {

            temp <- buttons$next_home; buttons$next_home + 1
            buttons$end_home <- 1
            updateTabsetPanel(session, "top_tabs", selected = "Instructions")

            for (i in 1:n_about_you){
              # loop for saving all questions about experts

              about_you_que <- paste0("about_you_",i)

              if (about_you_que%in%names(input)){

                if(!is.null(input[[about_you_que]])){

                  save_about_you_que <- input[[about_you_que]]
                  save_about_you_colnames <- rep(paste0("about_you_", i), length(save_about_you_que))

                } else {

                  save_about_you_que <- NA
                  save_about_you_colnames <- paste0("about_you_", i)

                }

              } else {

                save_about_you_que <- NA
                save_about_you_colnames <- paste0("about_you_",i)

              }

              save[["about_you_all"]] <- c(save[["about_you_all"]], save_about_you_que)
              save[["about_you_colnames"]] <- c(save[["about_you_colnames"]], save_about_you_colnames)

            } #close for loop for "about you" questions

            save[["all_answers"]] <- save[["about_you_all"]]
            save[["all_answers_colnames"]] <- save[["about_you_colnames"]]

            data <- t(save[["all_answers"]])
            colnames(data) <- save[["all_answers_colnames"]]

            # data <- t(save[["about_you_all"]])
            # colnames(data) <- save[["about_you_colnames"]]
            write.csv(data, con)

          }

        )

    }


    observeEvent(input$next_home,{

    # allows the user to move through different pages of the Home screen
    # (home page, consent form and questions about experts, if relevant)

      if (!include_about_you){

      # if not asking questions about experts, move onto next page on the Home tab

      temp <- buttons$next_home; buttons$next_home <- temp + 1


      if(buttons$next_home == last_home_page){

        # if expert is on the last page of the Home tab, move to "Instructions tab"

        buttons$end_home <- 1
        updateTabsetPanel(session, "top_tabs",
                          selected = "Instructions"
        )

      }

    } else if (buttons$next_home < (last_home_page-1)){

      temp <- buttons$next_home; buttons$next_home <- temp + 1

    } else {

      if(save_method == "dropbox"){

      for (i in 1:n_about_you){

        # loop for saving all questions about experts

        about_you_que <- paste0("about_you_",i)

        if (about_you_que%in%names(input)){

          if(!is.null(input[[about_you_que]])){

            save_about_you_que <- input[[about_you_que]]
            save_about_you_colnames <- rep(paste0("about_you_", i), length(save_about_you_que))

          } else {

            save_about_you_que <- NA
            save_about_you_colnames <- paste0("about_you_", i)

          }

        } else {

          save_about_you_que <- NA
          save_about_you_colnames <- paste0("about_you_",i)

        }

        save[["about_you_all"]] <- c(save[["about_you_all"]],save_about_you_que)
        save[["about_you_colnames"]] <- c(save[["about_you_colnames"]],save_about_you_colnames)

      } #close for loop for "about you" questions

      f_save_answers(save[["about_you_all"]], save[["about_you_colnames"]], paste0(buttons$expert_id,"_about_you.csv"))

    }

      temp <- buttons$next_home; buttons$next_home + 1
      buttons$end_home <- 1
      updateTabsetPanel(session, "top_tabs",
                            selected = "Instructions"
          )


      } #close actions relating to "about you" questions


  })


  # when using Chips and Bins, and an expert enters or updates their plausible range
  lapply(X = 1:tot_eli_ques, FUN = function(i){

      observeEvent(input[[paste0("show_plot_",i)]],{

        que_name <- eli_que_names[i]

        # save inputs as reactive values
        elici_minis[[que_name]] <- input[[paste0("min",i)]]
        elici_maxis[[que_name]] <- input[[paste0("max",i)]]

        #check whether plausible range is within parameter limits and elici_minis < elici_maxis
        condition <- f_cond_min_max(elici_minis[[que_name]], elici_maxis[[que_name]], quant_limit_lower[i], quant_limit_upper[i])

        if(condition==1){

          show_plot[[que_name]] <- 1 # show plot
          enter_plot[[que_name]] <- 0 # do not show feedback on expert's plot

          # update plot parameters
          chips_width[[que_name]] <- f_width(elici_minis[[que_name]], elici_maxis[[que_name]], bins)
          chips_lower[[que_name]] <- f_lower(elici_minis[[que_name]], chips_width[[que_name]], quant_limit_lower[i])
          chips_upper[[que_name]] <- f_upper(elici_maxis[[que_name]], chips_width[[que_name]], quant_limit_upper[i])
          chips_nbins[[que_name]] <- f_nbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
          chips_nchip[[que_name]] <- 2 * chips_nbins[[que_name]]
          chips_nhigh[[que_name]] <- 2 * chips_nbins[[que_name]]
          chips_lbins[[que_name]] <- f_lbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
          chips_rbins[[que_name]] <- f_rbins(chips_lower[[que_name]], chips_upper[[que_name]], chips_width[[que_name]])
          chips_value[[que_name]] <- chips_rbins[[que_name]]
          chips_chips[[que_name]] <- rep(0,chips_nbins[[que_name]])

        } else {

          # if condition == 0, who error message
          showModal(modalDialog (uiOutput (paste0("no_plot_",i)), size="l"))

        }

      })

    }
  )

  # when using Chips and Bins, and an expert enters or updates their plot
  lapply(X = 1:tot_eli_ques, FUN = function(i){

    observeEvent(input[[paste0("enter_plot_",i)]],{

      que_name <- eli_que_names[i]

        if(round(chips_nchip[[que_name]]-sum(chips_chips[[que_name]]),digits=0) > 0 ){

          # error mesage if not all chips are used
          showModal(modalDialog (uiOutput ("no_chips"), size="l"))

        } else {

          enter_plot[[que_name]] <- 1

        }

      })

    })

  #same as above, for the test plot
  observeEvent(input[["enter_plot_0"]],{

    if(sum(chips_chips[["test"]]) < chips_nchip[["test"]]){

      showModal(modalDialog (uiOutput ("no_chips"), size="l"))

    } else {

      enter_plot[["test"]] <- 1

    }

  })


  # when using quartiles or tertiles and an expert enters their plausible range
  lapply(X = 1:tot_eli_ques, FUN = function(i){

    observeEvent(input[[paste0("enter_min_max_",i)]],{

      que_name <- eli_que_names[i]

      # save inputs as reactive values
      elici_minis[[que_name]] <- input[[paste0("min",i)]]
      elici_maxis[[que_name]] <- input[[paste0("max",i)]]

      #check whether the plausible range is within parameter limits and elici_minis < elici_maxis
      condition <- f_cond_min_max(elici_minis[[que_name]], elici_maxis[[que_name]], quant_limit_lower[i], quant_limit_upper[i])

      if(condition==1){

        enter_min_max[[que_name]] <- 1

      } else {

        #show error message if condition = 0
        showModal(modalDialog (uiOutput (paste0("no_plot_",i)), size="l"))

      }

    })

  })

  # when using quartiles and an expert enters their quartiles
  lapply(X = 1:tot_eli_ques, FUN = function(i){

    observeEvent(input[[paste0("enter_quarts_",i)]],{

      que_name <- eli_que_names[i]

      # save most recent minimum and maximum again, in case they were updated
      elici_minis[[que_name]] <- input[[paste0("min",i)]]
      elici_maxis[[que_name]] <- input[[paste0("max",i)]]

      # save inputs as reactive values
      elici_q1[[que_name]] <- input[[paste0("quartile1_",i)]]
      elici_q2[[que_name]] <- input[[paste0("quartile2_",i)]]
      elici_q3[[que_name]] <- input[[paste0("quartile3_",i)]]

      #check whether the plausible range is within parameter limits and elici_minis < elici_maxis
      condition1 <- f_cond_min_max(elici_minis[[que_name]], elici_maxis[[que_name]], quant_limit_lower[i], quant_limit_upper[i])

      # check whether quartiles are within plausible range and quartiles are in logical order
      condition2 <- f_cond_quartiles(elici_minis[[que_name]], elici_maxis[[que_name]], elici_q1[[que_name]], elici_q2[[que_name]], elici_q3[[que_name]])

      if(condition1 == 1 & condition2 == 1){

        enter_quarts[[que_name]] <- 1

        if(save_method == "local") { buttons[[paste0("conditions_",i)]] <- 1 }

        } else {

        showModal(modalDialog (uiOutput (paste0("no_quantiles_",i)), size="l"))

      }

    })

  })

  #same as above, for the test question
  observeEvent(input[["enter_quarts_0"]],{

    # save inputs as reactive values
    elici_q1[["test"]] <- input[["quartile1_0"]]
    elici_q2[["test"]] <- input[["quartile2_0"]]
    elici_q3[["test"]] <- input[["quartile3_0"]]

    #check whether the plausible range is within parameter limits and elici_minis < elici_maxis
    condition1 <- f_cond_min_max(elici_minis[["test"]], elici_maxis[["test"]], 0, NA)

    # check whether quartiles are within plausible range and quartiles are in logical order
    condition2 <- f_cond_quartiles(elici_minis[["test"]], elici_maxis[["test"]], elici_q1[["test"]], elici_q2[["test"]], elici_q3[["test"]])

    if(condition1 == 1 & condition2 == 1){

      enter_quarts[["test"]] <- 1

    } else {

      showModal(modalDialog (uiOutput ("no_quantiles_0"), size="l"))

    }

  })


  # when using tertiles and an expert enters their tertiles
  lapply(X = 1:tot_eli_ques, FUN = function(i){

    observeEvent(input[[paste0("enter_terts_",i)]],{

      que_name <- eli_que_names[i]

      # save inputs as reactive values
      elici_minis[[que_name]] <- input[[paste0("min",i)]]
      elici_maxis[[que_name]] <- input[[paste0("max",i)]]

      # save inputs as reactive values
      elici_t1[[que_name]] <- input[[paste0("tertile1_",i)]]
      elici_t2[[que_name]] <- input[[paste0("tertile2_",i)]]

      #check whether the plausible range is within parameter limits and elici_minis < elici_maxis
      condition1 <- f_cond_min_max(elici_minis[[que_name]], elici_maxis[[que_name]], quant_limit_lower[i], quant_limit_upper[i])

      #check whether quartiles are within plausible range and tertiles are in logical order
      condition2 <- f_cond_tertiles(elici_minis[[que_name]], elici_maxis[[que_name]], elici_t1[[que_name]], elici_t2[[que_name]])

      if(condition1 == 1 & condition2 == 1){

        enter_terts[[que_name]] <- 1

        if(save_method == "local") { buttons[[paste0("conditions_",i)]] <- 1 }

      } else {

        showModal(modalDialog (uiOutput (paste0("no_quantiles_",i)), size="l"))

      }

    })

  }
  )

  #same as above, for the test question
  observeEvent(input[["enter_terts_0"]],{

    # save inputs as reactive values
    elici_t1[["test"]] <- input[["tertile1_0"]]
    elici_t2[["test"]] <- input[["tertile2_0"]]

    #check whether the plausible range is within parameter limits and elici_minis < elici_maxis
    condition1 <- f_cond_min_max(elici_minis[["test"]], elici_maxis[["test"]], 0, NA)

    # check whether quartiles are within plausible range and quartiles are in logical order
    condition2 <- f_cond_tertiles(elici_minis[["test"]], elici_maxis[["test"]], elici_t1[["test"]], elici_t2[["test"]])

    if(condition1 == 1 & condition2 == 1){

      enter_terts[["test"]] <- 1

    } else {

      showModal(modalDialog (uiOutput ("no_quantiles_0"), size="l"))

    }

  })

  # if(save_method == "local"){
  #
  #   if(elicitation_method == "chips and bins"){

  if(save_method == "local" & elicitation_method == "chips and bins"){

      conditions <- lapply(X = 1:tot_eli_ques, FUN = function(i){

        reactive({

          ifelse(sum(chips_chips[[eli_que_names[i]]]) < chips_nchip[[eli_que_names[i]]],
                 0,
                 1)
        })

      })

    # } else if (elicitation_method == "quartiles") {
    #
    #   conditions <- lapply(X = 1:tot_eli_ques, FUN = function(i){
    #
    #     reactive({
    #
    #       # ifelse(f_cond_min_max(elici_minis[[eli_que_names[i]]], elici_maxis[[eli_que_names[i]]], quant_limit_lower[i], quant_limit_upper[i]) +
    #       #          f_cond_quartiles(elici_minis[[eli_que_names[i]]], elici_maxis[[eli_que_names[i]]], elici_q1[[eli_que_names[i]]], elici_q2[[eli_que_names[i]]], elici_q3[[eli_que_names[i]]]) < 2,
    #       #        0,
    #       #        1)
    #
    #       ifelse(sum(c(paste0("min",i),
    #                    paste0("max",i),
    #                    paste0("quartile1_",i),
    #                    paste0("quartile2_",i),
    #                    paste0("quartile3_",i)) %in% names(input))==5,
    #              ifelse(elici_minis[[eli_que_names[i]]] == input[[paste0("min",i)]] &
    #                       elici_maxis[[eli_que_names[i]]] == input[[paste0("max",i)]] &
    #                       elici_q1[[eli_que_names[i]]] == input[[paste0("quartile1_",i)]] &
    #                       elici_q2[[eli_que_names[i]]] == input[[paste0("quartile2_",i)]] &
    #                       elici_q3[[eli_que_names[i]]] == input[[paste0("quartile3_",i)]],
    #                     1,0),
    #              0)
    #     })
    #
    #   })
#
#     } else if (elicitation_method == "tertiles"){
#
#       conditions <- lapply(X = 1:tot_eli_ques, FUN = function(i){
#
#         eventReactive(input[[paste0("enter_terts_",i)]],{
#
# #           ifelse(f_cond_min_max(elici_minis[[eli_que_names[i]]], elici_maxis[[eli_que_names[i]]], quant_limit_lower[i], quant_limit_upper[i]) +
# #                    f_cond_tertiles(elici_minis[[eli_que_names[i]]], elici_maxis[[eli_que_names[i]]], elici_t1[[eli_que_names[i]]], elici_t2[[eli_que_names[i]]]) < 2,
# #                  0,
# #                  1)
#
#           ifelse(sum(c(paste0("min",i),
#                        paste0("max",i),
#                        paste0("tertile1_",i),
#                        paste0("tertile2_",i)) %in% names(input)) == 4,
#                  ifelse(elici_minis[[eli_que_names[i]]] == input[[paste0("min",i)]] &
#                           elici_maxis[[eli_que_names[i]]] == input[[paste0("max",i)]] &
#                           elici_t1[[eli_que_names[i]]] == input[[paste0("tertile1_",i)]] &
#                           elici_t2[[eli_que_names[i]]] == input[[paste0("tertile2_",i)]],
#                         1,0),
#                  0)
#
#         })
#
#       })
#
#     }

  } else {

    conditions <- lapply(X = 1:tot_eli_ques, FUN = function(i){

      reactive({0})

    })

  }

  names(conditions) <- paste0('que_', 1:tot_eli_ques)

  # when expert is ready to move onto next question - save answers and change tab
  lapply(X = 1:tot_eli_ques, FUN = function(i){

        observeEvent(input[[paste0("next_que_",i)]], {

          que_name <- eli_que_names[i]

          if(elicitation_method == "chips and bins"){

            if(sum(chips_chips[[que_name]]) < chips_nchip[[que_name]]){

              # show error message if not all chips are used
              showModal(modalDialog (uiOutput ("no_chips"), size="l"))

              } else {

                temp1 <- buttons$que_no; buttons$que_no <- temp1 + 1

                # save answers

                temp2<-c(buttons$expert_id, elici_minis[[que_name]], elici_maxis[[que_name]], chips_value[[que_name]], chips_chips[[que_name]])
                save[[paste0("next_que_", i)]] <- temp2
                save[[paste0("next_que_", i, "_colnames")]] <-   c("expert_id", paste0("que_",i,"_min"), paste0("que_",i,"_max"),
                                                                  paste0("que_",i,"_bins_",1:((length(temp2)-2)/2)),
                                                                  paste0("que_",i,"_chip_",1:((length(temp2)-2)/2)))

                f_save_answers(save[[paste0("next_que_", i)]],
                               save[[paste0("next_que_", i, "_colnames")]],
                               paste0(buttons$expert_id,"_que_",i,".csv"))

                if(paste0("comment_", i) %in% names(input)){

                  if(input[[paste0("comment_", i)]]!=""){

                    comments[[que_name]] <- input[[paste0("comment_", i)]]

                    f_save_answers(c(buttons$expert_id, comments[[que_name]]),
                                   c("expert_id", paste0("que_", i, "_comment")),
                                   paste0(buttons$expert_id, "_comment_que_", i, ".csv"))

                  }


                }


                if(tot_eli_ques > 1 & i < tot_eli_ques){

                  # if there are more questions to come, move onto the next tab
                  updateTabsetPanel(session, "question_tabs",
                                    selected = paste0("Question ",i+1))

                } else {

                  # if this is the last question, show a pop up message
                  showModal(modalDialog (uiOutput ("end_of_exercise"), size="l"))

                }

              }

          } else if (elicitation_method == "quartiles"){

            # check whether the expert has updated their summary statements before proceeding
            if(sum(c(paste0("min",i),
                     paste0("max",i),
                     paste0("quartile1_",i),
                     paste0("quartile1_",i),
                     paste0("quartile1_",i)) %in% names(input))==5){

              if(elici_minis[[que_name]] == input[[paste0("min",i)]] &
                 elici_maxis[[que_name]] == input[[paste0("max",i)]] &
                 elici_q1[[que_name]] == input[[paste0("quartile1_",i)]] &
                 elici_q2[[que_name]] == input[[paste0("quartile2_",i)]] &
                 elici_q3[[que_name]] == input[[paste0("quartile3_",i)]]) {

                condition3 <- 1

              } else {

                condition3 <- 0

              }

              } else {

              condition3 <- 0

            }

            if(condition3 == 0){

              showModal(modalDialog (uiOutput (paste0("update_values_",i)), size="l"))

            } else {

              temp1 <- buttons$que_no; buttons$que_no <- temp1 + 1

              # save answers

              temp1 <- c(buttons$expert_id, elici_minis[[que_name]], elici_maxis[[que_name]], elici_q1[[que_name]], elici_q2[[que_name]], elici_q3[[que_name]])
              save[[paste0("next_que_", i)]] <- temp1
              save[[paste0("next_que_", i, "_colnames")]] <- c("expert_id",
                                                               paste0("que_",i,"_min"),
                                                               paste0("que_",i,"_max"),
                                                               paste0("que_",i,"_lower_quartile"),
                                                               paste0("que_",i,"_median"),
                                                               paste0("que_",i,"_upper_quartile"))

              f_save_answers(save[[paste0("next_que_", i)]],
                             save[[paste0("next_que_", i, "_colnames")]],
                             paste0(buttons$expert_id,"_que_",i,".csv"))

              if(paste0("comment_", i)%in%names(input)){

                if(input[[paste0("comment_", i)]]!=""){

                  comments[[que_name]] <- input[[paste0("comment_", i)]]

                  f_save_answers(c(buttons$expert_id, comments[[que_name]]),
                                 c("expert_id", paste0("que_", i, "_comment")),
                                 paste0(buttons$expert_id, "_comment_que_", i, ".csv"))

                }


              }

              if(tot_eli_ques > 1 & i < tot_eli_ques){

                # if there are more questions to come, move onto the next tab
                updateTabsetPanel(session, "question_tabs",
                                  selected = paste0("Question ",i+1))

              } else {

                # if this is the last question, show a pop up message
                showModal(modalDialog (uiOutput ("end_of_exercise"), size="l"))

              }

            }


          } else if (elicitation_method == "tertiles"){

            if(sum(c(paste0("min",i),
                     paste0("max",i),
                     paste0("tertile1_",i),
                     paste0("tertile1_",i)) %in% names(input))==4){

              if(elici_minis[[que_name]] == input[[paste0("min",i)]] &
                 elici_maxis[[que_name]] == input[[paste0("max",i)]] &
                 elici_t1[[que_name]] == input[[paste0("tertile1_",i)]] &
                 elici_t2[[que_name]] == input[[paste0("tertile2_",i)]]) {

                condition3 <- 1

              } else {

                condition3 <- 0

              }

            } else {

              condition3 <- 0

            }

            if(condition3 == 0){

              showModal(modalDialog (uiOutput (paste0("update_values_",i)), size="l"))

            } else {

              temp1 <- buttons$que_no; buttons$que_no <- temp1 + 1

              # save answers

              temp1 <- c(buttons$expert_id, elici_minis[[que_name]], elici_maxis[[que_name]], elici_t1[[que_name]], elici_t2[[que_name]])
              save[[paste0("next_que_", i)]] <- temp1
              save[[paste0("next_que_", i, "_colnames")]] <- c("expert_id",
                                                               paste0("que_",i,"_min"),
                                                               paste0("que_",i,"_max"),
                                                               paste0("que_",i,"_lower_tertile"),
                                                               paste0("que_",i,"_upper_tertile"))

              f_save_answers(save[[paste0("next_que_", i)]],
                             save[[paste0("next_que_", i, "_colnames")]],
                             paste0(buttons$expert_id,"_que_",i,".csv"))

              if(paste0("comment_", i)%in%names(input)){

                if(input[[paste0("comment_", i)]]!=""){

                  comments[[que_name]] <- input[[paste0("comment_", i)]]

                  f_save_answers(c(buttons$expert_id, comments[[que_name]]),
                                 c("expert_id", paste0("que_", i, "_comment")),
                                 paste0(buttons$expert_id, "_comment_que_", i, ".csv"))

                }


              }

              if(tot_eli_ques > 1 & i < tot_eli_ques){

                # if there are more questions to come, move onto the next tab
                updateTabsetPanel(session, "question_tabs",
                                  selected = paste0("Question ",i+1))

              } else {

                # if this is the last question, show a pop up message
                showModal(modalDialog (uiOutput ("end_of_exercise"), size="l"))

              }

            }

          }

  })

    })

if(save_method == "local"){

  lapply(X = 1:tot_eli_ques, FUN = function(i){

    output[[paste0("download_",i)]] <-

      downloadHandler(

        filename = paste0(buttons$expert_id,"_download_all.csv"),
        content = function(con) {

          que_name <- eli_que_names[i]
          temp1 <- buttons$que_no; buttons$que_no <- temp1 + 1

          if(elicitation_method == "chips and bins"){

            # save answers

            temp2<-c(buttons$expert_id, elici_minis[[que_name]], elici_maxis[[que_name]], chips_value[[que_name]], chips_chips[[que_name]])
            save[[paste0("next_que_", i)]] <- temp2
            save[[paste0("next_que_", i, "_colnames")]] <-   c("expert_id", paste0("que_",i,"_min"), paste0("que_",i,"_max"),
                                                               paste0("que_",i,"_bins_",1:((length(temp2)-2)/2)),
                                                               paste0("que_",i,"_chip_",1:((length(temp2)-2)/2)))

          } else if (elicitation_method == "quartiles") {

            # elici_minis[[que_name]] <- input[[paste0("min",i)]]
            # elici_maxis[[que_name]] <- input[[paste0("max",i)]]
            # elici_q1[[que_name]] <- input[[paste0("quartile1_",i)]]
            # elici_q2[[que_name]] <- input[[paste0("quartile2_",i)]]
            # elici_q3[[que_name]] <- input[[paste0("quartile3_",i)]]

            temp2 <- c(buttons$expert_id, elici_minis[[que_name]], elici_maxis[[que_name]], elici_q1[[que_name]], elici_q2[[que_name]], elici_q3[[que_name]])
            save[[paste0("next_que_", i)]] <- temp2
            save[[paste0("next_que_", i, "_colnames")]] <- c("expert_id",
                                                             paste0("que_",i,"_min"),
                                                             paste0("que_",i,"_max"),
                                                             paste0("que_",i,"_lower_quartile"),
                                                             paste0("que_",i,"_median"),
                                                             paste0("que_",i,"_upper_quartile"))


          } else {

            # # save most recent inputs as reactive values
            # elici_minis[[que_name]] <- input[[paste0("min",i)]]
            # elici_maxis[[que_name]] <- input[[paste0("max",i)]]
            # elici_t1[[que_name]] <- input[[paste0("tertile1_",i)]]
            # elici_t2[[que_name]] <- input[[paste0("tertile2_",i)]]

            temp2 <- c(buttons$expert_id, elici_minis[[que_name]], elici_maxis[[que_name]], elici_t1[[que_name]], elici_t2[[que_name]])
            save[[paste0("next_que_", i)]] <- temp2
            save[[paste0("next_que_", i, "_colnames")]] <- c("expert_id",
                                                             paste0("que_",i,"_min"),
                                                             paste0("que_",i,"_max"),
                                                             paste0("que_",i,"_lower_tertile"),
                                                             paste0("que_",i,"_upper_tertile"))


          }


          if(paste0("comment_", i)%in%names(input)){
            if(input[[paste0("comment_", i)]]!=""){
              save[[paste0("next_que_", i)]] <- c(save[[paste0("next_que_", i)]],input[[paste0("comment_", i)]])
              save[[paste0("next_que_", i, "_colnames")]] <-   c(save[[paste0("next_que_", i, "_colnames")]], paste0("que_",i,"_comment_"))
            }
          }

          outdated_answers <- grep(paste0("que_",i),save[["all_answers_colnames"]])

          if(length(outdated_answers)>0){

            save[["all_answers"]] <- save[["all_answers"]][-outdated_answers]
            save[["all_answers_colnames"]] <- save[["all_answers_colnames"]][-outdated_answers]

          }

          save[["all_answers"]] <- c(save[["all_answers"]], save[[paste0("next_que_", i)]])
          save[["all_answers_colnames"]] <- c(save[["all_answers_colnames"]], save[[paste0("next_que_", i, "_colnames")]])

          data <- t(save[["all_answers"]])
          colnames(data) <- save[["all_answers_colnames"]]
          write.csv(data, con)

          }

      )

    })

}

  # when an expert finishes training

  observeEvent(input$next_que_0,{

        temp <- buttons$next_que_0; buttons$next_que_0 <- temp + 1
        updateTabsetPanel(session, "top_tabs",
                          selected = "Background information"
        )

  })

  observeEvent(input$start_que,{

    temp <- buttons$start_que; buttons$start_que <- temp+1

  })

  observeEvent(input$start_que,{

    updateTabsetPanel(session, "top_tabs",
                      selected = "Questions"
    )

  })


  ######## UIs #######

  ##### error messages #####

  # error message when an expert doesn't enter their unique identifier

  output$no_unique_id <- renderUI({

    tagList(div(strong("Please note that you must enter your unique identifier in order to proceed.")))

  })

  # error message when expert's plausible range is not within parameter limits or max < min
  # text depends on whether the parameter has lower and upper limits

  lapply(X = 1:tot_eli_ques, FUN = function(i){

    output[[paste0("no_plot_",i)]]<-renderUI({

      if(!is.na(quant_limit_lower[i]) & !is.na(quant_limit_upper[i])){

        tagList(div(
          strong("Please make sure you've entered the minimum and the
             maximum, that both values are between ", quant_limit_lower[i],
                 " and ", paste0(quant_limit_upper[i],
                 ", and that the minimum is lower than the maximum."))
        ))

      } else if (!is.na(quant_limit_lower[i])) {

        tagList(div(
          strong("Please make sure you've entered the minimum and the
             maximum, that both values are greater than ", paste0(quant_limit_lower[i],
                 ", and that the minimum is lower than the maximum."))
        ))

      } else if (!is.na(quant_limit_upper[i])) {

        tagList(div(
          strong("Please make sure you've entered the minimum and the
             maximum, that both values are less than ", paste0(quant_limit_upper[i],
                 ", and that the minimum is lower than the maximum."))
        ))

      } else {

        tagList(div(
          strong("Please make sure you've entered the minimum and the
             maximum, and that the minimum is lower than the maximum.")
        ))

      }

    }) #close no_plot_i interface

  }) #close no_plot_i lapply


  # error message when not all chips are used

  output$no_chips<-renderUI({

    tagList(div(
      p(strong("Please make sure you use all the available chips before proceeding."))
    ))

  })

  # error message when quartiles or tertiles are not within expert's plausible range or in logical order

  lapply(X = 0:tot_eli_ques, FUN = function(i){

    output[[paste0("no_quantiles_",i)]]<-renderUI({

      if(i==0){
        que_name <- "test"
      } else {
        que_name <- eli_que_names[i]
      }


      if(elicitation_method == "quartiles"){

        tagList(div(
          strong(
          "Please make sure you've entered all quantities,
          that the minimum is lower than the maximum,
          that all three quartiles are between ",
          elici_minis[[que_name]], " and ", paste0(elici_maxis[[que_name]],
          ", and that your median is between your lower and upper quartiles."))
        ))

      } else if(elicitation_method == "tertiles"){

        tagList(div(
          strong(
          "Please make sure you've entered all quantities,
          that the minimum is lower than the maximum,
          that both tertiles are between ",
          elici_minis[[que_name]], " and ", paste0(elici_maxis[[que_name]],
          ", and that your lower tertile is less than your upper tertile."))
        ))

      }


    })

  })

  lapply(X = 0:tot_eli_ques, FUN = function(i){

    output[[paste0("update_values_",i)]]<-renderUI({

      tagList(div(strong(
        "The summary statements do not represent the values entered in the cells above.",
        br(), br(),
        "Please update the summary statements by clicking on 'Update values' before saving your answers."
        )))

    })
  }
  )

  output$end_of_exercise <- renderUI({

    tagList(div(
      p(strong("You have now completed the exercise. You can go back and check (and if required, edit) your answers, or simply close this web page."))
    ))

  })

  ##### chips and bins plots #####

  lapply(X = 1:tot_eli_ques, FUN = function(i){

    que_name <- eli_que_names[i]

    output[[paste0("plot_",i)]]<- renderPlot({
      f_plot_the_plot(chips_lower[[que_name]], chips_upper[[que_name]],
                      chips_nhigh[[que_name]], chips_nbins[[que_name]],
                      chips_lbins[[que_name]], chips_rbins[[que_name]],
                      chips_chips[[que_name]], quantity[i], units[i])
    }, height = 330, width = 400)#530

    observeEvent(input[[paste0("location_",i)]], { #add red chips to bins
      chips_chips[[que_name]] <- f_add_chips(chips_nbins[[que_name]],
                                             chips_lbins[[que_name]],
                                             chips_rbins[[que_name]],
                                             chips_nchip[[que_name]],
                                             chips_chips[[que_name]],
                                             input[[paste0("location_",i)]]$x,
                                             input[[paste0("location_",i)]]$y)
    })

  })

  # chips and bins plot for training

  output[["plot_0"]]<- renderPlot({
    f_plot_the_plot(chips_lower[["test"]], chips_upper[["test"]],
                    chips_nhigh[["test"]], chips_nbins[["test"]],
                    chips_lbins[["test"]], chips_rbins[["test"]],
                    chips_chips[["test"]], "length of stay", "days")
  }, height = 330, width = 400)#530

  observeEvent(input[["location_0"]], { #add red chips to bins
    chips_chips[["test"]] <- f_add_chips(chips_nbins[["test"]],
                                                   chips_lbins[["test"]],
                                                   chips_rbins[["test"]],
                                                   chips_nchip[["test"]],
                                                   chips_chips[["test"]],
                                                   input[["location_0"]]$x,
                                                   input[["location_0"]]$y)
  })

  ##### quartile figures ####

  lapply(X = 0:tot_eli_ques, FUN = function(i){

    if(i == 0){
      que_name <- "test"
    } else {
      que_name <- eli_que_names[i]
    }


    output[[paste0("quart_fig_",i)]]<- renderPlot({

      if(length(elici_q1[[que_name]])==0 | length(elici_q2[[que_name]])==0 |length(elici_q3[[que_name]])==0){

        par(mar=c(2,0,2,0), bty="n")
        plot(c(10, 20), c(-1,2), type="n",
             ylim = c(0, 1),  ylab = "", yaxt = "n",
             xlim = c(10, 20), xlab = "", xaxt = "n")

      } else {

              f_quartile_figure(elici_minis[[que_name]],
                                elici_maxis[[que_name]],
                                elici_q1[[que_name]],
                                elici_q2[[que_name]],
                                elici_q3[[que_name]])

      }

    }, height = 70, width = 600)

  })

  ##### tertile figures ####

  lapply(X = 0:tot_eli_ques, FUN = function(i){

    if(i == 0){
      que_name <- "test"
    } else {
      que_name <- eli_que_names[i]
    }

    output[[paste0("terts_fig_",i)]]<- renderPlot({

      if(length(elici_t1[[que_name]])==0 | length(elici_t2[[que_name]])==0){

        par(mar=c(2,0,2,0), bty="n")
        plot(c(10, 20), c(-1,2), type="n",
             ylim = c(0, 1),  ylab = "", yaxt = "n",
             xlim = c(10, 20), xlab = "", xaxt = "n")

      } else {

      f_tertile_figure(elici_minis[[que_name]],
                        elici_maxis[[que_name]],
                        elici_t1[[que_name]],
                        elici_t2[[que_name]])

    }

    }, height = 70, width = 600)

  })

  ##### UI interfaces for each tab #####

  ## home page
  output$tab_home<-renderUI({

    tagList(div(
      br(),br(),

      if (buttons$enter_unique_id == 0){

        tagList(div(br(),
                    if(save_method == "local" & buttons$enter_previous_responses == 0){
                      p(strong("Is this the first time you are completing this exercise?"),
                        radioButtons("radio_previous_responses", "", choices = c("Yes" = 1, "No" = 2), selected = 1),
                        column(1, offset = 9, actionButton("enter_previous_responses", "Enter", width='120px', style="background-color: lightgrey"))
                      )} else if (save_method == "local" & buttons$enter_previous_responses == 1 & buttons$radio_previous_responses == 2) {
                      p("Please load your previous answers by clicking on 'Browse' and selecting
                        the most recent saved file, then click on enter to continue.", br(), br(),
                        "Note that you must load the correct file, or your previous answers won't be retrieved.",
                        fluidRow(
                          column(3, fileInput("load_past_answers", "", accept = ".csv", width ='360px')),br(),
                          column(1, offset = 6, actionButton("get_previous_responses", "Enter", width='120px', style="background-color: lightgrey"))
                        ))
                      } else {
                        p(strong("Please enter the unique identifier you've been provided, then click on 'Enter'."), br(),
                          fluidRow(
                            column(3, numericInput("expert_id", "", NULL, min = 0)),br(),
                            column(1, offset = 6, actionButton("enter_unique_id", "Enter", width='120px', style="background-color: lightgrey"))
                            )
                        )
                        },
                    br(), br()))

        } else if (buttons$next_home == 0){

        tagList(div(includeHTML("www/text_home.htm"), br(),
                    fluidRow(
                      column(9, p(style="font-size:90%;", "Please click on 'Next' to continue")),
                      column(1, actionButton("next_home", "Next", width='120px', style="background-color: lightgrey"))
                      )
                    ))

      } else if (buttons$next_home==1 & include_consent) {

        tagList(div(includeHTML("www/text_consent.htm"), br(),
                    fluidRow(
                      column(9, p(style="font-size:90%;", "Please click on 'Next' to continue")),
                      column(1, actionButton("next_home", "Next", width='120px', style="background-color: lightgrey"))
                    )
        ))

      } else if ((buttons$next_home>=1 & include_about_you & buttons$end_home == 0)|
                 (buttons$next_home>=2 & include_consent & include_about_you & buttons$end_home == 0)){

        tagList(div(
          about_you_ques, br(),
          if(save_method == "dropbox"){
            fluidRow(
              column(9, p(style="font-size:90%;", "Please click on 'Next' to continue")),
              column(1, actionButton("next_home", "Save", width='120px', style="background-color: lightgrey"))
          )
          } else {
            fluidRow(
              column(9, p(style="font-size:90%;", "Please click on 'Next' to continue")),
              column(1, downloadButton("download_about_you", "Save", width='120px', style="background-color: lightgrey"))
            )
          }

        ))

      } else {

        "Thank you. You may proceed to the 'Instructions' tab"

      }

      ))

  }) #close output$tab_home


  ## Instructions/training
  output$tab_instructions<-renderUI({

    if (buttons$enter_unique_id == 0) {

      tagList(div(
        br(), br(),
        "Please note that you must enter your unique identifier on the 'Introduction' tab in order to proceed."
      ))

    } else if(conditional_release & buttons$end_home == 0){

      tagList(div(br(),br(),
                  "Please make sure you have read all relevant sections
                  and answered all questions on the 'Introduction' tab in order to proceed."
      ))

    } else {

      tagList(div(
        br(),br(),
        ifelse(elicitation_method == "chips and bins",
               tagList(div(
                 includeHTML("www/text_instructions_chips_and_bins.htm"),
                 br(),hr(),br(),
                 p("Please add", chips_nchip$test,"chips to the grid below to express your uncertainty.
                   The more chips you place in a particular bin the more certain you are
                   that the proportion lies in that bin."),br(), br(),
                 "You can use",strong(chips_nchip$test-sum(chips_chips$test)), " more chips.",
                 HTML("<div style='height: 350px;'>"),
                 plotOutput("plot_0", click="location_0"),
                 HTML("</div>"), br(),
                 ifelse(enter_plot$test == 0,
                        tagList(div(
                          fluidRow(
                            column(9, "When you are happy with your answers please click 'Enter', then scroll down."),
                            column(2, actionButton("enter_plot_0", "Enter", width='120px', style="background-color: lightgrey")
                            )), br(), br(),
                        )),
                        tagList(div(
                          hr(), br(),
                          strong("Summary"),br(),br(),
                          "Your answers imply that",br(),br(),
                          div(f_text_fback_chips(chips_chips$test, chips_lbins$test, chips_rbins$test, "length of stay", "days"),
                              style='width:700px; padding-left:45px;'), br(),br(),
                          "If these summary statements do not represent your beliefs you can modify the grid.",br(),
                          hr(),
                          fluidRow(
                            column(9, p(style="font-size:90%;", "Once you are satisfied that you can express your beliefs in this way, click on 'Next' to continue.")),
                            column(1, tagList(div(actionButton("next_que_0", "Next", width='120px', style="background-color: lightgrey"))))
                          ), br(),
                          strong("Note you will be able to refer to these instructions at any point in the exercise by clicking on the 'Instructions' tab."), br(),br(), br(), br()
                        ))

                 )
                 )),#close text for chips and bins
               ifelse(elicitation_method=="quartiles",
                      tagList(div(
                        includeHTML("www/text_instructions_quartiles.htm"),
                        br(),hr(),br(),
                        fluidRow(column(10,p("Can you determine a value ", strong("(your median, M)"), " such that the proportion is equally likely to be less than or greater than this value?"),br(),
                                        p("Suppose you were told that the proportion is below your assessed median. Can you now provide a new value ", strong("(your lower quartile Q1)", .noWS = c('after')), ", so that the proportion of patients is equally likely to be less than or greater than this value?"), br(),
                                        p("Suppose you were told that the proportion is above your assessed median. Can you now provide a new value ", strong("(your upper quartile Q3)", .noWS = c('after')), ", so that the proportion of patients is equally likely to be less than or greater than this value?"), br()),
                                 column(2,numericInput("quartile2_0", NULL, elici_q2$test, min = elici_minis$test, max = elici_maxis$test), br(), br(),
                                        numericInput("quartile1_0", NULL, elici_q1$test, min = elici_minis$test, max = elici_maxis$test), br(), br(),
                                        numericInput("quartile3_0", NULL, elici_q3$test, min = elici_minis$test, max = elici_maxis$test), br())
                        ), br(),
                        HTML("<div style='height: 70px;width: 600px'>"),
                        plotOutput("quart_fig_0"),
                        HTML("</div>"), br(),
                        ifelse(enter_quarts$test == 0,
                               tagList(div(
                                 fluidRow(
                                   column(9, p(style="font-size:90%;","When you are happy with your answers please click on 'Enter', then scroll down.")),
                                   column(1, actionButton("enter_quarts_0", "Enter", width='120px', style = "background-color: lightgrey"))))),
                               tagList(div(
                                 fluidRow(
                                   column(9, p(style="font-size:90%;","You can change any of the above values, but remember to click on 'Update values' to view updated summary statements.")),
                                   column(1, actionButton("enter_quarts_0", "Update values", width='120px', style = "background-color: lightgrey"))),
                                 hr(), br(),
                                 strong("Summary"),br(),br(),
                                 "Your answers imply that", br(), br(),
                                 tags$li("the proportion is", strong(paste0("equally likely to be less than and greater than ", elici_q2$test,","))),
                                 tags$li("the proportion is", strong("equally likely to be between", elici_q1$test, "and", elici_q3$test,
                                                                     "as it is to be outside this range.")), br(), br(),
                                 "If these summary statements do not represent your beliefs you can modify your answers and click on 'Update values'.",br(),
                                 hr(),
                                 fluidRow(
                                   column(9, p(style="font-size:90%;", "Once you are satisfied that you can express your beliefs in this way, click on 'Next' to continue.")),
                                   column(1, actionButton("next_que_0", "Next", width='120px', style="background-color: lightgrey"))
                                 ), br(), br(),
                                 "Note you will be able to refer to these instructions at any point in the exercise by clicking on the 'Instructions' tab.", br(), br(), br()
                               ))
                        )
                      )),
                      tagList(div(
                        includeHTML("www/text_instructions_tertiles.htm"),
                        br(),hr(),br(),
                        "Can you provide two values within your plausible range that divide the range of values into three equally likely intervals?", br(),br(),
                        fluidRow(column(4,p("Value 1 ", strong("(your lower tertile)"),":")),
                                 column(3, numericInput("tertile1_0", NULL, elici_t1$test, min = 0, max = elici_maxis$test))), br(),
                        fluidRow(column(4,p("Value 2 ", strong("(your upper tertile)"),":")),
                                 column(3, numericInput("tertile2_0", NULL, elici_t2$test, min = 0, max = elici_maxis$test))), br(),
                        br(),
                        HTML("<div style='height: 70px;width: 600px'>"),
                        plotOutput("terts_fig_0"),
                        HTML("</div>"), br(),
                        ifelse(enter_terts$test == 0,
                               tagList(div(
                                 fluidRow(
                                   column(9, p(style="font-size:90%;","When you are happy with your answers please click on 'Enter', then scroll down.")),
                                   column(1, actionButton("enter_terts_0", "Enter", width='120px', style = "background-color: lightgrey"))))),
                               tagList(div(
                                 fluidRow(
                                   column(9, p(style="font-size:90%;","You can change any of the above values, but remember to click on 'Update values' to view updated summary statements.")),
                                   column(1, actionButton("enter_terts_0", "Update values", width='120px', style = "background-color: lightgrey"))),
                                 hr(), br(),
                                 strong("Summary"),br(),br(),
                                 "Your answers imply that the proportion is",
                                 strong("equally likely to be less than", elici_t1$test),
                                 "as it is to be", strong("greater than", elici_t2$test),
                                 "or", strong("between", elici_t1$test,"and", elici_t2$test), ".", br(), br(),
                                 "If these summary statements do not represent your beliefs you can modify your answers and click on 'Update values'.",br(),
                                 hr(),
                                 fluidRow(
                                   column(9, p(style="font-size:90%;", "Once you are satisfied that you can express your beliefs in this way, click on 'Next' to continue.")),
                                   column(1, actionButton("next_que_0", "Next", width='120px', style="background-color: lightgrey"))
                                 ), br(), br(),
                                 "Note you will be able to refer to these instructions at any point in the exercise by clicking on the 'Instructions' tab.", br(), br(), br()
                               ))
                        )
                      ))
               )
        )


      ))#close tagList
    }



  }) #close output$tab_instructions


  ## Background information (should be bespoke)
  output$tab_background_info<-renderUI({

    if (buttons$enter_unique_id == 0) {

      tagList(div(
        br(), br(),
        "Please note that you must enter your unique identifier on the 'Introduction' tab in order to proceed."
      ))

    } else if(conditional_release & buttons$next_que_0 == 0){

      tagList(div(br(),br(),
                  "Please make sure you have completed the practice example
                  on the 'Instructions' tab in order to proceed."
      ))

    } else {

      tagList(div(
        br(),br(),
        includeHTML("www/text_background_info.htm"), br(),
        fluidRow(
          column(8, p(style="font-size:90%;", "Please click on 'Next' to start the exercise.")),
          column(1, offset=1, tagList(div(actionButton("start_que", "Next", width='120px', style="background-color: lightgrey"))))),br(),
        strong("Note you will be able to refer to this information at any point in the exercise by clicking on the 'Background information' tab."),
        br(),br(), br(), br()
        ))

    }

  }) #close output$tab_background_info


  ## Elicitation questions
  lapply(X = 1:tot_eli_ques, FUN = function(i){

    que_name <- eli_que_names[i]

    output[[paste0("eli_question_",i)]] <- renderUI({

      if(elicitation_method == "chips and bins"){

        tagList(div(

          f_chips_and_bins(i,
                           elici_minis[[que_name]],
                           elici_maxis[[que_name]],
                           chips_nchip[[que_name]],
                           chips_chips[[que_name]],
                           chips_lbins[[que_name]],
                           chips_rbins[[que_name]],
                           show_plot[[que_name]],
                           enter_plot[[que_name]],
                           comments[[que_name]],
                           conditions[[que_name]]()
          )

        ))

      } else if(elicitation_method == "quartiles"){

        tagList(div(

          f_quartiles(i,
                      elici_minis[[que_name]],
                      elici_maxis[[que_name]],
                      elici_q1[[que_name]],
                      elici_q2[[que_name]],
                      elici_q3[[que_name]],
                      enter_min_max[[que_name]],
                      enter_quarts[[que_name]],
                      comments[[que_name]],
                      buttons[[paste0("conditions_",i)]]
          )

        ))

      } else if(elicitation_method == "tertiles"){

        tagList(div(

          f_tertiles(i,
                     elici_minis[[que_name]],
                     elici_maxis[[que_name]],
                     elici_t1[[que_name]],
                     elici_t2[[que_name]],
                     enter_min_max[[que_name]],
                     enter_terts[[que_name]],
                     comments[[que_name]],
                     buttons[[paste0("conditions_",i)]]
          )

        ))

      }



    }) #close output$eli_question_i

  }) #close lapply


  ## Tab with questions
  output$tab_questions<-renderUI({

    if (buttons$enter_unique_id == 0) {

      tagList(div(
        br(), br(),
        "Please note that you must enter your unique identifier on the 'Introduction' tab in order to proceed."
      ))

    } else if(conditional_release & buttons$start_que == 0){

      tagList(div(br(),br(),
                  "Please make sure you have read all relevant information
                  on the 'Bakcground information' tab in order to proceed."
      ))

    } else {

    if(tot_eli_ques == 1){

      uiOutput("eli_question_1")

      } else {

        do.call(tabsetPanel, c(id="question_tabs",
                               lapply(1:tot_eli_ques, function(i) {

                                 tabPanel(paste0("Question ",i),
                                          if (buttons$enter_unique_id == 0) {
                                            tagList(div(
                                              br(), br(),
                                              "Please note that you must enter your unique identifier on the 'Introduction' tab in order to proceed."
                                              ))
                                            } else if (conditional_release & i > 1 & buttons$que_no < i){
                                              tagList(div(br(),br(),
                                                          paste0("Please make sure you have answered Question ", i-1,
                                                   " and saved your answers in order to proceed.")
                                                   ))
                                              } else {
                                                uiOutput(paste0("eli_question_",i))
                                              }

                                          )

                                 })

        ))


      } #action for multiple questions

    }

  }) #close output$tab_background_info


} #close shinyServer


