
library(SHELF)
library(dplyr)

rm(list = ls())

# select which distributions to fit (choose from Normal, Gamma, Log.normal, Beta)
# if fitting multiple distributions per elicitation question,
# code starting from '##### fit distributions #####' will need to be rerun for each distribution.
dist_per_question <- c("Beta", "Log.normal", "Normal", "Normal")

source("manual_inputs.R")
source("analysis_files/analysis_functions.R")

# files with experts' responses must be named in the following format:
# expertID_doanload_all.csv
folder_with_responses <- "analysis_files/experts_responses" # name of folder where experts' answers are saved (should be in the working directory)

n_que <- length(quantity) # total number of elicited questions

que_names <- paste0("que_",1:n_que) # list of elicitation questions, as they appear in file names

list_of_files <- dir(folder_with_responses) # list of all saved files
list_of_files <- list_of_files[list_of_files%in%paste0(all_expert_ids,"_download_all.csv")] # only keep files with correct file name format (expertID_downlaod_all.csv)

#assign numbers to experts who tool part
expert_number <- cbind("Expert ID" = gsub("_download_all.csv","", list_of_files),
                       "Expert number" = 1:length(list_of_files))

##### import experts' answers #####

if(save_method == "dropbox") {

  # import experts' answers about their experience. Only relevant if include_about_you <- TRUE
  if (include_about_you == TRUE){

    about_you_answers <- lapply(paste0(folder_with_responses,"/",list_of_files[grep("about_you",list_of_files)]), read.csv, stringsAsFactors = FALSE)
    about_you_answers <- bind_rows(about_you_answers)

  }

  # import elicitation questions
  eli_que_answers <- lapply(paste0(folder_with_responses,"/",list_of_files[grep(paste(paste0(all_expert_ids,"_que_"),collapse="|"),list_of_files)]), read.csv, stringsAsFactors = FALSE)
  aa<-unlist(lapply(eli_que_answers, "[", "expert_id"))
  bb<-unlist(lapply(eli_que_answers,function(x){gsub("_min","",colnames(x[2]))}))
  names(eli_que_answers) <- paste(aa,bb,sep="_"); rm(aa,bb)

  # import rationale and comments
  comment_answers <- lapply(paste0(folder_with_responses,"/",list_of_files[grep("_comment",list_of_files)]), read.csv, stringsAsFactors = FALSE)
  comment_answers <- bind_rows(comment_answers)

} else {

  all_answers <- lapply(paste0(folder_with_responses,"/",list_of_files), read.csv, stringsAsFactors = FALSE)
  names(all_answers) <- gsub("_download_all.csv","", list_of_files)
  all_answers <- lapply(all_answers, "[", -c(1,2))

  if (include_about_you == TRUE){

    about_you_answers <- lapply(all_answers, function(x){subset(x, select=(grep("about_you",colnames(x))))})
    about_you_answers <- bind_rows(about_you_answers)

  }

  comment_answers <- lapply(all_answers, function(x){subset(x, select=(grep("comment",colnames(x))))})
  comment_answers <- bind_rows(comment_answers)


  eli_que_answers <- list()


  for (i in 1:length(all_answers)){
    for(j in 1:n_que){

      if(elicitation_method == "chips and bins"){

        aa <- all_answers[[names(all_answers)[i]]][,grep(paste0("que_",j,"_min|que_",j,"_max|que_",j,"_bins|que_",j,"_chip"),colnames(all_answers[[names(all_answers)[i]]]))]

      } else if (elicitation_method == "quartiles"){

        aa <- all_answers[[names(all_answers)[i]]][,grep(paste0("que_",j,"_min|que_",j,"_max|que_",j,"_lower_quartile|que_",j,"_median|que_",j,"_upper_quartile"),colnames(all_answers[[names(all_answers)[i]]]))]

      } else if (elicitation_method == "tertiles"){

        aa <- all_answers[[names(all_answers)[i]]][,grep(paste0("que_",j,"_min|que_",j,"_max|que_",j,"_lower_tertile|que_",j,"_upper_tertile"),colnames(all_answers[[names(all_answers)[i]]]))]

      }

      if(length(aa)>0){
        eli_que_answers[[paste0(names(all_answers)[i],"_que_",j)]] <- aa
        }; rm(aa)

    }
  }; rm(i,j)

}

##### fit distributions #####

#create lists for saving fitted parameters
save_fitted_dist <- list()
individual_fit_plots <- list()

#fit pre-determined distributions
for (i in 1:length(names(eli_que_answers))) {

  file_name <- names(eli_que_answers)[i]
  que_no <- as.numeric(gsub(paste0(all_expert_ids,"_que_",collapse="|"),"",file_name)) # question number for i
  temp_id <- gsub(paste0("_que_", que_no), "", file_name) # expert's unique ID (for i)
  expert_no <- expert_number[which(expert_number[,1] == temp_id), 2]; rm(temp_id)

  file_name2 <- paste0(file_name,"_",dist_per_question[que_no])

  if (elicitation_method == "chips and bins"){

    temp <- f_chips_and_bins_probabilities(eli_que_answers[[file_name]])
    elicited_value <- temp[1,]
    elicited_probs <- temp[2,]; rm(temp)

  } else if (elicitation_method == "quartiles"){

    elicited_value <- unlist(eli_que_answers[[file_name]][c(1, 3, 4, 5, 2)])
    elicited_probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)

  } else if (elicitation_method == "tertiles"){

    elicited_value <- unlist(eli_que_answers[[file_name]][c(1, 3, 4, 2)])
    elicited_probs <- c(0.01, 0.33, 0.66, 0.99)

  }


  save_fitted_dist[[file_name2]] <-
    fitdist(elicited_value, elicited_probs,
            lower = ifelse(is.na(quant_limit_lower[que_no]),-Inf, quant_limit_lower[que_no]),
            upper = ifelse(is.na(quant_limit_upper[que_no]), Inf, quant_limit_upper[que_no])
            )[[dist_per_question[que_no]]]


  # calculate mean and variance for betas and gammas
  if(dist_per_question[que_no] == "Beta"){

    alfa <- as.numeric(save_fitted_dist[[file_name2]][1])
    beta <- as.numeric(save_fitted_dist[[file_name2]][2])

    save_fitted_dist[[file_name2]]$mean <- alfa/(alfa+beta)
    #save_fitted_dist[[file_name2]]$mean <- (alfa-1)/(alfa+beta-2)
    save_fitted_dist[[file_name2]]$variance <- (alfa*beta) / ((alfa+beta)^2 * (alfa+beta+1))

    fitted_quantiles <- qbeta(seq(0.01,0.99, by = 0.01), alfa, beta) * 100

    rm(alfa, beta)

  } else if(dist_per_question[que_no] == "Gamma"){

    shape<- as.numeric(save_fitted_dist[[file_name2]][1])
    rate <- as.numeric(save_fitted_dist[[file_name2]][2])

    save_fitted_dist[[file_name2]]$mean <- shape/rate
    save_fitted_dist[[file_name2]]$variance <- shape/rate^2

    fitted_quantiles <- qgamma(seq(0.01,0.99, by = 0.01), shape = shape, rate = rate)

    rm(rate, shape)

  } else if(dist_per_question[que_no] == "Normal"){

    fitted_quantiles <- qnorm(seq(0.01,0.99, by = 0.01), save_fitted_dist[[file_name2]]$mean, save_fitted_dist[[file_name2]]$sd)

  } else if(dist_per_question[que_no] == "Log.normal"){

    fitted_quantiles <- qlnorm(seq(0.01,0.99, by = 0.01), save_fitted_dist[[file_name2]]$mean.log.X, save_fitted_dist[[file_name2]]$sd.log.X)

  }

  lim_1 <- 2*elicited_value[1] - elicited_value[2]
  lim_2 <- elicited_value[length(elicited_value)]

  f_plot_compare_fits(lim_1, lim_2, quantity[que_no], units[que_no], elicited_value, elicited_probs, fitted_quantiles, que_no, expert_no)
  individual_fit_plots[[file_name]] <- recordPlot()

  }; rm(i, file_name, file_name2,
      lim_1, lim_2, elicited_value, elicited_probs, fitted_quantiles, que_no, expert_no)


##### aggregate distributions #####

aggregate_fitted_dist <- list()
compare_experts_plots <- list()

for (i in 1:n_que){

  # save individual experts' answers in one table per question
  indis <- bind_rows(save_fitted_dist[grep(paste0(que_names[i], "_"), names(save_fitted_dist))])

  # derive aggregate mean and variance (+ dist parameters if relevant)
  if (dist_per_question[i]=="Beta") {

    agg_mean <- mean(indis[,"mean"])
    agg_vars <- f_group_variance(indis[,"mean"], indis[,"variance"], agg_mean)

    agg_alpha <- ((1 - agg_mean) / agg_vars - 1 / agg_mean) * agg_mean ^ 2
    agg_beta <- agg_alpha * (1 / agg_mean - 1)

    aggregate_fitted_dist[[que_names[i]]] <- rbind(indis, c(agg_alpha, agg_beta, agg_mean, agg_vars)); rm(agg_alpha, agg_beta)

    #derive 95% confidence intervals for each prior
    select_plot_limits <- unlist(apply(aggregate_fitted_dist[[que_names[i]]], 1,
                                       function(x){qbeta(c(0.025,0.975),x[1], x[2])}
                                       ))

    # select most extreme intervals as axis limits for plots
    lim_1 <- min (select_plot_limits[1,]) * 100
    lim_2 <- max (select_plot_limits[2,]) * 100

    densities <- apply(aggregate_fitted_dist[[que_names[i]]], 1,
                       function(x){dbeta( seq(lim_1, lim_2, length.out = 100)/100, x[1], x[2])}
                       )

  } else if (dist_per_question[i]=="Gamma") {

    agg_mean <- mean(indis[,"mean"])
    agg_vars <- f_group_variance(indis[,"mean"], indis[,"variance"], agg_mean)

    agg_rate <- agg_mean / agg_vars
    agg_shape <- agg_mean * agg_rate

    aggregate_fitted_dist[[que_names[i]]] <- rbind(indis, c(agg_shape, agg_rate, agg_mean, agg_vars)); rm(agg_rate, agg_shape)

    #derive 95% confidence intervals for each prior
    select_plot_limits <- unlist(apply(aggregate_fitted_dist[[que_names[i]]], 1,
                                       function(x){qgamma(c(0.025,0.975),
                                                          shape = x[1], rate = x[2])}
    ))

    # select most extreme intervals as axis limits for plots
    lim_1 <- min (select_plot_limits[1,])
    lim_2 <- max (select_plot_limits[2,])

    densities <- apply(aggregate_fitted_dist[[que_names[i]]], 1,
                       function(x){dgamma( seq(lim_1, lim_2, length.out = 100),
                                           shape = x[1], rate = x[2])}
    )

  } else if (dist_per_question[i]=="Normal") {

    agg_mean <- mean(indis[,"mean"])
    agg_vars <- f_group_variance(indis[,"mean"], indis[,"sd"]^2, agg_mean)

    aggregate_fitted_dist[[que_names[i]]] <- rbind(indis, c(agg_mean, sqrt(agg_vars)))

    #derive 95% confidence intervals for each prior
    select_plot_limits <- unlist(apply(aggregate_fitted_dist[[que_names[i]]], 1,
                                       function(x){qnorm(c(0.025,0.975),x[1], x[2])}
    ))

    # select most extreme intervals as axis limits for plots
    lim_1 <- min (select_plot_limits[1,])
    lim_2 <- max (select_plot_limits[2,])

    densities <- apply(aggregate_fitted_dist[[que_names[i]]], 1,
                       function(x){dnorm( seq(lim_1, lim_2, length.out = 100), x[1], x[2])}
    )

  } else if (dist_per_question[i]=="Log.normal") {

    agg_mean <- mean(indis[,"mean.log.X"])
    agg_vars <- f_group_variance(indis[,"mean.log.X"], indis[,"sd.log.X"]^2, agg_mean)

    aggregate_fitted_dist[[que_names[i]]] <- rbind(indis, c(agg_mean, sqrt(agg_vars)))

    #derive 95% confidence intervals for each prior
    select_plot_limits <- unlist(apply(aggregate_fitted_dist[[que_names[i]]], 1,
                                       function(x){qlnorm(c(0.025,0.975),x[1], x[2])}
    ))

    # select most extreme intervals as axis limits for plots
    lim_1 <- min (select_plot_limits[1,])
    lim_2 <- max (select_plot_limits[2,])

    densities <- apply(aggregate_fitted_dist[[que_names[i]]], 1,
                       function(x){dlnorm( seq(lim_1, lim_2, length.out = 100), x[1], x[2])}
    )

  }; rm(agg_mean, agg_vars, select_plot_limits)

  # set rownames
  aa <- names(save_fitted_dist[grep(paste0(que_names[i], "_"), names(save_fitted_dist))])
  temp_expert_ids <- gsub(paste0("_", que_names[i], "_", dist_per_question[i]), "", aa)
  temp_expert_nos <- integer(0)

  for (a in 1:length(temp_expert_ids)){
    temp <- expert_number[which(expert_number[,1]==temp_expert_ids[a]),2]
    temp_expert_nos <- c(temp_expert_nos,temp)
    }; rm(temp_expert_ids,temp)

  rownames(aggregate_fitted_dist[[que_names[i]]]) <- c(paste0("Expert ",temp_expert_nos), "Pooled")
  colnames(densities) <- rownames(aggregate_fitted_dist[[que_names[i]]])
  rm(temp_expert_nos,aa)

  f_plot_compare_experts(lim_1, lim_2, quantity[i], units[i], densities, i)
  compare_experts_plots[[que_names[i]]] <- recordPlot()


}; rm(i, lim_1, lim_2, densities, indis)


##### save results #####

# all results are saved in analysis/results folder
# plots are saved along the way

for (i in 1:n_que){

  write.csv(aggregate_fitted_dist[[i]], paste0("analysis_files/results/question_",i,"_",
                                               dist_per_question[i],"_priors.csv"))
}

for(i in 1:length(individual_fit_plots)){
  jpeg(file=paste0("analysis_files/results/individual_fit_plots_", names(individual_fit_plots)[i], ".jpeg"), width=600, height=400)
  replayPlot(individual_fit_plots[[i]])
  dev.off()
}

for(i in 1:n_que){
  jpeg(file=paste0("analysis_files/results/compare_experts_plots_", names(compare_experts_plots)[i], ".jpeg"), width=600, height=400)
  replayPlot(compare_experts_plots[[i]])
  dev.off()
}

