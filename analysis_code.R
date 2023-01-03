
library(SHELF)
library(dplyr)

rm(list = ls())

folder_with_responses <- "analysis_files/experts_responses" # name of folder where experts' answers are saved (should be in the working directory)

source("manual_inputs.R")

n_que <- length(quantity) # total number of elicited questions

que_names <- paste0("que_",1:n_que) # list of elicitation questions, as they appear in file names

list_of_files <- dir(folder_with_responses) # list of all saved files

# select which distributions to fit (choose from Normal, Gamma, Log.normal, Beta)
# if fitting multiple distributions per elicitation question,
# code starting from '##### fit distributions #####' will need to be rerun for each distribution.
dist_per_question <- c("Beta", "Log.normal", "Normal", "Normal")

##### functions #####

# derive plot that compares experts' and fitted cummulative density curves
f_plot_compare_fits <- function (lim_1, lim_2, quantity, units, chips_value, chips_probs, fitted_quantiles, que_no, expert_no) {

  par(ps = 12, mar = c(4, 4, 0, 0))
  plot(0, ylim = c(0, 1), xlim = c(lim_1, lim_2),
       ylab = "Cummulative probability", xlab = paste0(quantity, " (", units, ")"),
       pch = "")
  lines(chips_value, chips_probs, lwd = 2, lty = 1)
  lines(fitted_quantiles, seq(0.01, 0.99, by = 0.01), lwd = 1, lty = 2)
  legend("topleft", c(paste0("Expert ", expert_no, ", question ", que_no), "expert", "fitted"),
         lwd = c(0, 2, 1), lty = c(0, 1, 2), bty = "n")

}

# derive plot that compares different experts and their pooled prior
f_plot_compare_experts <- function (lim_1, lim_2, quantity, units, densities, que_no) {

  par(ps = 12, mar = c(4, 4, 0, 0))
  plot(0, ylim = c(0, max(densities)), xlim = c(lim_1, lim_2),
       ylab = "Probability density", xlab = paste0(quantity, " (", units, ")"),
       pch = "")
  for (i in 1:(ncol(densities) - 1)) {
    lines(seq(lim_1, lim_2, length.out = 100), densities[,i], lwd = 2, lty = i + 1)
  }
  lines(seq(lim_1, lim_2, length.out = 100), densities[,ncol(densities)], lwd = 3, lty = 1)
  legend("topright", c(paste0("Question ", que_no), paste0("expert ",colnames(densities))),
         lwd = c(0, rep(2, ncol(densities)-1), 3), lty = c(0, 2:ncol(densities), 1), bty = "n")

}

# derives aggregate variance from experts' individual means and variances, and the aggregate mean
f_group_variance<-function(exp_means,exp_vars,group_mean){
  # exp_means = vector of experts' means. length(exp_means) = the number of experts
  # exp_vars = vector of experts' variances. length(exp_vars) = the number of experts
  # group_mean = experts' aggregate mean (scalar)

  mean (exp_vars, na.rm=TRUE) + mean (exp_means^2 - group_mean^2, na.rm=TRUE)

}

if(elicitation_method == "chips and bins"){

  # function to derive quantiles and probabilities from histograms
  f_chips_and_bins_probabilities <- function(file){
    # file = one row data frame with expert's answers.
    # for question 1, column names are
    # "expert_id" (expert's unique identifier)
    # "que_1_min", "que_1_max" (expert's plausible range)
    # "que_1_bins_1", "que_1_bins_2", "que_1_bins_3", etc. (upper boundary of each bin in the grid)
    # "que_1_chip_1", "que_1_chip_2", "que_1_chip_3", etc. (number of chips in each bin)

    # derive plot parameters
    elici_mini <- file[grep("min", colnames(file))] # lower end of expert's plausible range
    elici_maxi <- file[grep("max", colnames(file))] # upper end of expert's plausible range
    chips_nbins <- length(grep("bins", colnames(file))) # number of bins in expert's grid
    chips_value <- as.numeric(file[grep("bins", colnames(file))]) # upper end of each bin in expert's grid
    chips_chips <- as.numeric(file[grep("chip", colnames(file))]) # number of chips in each bin
    chips_width <- chips_value[2] - chips_value[1] # bin width in the grid
    chips_rbins <- chips_value # left boundary of each bin
    chips_lbins <- chips_rbins-chips_width # right boundary of each bin
    chips_lower <- chips_lbins[1] # lower end of the grid
    chips_upper <- chips_lbins[length(chips_lbins)] # upper end of the grid
    chips_nchip <- sum(chips_chips) # total number of chips used

    # the following code:
    # adjust proportions from chips and bins so they add up to 98% (assuming
    # 2% outside plausible range). Does not adjust if plausible range = parameter limit.
    # if chips placed outside the plausible range, use the number of chips instead of 2%
    # if no chips in bins within expert's plausible range, add half a chip
    if(chips_chips[1] == 0 & chips_chips[chips_nbins] == 0) {

      chips_value <- chips_value[-chips_nbins]
      chips_chips <- chips_chips[-chips_nbins]

      chips_chips[1+which(chips_chips[-1] == 0)] <- 0.5

      chips_probs <- 0.99 * cumsum(chips_chips) / sum(chips_chips)
      chips_probs[1] <- 0.005

      } else if (chips_chips[1] == 0) {

        chips_value <- chips_value[-chips_nbins]
        chips_chips <- chips_chips[-chips_nbins]

        chips_chips[1+which(chips_chips[-1] == 0)] <- 0.5

        chips_probs <- 0.995 * cumsum(chips_chips) / (sum(chips_chips) + 1)
        chips_probs[1] <- 0.005

        } else if (chips_chips[chips_nbins] == 0) {

          chips_value <- chips_value[-chips_nbins]
          chips_chips <- chips_chips[-chips_nbins]

          chips_chips[which(chips_chips == 0)] <- 0.5

          chips_probs <- 0.995 * cumsum(chips_chips) / sum(chips_chips)
          chips_probs[1] <- 0.005

          } else {

            chips_value <- chips_value[-chips_nbins]
            chips_chips <- chips_chips[-chips_nbins]

            chips_probs <- cumsum(chips_chips) / (sum(chips_chips)+1)

          }

    rbind(chips_value,chips_probs)

  }

  # function that draws expert's chips and bins plot
  f_draw_experts_grids <- function(file){

    que_no <- as.numeric(gsub("que_|_min","",colnames(file)[2]))

    elici_mini <- file[grep("min", colnames(file))] # lower end of expert's plausible range
    elici_maxi <- file[grep("max", colnames(file))] # upper end of expert's plausible range
    chips_nbins <- length(grep("bins", colnames(file))) # number of bins in expert's grid
    chips_value <- as.numeric(file[grep("bins", colnames(file))]) # upper end of each bin in expert's grid
    chips_chips <- as.numeric(file[grep("chip", colnames(file))]) # number of chips in each bin
    chips_width <- chips_value[2] - chips_value[1] # bin width in the grid
    chips_rbins <- chips_value # left boundary of each bin
    chips_lbins <- chips_rbins-chips_width # right boundary of each bin
    chips_lower <- chips_lbins[1] # lower end of the grid
    chips_upper <- chips_lbins[length(chips_lbins)] # upper end of the grid
    chips_nchip <- sum(chips_chips) # total number of chips used



    # ADD CODE TO DRAW THE HISTOGRAM, THEN FACET ALL GRAPHS FOR THAT QUESTION
    par(ps = 12, mar = c(4, 0, 0, 0))
    plot(c(chips_lower, chips_upper), c(0, 0),
         xlim = c(chips_lower, chips_upper),
         ylim = c(-1, chips_nchip), type="l",
         ylab = "",
         xlab = paste0(quantity[que_no],ifelse(units[que_no]!="", paste0(" (", units[que_no], ")"), "")),
         xaxp = c(chips_lower, chips_upper, chips_nbins-1),
         yaxt= "n")

    for(i in 1:chips_nbins){
      lines(c(chips_lbins[i], chips_lbins[i]),
            c(0, chips_nchip), lty=3, col=8)
    }

    lines(c(chips_rbins[chips_nbins], chips_rbins[chips_nbins]),
          c(0, chips_nchip), lty=3, col=8)

    for(i in 1:chips_nchip){
      lines(c(chips_lower, chips_upper), c(i, i), lty=3, col=8)
    }

    for(i in 1:chips_nbins){
      if(chips_chips[i] > 0){
        rect(rep(chips_lbins[i], chips_chips[i]), c(0 : (chips_chips[i] - 1)),
             rep(chips_rbins[i], chips_chips[i]), c(1 : chips_chips[i]), col = 2)
      }
    }

  }

}


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
  expert_no <- gsub(paste0("_que_", que_no), "", file_name) # expert's unique ID (for i)
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
  rownames(aggregate_fitted_dist[[que_names[i]]]) <- c(gsub(paste0("_", que_names[i], "_", dist_per_question[i]), "", aa), "Pooled")
  colnames(densities) <- rownames(aggregate_fitted_dist[[que_names[i]]])
  rm(aa)

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

