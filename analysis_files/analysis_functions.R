

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



# function to derive quantiles and probabilities from histograms
if(elicitation_method == "chips and bins"){
  

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
