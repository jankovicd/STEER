

app_location <- "//storage/che/teehta/Dina/URP elicitation/app_outline/app_outline_v7"
#set to the path and folder name where the app code is saved.

#a)
include_consent <- FALSE
# To include the consent form in the app, set to TRUE, otherwise FALSE. 
# The form can be edited by editing "text_home.htm" file in the "www" folder of the app.

#b)
include_about_you <- TRUE
# to include questions about experts, set to TRUE, otherwise FALSE

#c)
n_about_you <- 2
# the number of questions about experts in the "about_you.R" file.

#d)
elicitation_method <- "chips and bins"
# set to "chips and bins", "quartiles" or "tertiles"

#e)
# quantities being elicited e.g. proportion, rate, duration
quantity <- c("proportion",
              "time required for the drug to start working",
              "reduction in HbA1C",
              "relative risk")
# Feeds into:
# 1. "I believe it is very unlikely that
# - the >>quantity<< is greater than x
# - the >>quantity<< is less than y."
# 2. "There is a z% probability that the >>quantity<<, on average, 
# is between x and y.

#f)
# units for each quantity being elicited. Use "" if no unit (e.g. relative risk).
units <- c("percent", "minutes", "HbA1C units", "")
# feeds into graphs and feedback text:
# "There is a x% probability that the >>quantity<< is between y and z >>units<<."

#g)
# Lower and upper limit of each quantity being elicited. Use NA if no limit (e.g. upper limit of relative risk).
quant_limit_lower <- c(0, 0, 0, NA)
quant_limit_upper <- c(100, NA, 50, NA)

#h)  
eli_que_text <- c(
  "What proportion of patients will respond to drug A after 3 months of treatment?",
  "How long will it take for Drug A to start working?",
  "How much does Drug A reduce HbA1C after three months of treatment?",
  "If after 3 months of treatment Drug B is 1.6 times more effective than Drug A (i.e. relative risk = 1.6), 
  how much more effective is Drug B after 6 months of treatment?"
)

#i)
conditional_release <- FALSE
# to force experts to complete all sections set to TRUE, otherwise FALSE
# usually set to FALSE while editing the app, then TRUE in the final, published version.


#j)
# name of dropbox folder where elicitation responses are saved
folder_name<-"elicitation_responses"

# If saving_mode is set to "dropbox", on first running create a Dropbox folder where
# the answers will be saved ("folder_name" above), and open the Dropbox folder on your Browser
# install and load the rdrop2 package:
# install.packages("rdrop2"); library(rdrop2)
# then run the following two lines of code:
# token <- drop_auth()
# when prompted, allow R access to the folder on the Dropbox website
# saveRDS(token, "droptoken.rds")


