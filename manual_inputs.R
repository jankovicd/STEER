
#a)
all_expert_ids <- c(1111, 2222, 3333, 4444, 5555, 6666, 1234)
# list all expert's unique identifiers.
# experts will only be able to access the app if they enter one of your preset
# unique identifiers. The unique identifiers are used to save experts' answers,
# so there should be one unique ID per expert.
# the numbers provided above are just examples.

#b)
include_consent <- TRUE
# to include the consent form in the app, set to TRUE, otherwise FALSE.
# the form can be edited by editing "text_home.htm" file in the "www" folder of the app.

#c)
include_about_you <- FALSE
# to include questions about experts, set to TRUE, otherwise FALSE
n_about_you <- 2
# the number of questions about experts in the "about_you.R" file.

#d)
elicitation_method <- "chips and bins"
# set to "chips and bins", "quartiles" or "tertiles"
# note that chips and bins are sometimes referred to as roulette or histogram, while
# quartiles is sometimes referred to as bisection.
# only one method can be used per app.

#e)
# quantities being elicited e.g. proportion, rate, duration
# The length of "quantity" determines the number of questions in the app
quantity <- c("proportion",
              "time required for the drug to start working",
              "reduction in HbA1C",
              "relative risk")
# Feeds into:
# 1. "I believe it is very unlikely that
# - the >>quantity<< is greater than x
# - the >>quantity<< is less than y."
# 2. "There is a z% probability that the >>quantity<<, on average, is between x and y.

#f)
# units for each quantity being elicited. Use "" if no unit (e.g. relative risk).
units <- c("percent", "minutes", "HbA1C units", "")
# length should be the same as "quantity" above
# Feeds into graphs and feedback text:
# "There is a x% probability that the >>quantity<< is between y and z >>units<<."

#g)
# Lower and upper limit of each quantity being elicited. Use NA if no limit (e.g. upper limit of relative risk).
quant_limit_lower <- c(0, 0, 0, NA)
quant_limit_upper <- c(100, NA, 50, NA)
#length of each vector should be the same as "quantity" and "units" above

#h)
eli_que_text <- c(
  "What proportion of patients will respond to drug A after 3 months of treatment?",
  "How long will it take for Drug A to start working?",
  "How much does Drug A reduce HbA1C after three months of treatment?",
  "If after 3 months of treatment Drug B is 1.6 times more effective than Drug A (i.e. relative risk = 1.6),
  how much more effective is Drug B after 6 months of treatment?"
)
#length should be the same as "quantity", "units", "quant_limit_lower" and "quant_limit_upper"

#i)
conditional_release <- FALSE
# to force experts to complete all sections set to TRUE, otherwise FALSE
# usually set to FALSE while editing the app, then TRUE in the final, published version.


#j)
save_method <- "local"
# set to "local" to save files on expert's computer and email the answers over or
# "dropbox" to use dropbox

# if save_method <- "dropbox" name of dropbox folder where elicitation responses are saved
folder_name<-"elicitation_responses"
# note that only one app can be used per folder

# If using Dropbox to save answers, carry out the following 8 steps:
# 1. On first running create a Dropbox folder where the answers will
# be saved ("folder_name" above), and open the Dropbox folder on your Browser
# 2. Install and load the rdrop2 package by running the following line of code:
# install.packages("rdrop2"); library(rdrop2)
# 3. Create the app name in the following line of code:
# app_name <- "r_tool_tertiles"
# 4. Run the following code:
# token <- drop_auth()
# 5. When prompted, allow R access to the folder on the Dropbox website
# 6. Run the following code:
# dropbox_endpoint <- httr::oauth_endpoint(authorize = "https://www.dropbox.com/oauth2/authorize",
#                                          access = "https://api.dropbox.com/oauth2/token")
#
# dropbox_app <- httr::oauth_app(appname=app_name, key = "mmhfsybffdom42w",
#                                secret = "l8zeqqqgm1ne5z0")
#
# token <- httr::oauth2.0_token(endpoint=dropbox_endpoint, app=dropbox_app,cache = TRUE,
#                                       query_authorize_extra = list(token_access_type= "offline"))
# 7. When prompted, allow R access to the folder on the Dropbox website
# 8. Save the token by running the following code (make sure the token is saved to the app folder):
# saveRDS(token, "droptoken.rds")


