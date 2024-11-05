
#a)
all_expert_ids <- c(1111, 2222, 3333, 4444, 5555, 6666, 1234)
# list all expert's unique identifiers.
# experts will only be able to access the app if they enter one of your preset
# unique identifiers. The unique identifiers are used to save experts' answers,
# so there should be one unique ID per expert.
# the numbers provided above are just examples.

#b)
include_consent <- FALSE
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
              "reduction in HbA1C")
# Feeds into:
# 1. "I believe it is very unlikely that
# - the >>quantity<< is greater than x
# - the >>quantity<< is less than y."
# 2. "There is a z% probability that the >>quantity<<, on average, is between x and y.

#f)
# units for each quantity being elicited. Use "" if no unit (e.g. relative risk).
units <- c("percent", "minutes", "HbA1C units")
# length should be the same as "quantity" above
# Feeds into graphs and feedback text:
# "There is a x% probability that the >>quantity<< is between y and z >>units<<."

#g)
# Lower and upper limit of each quantity being elicited. Use NA if no limit (e.g. upper limit of relative risk).
quant_limit_lower <- c(0, 0, 0)
quant_limit_upper <- c(100, NA, 50)
#length of each vector should be the same as "quantity" and "units" above

#h)
eli_que_text <- c(
  "What proportion of patients will respond to drug A after 3 months of treatment?",
  "How long will it take for Drug A to start working?",
  "How much does Drug A reduce HbA1C after three months of treatment?"
)
#length should be the same as "quantity", "units", "quant_limit_lower" and "quant_limit_upper"

#i)
conditional_release <- FALSE
# to force experts to complete all sections set to TRUE, otherwise FALSE
# usually set to FALSE while editing the app, then TRUE in the final, published version.



