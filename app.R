
list_of_packages <- c("shiny", "rdrop2") # list of packages required to run the code

lapply(list_of_packages,                 # install and load all relevant packages
       function(x) if(!require(x,character.only = TRUE)) install.packages(x, dep=TRUE))

options(encoding = "UTF-8")

dummy_app <- FALSE                      # When set to FALSE, requires a unique ID code to run the app. ID codes are set in "manual_inputs.R"

runApp(appDir = file.path(getwd()),     # This looks for the ui.R, server.R and www folder within the wd. Do not rename these.
       launch.browser = TRUE,           # This line will open the application in the user's default browser
       quiet = FALSE,
       display.mode = "normal")
