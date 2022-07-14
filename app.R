library(shiny)

options(encoding = "UTF-8")

runApp(appDir = file.path(getwd()),     # This looks for the ui.R, server.R and www folder within the wd. Do not rename these.
       launch.browser = TRUE,           # This line will open the application in the user's default browser
       quiet = FALSE,
       display.mode = "normal")
