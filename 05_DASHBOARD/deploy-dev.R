# Load the rsconnect package
library(rsconnect)

# Authenticate
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINY_ACC_NAME"),
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET"))

# Deploy
rsconnect::deployApp(
    appDir = "./05_DASHBOARD",
    appFiles = "app.R",
    appName = "Dashboard-dev",
    forceUpdate = TRUE)
