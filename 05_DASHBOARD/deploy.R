# Load the rsconnect package
library(rsconnect)

# Authenticate
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINY_ACC_NAME"),
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET"))

# Deploy
rsconnect::deployApp(
    appFiles = "./05_DASHBOARD/app.R",
    appName = "Dashboard")
