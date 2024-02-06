# Load the rsconnect package
library(rsconnect)

# Authenticate
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINY_ACC_NAME"),
    token = Sys.getenv("SHINY_TOKEN"),
    secret = Sys.getenv("SHINY_SECRET"))

# Deploy
rsconnect::deployApp(
    
    appFiles = "app.R",
    appName = "Dashboard")
