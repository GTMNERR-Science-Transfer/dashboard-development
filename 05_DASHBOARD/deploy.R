# Load the rsconnect package
library(rsconnect)

# Authenticate
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINY_ACC_NAME"),
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET"))

# Print the current working directory
print(getwd())

# Deploy
deployApp(appFiles = c("./app.R"))
