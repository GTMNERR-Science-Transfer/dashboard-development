# Authenticate
# setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
#                token = Sys.getenv("TOKEN"),
#                secret = Sys.getenv("SECRET"))

setAccountInfo(name = Sys.getenv("christophermarais"),
               token = Sys.getenv("908D29B518B4A47C935C6E63B03D4C73"),
               secret = Sys.getenv("cH2hwZOTvRMKm5ouNDaX6zJrDo0WVVVsvOTzKRqu"))

# Deploy
deployApp(appFiles = c("app.R", "likes.rds"))
