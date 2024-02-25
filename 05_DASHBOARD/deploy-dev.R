# Load the rsconnect package
library(rsconnect)

# a function to list all files in a directory and its subdirectories
list_all_files <- function(directories) {
  all_files <- c() # Initialize an empty vector to store all file paths
  
  # Loop through each directory provided
  for (dir in directories) {
    if (!dir.exists(dir)) {
      next # Skip if directory does not exist
    }
    
    # Use list.files to recursively list all files in the directory and its subdirectories
    # full.names = TRUE returns the full paths, which is what we want
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    
    # Append the files found to the all_files vector
    all_files <- c(all_files, files)
  }
  
  return(all_files)
}

# Authenticate
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINY_ACC_NAME"),
    token = Sys.getenv("SHINY_TOKEN"),
    secret = Sys.getenv("SHINY_SECRET"))

# Deploy
req_files <- list_all_files(c("./04_Tests", "./03_Data_for_app")) # get all the required files
rsconnect::deployApp(
    # appDir = "./05_DASHBOARD",
    # appFiles = c("app.R", req_files),
    appName = "Dashboard-dev",
    forceUpdate = TRUE)
