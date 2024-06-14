# Function to find the directory of a file named app.R
find_directory_of_file <- function(file_name, start_dir=getwd()) {
  # Recursively list all files starting from the start_dir
  app_dir <- fs::dir_ls(start_dir, recurse = TRUE, glob=file_name)
  
  # Check if any file named app.R is found
  if (length(app_dir) > 0) {
    # Assuming you want the directory of the first matching file
    file_dir <- fs::path_dir(app_dir[1])
    return(file_dir)
  } else {
    return(NULL) # Return NULL if the file is not found
  }
}