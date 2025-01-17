#### DEBUGGING ####
# Geraldine Klarenberg
# Added 12/13/2024

# Run the code below to start the reactive log visualizer

#install.packages("reactlog")
options(shiny.reactlog = TRUE)

# Then run the app in the usual way. At any point, press Ctrl + F3 (or Command + F3)
# to launch the reactive log visualization in your browser. It will show reactive
# activity up until you pressed Ctrl/Command + F3
# Refresh the browser for more recent activity.

# Alternatively, run the line below
shiny::reactlogShow()
# This option is static. Refreshing the browser does not do anything. You have to
# rerun the line of code again