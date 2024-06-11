# Define the function to check and load packages
check_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of packages to check and load
packages_to_check <- c("reshape2","ggplot2","dplyr","hrbrthemes","gghighlight","ggbreak")

# Call the function with the list of packages
check_and_load_packages(packages_to_check)
