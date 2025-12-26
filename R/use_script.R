#' Create R script in the manuscript.
#'
#' @param script_type Part of the R script.
#' @param name Nmae of the R script.
#' @param prefix Prefix of the file name.
#'
#' @return Target path, invisibly.
#' @export
#'
use_script <- function(script_type, name, prefix = "rank") {
  # deal with prefix
  if (prefix == "rank"){
    p_name <- length(list.files("scripts")) + 1
    p_name <- sprintf("%03d", p_name)
  } else if (prefix == "date"){
    p_name <- format(Sys.time(), "%Y%m%d%H%M")
  }

  script_type <- gsub(" +", "-", str_to_title_base(script_type))
  name <- gsub(" +", "-", str_to_title_base(name))
  ll <- length(list.files("scripts", pattern = script_type)) + 1
  filename <- paste0(c(p_name, paste0(script_type, "-", ll), name), collapse = "_")
  if (!grepl('\\.R$', filename)){
    filename <- paste0(filename, ".R")
  }
  filepath <- file.path(getwd(), paste0("scripts/", filename))

  # create defalut content for new created R file
  content <- default_content(script_type = script_type)

  # Ensure the R/ directory exists
  if (!dir.exists("scripts")) {
    dir.create("scripts")
    message("Created directory: scripts")
  }

  if (!file.exists(filepath)) {
    writeLines(content, con = filepath)
    message("File created: ", filename)
  } else {
    message("File already exists: ", filename)
  }

  # Check if running in RStudio and open the file
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(filepath)
  } else {
    message("Not in RStudio, opening in default editor.")
    file.edit(filepath)
  }
}


default_content <- function(script_type = "data-cleaning") {
  fdate <- format(Sys.Date(), "%Y-%m-%d")
  des <- ifelse(script_type == "data-cleaning", "performs ", "create ")
  content <-paste0(
    "# type: ", script_type, "\n",
    "# Date: ", fdate, "\n",
    "# Description: ", "This R script ", des, script_type, " in the manuscript.\n\n\n",
    "# Load necessary packages\n",
    "library(tidyverse)\n\n",
    "library(manuscript)\n\n",
    "# Load or Read data from the data directory\n\n\n\n",
    "# The script body\n\n\n\n",
    "# Output the result\n\n\n\n"
    )
  return(content)
}



