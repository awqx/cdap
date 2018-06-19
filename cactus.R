# Functions for downloading SDFs from NCI/CADD's Chemical Identifier Resolver.
install.packages("RCurl")
install.packages("stringr")
install.packages("tidyverse")
install.packages("XML")
library(RCurl)
library(stringr)
library(tidyverse)
library(XML)

make.regex <- function(string) {
  new.string <- str_replace(string, pattern = "\\(", 
                            replacement = "\\\\(")
  new.string <- str_replace(new.string, pattern = "\\)", 
                            replacement = "\\\\)")
  # new.string <- str_replace(new.string, pattern = "\\ÃÂ²|\\B\\s+H\\+|\u03b2", # alternatives: \\ÃÂ²|\\B\\s+H\\+
  #                           replacement = "\\(ÃÂ²|\u03b2\\)")
  return(new.string)
}

download.cactus.results <- function(guest, path, chemical.format) {
  report <- tryCatch({ 
    destfile       <- paste0(path, "/", guest, ".SDF")
    # Chemical format must be parsed to match all the outputs from NCI cactus
    Guest.URL      <- unlist(lapply(guest, URLencode, reserved = T))
    URL            <- paste0(
      "https://cactus.nci.nih.gov/chemical/structure/",
      Guest.URL,
      "/",
      chemical.format
    )
    Map(download.file, url = URL, destfile = destfile)
    data.frame(
      guest = guest,
      downloaded = "yes",
      warning = "no",
      error = "no"
    )
  },
  warning = function(warn) {
    message("Warning: either URL error or already existing directory.")
    destfile       <- paste0(path, "/", guest, ".SDF")
    Guest.URL      <- unlist(lapply(guest, URLencode, reserved = T))
    URL            <- paste0(
      "https://cactus.nci.nih.gov/chemical/structure/",
      Guest.URL, "/", chemical.format
    )
    Map(download.file, url = URL, destfile = destfile)
    data.frame(
      guest = guest,
      downloaded = "yes",
      warning = "yes", 
      error = "no"
    )
  },
  error = function(err) {
    message("An error occurred")
    data.frame(
      guest = guest,
      downloaded = "no",
      warning = "yes",
      error = "yes"
    )
    
  },
  finally = {
    message("Chemical name processed")
  })
  return(report)
}