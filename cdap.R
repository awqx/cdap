
# Setting up --------------------------------------------------------------

getwd()
source("cactus.R")
source("applicability.domain.R")
source("qsar.R")

# Downloading SDFs --------------------------------------------------------

#     Names ----
# Either write out the names of the guest molecules you want...
guest <- c("example1", "example2")

# Or read them from a .csv
guests <- read.csv("guests.csv", header = T) %>% .$guests

#     Structural files with CACTUS ----

# Create a folder to hold the molecules
dir.create("guests")

# Download only a single guest molecule
results.dwnld <- download.cactus.results(guest, path = "guests", 
                                         chemical.format = "SDF")

# Download multiple guest molecules
results.dwnld <-
  do.call(
    rbind,
    lapply(
      guest,
      download.cactus.results,
      path = filepath,
      chemical.format = "SDF"
    )
  ) 

# print(results.dwnld)
# View(results.dwnld)
