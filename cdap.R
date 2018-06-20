
# Setting up --------------------------------------------------------------

getwd()
source("cactus.R")
source("applicability.domain.R")
source("qsar.R")

# Downloading SDFs --------------------------------------------------------

#     Names ----
# Either write out the names of the guest molecules you want...
guests <- c("cinnarizine", "diethylamine", "acetaminophen", "piroxicam")

# Or read them from a .csv
guests <- read.csv("guests.csv", header = T) %>% .$guests

#     Structural files with CACTUS ----

# Create a folder to hold the molecules
dir.create("guests")

# Download only a single guest molecule
results.dwnld <- download.cactus.results(guests, path = "guests", 
                                         chemical.format = "SDF")

# Download multiple guest molecules
results.dwnld <-
  do.call(
    rbind,
    lapply(
      guests,
      download.cactus.results,
      path = "guests",
      chemical.format = "SDF"
    )
  ) 

# print(results.dwnld)
# View(results.dwnld)

# Chemical descriptors ----------------------------------------------------

# Reading the file containing the descriptors
desc <- read.csv("desc.csv", header = T) %>% 
  rename(guest = Name) %>% 
  mutate(guest = as.character(guest))


# QSAR prediction ---------------------------------------------------------

# Predict dG values for alpha- and beta-CD
alpha.results <- predict.alpha(desc)
beta.results <- predict.beta(desc)

# View the results
alpha.results[[1]]
beta.results[[1]]

# View the reuslts of each separate QSAR
alpha.results[[2]]
beta.results[[2]]

# View molecules that fall outside the applicability domain
alpha.results[[3]]
beta.results[[3]]

# Graph and compare
#     A graph comparing alpha-CD guests
alpha.dg <- alpha.results[[1]] %>% .[order(.$dG), ]
ggplot(alpha.dg, aes(x = guests, y = dG)) + 
  geom_bar(stat = "identity") + 
  theme_bw()
#     A graph comparing beta-CD guests
beta.dg <- beta.results[[1]] %>% .[order(.$dG), ]
ggplot(beta.dg, aes(x = guests, y = dG)) + 
  geom_bar(stat = "identity") + 
  theme_bw()
#     A graph comparing alpha- and beta-CD guests together
combined.dg <- rbind(
  mutate(alpha.dg, host = "alpha"), 
  mutate(beta.dg, host = "beta")
)

ggplot(combined.dg, aes(x = guests, y = dG, fill = host)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  labs(x = "Guest molecules", 
       y = "Predicted dG, kJ/mol", 
       fill = "CD type", 
       title = "Ensemble QSAR predictions")

# Save the results
saveRDS(combined.dg, "results.RDS")
write.csv(combined.dg, "results.csv", row.names = F)
