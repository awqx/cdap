# Functions for implementing ensemble prediction with QSARs
install.packages("caret")
install.packages("Cubist")
install.packages("e1071")
install.packages("glmnet")
install.packages("kernlab")
install.packages("pls")
install.packages("randomForest")
library("caret")
library("Cubist")
library("e1071")
library("glmnet")
library("kernlab")
library("pls")
library("randomForest")

source("applicability.domain.R")

# desc: a data.frame containing "guests" and 1376 PaDEL Descriptors
# feat: a vector containing the list of selected variables
# pp.settings: a preProcess object created by caret
preprocess.desc <- function(desc, feat, pp.settings) {
  guests <- desc[ , 1]
  desc <- desc[ , -1]
  colnames(desc) <- str_replace(colnames(desc), "-", ".")
  desc <- do.call(data.frame, lapply(desc, 
                                     function(x)
                                       replace(x, is.infinite(x), NA)))
  desc <- desc %>% 
    predict(pp.settings, .) %>% select(., feat) %>% cbind(guests, .)
  desc.ad <- domain.num(desc)
  outliers <- desc.ad %>% filter(domain == "outside") %>% .$guest
  desc <- desc %>% filter(!guests %in% outliers)
  return(list(desc, outliers))
}


# df should be a data.frame with "guests" (or equivalent) as the first row
predict.alpha <- function(df) {
  outliers <- c()
  features <- readRDS("models/alpha.vars.RDS")
  name.models <- list.files("models/alpha") %>% str_remove(".RDS$")
  path.models <- paste0("models/alpha/", list.files("models/alpha"))
  results <- data.frame()
  
  for(n in 1:length(name.models)) {
    pp.settings <- readRDS(path.models[n])[[1]]
    pp.results <- preprocess.desc(df, features, pp.settings)
    outliers <- c(outliers, pp.results[[2]])
    desc.pp <- pp.results[[1]]
    guests <- desc.pp$guests
    desc.pp <- desc.pp[ , -1]
    qsar <- readRDS(path.models[n])[[2]]
    if(str_detect(name.models[n], "glm")) {
      desc.pp <- data.matrix(desc.pp)
      pred <- predict.glmnet(qsar, desc.pp, 
                             s = tail(qsar$lambda, n = 1))
    } else
      pred <- predict(qsar, desc.pp)
    results.qsar <- data.frame(guests, pred) 
    colnames(results.qsar)[2] <- name.models[n]
    if(n == 1) # initialize the data.frame if it's the first
      results <- results.qsar
    else
      results <- inner_join(results, results.qsar, by = "guests")
  }
  outliers <- unique(outliers)
  full.results <- results %>% 
    mutate(ensemble = rowMeans(results[ , -1]))
  predictions <- full.results %>% select(., guests, ensemble) %>%
    rename(dG = ensemble)
  
  return(list(predictions, full.results, outliers))
}

predict.beta <- function(df) {
  outliers <- c()
  features <- readRDS("models/beta.vars.RDS")
  name.models <- list.files("models/beta") %>% str_remove(".RDS$")
  path.models <- paste0("models/beta/", list.files("models/beta"))
  results <- data.frame()
  
  for(n in 1:length(name.models)) {
    pp.settings <- readRDS(path.models[n])[[1]]
    pp.results <- preprocess.desc(df, features, pp.settings)
    outliers <- c(outliers, pp.results[[2]])
    desc.pp <- pp.results[[1]]
    guests <- desc.pp$guests
    desc.pp <- desc.pp[ , -1]
    qsar <- readRDS(path.models[n])[[2]]
    if(str_detect(name.models[n], "glm")) {
      desc.pp <- data.matrix(desc.pp)
      pred <- predict.glmnet(qsar, desc.pp, 
                             s = tail(qsar$lambda, n = 1))
    } else
      pred <- predict(qsar, desc.pp)
    results.qsar <- data.frame(guests, pred) 
    colnames(results.qsar)[2] <- name.models[n]
    if(n == 1) # initialize the data.frame if it's the first
      results <- results.qsar
    else
      results <- inner_join(results, results.qsar, by = "guests")
  }
  outliers <- unique(outliers)
  full.results <- results %>% 
    mutate(ensemble = rowMeans(results[ , -1]))
  predictions <- full.results %>% select(., guests, ensemble) %>%
    rename(dG = ensemble)
  
  return(list(predictions, full.results, outliers))
}
