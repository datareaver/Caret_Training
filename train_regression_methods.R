# Toolkit -----------------------------------------------------------------


library(dplyr)
library(caret)
library(stringr)
library(pacman)
library(tidyr)


# Select Methods ----------------------------------------------------------


tag <- read.csv("tag_data.csv", row.names = 1)
tag <- as.matrix(tag)
model.list <- read.csv("model_list.csv")

# Select only models for regression
regModels <- tag[tag[,"Regression"] == 1,]

all <- 1:nrow(regModels)
# Seed the analysis with the LM model
start <- grep("(lm)", rownames(regModels), fixed = TRUE)
pool <- all[all != start]

# Select 4 model models by maximizing the Jaccard
# dissimilarity between sets of models
nextMods <- maxDissim(regModels[start,,drop = FALSE],
                      regModels[pool, ],
                      method = "Jaccard",
                      n = 3)

methods <- str_extract(rownames(regModels)[c(start, nextMods)],"\\([^()]+\\)")
methods <- substring(methods, 2, nchar(methods)-1)

#load and/or install packages
packages <- model.list[model.list[["method.Argument.Value"]] %in% methods, "Packages"]
packages <- packages[packages != ""] %>% strsplit(", ") %>% unlist() %>% unique()
p_load(char=packages)

#methods <- c("lm","glmnet","rf","gbm","rpart")


# Train Models ------------------------------------------------------------

form <- as.formula("Sepal.Length ~ .")
train.set <- iris

train_methods <- function(method) {

  fit.control <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5)

# customize training parameters
#   fit.grid <- expand.grid(interaction.depth = 1:3,
#                           n.trees = 50*1:10,
#                           shrinkage = 0.1)

  train(formula=form,data=train.set,method=method,trControl=fit.control)#tuneGrid=fit.grid,tuneLength=10)

}

models <- lapply(methods,train_methods)

# Predictions
predictions <- lapply(models,function(x) data.frame(method=getElement(x,"method"),pred=predict(x,iris))) %>%
  rbind_all() %>% gather

# Model comparison
scores <- lapply(models,getTrainPerf) %>% rbind_all()

