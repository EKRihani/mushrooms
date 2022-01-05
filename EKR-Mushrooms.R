##########################
#     INITIAL SET-UP     #
##########################

# Check and install required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Load required libraries
library(tidyverse)      # Set of packages used in everyday data analyses
library(caret)      # Set of packages for machine learning
#library(data.table)
#library(mice)        # Package for filling NA's (Multivariate Imputation by Chained Equations)
#library(lattice)
#library(ranger)   # Required by MICE
#library(evtree)   # for evtree
#library(fastAdaboost)   # for adaboost
#library(randomForest)   # for rf
#library(ranger)   # for ranger
library(binda)

# Get, decompress, import data file
datafile <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00615/MushroomDataset.zip", datafile)
#datafile <- "~/projects/mushrooms/MushroomDataset.zip"    # Use Local File (faster)
datafile <- unzip(datafile, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(datafile, header = TRUE, sep = ";")

################################
#  DATA FORMATTING / CLEANING  #
################################

# Get initial dataset structure informations
structure_initial <- sapply(X = dataset, FUN = class, simplify = TRUE) # Get all initial dataset variables classes
unique_length <- function (x) {length(unique(x))}  # Define function : count levels of a variable
structure_uniques <- sapply(dataset, FUN = unique_length) # Count levels of all dataset variables

# Change dataset factors into intelligible words
dataset$class <- recode_factor(dataset$class, e = "edible", p = "poisonous")
dataset$cap.shape <- recode_factor(dataset$cap.shape, b = "bell", c = "conical", x = "convex", f = "flat", s = "sunken", p = "spherical", o = "other")
dataset$cap.surface <- recode_factor(dataset$cap.surface, i = "fibrous", g = "grooves", y = "scaly", s = "smooth", h = "shiny", l = "leathery", k = "silky", t = "sticky", w = "wrinkled", e = "fleshy")
dataset$cap.color <- recode_factor(dataset$cap.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black")
dataset$gill.attachment <- recode_factor(dataset$gill.attachment, a = "adnate", x = "adnexed", d = "decurrent", e = "free", s = "sinuate", p = "pores", f = "none", "?" = "unknown")
dataset$gill.spacing <- recode_factor(dataset$gill.spacing, c = "close", d = "distant", f = "none")
dataset$gill.color <- recode_factor(dataset$gill.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$stem.root <- recode_factor(dataset$stem.root, b = "bulbous", s = "swollen", c = "club", u = "cup", e = "equal", z = "rhizomorphs", r = "rooted")
dataset$stem.surface <- recode_factor(dataset$stem.surface, i = "fibrous", g = "grooves", y = "scaly", s = "smooth", h = "shiny", l = "leathery", k = "silky", t = "sticky", w = "wrinkled", e = "fleshy", f = "none")
dataset$stem.color <- recode_factor(dataset$stem.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$veil.type <- recode_factor(dataset$veil.type, p = "partial", u = "universal")
dataset$veil.color <- recode_factor(dataset$veil.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$ring.type <- recode_factor(dataset$ring.type, c = "cobwebby", e = "evanescent", r = "flaring", g = "grooved", l = "large", p = "pendant", s = "sheathing", z = "zone", y = "scaly", m = "movable", f = "none", "?" = "unknown")
dataset$spore.print.color <- recode_factor(dataset$stem.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$habitat <- recode_factor(dataset$habitat, g = "grasses", l = "leaves", m = "meadows", p = "paths", h = "heaths", u = "urban", w = "waste", d = "woods")
dataset$season <- recode_factor(dataset$season, s = "spring", u = "summer", a = "autumn", w = "winter")
dataset$does.bruise.or.bleed <- as.logical(as.character(recode_factor(dataset$does.bruise.or.bleed, t = TRUE, f = FALSE)))
dataset$has.ring <- as.logical(as.character(recode_factor(dataset$has.ring, t = TRUE, f = FALSE)))
head(dataset)

# Get final dataset structure information
structure_final <- sapply(X = dataset, FUN = class, simplify = TRUE) # Get all final dataset variables classes

# Merge initial and final dataset structure information
structure_dataset <- data.frame(cbind(structure_initial, structure_uniques, structure_final))
colnames(structure_dataset) <- c("Initial", "Levels", "Final")

##################################
#     INTRODUCTORY ANALYSIS      #
##################################

# Introductory summaries
summary_number <- nrow(dataset)  # Mushroom count
summary_dataset <- summary(dataset) # Basic summary of all categories

# Distribution plots for numeric/integer parameters
num_index <- which(structure_dataset$Final %in% c("integer", "numeric"))
num_names <- row.names(structure_dataset[num_index,])
l <- length(num_index)

for (n in 1:l){
   plot_title <- paste("Mushroom", num_names[n], "distribution")
   plot <- dataset %>%
      ggplot(aes_string(x = num_names[n])) + #aes_string allows use of string instead of variable name
      ggtitle(plot_title) +
      ylab("Frequency") +
      xlab(num_names[n]) +
      geom_histogram(fill = "gray45") +
#      geom_density() +
      theme_bw()
   plotname <- paste0("study_distrib_", num_names[n])   # Concatenate "plot_distrib" with the column name
   assign(plotname, plot)     # Assign the plot to the plot_distrib_colname name
}

############################
#     MACHINE TRAINING     #
############################

# Create training and evaluation sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = dataset$cap.diameter, times = 1, p = 0.1, list = FALSE)
trainvalid_set <- dataset[-test_index,]
evaluation_set <- dataset[test_index,]
#rm(dataset)

for (n in 1:l){
   plot_title <- paste("Mushroom", num_names[n], "distribution")
   plot <- trainvalid_set %>%
      ggplot(aes_string(x = num_names[n], color = trainvalid_set$class)) + #aes_string allows use of string instead of variable name
      ggtitle(plot_title) +
      ylab("Frequency") +
      xlab(num_names[n]) +
      geom_density(size = 1.5) +
      scale_y_sqrt() +
      theme_bw()
   plotname <- paste0("train_distrib_", num_names[n])   # Concatenate "plot_distrib" with the column name
   assign(plotname, plot)     # Assign the plot to the plot_distrib_colname name
}

plot_stemHW <- trainvalid_set %>%
   ggplot(aes(x = stem.height, y = stem.width, color = class)) +
   #   scale_x_sqrt() + scale_y_sqrt() +
   geom_point(alpha = .3) +
   theme_bw()

plot_cap_stemW <- trainvalid_set %>%
   ggplot(aes(x = cap.diameter, y = stem.width, color = class)) +
#   scale_x_sqrt() + scale_y_sqrt() +
   geom_point(alpha = .3) +
   theme_bw()

plot_cap_stemH <- trainvalid_set %>%
   ggplot(aes(x = cap.diameter, y = stem.height, color = class)) +
#   scale_x_sqrt() + scale_y_sqrt() +
   geom_point(alpha = .3) +
   theme_bw()

plot_capSS <- trainvalid_set %>%
   ggplot(aes(x = cap.shape, y = cap.surface, color = class)) +
   #   scale_x_sqrt() + scale_y_sqrt() +
   geom_jitter(alpha = .3) +
   theme_bw()

trainvalid_set %>%
   ggplot(aes(x = stem.root, y = stem.color, color = class)) +
   #   scale_x_sqrt() + scale_y_sqrt() +
   geom_jitter(alpha = .3) +
   theme_bw()

# Create training and validation sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = trainvalid_set$cap.diameter, times = 1, p = 0.1, list = FALSE)
training_set <- trainvalid_set[-test_index,]
validation_set <- trainvalid_set[test_index,]

#knn_fitting <- train(class ~ cap.diameter + cap.shape, method = "knn", data = training_set)
#knn_prediction <- predict(knn_fitting, validation_set, type = "raw")
#confusionMatrix(knn_prediction, validation_set$class)$overall[["Accuracy"]]
#confusionMatrix(knn_prediction, validation_set$class)

glm_fitting <- train(class ~ ., method = "glm", data = training_set)
glm_prediction <- predict(glm_fitting, validation_set, type = "raw")
confusionMatrix(glm_prediction, validation_set$class)$overall[["Accuracy"]]

#qda_fitting <- train(class ~ ., method = "qda", data = training_set)
#qda_prediction <- predict(qda_fitting, validation_set, type = "raw")
#confusionMatrix(qda_prediction, validation_set$class)$overall[["Accuracy"]]

#fitting <- train(class ~ ., method = "rpart", tuneGrid  = data.frame(cp = seq(0.005, 0.015, len = 20)), data = training_set)
fitting <- train(class ~ ., method = "rpart", data = training_set, tuneGrid =data.frame(cp =.012))
fitting <- train(class ~ ., method = "binda", data = training_set)
fitting <- train(class ~ ., method = "ranger", data = training_set, num.trees = 5)

prediction <- predict(fitting, validation_set, type = "raw")
confusionMatrix(prediction, validation_set$class)$overall[["Accuracy"]]
plot(fitting)
plot(fitting$finalModel, margin = .1)
text(fitting$finalModel)
#text(fitting$finalModel, cex = .75)

model_list <- names(getModelInfo())

# http://topepo.github.io/caret/available-models.html

save.image(file = "EKR-heartfailure.RData")

############
trainvalid_set %>%
   ggplot(aes(y = MaxHR, x = Age, color = class)) +
   geom_point(alpha = .7) +
   stat_ellipse(type="norm",  lwd  =  1.5)