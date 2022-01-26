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
#library(binda) # for binda
library(DataExplorer)   # For exploratory analysis

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

# What is stem.root = f ? The metadata doesn't indicate it, let's find out...
data_missing_f <- dataset %>% filter(stem.root == "f") %>% select(stem.root, stem.color, stem.surface, stem.width, stem.height) # select all useful (i.e. stem) properties
uniques_missing_f <- unique(data_missing_f)

# Change dataset factors into intelligible words
dataset$class <- recode_factor(dataset$class, e = "edible", p = "poisonous")
dataset$cap.shape <- recode_factor(dataset$cap.shape, b = "bell", c = "conical", x = "convex", f = "flat", s = "sunken", p = "spherical", o = "other")
dataset$cap.surface <- recode_factor(dataset$cap.surface, i = "fibrous", g = "grooves", y = "scaly", s = "smooth", h = "shiny", l = "leathery", k = "silky", t = "sticky", w = "wrinkled", e = "fleshy")
dataset$cap.color <- recode_factor(dataset$cap.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black")
dataset$gill.attachment <- recode_factor(dataset$gill.attachment, a = "adnate", x = "adnexed", d = "decurrent", e = "free", s = "sinuate", p = "pores", f = "none", "?" = "unknown")
dataset$gill.spacing <- recode_factor(dataset$gill.spacing, c = "close", d = "distant", f = "none")
dataset$gill.color <- recode_factor(dataset$gill.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$stem.root <- recode_factor(dataset$stem.root, b = "bulbous", s = "swollen", c = "club", u = "cup", e = "equal", z = "rhizomorphs", r = "rooted", f = "none")
dataset$stem.surface <- recode_factor(dataset$stem.surface, i = "fibrous", g = "grooves", y = "scaly", s = "smooth", h = "shiny", l = "leathery", k = "silky", t = "sticky", w = "wrinkled", e = "fleshy", f = "none")
dataset$stem.color <- recode_factor(dataset$stem.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$veil.type <- recode_factor(dataset$veil.type, p = "partial", u = "universal")
dataset$veil.color <- recode_factor(dataset$veil.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
dataset$ring.type <- recode_factor(dataset$ring.type, c = "cobwebby", e = "evanescent", r = "flaring", g = "grooved", l = "large", p = "pendant", s = "sheathing", z = "zone", y = "scaly", m = "movable", f = "none", "?" = "unknown")
dataset$spore.print.color <- recode_factor(dataset$spore.print.color, n = "brown", b = "buff", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow", l = "blue", o = "orange", k = "black", f = "none")
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

# Exploratory analysis with DataExplorer https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html
plot_str(dataset)
plot_bar(dataset)

# With summarytools ? https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html

# Introductory summaries
summary_number <- nrow(dataset)  # Mushroom count
summary_dataset <- summary(dataset) # Basic summary of all categories

# Distribution plots for numeric/integer parameters
dataset_names <-row.names(structure_dataset)
l <- nrow(structure_dataset)

# Plot all monovariate distributions of the entire dataset
for (n in 1:l){
   plot_title <- paste("Mushroom", dataset_names[n], "distribution")
   plot <- dataset %>%
      ggplot(aes_string(x = dataset_names[n])) + #aes_string allows use of string instead of variable name
      ggtitle(plot_title) +
      ylab("") +
      xlab(dataset_names[n]) +
      theme_bw()
   if(structure_dataset$Final[n] %in% c("integer", "numeric")) # Histogram for integer/numeric, Barplot for character/factors/logical
   {plot <- plot + geom_histogram(fill = "gray45")}
   else
   {plot <- plot + geom_bar(fill = "gray45")}
   plotname <- paste0("study_distrib_", dataset_names[n])   # Concatenate "plot_distrib" with the column name
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

plot_bar(trainvalid_set, by = "class")



# Correlation ???
# library(ggcorrplot)
# model.matrix(~0+., trainvalid_set) %>% 
#    cor(use="pairwise.complete.obs") %>% 
#    ggcorrplot(show.diag = F, type="lower", lab=FALSE)

# Create training and validation sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = trainvalid_set$cap.diameter, times = 1, p = 0.1, list = FALSE)
training_set <- trainvalid_set[-test_index,]
validation_set <- trainvalid_set[test_index,]

# Plot all monovariate distributions of the training set (poisonous vs edible)
for (n in 2:l){    # Column 1 (class) isn't plotted since it's the fill attribute
   plot_title <- paste("Mushroom", dataset_names[n], "distribution")
   plot <- training_set %>%
      ggplot(aes_string(x = dataset_names[n], fill = training_set$class)) + #aes_string allows use of string instead of variable name
      ggtitle(plot_title) +
      ylab("Frequency") +
      xlab(dataset_names[n]) +
      scale_y_log10() +
      theme_bw()
   if(structure_dataset$Final[n] %in% c("integer", "numeric"))  # Histogram for integer/numeric, Barplot for character/factors/logical
   {plot <- plot + geom_histogram()}
   else
   {plot <- plot + geom_bar()}
   plotname <- paste0("train_distrib_",dataset_names[n])   # Concatenate "train_distrib" with the column name
   assign(plotname, plot)     # Assign the plot to the train_distrib_colname name
}

#Plot all bivariate distributions of the training set (poisonous vs edible)
# for (n in 2:l){    # Column 1 (class) isn't plotted since it's the color attribute
#   for (m in 2:l){
#      plot <- training_set %>%
#         ggplot(aes_string(x = dataset_names[n], y = dataset_names[m], color = training_set$class)) + #aes_string allows use of string instead of variable name
#         labs(colour = "class", x = dataset_names[n], y =dataset_names[m]) +
#         theme_bw()
#       if(structure_dataset$Final[n] %in% c("integer", "numeric") & structure_dataset$Final[m] %in% c("integer", "numeric"))  # Histogram for 2x integer/numeric,
#          {plot <- plot + geom_point(alpha = .5, shape = 20)} # regular scatterplot if all variables are numeric/integer
#       else
#          {plot <- plot + geom_jitter(alpha = .5, shape = 20)} # jitter if 1 or 2 variables are character/factors/logical
#       if(structure_dataset$Final[n] %in% c("factor", "logical", "character"))
#          {plot <- plot + scale_x_discrete(guide = guide_axis(angle = 90))} # rotate X axis labels if text
#       plotname <- paste0("train_distrib_",dataset_names[n],"_",dataset_names[m])   # Concatenate "train_distrib" with the column names
#       assign(plotname, plot)     # Assign the plot to the train_distrib_colname1_colname2 name
#    }
# }

predictions <- validation_set
predictions$edible <- as.logical(as.character(recode_factor(predictions$class, edible = TRUE, poisonous = FALSE))) # Switch to logical values
predictions$stupid_predict = FALSE   # Consider all mushrooms as poisonous
predictions <- predictions %>% 
   mutate(mono_predict = (cap.diameter > 35 | habitat %in% c("urban", "waste") | ring.type =="movable" | spore.print.color =="gray" | 
                                 stem.color == "buff" | stem.height > 21 | stem.width > 60 | veil.color == "yellow"))
predictions <- predictions %>% 
   mutate(bi_predict = (cap.diameter > 35 | habitat %in% c("urban", "waste") | ring.type =="movable" | spore.print.color =="gray" | 
                             stem.color == "buff" | stem.height > 21 | stem.width > 60 | veil.color == "yellow" | 
                           cap.color == "buff" & (cap.diameter > 10 | cap.surface == "fleshy" | gill.attachment == "" | gill.color %in% c("buff", "purple", "orange") | gill.spacing == "distant" | habitat %in% c("grasses", "meadows") | has.ring == TRUE | season == "winter") | 
                           cap.color == "blue" & (cap.diameter > 7 | cap.shape == "sunken" | cap.surface %in% c("fleshy", "") | gill.attachment %in% c("adnexed", "decurrent") | gill.color %in% c("buff", "green", "yellow") | habitat == "leaves" | season == "winter") | 
                           cap.color == "gray" & (cap.shape == "conical" | cap.surface %in% c("grooves", "wrinkled", "fleshy", "d") | gill.attachment == "pores" | ring.type == "large") | 
                           cap.color == "red" & (cap.shape %in% c("conical", "spherical") | gill.color == "orange" | habitat == "heaths" | ring.type == "") | 
                           cap.color == "orange" & (cap.shape == "spherical" | gill.attachment == "sinuate" | gill.color %in% c("buff", "red") | gill.spacing == "distant") | 
                           cap.color == "pink" & (cap.surface %in% c("smooth", "shiny") | gill.color %in% c("red", "orange")) | 
                           cap.color == "purple" & (cap.surface %in% c("smooth", "d") | gill.attachment %in% c("decurrent", "") | gill.color %in% c("buff", "pink") | gill.spacing == "distant" | season == "winter") |
                           cap.color == "white" & (cap.surface == "leathery" | gill.color %in% c("orange", "purple") | ring.type %in% c("flaring", "grooved", "")) |
                           cap.color == "yellow" & (gill.attachment == "free" | gill.color == "purple" | ring.type %in% c("evanescent", "grooved")) |
                           cap.color == "black" & (cap.surface == "shiny" | gill.attachment == "sinuate" | gill.spacing == "distant")
                        )
          ) # IDENTIFIER CAP.SURFACE = "D" ????

mean(predictions$edible == predictions$stupid_predict)   # Accuracy of the "all poisonous" model
mean(predictions$edible == predictions$mono_predict)     # Accuracy of the single criterion model
mean(predictions$edible == predictions$bi_predict)     # Accuracy of the double criterion model


#https://topepo.github.io/caret/available-models.html
fit_test <- function(fit_model){
start_time <- Sys.time()   # A SUPPRIMER
fitting <- NULL
prediction <- NULL
accuracy <- NULL
fitting <- train(class ~ ., method = fit_model, data = training_set) # REMPLACER fit_model par "le_modèle" + ajouter paramètres...
prediction <- predict(fitting, validation_set, type = "raw")
accuracy <- confusionMatrix(prediction, validation_set$class)$overall[["Accuracy"]]
end_time <- Sys.time()  # A SUPPRIMER
time <- difftime(end_time, start_time, units = "secs")  # A SUPPRIMER
c(accuracy, time)
}
fit_test("LogitBoost")

#qda_fitting <- train(class ~ ., method = "qda", data = training_set)
#qda_prediction <- predict(qda_fitting, validation_set, type = "raw")
#confusionMatrix(qda_prediction, validation_set$class)$overall[["Accuracy"]]

#fitting <- train(class ~ ., method = "rpart", tuneGrid  = data.frame(cp = seq(0.005, 0.015, len = 20)), data = training_set)
fitting <- train(class ~ ., method = "rpart", data = training_set, tuneGrid=data.frame(cp = .012))
fitting <- train(class ~ ., method = "binda", data = training_set)
fitting <- train(class ~ ., method = "ranger", data = training_set, num.trees = 5)
fitting <- train(class ~ stem.root + veil.color + stem.height, method = "knn", data = training_set)

prediction <- predict(fitting, validation_set, type = "raw")
confusionMatrix(prediction, validation_set$class)$overall[["Accuracy"]]
plot(fitting)
plot(fitting$finalModel, margin = .1)
text(fitting$finalModel, cex = .75)
#text(fitting$finalModel, cex = .75)

model_list <- names(getModelInfo())

# http://topepo.github.io/caret/available-models.html

save.image(file = "EKR-mushrooms.RData")
load("EKR-mushrooms.RData")

####################################################################
# Choix modèles avec le plus de dissimilarités
#https://topepo.github.io/caret/models-clustered-by-tag-similarity.html

# Download caret models tags list, import
model_tags <- tempfile()
download.file("https://topepo.github.io/caret/tag_data.csv", model_tags)
model_tags <- read.csv(model_tags, row.names = 1)
model_tags <- as.matrix(model_tags)

# Select regression models
regression_models <- tag[tag[,"Regression"] == 1,]

all <- 1:nrow(regression_models)
## Seed the analysis with the SVM model
start <- grep("(svmRadial)", rownames(regression_models), fixed = TRUE)
pool <- all[all != start]

## Select 4 model models by maximizing the Jaccard dissimilarity between sets of models
nextMods <- maxDissim(regression_models[start,,drop = FALSE], 
                      regression_models[pool, ], 
                      method = "Jaccard",
                      n = 4)

rownames(regression_models)[c(start, nextMods)]