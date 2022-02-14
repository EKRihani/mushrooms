##########################
#     INITIAL SET-UP     #
##########################

# Check and install required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Load required libraries
library(tidyverse)      # Set of packages used in everyday data analyses
library(caret)      # Set of packages for machine learning
library(DataExplorer)   # For exploratory analysis
library(RColorBrewer)     # ggplot2 palettes
library(GGally)      # Correlation plots (pairs)

# Get, decompress, import data file
datafile <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00615/MushroomDataset.zip", datafile)
#datafile <- "~/projects/mushrooms/MushroomDataset.zip"    # Fichier local, A ENLEVER
datafile <- unzip(datafile, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(datafile, header = TRUE, sep = ";")



################################
#  DATA FORMATTING / CLEANING  #
################################

# Get initial dataset structure informations
structure_initial <- sapply(X = dataset, FUN = class, simplify = TRUE) # Get all initial dataset variables classes
unique_length <- function (x) {length(unique(x))}  # Define function : count levels of a variable
structure_uniques <- sapply(dataset, FUN = unique_length) # Count levels of all dataset variables


# What is "stem.root = f" ? The metadata doesn't indicate it, let's find out...
data_missing_f <- dataset %>% 
   filter(stem.root == "f") %>% 
   select(stem.root, stem.color, stem.surface, stem.width, stem.height) # select all useful (i.e. stem.x) properties
uniques_missing_f <- unique(data_missing_f)

# Change dataset factors into intelligible words
dataset$class <- recode_factor(dataset$class, e = "edible", p = "poisonous")
dataset$cap.shape <- recode_factor(dataset$cap.shape, b = "bell", c = "conical", x = "convex", f = "flat", s = "sunken", p = "spherical", o = "other")
dataset$cap.surface <- recode_factor(dataset$cap.surface, i = "fibrous", g = "grooves", y = "scaly", s = "smooth", h = "shiny", l = "leathery", k = "silky", t = "sticky", w = "wrinkled", e = "fleshy", d = "downy")
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
structure_dataset$Levels <- as.numeric(structure_dataset$Levels)



##################################
#     INTRODUCTORY ANALYSIS      #
##################################

# Exploratory analysis with DataExplorer https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html
plot_str(dataset)
plot_bar(dataset)
# Avec summarytools ? https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html

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



##########################################################
#     TRAINING, VALIDATION, EVALUATION SETS CREATION     #
##########################################################

# Create training and evaluation sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = dataset$cap.diameter, times = 1, p = 0.1, list = FALSE)
trainvalid_set <- dataset[-test_index,]
evaluation_set <- dataset[test_index,]
#rm(dataset)

plot_bar(trainvalid_set, by = "class")

# Create training and validation sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = trainvalid_set$cap.diameter, times = 1, p = 0.1, list = FALSE)
training_set <- trainvalid_set[-test_index,]
validation_set <- trainvalid_set[test_index,]



#######################################
#     SIMPLE CLASSIFICATION MODEL     #
#######################################

# Create criteria lists for single variable classification
factors_list <- training_set %>% select_if(is.factor) %>% gather(factor, level) %>% unique() %>% select(factor, level) %>% filter(factor != "class")

factors_type <- training_set %>% summary.default %>% as.data.frame %>% group_by(Var1) %>% spread(Var2, Freq) %>% as.data.frame   # Get training_set structure
factors_type$Class <- if_else(factors_type$Class == "-none-", factors_type$Mode, factors_type$Class)     # Create coherent Class column
factors_type <- factors_type %>% select(-Mode, -Length)    # Clean factors_type
colnames(factors_type) <- c("factor", "type")

add_factorsL <- factors_type %>% filter(type == "logical") %>% slice(rep(1:n(), each = 2)) %>% mutate(level = rep(c("TRUE", "FALSE"), times = n()/2)) # Create TRUE/FALSE for each logical factor
add_factorsN <- factors_type %>% filter(type == "numeric") %>% slice(rep(1:n(), each = 2)) %>% mutate(level = rep(c("min", "max"), times = n()/2))      # Create rows for numeric factors

factors_list <- left_join(factors_list, factors_type)
factors_list <- rbind(factors_list, add_factorsL, add_factorsN)
rm(add_factorsL, add_factorsN)    # Clear environment

# Build factor lists for 1 variable analysis
factors_list1 <- factors_list
factors_list1$all_edible <- FALSE       # Set as poisonous by default

# Build factor lists for 2 variables analysis
n <- nrow(factors_list)
index_list2 <- t(combn(n, 2))          # Create all combinations of 2 factors
half1_list2 <- factors_list[index_list2[,1],]
colnames(half1_list2) <- c("factor1", "level1", "type1")
half2_list2 <- factors_list[index_list2[,2],]
colnames(half2_list2) <- c("factor2", "level2", "type2")
factors_list2 <- cbind(half1_list2, half2_list2)
factors_list2 <- factors_list2 %>% filter(factor1 != factor2)

factors_list2$all_edible <- FALSE      # Set as poisonous by default
rm(half1_list2, half2_list2, index_list2)    # Clear environment

#Check factors_list2 structure : in theory, by construction, there should be NO text factor2 + numeric factor1
factors_check <- factors_list2 %>% filter(type2  %in% c("logical", "factor", "character"), type1 %in% c("integer", "numeric")) %>% nrow


#Define function : min/max and rounding mode selection
minmaxing <- function(input_level, margin_value){
   margin <- as.numeric(as.character(recode_factor(input_level, min = -margin_value, max = margin_value))) # Set margin as +M (max)/ or -M (min)
   c(match.fun(input_level), margin)   # Set vector with "min"/"max" function as [[1]] and +/- margin numeric as [[2]]
   }

# Define function : rounding + min/max concatenation + sup/inf conversion
supinf <- function(input_value, min_max, list_number, level_number){
   level <- paste0("factors_list", list_number, "$level", level_number, "[n]")  # Select factors_list, factors_list1 or factors_list2 .$levels
   input_value %>%
      round(., digits = 1) %>% 
       + min_max[[2]] %>%   # Add/remove margin
      max(., 0) %>%        # Change negatives values to zero
      paste0(eval(parse(text = level)), .) %>%      # Paste "min" or "max" before rounded value
      str_replace_all(., "min", "< ") %>%      # Replace "min" by "< "
      str_replace_all(., "max", "> ")      # Paste "max" by "> "
}


margin <- 1.1

# Find all "edible-only" criteria
l <- nrow(factors_list1)
for (n in 1:l){
    if(factors_list1$type[n] %in% c("logical", "factor", "character"))
       {
          factors_list1$all_edible[n] <-training_set %>% 
             filter(class == "poisonous", get(factors_list1$factor[n]) == factors_list1$level[n]) %>% 
             nrow() == 0  # Find if (for this factor/level combination) there are no poisonous, i.e. ONLY edible species
       }
    else          # Type = integer or numeric
       {
         minmax <- minmaxing(factors_list1$level[n], margin)     # Setting min/max and rounding values for ".$level"
         current_val <- training_set %>% filter(class == "poisonous") %>% select(factors_list1$factor[n]) %>% minmax[[1]](.)
         extremum <- training_set %>%  select(factors_list1$factor[n]) %>% minmax[[1]](.)
         factors_list1$all_edible[n] <- current_val != extremum
         factors_list1$level[n] <- supinf(current_val, minmax, "1", "")   # Round by excess or default, paste < or > on factors_list"1"$level""
       }
}

# Get relevant (i.e. edible-only) factors, data types and levels (criterion)
factors_to_remove <- factors_list1 %>% filter(all_edible == TRUE, type %in% c("factor", "logical", "character")) %>% select(factor, level)

# Concatenate all factor/levels as one criteria string, before removing them from the dual-criteria list
one_crit1 <- paste0("(factors_list2$factor1 == '", factors_to_remove$factor,"' & factors_list2$level1 == '", factors_to_remove$level,"')", collapse = " | ")
one_crit2 <- paste0("(factors_list2$factor2 == '", factors_to_remove$factor,"' & factors_list2$level2 == '", factors_to_remove$level,"')", collapse = " | ")
single_crit_removal <- paste(one_crit1, one_crit2, sep = " | ")   # Concatenate factor1 and factor2 lists
single_crit_index <- which(eval(parse(text = single_crit_removal)))  # Get all indices

# Remove all single-criteria with only edible mushrooms from the dual-criteria list
factors_list2 <- factors_list2[-single_crit_index,]
rm(one_crit1, one_crit2, single_crit_removal, single_crit_index)    # Clear environment

l <- nrow(factors_list2)
for (n in 1:l){
   if(factors_list2$type1[n] %in% c("logical", "factor", "character") & factors_list2$type2[n] %in% c("logical", "factor", "character")) # factor1 & factor 2 are text
   {
      count <- training_set %>%
         filter(get(factors_list2$factor1[n]) == factors_list2$level1[n], get(factors_list2$factor2[n]) == factors_list2$level2[n]) %>%
         nrow
      count_poison <- training_set %>%
         filter(class == "poisonous", get(factors_list2$factor1[n]) == factors_list2$level1[n], get(factors_list2$factor2[n]) == factors_list2$level2[n]) %>%
         nrow
      factors_list2$all_edible[n] <- count != 0 & count_poison == 0 # Find if (for this factor/level combination) there are mushrooms AND no poisonous, i.e. ONLY edible species
   }
   else          # factor1 is text & factor2 is number
   {if(factors_list2$type1[n] %in% c("logical", "factor", "character") & factors_list2$type2[n] %in% c("numeric", "integer"))
      {
         minmax <- minmaxing(factors_list2$level2[n], margin)
         current_val <-training_set %>% filter(class == "poisonous", get(factors_list2$factor1[n]) == factors_list2$level1[n]) %>% select(factors_list2$factor2[n]) %>% minmax[[1]](.) # %>% as.character
         extremum <- training_set %>% filter(get(factors_list2$factor1[n]) == factors_list2$level1[n]) %>% select(factors_list2$factor2[n]) %>% minmax[[1]](.) # %>% as.character
         factors_list2$all_edible[n] <- current_val != extremum
         factors_list2$level2[n] <- supinf(current_val, minmax, 2, 2)     # Add or substract margin, paste < or > on factors_list"2"$level"2"
      }
   else     # factor1 & factor2 are numbers
      {
         minmax1 <- minmaxing(factors_list2$level1[n], margin)
         minmax2 <- minmaxing(factors_list2$level2[n], margin)
         current_val1 <- training_set %>% filter(class == "poisonous") %>% select(factors_list2$factor1[n]) %>%  minmax1[[1]](.)
         extremum1 <- training_set %>%  select(factors_list2$factor1[n]) %>%  minmax1[[1]](.)
         current_val2 <- training_set %>% filter(class == "poisonous") %>% select(factors_list2$factor2[n]) %>%  minmax2[[1]](.)
         extremum2 <- training_set %>%  select(factors_list2$factor2[n]) %>%  minmax2[[1]](.)
         factors_list2$all_edible[n] <- current_val1 != extremum1 & current_val2 != extremum2
         factors_list2$level1[n] <- supinf(current_val1, minmax, 2, 1)     # Add or substract margin, paste < or > on factors_list"2"$level"1"
         factors_list2$level2[n] <- supinf(current_val2, minmax, 2, 2)     # Add or substract margin, paste < or > on factors_list"2"$level"2"
      }
   }
}

# Show relevant (i.e. edible-only) factors, data types and levels (criterion)
relevant_factors1 <- factors_list1 %>% filter(all_edible == TRUE) %>% select(factor, level, type)
relevant_factors2 <- factors_list2 %>% filter(all_edible == TRUE) %>% select(factor1, level1, type1, factor2, level2, type2)

# Transform all text factors into "=='factors'" for data analysis
str_factors1 <- relevant_factors1 %>% filter(type %in% c("factor", "logical", "character")) %>% mutate(level = str_c("== '", level, "'"))
single_criteria <- relevant_factors1 %>% filter (type %in% c("numeric", "integer")) %>% rbind(str_factors1, .)
str_factors2f <- relevant_factors2 %>% filter(type1 %in% c("factor", "logical", "character")) %>% mutate(level1 = str_c("== '", level1, "'"))
str_factors2ff <- str_factors2f %>% filter(type2 %in% c("factor", "logical", "character")) %>% mutate(level2 = str_c("== '", level2, "'"))
str_factors2f <- str_factors2f %>% filter(type2 %in% c("numeric", "integer"))  %>% rbind(str_factors2ff, .)
double_criteria <- relevant_factors2 %>% filter(type1 %in% c("numeric", "integer"))  %>% rbind(str_factors2f, .)
rm(str_factors1, str_factors2f, str_factors2ff)    # Clear environment

# Concatenate all factor/levels as one criteria string
mono_criteria_list <- paste(single_criteria$factor, single_criteria$level, collapse = " | ")
double_criteria_list <- paste("(", double_criteria$factor1, double_criteria$level1, "&", double_criteria$factor2, double_criteria$level2, ")",collapse = " | ")
bi_criteria_list <- paste(mono_criteria_list, "|", double_criteria_list)

# Create a prediction dataset, with boolean factors (meaning "is.edible") as .$reference
predictions <- validation_set
predictions$reference <- as.logical(as.character(recode_factor(predictions$class, edible = TRUE, poisonous = FALSE))) # Switch to logical values

# Apply the three predictive models : stupid , single-criteron, double-criteria
predictions$stupid_predict = FALSE   # Consider all mushrooms as poisonous
predictions <- predictions %>% mutate(mono_predict = eval(parse(text = mono_criteria_list)))
predictions <- predictions %>% mutate(bi_predict = eval(parse(text = bi_criteria_list)))

# Convert .$reference from logical to factor (confusionMatrix works with factors)
predictions$reference <- as.factor(predictions$reference)
predictions$stupid_predict <- factor(predictions$stupid_predict, levels = c("FALSE","TRUE")) # Create level TRUE (not present in stupid model) for confusionMatrix use (reference & prediction must have the same levels)
predictions$mono_predict <- as.factor(predictions$mono_predict)
predictions$bi_predict <- as.factor(predictions$bi_predict)

# Confusion matrices
CM_stupid <- confusionMatrix(data = predictions$stupid_predict, reference = predictions$reference, positive = "TRUE")
CM_monocrit <- confusionMatrix(data = predictions$mono_predict, reference = predictions$reference, positive = "TRUE")
CM_bicrit <- confusionMatrix(data = predictions$bi_predict, reference = predictions$reference, positive = "TRUE")

CM_monocrit["byClass"]
CM_bicrit["byClass"]
CM_monocrit["table"]
CM_bicrit["table"]
predictions %>% filter (reference == FALSE & bi_predict == TRUE)

#############################################
#     DESCRIPTIVE TRAINING SET ANALYSIS     #
#############################################

# Plot all monovariate distributions of the training set (poisonous vs edible)
l <- nrow(structure_dataset)

for (n in 2:l){    # Column 1 (class) isn't plotted since it's the fill attribute
   plot_title <- paste("Mushroom", dataset_names[n], "distribution")
   plot <- training_set %>%
      ggplot(aes_string(x = dataset_names[n], fill = training_set$class)) + #aes_string allows use of string instead of variable name
      ggtitle(plot_title) +
      ylab("Frequency") +
      xlab(dataset_names[n]) +
      scale_y_log10() +
      #      scale_color_brewer(palette = "Set1", direction = -1) +
      theme_bw()
   if(structure_dataset$Final[n] %in% c("integer", "numeric"))  # Histogram for integer/numeric, Barplot for character/factors/logical
   {plot <- plot + geom_histogram()}
   else
   {plot <- plot + geom_bar()}
   plotname <- paste0("train_distrib_",dataset_names[n])   # Concatenate "train_distrib" with the column name
   assign(plotname, plot)     # Assign the plot to the train_distrib_colname name
   rm(plot)    # Clear environment
}
''
# For bivariate analysis : reduced dataset factor names list (<= 6 levels or numeric)
structure_dataset_reduced <- structure_dataset %>% filter(Levels <= 6 | Final == "numeric")
dataset_reduced_names <-  rownames(structure_dataset_reduced)
l <- length(dataset_reduced_names)

#Plot all bivariate distributions of the training set (poisonous vs edible)
# for (n in 2:l){    # Column 1 (class) isn't plotted since it's the color attribute
#   for (m in 2:l){
#      plot <- training_set %>%
#         ggplot(aes_string(x = dataset_reduced_names[n], y = dataset_reduced_names[m], color = training_set$class)) + #aes_string allows use of string instead of variable name
#         labs(colour = "class", x = dataset_reduced_names[n], y =dataset_reduced_names[m]) +
#         #scale_color_brewer(palette = "Set1", direction = -1) +
#         theme_bw()
#       if(structure_dataset_reduced$Final[n] %in% c("integer", "numeric") & structure_dataset_reduced$Final[m] %in% c("integer", "numeric"))  # Histogram for 2x integer/numeric,
#          {plot <- plot + geom_point(alpha = .4, shape = 20, size =2)} # regular scatterplot if all variables are numeric/integer
#       else
#          {plot <- plot + geom_jitter(alpha = .4, shape = 20, size = 2)} # jitter if 1 or 2 variables are character/factors/logical
#       if(structure_dataset_reduced$Final[n] %in% c("factor", "logical", "character"))
#          {plot <- plot + scale_x_discrete(guide = guide_axis(angle = 90))} # rotate X axis labels if text
#       plotname <- paste0("train_distrib_",dataset_reduced_names[n],"_",dataset_reduced_names[m])   # Concatenate "train_distrib" with the column names
#       assign(plotname, plot)     # Assign the plot to the train_distrib_colname1_colname2 name
#    }
# }

# Graphiques corrélations avec ggpairs.
pair_plots <- ggpairs(
   training_set,
   columns = c(2,6,7,10,11),
   lower = NULL,
   diag = list(continuous = wrap("densityDiag", alpha = .6), 
               discrete = wrap("barDiag")
   ),
   upper = list(continuous = wrap("points", alpha = .3, shape = 20), 
                combo = wrap("dot", alpha = .3, shape = 20),
                discrete = wrap("dot_no_facet", alpha = .3, shape = 20)
   ),
   ggplot2::aes(color = class)
)


###################################################
#     TRAINING SET ANALYSIS WITH CARET MODELS     #
###################################################

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

model_list <- names(getModelInfo())

# http://topepo.github.io/caret/available-models.html

save.image(file = "EKR-mushrooms.RData")
load("EKR-mushrooms.RData")