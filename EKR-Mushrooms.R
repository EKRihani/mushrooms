##########################
#     INITIAL SET-UP     #
##########################

# Check and install required libraries
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")          # Tools for machine learning
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")            # Linear Discriminant Analysis (lda2)
if(!require(mda)) install.packages("mda", repos = "http://cran.us.r-project.org")              # Penalized Discriminant Analysis (pda)
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")              # Generalized Additive Models (gamLoess)
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")          # Classification And Regression Tree (rpart, rpartCost)
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")            # Needed for rpartCost
if(!require(C50)) install.packages("C50", repos = "http://cran.us.r-project.org")              # C5.0 models (c50tree)
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")          # Conditional Inference Tree (ctree)
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")        # Random Forest, fast implementation (ranger)
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")          # Needed for ranger
if(!require(rFerns)) install.packages("rFerns", repos = "http://cran.us.r-project.org")        # Random Ferns (rFerns)
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")      # Random forests (Rborist)
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")        # Correlation plots (pairs)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")  # Set of packages used in everyday data analyses

# Load required libraries
library(tidyverse)
library(GGally)
library(caret)

# Get, decompress, import data file
URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00615/MushroomDataset.zip"
#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"  # Alternative URL

datafile <- tempfile()
download.file(URL, datafile)
datafile <- unzip(datafile, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(datafile, header = TRUE, sep = ";")

# Define function : return object size in Mb
obj_size <- function(fcn_object){
   object <- eval(parse(text = fcn_object))
   size <- format(object.size(object), units = "Mb")
   size <- str_remove(size, " Mb")
   size <- as.numeric(size)
   size
}

################################
#  DATA FORMATTING / CLEANING  #
################################

# Get initial dataset structure informations
structure_initial <- sapply(X = dataset, FUN = class, simplify = TRUE)  # Get all initial dataset variables classes
unique_length <- function (x) {length(unique(x))}                       # Define function : count levels of a variable
structure_uniques <- sapply(dataset, FUN = unique_length)               # Count levels of all dataset variables


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

# Get final dataset structure information
structure_final <- sapply(X = dataset, FUN = class, simplify = TRUE)    # Get all final dataset variables classes

# Merge initial and final dataset structure information
structure_dataset <- data.frame(cbind(structure_initial, structure_uniques, structure_final))
colnames(structure_dataset) <- c("Initial", "Levels", "Final")
structure_dataset$Levels <- as.numeric(as.character(structure_dataset$Levels))

##################################
#     INTRODUCTORY ANALYSIS      #
##################################

# Introductory summaries
summary_number <- nrow(dataset)  # Mushroom count
summary_dataset <- summary(dataset) # Basic summary of all categories

# Distribution plots for numeric/integer parameters
dataset_names <- row.names(structure_dataset)


#######################################################################
#     TRAINING, VALIDATION, EVALUATION SETS CREATION AND PLOTTING     #
#######################################################################

# Create training/validation (90%) and evaluation (10%) sets
set.seed(1)
test_index <- createDataPartition(y = dataset$cap.diameter, times = 1, p = 0.1, list = FALSE)
trainvalid_set <- dataset[-test_index,]
evaluation_set <- dataset[test_index,]

# Create training (90%) and validation (10%) sets
set.seed(1)
test_index <- createDataPartition(y = trainvalid_set$cap.diameter, times = 1, p = 0.1, list = FALSE)
training_set <- trainvalid_set[-test_index,]
validation_set <- trainvalid_set[test_index,]

# Plot all monovariate distributions of the training+validation set
l <- nrow(structure_dataset)
for (n in 1:l){
   plot_title <- paste("Mushroom", dataset_names[n], "distribution")
   plot <- trainvalid_set %>%
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

########################################################
#     SIMPLE CLASSIFICATION MODEL : CRITERIA LISTS     #
########################################################

# Create criteria lists (factor + type + values) for simple classification
factors_list <- training_set %>% 
   select(where(is.factor) | where(is.logical)) %>% 
   gather(factor, level) %>% 
   unique() %>% 
   select(factor, level) %>% 
   filter(factor != "class")  # Get all factor|logical levels

factors_type <- training_set %>% 
   summary.default %>% 
   as.data.frame %>% 
   group_by(Var1) %>% 
   spread(Var2, Freq) %>% 
   as.data.frame   # Get training_set structure

factors_type$Class <- if_else(factors_type$Class == "-none-", factors_type$Mode, factors_type$Class)     # Create coherent Class column

factors_type <- factors_type %>% 
   select(-Mode, -Length)              # Clean factors_type

colnames(factors_type) <- c("factor", "type")

numeric_list <- factors_type %>% 
   filter(type == "numeric") %>% select(factor) %>%         # Select numeric values, get only factor name
   slice(rep(1:n(), each = 2)) %>%                          # Duplicate each numeric factor
   mutate(level = rep(c("min", "max"), times = n()/2))      # Create min and max values for each numeric factor

factors_list <- rbind(factors_list, numeric_list)           # Merge factor|logical and numeric lists
factors_list <- left_join(factors_list, factors_type)       # Add relevant type to each factor

# Build factors list for 1 variable analysis
factors_list1 <- factors_list
factors_list1$all_edible <- FALSE                  # Set as poisonous by default

# Build factors list for 2 variables analysis
m <- nrow(factors_list)
index_list2 <- t(combn(m, 2))                            # Create all combinations of 2 numbers from 1 to m (indices)
half1_list2 <- factors_list[index_list2[,1],]            # Get 1st attribute, according to 1st index
colnames(half1_list2) <- c("factor1", "level1", "type1")
half2_list2 <- factors_list[index_list2[,2],]            # Get 2nd attribute, according to 2nd index
colnames(half2_list2) <- c("factor2", "level2", "type2")
factors_list2 <- cbind(half1_list2, half2_list2)
factors_list2 <- factors_list2 %>% filter(factor1 != factor2)     # Remove duplicates
factors_list2$all_edible <- FALSE                        # Set as poisonous by default

########################################################
#     SIMPLE CLASSIFICATION MODEL : MODEL BUILDING     #
########################################################

# Check factors_list2 structure : in theory, by construction, there should be NO text factor2 + numeric factor1
factors_check <- factors_list2 %>% 
   filter(type2 %in% c("logical", "factor", "character"), type1 %in% c("integer", "numeric")) %>% 
   nrow

# Define function : min/max and +/- margin selection
minmaxing <- function(fcn_input_level, fcn_margins){
   margin <- as.numeric(as.character(recode_factor(fcn_input_level, min = -fcn_margins, max = fcn_margins))) # Set +/- margin according to max or min
   c(match.fun(fcn_input_level), margin)               # Set vector with min/max (function) in [[1]] and +/- margin (numeric) in [[2]]
}

# Define function : rounding + margin computation + add ">" or "<" before the value.
infsup <- function(fcn_list_name, fcn_input_value, fcn_min_max, fcn_level_number, fcn_n_iter){
   level_value <- paste0(fcn_list_name, "$level", fcn_level_number, "[", fcn_n_iter, "]")  # Select factors_list, factors_list1 or factors_list2 .$levels
   fcn_input_value %>% 
      round(., digits = 1) %>% 
      + fcn_min_max[[2]] %>% 
      max(., 0) %>% # Round, add or remove margin value, set to zero if negative
      paste0(eval(parse(text = level_value)), .) %>% # Paste "min" or "max" before rounded value
      str_replace_all(., "min", "< ") %>%       # Replace "min" by "< "
      str_replace_all(., "max", "> ")           # Replace "max" by "> "
}

# Define function : find all "edible-only" criteria
single_crit_search <- function(fcn_training, fcn_crit_list, fcn_margin){
   l <- nrow(fcn_crit_list)
   crit_list_name <- deparse(substitute(fcn_crit_list))
   for (n in 1:l){
      if(fcn_crit_list$type[n] %in% c("logical", "factor", "character"))
      {
         fcn_crit_list$all_edible[n] <- fcn_training %>% 
            filter(class == "poisonous", get(fcn_crit_list$factor[n]) == fcn_crit_list$level[n]) %>% 
            nrow() == 0  # Find if (for this factor/level combination) there are no poisonous, i.e. ONLY edible species
      }
      else          # Type = integer or numeric
      {
         minmax <- minmaxing(fcn_crit_list$level[n], fcn_margin)     # Setting min/max and rounding values for ".$level"
         current_val <- fcn_training %>% 
            filter(class == "poisonous") %>% 
            select(fcn_crit_list$factor[n]) %>% 
            minmax[[1]](.)
         extremum <- fcn_training %>%  
            select(fcn_crit_list$factor[n]) %>% 
            minmax[[1]](.)
         fcn_crit_list$all_edible[n] <- current_val != extremum
         fcn_crit_list$level[n] <- infsup(crit_list_name, current_val, minmax, "", n)
      }
   }
   fcn_crit_list
}

# Define function : find all "edible-only" double-criteria
dual_crit_search <- function(fcn_training, fcn_crit_list, fcn_margin){
   l <- nrow(fcn_crit_list)
   crit_list_name <- deparse(substitute(fcn_crit_list))
   for (n in 1:l){
      if(fcn_crit_list$type1[n] %in% c("logical", "factor", "character") & fcn_crit_list$type2[n] %in% c("logical", "factor", "character")) # factor1 & factor 2 are text
      {
         count <- fcn_training %>%
            filter(get(fcn_crit_list$factor1[n]) == fcn_crit_list$level1[n], get(fcn_crit_list$factor2[n]) == fcn_crit_list$level2[n]) %>%
            nrow
         count_poison <- fcn_training %>%
            filter(class == "poisonous", get(fcn_crit_list$factor1[n]) == fcn_crit_list$level1[n], 
                   get(fcn_crit_list$factor2[n]) == fcn_crit_list$level2[n]) %>%
            nrow
         fcn_crit_list$all_edible[n] <- count != 0 & count_poison == 0 # Find if (for this factor/level combination) there are mushrooms AND no poisonous, i.e. ONLY edible species
      }
      else          # factor1 is text & factor2 is number
      {if(fcn_crit_list$type1[n] %in% c("logical", "factor", "character") & fcn_crit_list$type2[n] %in% c("numeric", "integer"))
      {
         minmax <- minmaxing(fcn_crit_list$level2[n], fcn_margin)
         current_val <- fcn_training %>% 
            filter(class == "poisonous", get(fcn_crit_list$factor1[n]) == fcn_crit_list$level1[n]) %>% 
            select(fcn_crit_list$factor2[n]) %>% 
            minmax[[1]](.)
         extremum <- fcn_training %>% filter(get(fcn_crit_list$factor1[n]) == fcn_crit_list$level1[n]) %>% 
            select(fcn_crit_list$factor2[n]) %>% 
            minmax[[1]](.)
         fcn_crit_list$all_edible[n] <- current_val != extremum
         fcn_crit_list$level2[n] <- infsup(crit_list_name, current_val, minmax, 2, n)
      }
         else     # factor1 & factor2 are numbers
         {
            minmax1 <- minmaxing(fcn_crit_list$level1[n], fcn_margin)
            minmax2 <- minmaxing(fcn_crit_list$level2[n], fcn_margin)
            current_val1 <- fcn_training %>% 
               filter(class == "poisonous") %>% 
               select(fcn_crit_list$factor1[n]) %>% 
               minmax1[[1]](.)
            extremum1 <- fcn_training %>% 
               select(fcn_crit_list$factor1[n]) %>% 
               minmax1[[1]](.)
            current_val2 <- fcn_training %>% 
               filter(class == "poisonous") %>% 
               select(fcn_crit_list$factor2[n]) %>% 
               minmax2[[1]](.)
            extremum2 <- fcn_training %>% 
               select(fcn_crit_list$factor2[n]) %>% 
               minmax2[[1]](.)
            fcn_crit_list$all_edible[n] <- current_val1 != extremum1 & current_val2 != extremum2
            fcn_crit_list$level1[n] <- infsup(crit_list_name, current_val1, minmax1, 1, n)  # Get "fcn_crit_list" as string in the infsup function
            fcn_crit_list$level2[n] <- infsup(crit_list_name, current_val2, minmax2, 2, n)
         }
      }
   }
   fcn_crit_list
}

# Define function : concatenate all single-criterion factor/levels as one string, before removing them from the dual-criteria list
single_remove <- function(fcn_singlecritlist, fcn_dualcritlist){
   dual_critlist_name <- deparse(substitute(fcn_dualcritlist))
   factors_to_remove <- fcn_singlecritlist %>% 
      filter(all_edible == TRUE, type %in% c("factor", "logical", "character")) %>% 
      select(factor, level)
   one_crit1 <- paste0("(", dual_critlist_name, "$factor1 == '", factors_to_remove$factor, "' & ", 
                       dual_critlist_name, "$level1 == '", factors_to_remove$level, "')", collapse = " | ")
   one_crit2 <- paste0("(", dual_critlist_name, "$factor2 == '", factors_to_remove$factor, "' & ", 
                       dual_critlist_name, "$level2 == '", factors_to_remove$level, "')", collapse = " | ")
   single_crit_removal <- paste(one_crit1, one_crit2, sep = " | ")            # Concatenate factor1 and factor2 lists
   single_crit_index <- which(eval(parse(text = single_crit_removal)))        # Get all indices
   
   fcn_dualcritlist[-single_crit_index,]
}

# Define functions : convert all text factors into criteria strings for the prediction step
crit2string1 <- function(fcn_singlecritlist){
   str_factors1 <- fcn_singlecritlist %>% 
      filter(all_edible == TRUE, type %in% c("factor", "logical", "character")) %>% 
      mutate(level = str_c("== '", level, "'"))
   single_criteria <- fcn_singlecritlist %>% 
      filter (all_edible == TRUE, type %in% c("numeric", "integer")) %>% 
      rbind(str_factors1, .)
   paste(single_criteria$factor, single_criteria$level, collapse = " | ")
}

crit2string2 <- function(fcn_singlecritlist, fcn_dualcritlist){
   mono_criteria_list <- crit2string1(fcn_singlecritlist)
   str_factors2f <- fcn_dualcritlist %>% 
      filter(all_edible == TRUE, type1 %in% c("factor", "logical", "character")) %>% 
      mutate(level1 = str_c("== '", level1, "'"))
   str_factors2ff <- str_factors2f %>% 
      filter(type2 %in% c("factor", "logical", "character")) %>% 
      mutate(level2 = str_c("== '", level2, "'"))
   str_factors2f <- str_factors2f %>% 
      filter(type2 %in% c("numeric", "integer"))  %>% 
      rbind(str_factors2ff, .)
   double_criteria <- fcn_dualcritlist %>% 
      filter(all_edible == TRUE, type1 %in% c("numeric", "integer")) %>% 
      rbind(str_factors2f, .)
   double_criteria_list <- paste("(", double_criteria$factor1, double_criteria$level1, "&", double_criteria$factor2, double_criteria$level2, ")",collapse = " | ")
   bi_criteria_list <- paste(mono_criteria_list, "|", double_criteria_list)
   
   c(mono_criteria_list, bi_criteria_list)
}

# Run criteria analyses for single and dual-criteria models
margin1 <- 2
factors_list1a <- single_crit_search(training_set, factors_list1, margin1)
factors_list2a <- single_remove(factors_list1a, factors_list2)
margin2 <- 3
factors_list2b <- dual_crit_search(training_set, factors_list2a, margin2)

# Show relevant (i.e. edible-only) factors, data types and levels (criterion)
relevant_factors1 <- factors_list1a %>% filter(all_edible == TRUE) %>% select(factor, level, type)
relevant_factors2 <- factors_list2b %>% filter(all_edible == TRUE) %>% select(factor1, level1, type1, factor2, level2, type2)

criteria_list_prediction <- crit2string2(factors_list1a, factors_list2b)

# Create a prediction dataset, with boolean factors (meaning "is.edible") as .$reference
predictions <- validation_set
predictions$reference <- as.logical(as.character(recode_factor(predictions$class, edible = TRUE, poisonous = FALSE))) # Switch to logical values

# Apply the three predictive models : stupid , single-criterion, double-criteria
predictions$stupid_predict = FALSE   # Consider all mushrooms as poisonous
predictions <- predictions %>% mutate(mono_predict = eval(parse(text = criteria_list_prediction[1])))
predictions <- predictions %>% mutate(bi_predict = eval(parse(text = criteria_list_prediction[2])))

# Convert .$reference from logical to factor (confusionMatrix works with factors)
predictions$reference <- as.factor(predictions$reference)
predictions$stupid_predict <- factor(predictions$stupid_predict, levels = c("FALSE","TRUE")) # Create level TRUE (not present in stupid model) for confusionMatrix use (reference & prediction must have the same levels)
predictions$mono_predict <- as.factor(predictions$mono_predict)
predictions$bi_predict <- as.factor(predictions$bi_predict)

# Confusion matrices
CM_stupid <- confusionMatrix(data = predictions$stupid_predict, reference = predictions$reference, positive = "TRUE")
CM_monocrit <- confusionMatrix(data = predictions$mono_predict, reference = predictions$reference, positive = "TRUE")
CM_bicrit <- confusionMatrix(data = predictions$bi_predict, reference = predictions$reference, positive = "TRUE")

# Define functions : get sensitivity/specificity according to margin, for single-crit tuning
tuning1a <- function(fcn_trainset, fcn_factorlist, fcn_margin){
   factlistname <- deparse(substitute(fcn_factorlist))            # Get factor list name as string
   SCS <- paste0("single_crit_search(fcn_trainset, ", factlistname, ", fcn_margin)")      # Create single-search string
   fact_list <- eval(parse(text = SCS))            # Evaluate single-search string (or nested functions will not detect accurately the original factor list name)
   critlist_prediction <- crit2string1(fact_list)
   predictions$tuning <- FALSE
   predictions <- predictions %>% mutate(tuning = eval(parse(text = critlist_prediction[[1]])))
   predictions$tuning <- as.factor(predictions$tuning)
   CM <- confusionMatrix(data = predictions$tuning, reference = predictions$reference, positive = "TRUE")
   sensitivity <- round(CM$byClass["Sensitivity"], 4)
   specificity <- round(CM$byClass["Specificity"], 4)
   F1 <- round(CM$byClass["F1"], 4)
   names(fcn_margin) <- "Margin"
   c(fcn_margin, sensitivity, specificity, F1)
}

tuning2a <- function(fcn_trainset, fcn_factorlist1, fcn_factorlist2, fcn_margin2){
   factlistname2 <- deparse(substitute(fcn_factorlist2))            # Get factor list name as string
   DCS <- paste0("dual_crit_search(fcn_trainset, ", factlistname2, ", fcn_margin2)")      # Create dual-search string
   fact2_list <- eval(parse(text = DCS))            # Evaluate dual-search string (or nested functions will not detect accurately the original factor list name)
   critlist_prediction <- crit2string2(fcn_factorlist1, fact2_list)
   predictions$tuning <- FALSE
   predictions <- predictions %>% mutate(tuning = eval(parse(text = critlist_prediction[[2]])))
   predictions$tuning <- as.factor(predictions$tuning)
   CM <- confusionMatrix(data = predictions$tuning, reference = predictions$reference, positive = "TRUE")
   sensitivity <- round(CM$byClass["Sensitivity"], 4)
   specificity <- round(CM$byClass["Specificity"], 4)
   F1 <- round(CM$byClass["F1"], 4)
   names(fcn_margin2) <- "Margin"
   c(fcn_margin2, sensitivity, specificity, F1)
}

tuning1b <- function(fcn_margin){
   tuning1a(training_set, factors_list1, fcn_margin)
}

tuning2b <- function(fcn_margin){
   tuning2a(training_set, factors_list1a, factors_list2a, fcn_margin)
}

# Set margin list and run single list classifier tuning
margin_1 <- c(seq(from = 0, to = 0.4, by = 0.1), seq(from = 0.5, to = 4, by = 0.5))
single_crit_tune <- t(sapply(margin_1, FUN = tuning1b))
single_crit_tune <- as.data.frame(single_crit_tune)

# Get best margin (highest specificity, then highest sensitivity, then safest margin)
best_margin1 <- single_crit_tune %>% 
   filter(Specificity == max(Specificity)) %>% 
   filter(Sensitivity == max(Sensitivity)) %>% 
   filter(Margin == max(Margin))

# Run single list  classifier with optimum margin, set crit list for factor 2
factors_list1a <- single_crit_search(training_set, factors_list1, as.numeric(best_margin1["Margin"]))
factors_list2a <- single_remove(factors_list1a, factors_list2)

# Run double list classifier tuning
margin_2 <- seq(from = 0, to = 2, by = 0.2)
dual_crit_tune <- t(sapply(margin_2, FUN = tuning2b))
dual_crit_tune <- as.data.frame(dual_crit_tune)

# Get best margin (highest specificity, then highest sensitivity, then safest margin)
best_margin2 <- dual_crit_tune %>% 
   filter(Specificity == max(Specificity)) %>% 
   filter(Sensitivity == max(Sensitivity)) %>% 
   filter(Margin == max(Margin))

# Define function : Sensitivity, Specificity, F1-Score plot
SenSpeF1plot <- function(fcn_tuneresult, fcn_n){
   fcn_tuneresult %>%
      ggplot(aes_string(x = "Margin", y = names(fcn_tuneresult)[fcn_n])) + #aes_string allows use of string instead of variable name
      geom_point() +
      theme_bw()
}

# Plot single_crit tuning parameters
l <- ncol(single_crit_tune)
for (n in 2:l){         # Don't plot first col : it is the x axis
   plot <- SenSpeF1plot(single_crit_tune, n)
   plotname <- paste0("SCtune", names(single_crit_tune)[n])   # Concatenate "SCtune" with the column name
   assign(plotname, plot)     # Assign the plot to the SCtune_colname name
}

# Plot dual_crit tuning parameters
l <- ncol(dual_crit_tune)
for (n in 2:l){         # Don't plot first col : it is the x axis
   plot <- SenSpeF1plot(dual_crit_tune, n)
   plotname <- paste0("DCtune", names(dual_crit_tune)[n])   # Concatenate "DCtune" with the column name
   assign(plotname, plot)     # Assign the plot to the DCtune_colname name
}

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
      theme_bw()
   if(structure_dataset$Final[n] %in% c("integer", "numeric"))  # Histogram for integer/numeric, Barplot for character/factors/logical
   {plot <- plot + geom_histogram()}
   else
   {plot <- plot + geom_bar()}
   plotname <- paste0("train_distrib_",dataset_names[n])   # Concatenate "train_distrib" with the column name
   assign(plotname, plot)     # Assign the plot to the train_distrib_colname name
}

# Correlation graphs for a small selection of criterias
pair_plots <- ggpairs(
   training_set,
   columns = c(2,15,17,10),
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

# Get all object sizes and clean environment (objects not used in the following steps or in the report)
#save.image(file = "EKR-mushrooms-dump1.RData")        # Save everything before environment cleaning
object_list <- objects() 
sizes_list1 <- sapply(X = object_list, FUN = obj_size)
rm(URL, datafile)
rm(dataset)
rm(study_distrib_cap.color, study_distrib_cap.shape, study_distrib_cap.surface, study_distrib_class, study_distrib_does.bruise.or.bleed, 
   study_distrib_gill.attachment, study_distrib_gill.color, study_distrib_gill.spacing, study_distrib_habitat, study_distrib_has.ring,
   study_distrib_ring.type, study_distrib_season, study_distrib_spore.print.color, study_distrib_stem.color, study_distrib_stem.root, 
   study_distrib_stem.surface, study_distrib_veil.color, study_distrib_veil.type)
rm(train_distrib_cap.color, train_distrib_cap.shape, train_distrib_cap.surface, train_distrib_does.bruise.or.bleed, 
   train_distrib_gill.attachment, train_distrib_gill.spacing, train_distrib_has.ring, train_distrib_ring.type, train_distrib_season, 
   train_distrib_stem.color, train_distrib_stem.root, train_distrib_stem.surface, train_distrib_veil.type)
rm(plot, test_index, half1_list2, half2_list2)
gc()

###################################################
#     TRAINING SET ANALYSIS WITH CARET MODELS     #
###################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

# Define function : run model with given parameters, evaluate the performance (Specificity), return fitting results
fit_test <- function(fcn_model){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, method = "cv", number = 10)   # Set performance evaluation parameters to twoClassSummary (ROC, Sens, Spec), with 10-fold cross-validation
   cmd <- paste0("train(class ~ ., method = '",      # Build command, set performance metric to Specificity
                        fcn_model[1], 
                        "', data = trainvalid_set, trControl = tr_ctrl, metric = 'Spec', ", 
                        fcn_model[2],")")
   fitting <- eval(parse(text = cmd))        # Run command
   fitting
}

# Discriminant Analysis Models
set_lda2_dim <- c("lda2", "tuneGrid  = data.frame(dimen = seq(from = 1, to = 16, by = 3))")
set_pda_lambda <-  c("pda", "tuneGrid  = data.frame(lambda = seq(from = 1, to = 51, by = 10))")
fit_lda2_dim <- fit_test(set_lda2_dim)
fit_pda_lambda <- fit_test(set_pda_lambda)
# Extract results of interest
fit_lda2_dim_plot <- ggplot(fit_lda2_dim)
fit_lda2_dim_results <- fit_lda2_dim$results
fit_pda_lambda_plot <- ggplot(fit_pda_lambda)
fit_pda_lambda_results <- fit_pda_lambda$results
# Compute variable importances
da_varimp <- cbind(varImp(fit_lda2_dim)$importance["edible"], 
                   varImp(fit_pda_lambda)$importance["edible"])
names(da_varimp) <- c("lda2", "pda")

# Generalized Additive Model
set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
set_gamLoess_degree <-  c("gamLoess", "tuneGrid  = data.frame(degree = c(0, 1), span = 0.5)")
fit_gamLoess_span <- fit_test(set_gamLoess_span)
fit_gamLoess_degree <- fit_test(set_gamLoess_degree)
# Extract results of interest
fit_gamLoess_span_plot <- ggplot(fit_gamLoess_span)
fit_gamLoess_span_results <- fit_gamLoess_span$results
fit_gamLoess_degree_plot <- ggplot(fit_gamLoess_degree)
fit_gamLoess_degree_results <- fit_gamLoess_degree$results

# Tree-based Models
set_rpart_cp <- c("rpart", "tuneGrid  = data.frame(cp = c(1e-5, 1e-4, 1e-3, 1e-2, 5e-2))")
set_rpartcost_complexity <- c("rpartCost", "tuneGrid  = data.frame(cp = c(1e-5, 1e-4, 1e-3, 1e-2, 0.05), Cost = 1)")
set_rpartcost_cost <- c("rpartCost", "tuneGrid  = data.frame(Cost = c(0.01, 0.4, 0.7, 1, 1.5, 2, 2.5), cp = .01)")
set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
set_c50tree <- c("C5.0Tree", "")
fit_rpart_cp <- fit_test(set_rpart_cp)
fit_rpartcost_complexity <- fit_test(set_rpartcost_complexity)
fit_rpartcost_cost <- fit_test(set_rpartcost_cost)
fit_ctree_criterion <- fit_test(set_ctree_criterion)
fit_c50tree <- fit_test(set_c50tree)
# Extract results of interest
fit_rpart_cp_results <- fit_rpart_cp$results
fit_rpartcost_complexity_plot <- ggplot(fit_rpartcost_complexity)
fit_rpartcost_complexity_results <- fit_rpartcost_complexity$results
fit_rpartcost_complexity_bestTune <- fit_rpartcost_complexity$bestTune
fit_rpartcost_cost_plot <- ggplot(fit_rpartcost_cost)
fit_rpartcost_cost_results <- fit_rpartcost_cost$results
fit_rpartcost_cost_bestTune <- fit_rpartcost_cost$bestTune
fit_ctree_criterion_plot <- ggplot(fit_ctree_criterion)
fit_ctree_criterion_results <- fit_ctree_criterion$results
fit_c50tree_results <- fit_c50tree$results
# Run best CART model
set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = data.frame(cp = ",
                                            fit_rpartcost_complexity$bestTune$cp, 
                                            ", Cost = ", fit_rpartcost_cost$bestTune$Cost, ")" ))
fit_rpartcost_best <- fit_test(set_rpartcost_best)
fit_rpartcost_best_results <- fit_rpartcost_best$results
# Compute variable importances
tree_varimp <- cbind(varImp(fit_rpartcost_best)$importance["edible"],
                     varImp(fit_ctree_criterion)$importance["edible"]
)
names(tree_varimp) <- c("CART", "ctree")
# Get object list sizes and clean environment
#save.image(file = "EKR-mushrooms-dump2.RData")        # Save everything before environment cleaning
object_list <- objects() 
sizes_list2 <- sapply(X = object_list, FUN = obj_size)
rm(fit_lda2_dim, fit_pda_lambda)
rm(fit_gamLoess_span, fit_gamLoess_degree)
rm(fit_rpartcost_complexity, fit_rpartcost_cost, fit_rpart_cp, fit_rpart_example, fit_ctree_criterion, fit_c50tree, fit_rpartcost_best)
gc()

# Random Forest Models
set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")
set_ranger_mtry <- c("ranger", "tuneGrid  = data.frame(mtry = seq(from = 1, to = 106, by = 15), splitrule = 'extratrees', min.node.size = 2), num.trees = 6")
set_ranger_splitrule <- c("ranger", "tuneGrid  = data.frame(splitrule = c('gini', 'extratrees'), mtry = 50, min.node.size = 2), num.trees = 6")
set_ranger_nodesize <- c("ranger", "tuneGrid  = data.frame(min.node.size = seq(from = 1, to = 15, by = 2), mtry = 50, splitrule = 'extratrees'), num.trees = 6")
set_Rborist_pred <- c("Rborist", "tuneGrid  = data.frame(predFixed = seq(from = 1, to = 41, by = 5), minNode = 2), ntrees = 3")
set_Rborist_minNode <- c("Rborist", "tuneGrid  = data.frame(minNode = 1:5, predFixed =50), ntrees = 3")
fit_rFerns_depth <- fit_test(set_rFerns_depth)
fit_ranger_mtry <- fit_test(set_ranger_mtry)
fit_ranger_splitrule <- fit_test(set_ranger_splitrule)
fit_ranger_nodesize <- fit_test(set_ranger_nodesize)
fit_Rborist_pred <- fit_test(set_Rborist_pred)
fit_Rborist_minNode <- fit_test(set_Rborist_minNode)

# Extract results of interest
fit_rFerns_depth_plot <- ggplot(fit_rFerns_depth)
fit_rFerns_depth_results <- fit_rFerns_depth$results
fit_ranger_mtry_plot <- ggplot(fit_ranger_mtry)
fit_ranger_mtry_results <- fit_ranger_mtry$results
fit_ranger_mtry_bestTune <- fit_ranger_mtry$bestTune
fit_ranger_splitrule_plot <- ggplot(fit_ranger_splitrule)
fit_ranger_splitrule_results <- fit_ranger_splitrule$results
fit_ranger_splitrule_bestTune <- fit_ranger_splitrule$bestTune
fit_ranger_nodesize_plot <- ggplot(fit_ranger_nodesize)
fit_ranger_nodesize_results <- fit_ranger_nodesize$results
fit_ranger_nodesize_bestTune <- fit_ranger_nodesize$bestTune
fit_Rborist_pred_plot <- ggplot(fit_Rborist_pred)
fit_Rborist_pred_results <- fit_Rborist_pred$results
fit_Rborist_pred_bestTune <- fit_Rborist_pred$bestTune
fit_Rborist_minNode_plot <- ggplot(fit_Rborist_minNode)
fit_Rborist_minNode_results <- fit_Rborist_minNode$results
fit_Rborist_minNode_bestTune <- fit_Rborist_minNode$bestTune
# Run optimal ranger model
set_ranger_best <- c("ranger", paste0("tuneGrid  = data.frame(min.node.size = ", 
                                            fit_ranger_nodesize_bestTune$min.node.size, 
                                            ", splitrule = '", fit_ranger_splitrule_bestTune$splitrule,
                                            "', mtry = ", fit_ranger_splitrule_bestTune$mtry, ")", 
                                            ", num.trees = 10"))
fit_ranger_best <- fit_test(set_ranger_best)
fit_ranger_best_results <- fit_ranger_best$results
# Run optimal Rborist model
set_Rborist_best <- c("Rborist", paste0("tuneGrid  = data.frame(predFixed = 6, ",    # Value is forced, 6 gives a Spec = 1, and a much better sensitivity
                                        "minNode = ", fit_Rborist_minNode_bestTune$minNode, ")",
                                        ", ntrees = 3"))
fit_Rborist_best <- fit_test(set_Rborist_best)
fit_Rborist_best_results <- fit_Rborist_best$results
# Compute variable importances
rf_varimp <- varImp(fit_rFerns_depth)$importance["edible"]

# Get object list sizes and clean environment (fit_rFerns size is > 1 GB !)
#save.image(file = "EKR-mushrooms-dump3.RData")        # Save everything before environment cleaning
object_list <- objects() 
sizes_list3 <- sapply(X = object_list, FUN = obj_size)
rm(fit_rFerns_depth, fit_ranger_mtry, fit_ranger_splitrule, fit_ranger_nodesize, fit_ranger_best, 
   fit_Rborist_pred, fit_Rborist_minNode, fit_Rborist_best)
gc()

# For complete factor combinations testing (SUPER SLOW) : Compute and Plot
# set_ranger <- c("ranger", "tuneGrid = expand.grid(mtry = seq(from = 1, to = 21, by = 5),
#                                                 splitrule = c('gini', 'extratrees'),
#                                                 min.node.size = seq(from = 1, to = 16, by = 5)
#                 )" )
# fit_ranger <- fit_test(set_ranger)
# plot(fit_ranger, metric = "Spec", plotType = "level", scales = list(x = list(rot = 90)))
# plot(fit_ranger, metric = "Spec")
# ggplot(fit_ranger)

#########################################################
#     MODEL PERFORMANCE AGAINST THE EVALUATION SET      #
#########################################################

# Set single list and dual classifiers with optimum margin hyperparameters
factors_list1aF <- single_crit_search(trainvalid_set, factors_list1, as.numeric(best_margin1["Margin"]))
factors_list2aF <- single_remove(factors_list1aF, factors_list2)
factors_list2bF <- dual_crit_search(trainvalid_set, factors_list2aF, as.numeric(best_margin2["Margin"]))
criteria_list_evaluation <- crit2string2(factors_list1aF, factors_list2bF)

# Set prediction list and run the classifier
evaluation <- evaluation_set
evaluation$reference <- as.logical(as.character(recode_factor(evaluation$class, edible = TRUE, poisonous = FALSE))) # Switch to logical values
evaluation <- evaluation %>% mutate(bi_predict = eval(parse(text = criteria_list_evaluation[2])))

# Set .$reference from logical to factor, then compute confusion matrix
evaluation$reference <- as.factor(evaluation$reference)
evaluation$bi_predict <- as.factor(evaluation$bi_predict)
CM_bifinal <- confusionMatrix(data = evaluation$bi_predict, reference = evaluation$reference, positive = "TRUE")

results_biclass <- c(CM_bifinal$byClass["Sensitivity"], CM_bifinal$byClass["Specificity"], CM_bifinal$byClass["F1"])
results_biclass <- round(results_biclass, 4)

# Bi-classifier performance comparison table : validation vs evaluation
bi_perf_comp <- rbind(best_margin2[2:4], results_biclass)
rownames(bi_perf_comp) <- c("Evaluation", "Validation")

start_time <- Sys.time()     # Start chronometer
cmd <- paste0("train(class ~ ., method = 'ranger', data = trainvalid_set,", set_ranger_best[2], ")") # Build command
fit_ranger_final <- eval(parse(text = cmd))     # Run command
pred_ranger_final <- predict(object = fit_ranger_final, newdata = evaluation_set)
CM_ranger_final <- confusionMatrix(data = pred_ranger_final, reference = evaluation_set$class)
results_ranger <- c(CM_ranger_final$byClass["Sensitivity"], CM_ranger_final$byClass["Specificity"], CM_ranger_final$byClass["F1"])
end_time <- Sys.time()     # Stop chronometer
time_ranger <- difftime(end_time, start_time)
time_ranger <- time_ranger %>% as.numeric %>% round(.,2)


start_time <- Sys.time()            # Start chronometer
cmd <- paste0("train(class ~ ., method = 'Rborist', data = trainvalid_set,", set_Rborist_best[2], ")") # Build command
fit_Rborist_final <- eval(parse(text = cmd))     # Run command
pred_Rborist_final <- predict(object = fit_Rborist_final, newdata = evaluation_set)
CM_Rborist_final <- confusionMatrix(data = pred_Rborist_final, reference = evaluation_set$class)
results_Rborist <- c(CM_Rborist_final$byClass["Sensitivity"], CM_Rborist_final$byClass["Specificity"], CM_Rborist_final$byClass["F1"])
end_time <- Sys.time()              # Stop chronometer
time_Rborist <- difftime(end_time, start_time)
time_Rborist <- time_Rborist %>% as.numeric %>% round(.,2)

result_Rborist <- c(CM_Rborist_final$byClass["Sensitivity"], CM_Rborist_final$byClass["Specificity"], CM_Rborist_final$byClass["F1"], time_Rborist)
result_ranger <- c(CM_ranger_final$byClass["Sensitivity"], CM_ranger_final$byClass["Specificity"], CM_ranger_final$byClass["F1"], time_ranger)
rt_result <- rbind(result_ranger, result_Rborist)
colnames(rt_result) <- c("Sensitivity", "Specificity", "F1 score", "Run time (min)")
rownames(rt_result) <- c("Rborist", "Ranger")

# Get object list sizes and clean environment
#save.image(file = "EKR-mushrooms-dump4.RData")        # Save everything before environment cleaning
object_list <- objects() 
sizes_list4 <- sapply(X = object_list, FUN = obj_size)
rm(fit_ranger_final, fit_Rborist_final)
gc()

#############################
#     MEMORY OCCUPATION     #
#############################

# Get object list sizes
object_list <- objects() 
sizes_list5 <- sapply(X = object_list, FUN = obj_size)
sizes_list <- c(sizes_list1, sizes_list2, sizes_list3, sizes_list4, sizes_list5)  # Get all object list sizes gathered during the study
sizes_list <- sizes_list[!duplicated(names(sizes_list))]             # Remove all duplicates
sizes_list <- t(data.frame(as.list(sizes_list)))
sizes_list <- cbind(rownames(sizes_list), data.frame(sizes_list, row.names=NULL))
sizes_list <- sizes_list[order(-sizes_list$sizes_list),]
names(sizes_list) <- c("object", "size (Mb)")

save.image(file = "EKR-mushrooms.RData")
load("EKR-mushrooms.RData")
