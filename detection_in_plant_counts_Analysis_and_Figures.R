#################################################
#
# Code for a paper on the detection probability of individuals in plant counts
# Data formatting, statistical analysis and plots
#
# detection_in_plant_counts_Analysis_and_Figures.R
#
# jan.perret@gmail.com
#################################################

# clean working space
rm(list = ls())

# load packages
library(tidyverse)

#### LOAD AND FORMAT DATA ####
#### load the measures taken in the field by the participants and the experimenter ####
df_data_counts <- read_csv2(file = "./data/raw/data_all_sessions_new_anonymous.csv", col_names = TRUE, col_types = cols(.default = "c"))

# replace the decimal mark "," by "."
df_data_counts[, c(5:14)] <- lapply(df_data_counts[, c(5:14)], function(x) gsub(",", ".", x))

# set column type from col_character to col_double for quadrat1:quadrat10
df_data_counts <- type_convert(df_data_counts, 
                               col_types = cols(
                                 species = col_character(),
                                 obs_id = col_character(),
                                 obs_letter = col_character(),
                                 metric = col_character(),
                                 quadrat1 = col_double(),
                                 quadrat2 = col_double(),
                                 quadrat3 = col_double(),
                                 quadrat4 = col_double(),
                                 quadrat5 = col_double(),
                                 quadrat6 = col_double(),
                                 quadrat7 = col_double(),
                                 quadrat8 = col_double(),
                                 quadrat9 = col_double(),
                                 quadrat10 = col_double()))


# separate the measures taken by the participants from the measures taken by the experimenter and convert to long format
df_data_participant <- subset(df_data_counts, df_data_counts$obs_id != "experimenter")
tab_pivot <- pivot_longer(data = df_data_participant,
                          cols = quadrat1:quadrat10,
                          names_to = "quadrat_num")
tab_pivot <- pivot_wider(data = tab_pivot,
                         names_from = metric,
                         values_from = value)
df_data_participant <- tab_pivot


df_data_experimenter <- subset(df_data_counts, df_data_counts$obs_id == "experimenter")
tab_pivot <- pivot_longer(data = df_data_experimenter,
                          cols = quadrat1:quadrat10,
                          names_to = "quadrat_num")
tab_pivot <- pivot_wider(data = tab_pivot,
                         names_from = metric,
                         values_from = value)
df_data_experimenter <- tab_pivot


# join column count_TRUE to df_data_participant
df_count_TRUE <- df_data_experimenter[, c("species", "quadrat_num", "count_TRUE")]
df_data_participant <- left_join(df_data_participant, df_count_TRUE, by = c("species", "quadrat_num"))

# compute the proportion of detected individuals for the 3 counting methods
df_data_participant <- cbind(df_data_participant, 
                          prop_30s = df_data_participant$count_30s / df_data_participant$count_TRUE,
                          prop_1m = df_data_participant$count_1m / df_data_participant$count_TRUE,
                          prop_cells = df_data_participant$count_cells / df_data_participant$count_TRUE)

# convert the df to tibble again
df_data_participant <- tibble(df_data_participant)

# add a column with median vegetation height to the df df_data_experimenter 
veg_height_median <- apply(df_data_experimenter[, c(9:17)], 1, median, na.rm = FALSE)
df_data_experimenter <- cbind(df_data_experimenter, veg_height_median)

# drop the columns with the vegetation heights we used to get the median height 
df_data_experimenter <- df_data_experimenter[, -c(9:17)]

# drop the columns obs_id and obs_letter
df_data_experimenter <- df_data_experimenter[, -c(2, 3)]



#### join the species_conspicuousness variable and session date ####
df_species_conspicuousness <- read_csv2(file = "./data/raw/data_species_conspicuousness.csv", col_names = TRUE, col_types = cols(.default = "c"))
df_species_conspicuousness$Date <- as.Date(df_species_conspicuousness$Date, tryFormats = c("%d/%m/%Y"))

# join the column species_conspicuousness to df_data_experimenter
df_data_experimenter <- left_join(df_data_experimenter, df_species_conspicuousness[, c(2, 3)], by = c("species" = "Species_code"))
df_data_experimenter$Species_conspicuousness <- as.numeric(df_data_experimenter$Species_conspicuousness) # convert to numeric

# join the date to df_data_participant
df_data_participant <- left_join(df_species_conspicuousness[, c(2, 4)], df_data_participant, by = c("Species_code" = "species"))
colnames(df_data_participant)[1] <- "species"



#### join quadrat order ####
df_quadrat_order <- read_csv2(file = "./data/raw/data_quadrat_order_anonymous.csv", col_names = TRUE, col_types = cols(.default = "c"))

# check if all participants have the 10 quadrat numbers
vect_digits <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

# for round 1
apply(sapply(vect_digits, grepl, df_quadrat_order$order_round_1), 1, all)

# for round 2
apply(sapply(vect_digits, grepl, df_quadrat_order$order_round_2), 1, all)

# for round 3
apply(sapply(vect_digits, grepl, df_quadrat_order$order_round_3), 1, all)
which(apply(sapply(vect_digits, grepl, df_quadrat_order$order_round_3), 1, all) == FALSE)
# -> it's ok, the 3 errors are for the 3 participants who couldn't finish the 3rd round

# get a dataframe with the time measurements
df_time <- df_data_participant[, c(1, 3, 4, 5)]

quadrat_index <- as.integer(sub("quadrat", "", df_time$quadrat_num))
df_time <- cbind(df_time, quadrat_index)
df_time <- tibble(df_time)


# initialise the dataframe to store the results
df_orders <- data.frame(species = NA, obs_id = NA, obs_letter = NA, quadrat_num = NA, quadrat_index = NA,
                        quadrat_order_30s = NA, quadrat_order_1m = NA, quadrat_order_cells = NA)

vect_order_round_3_SAVE <- NA


for (i in 1:nrow(df_quadrat_order)) {
  
  # get data of the current session-participant
  df_current_participant <- subset(df_time, df_time$species == df_quadrat_order$species[i] & df_time$obs_id == df_quadrat_order$obs_id[i])
  df_current_participant <- df_current_participant[order(df_current_participant$quadrat_index), ]
  
  # join the colums with quadrat order
  df_current_participant <- cbind(df_current_participant, 
                                  quadrat_order_30s = NA,
                                  quadrat_order_1m = NA,
                                  quadrat_order_cells = NA)
  
  # get the quadrat order for the 3 rounds
  my_order_round_1 <- df_quadrat_order$order_round_1[i]
  vect_order_round_1 <- as.integer(strsplit(my_order_round_1, split = "-")[[1]])
  my_order_round_2 <- df_quadrat_order$order_round_2[i]
  vect_order_round_2 <- as.integer(strsplit(my_order_round_2, split = "-")[[1]])
  my_order_round_3 <- df_quadrat_order$order_round_3[i]
  vect_order_round_3 <- as.integer(strsplit(my_order_round_3, split = "-")[[1]])
  
  ### ROUND 1
  # order df_current_participant by the order he made the quadrats
  df_current_participant <- df_current_participant[vect_order_round_1, ]
  
  # fill the order
  df_current_participant$quadrat_order_30s <- c(1:10)
  
  # reorder the dataframe in the incresing quadrat number
  df_current_participant <- df_current_participant[order(df_current_participant$quadrat_index), ]
  
  
  ### ROUND 2
  # order df_current_participant by the order he made the quadrats
  df_current_participant <- df_current_participant[vect_order_round_2, ]
  
  # fill the order
  df_current_participant$quadrat_order_1m <- c(1:10)
  
  # reorder the dataframe in the incresing quadrat number
  df_current_participant <- df_current_participant[order(df_current_participant$quadrat_index), ]
  
  
  ### ROUND 3
  # if round 3 was not made, store the results immediately
  if (all(is.na(vect_order_round_3))) { 
    df_orders <- rbind(df_orders, df_current_participant)
    next
  }
  
  # if the participant has not finished round 3
  if (length(vect_order_round_3) != 10) {
    
    # save original vector
    vect_order_round_3_SAVE <- vect_order_round_3
    
    # add missing quadrats
    vect_missing_quadrats <- c(1:10)[!c(1:10) %in% vect_order_round_3]
    vect_order_round_3 <- c(vect_order_round_3, vect_missing_quadrats)
    
  }
  
  # order df_current_participant by the order he made the quadrats
  df_current_participant <- df_current_participant[vect_order_round_3, ]
  
  # fill the order
  df_current_participant$quadrat_order_cells <- c(1:10)
  
  # remove clock times for the participants who didn't finish round 3
  if (!is.na(vect_order_round_3_SAVE)) {
    
    df_current_participant$quadrat_order_cells[(length(vect_order_round_3_SAVE) + 1):10] <- NA
    
  }
  
  # set vect_order_round_3_SAVE to NA again
  vect_order_round_3_SAVE <- NA
  
  # reorder the dataframe in the incresing quadrat number
  df_current_participant <- df_current_participant[order(df_current_participant$quadrat_index), ]
  
  # store the results
  df_orders <- rbind(df_orders, df_current_participant)
  
}

# remove the top empty row
df_orders <- df_orders[-1, ]

# remove the unnecessary columns to prepare for the join
df_orders <- df_orders[, -c(3, 5)]

# join the 3 quadrat order columns to df_data_participant
df_data_participant <- left_join(df_data_participant, df_orders, by = c("species", "obs_id", "quadrat_num"))

# export the two dataframes prior to data filtering for the Supplementary material
write.csv2(df_data_experimenter, file = "data/tidy/df_data_experimenter_before_filtering.csv", row.names = FALSE)
write.csv2(df_data_participant, file = "data/tidy/df_data_participant_before_filtering.csv", row.names = FALSE)



#### remove density outlier quadrats ####

# save the dataframe before removing the density outliers (for the plots in the supplementary material)
write.csv2(df_data_experimenter, file = "./data/tidy/df_data_experimenter_before_removing_density_outliers.csv", row.names = FALSE)

# remove the quadrats with more than 300 individuals
df_data_participant <- subset(df_data_participant, df_data_participant$count_TRUE < 300)
df_data_experimenter <- subset(df_data_experimenter, df_data_experimenter$count_TRUE < 300)



#### remove observers who made many excess detections ####
palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')

### remove participants who made a lot of over-detections
df_over_detections <- df_data_participant %>% 
  group_by(species, obs_id) %>% 
  summarise(excess_cells = sum(prop_cells > 1, na.rm = TRUE),
            excess_1m = sum(prop_1m > 1, na.rm = TRUE),
            excess_30s = sum(prop_30s > 1, na.rm = TRUE))
df_over_detections <- subset(df_over_detections, !(df_over_detections$excess_cells == 0 & df_over_detections$excess_1m == 0 & df_over_detections$excess_30s == 0))
df_over_detections %>% print(n = Inf)

# session-observers combinations to remove :
# ALLsp   obs_jaho
# ALLsp   obs_bqiy
# EUPHEL  obs_lbev
df_data_participant <- subset(df_data_participant, !(df_data_participant$species == "ALLsp" & df_data_participant$obs_id == "obs_jaho"))
df_data_participant <- subset(df_data_participant, !(df_data_participant$species == "ALLsp" & df_data_participant$obs_id == "obs_bqiy"))
df_data_participant <- subset(df_data_participant, !(df_data_participant$species == "EUPHEL" & df_data_participant$obs_id == "obs_lbev"))



#### create the composite variable for habitat closure ####
# make a summary plot per species
df_summary_per_species <- df_data_experimenter %>%
  group_by(species) %>%
  summarize(mean_height = mean(veg_height_median, na.rm = TRUE),
            mean_vegetation_cover = mean(vegetation_cover, na.rm = TRUE))

# keep only the quadrats containing individuals
data_experimenter_for_pca <- subset(df_data_experimenter, df_data_experimenter$count_TRUE > 0)

# make the pca
df_pca <- data.frame(vegetation_cover = data_experimenter_for_pca$vegetation_cover, veg_height_median = data_experimenter_for_pca$veg_height_median)
mypca <- ade4::dudi.pca(df_pca, scannf = FALSE, nf = 2)

# get the coordinates of the observations on the 1st principal component
habitat_closure <- -mypca$li[, 1]

# join the new composite variable to the dataframe
data_experimenter_for_pca <- cbind(data_experimenter_for_pca, habitat_closure = habitat_closure)

# plot the result
ggplot(data = data_experimenter_for_pca, aes(x = veg_height_median, y = vegetation_cover, color = habitat_closure)) + 
  geom_point(size = 2) +
  scale_color_gradient(low = "blue", high = "red")

# join the composite variable to df_data_experimenter
df_data_experimenter <- left_join(df_data_experimenter, data_experimenter_for_pca[, c(1, 2, 9)], by = c("species", "quadrat_num"))


### join the measures taken by the experimenter to df_data_participant
df_data_participant <- subset(df_data_participant, select = - c(count_TRUE)) # drop the column count_TRUE from df_data_participant
df_data_participant <- left_join(df_data_participant, df_data_experimenter, by = c("species", "quadrat_num"))

### export df_data_experimenter for the supplementary material
write.csv2(df_data_experimenter, file = "./data/tidy/df_data_experimenter.csv", row.names = FALSE)


#### join the experience in botany of the participants ####
df_data_exp_bota <- read_csv2("./data/raw/data_observer_experience_botany_anonymous.csv", col_names = TRUE, col_types = cols(.default = "c"))

# reclassify the experience levels into 5 categories instead of 10
df_data_exp_bota$exp_bota <- as.numeric(df_data_exp_bota$exp_bota)
df_data_exp_bota$exp_bota <- ceiling(df_data_exp_bota$exp_bota/2)

### join the experience in botany to df_data_participant
df_data_participant <- left_join(df_data_participant, df_data_exp_bota)



#### Make a sub-dataframe for every counting method ####
# remove empty quadrats
df_data_without_empty_quadrats <- subset(df_data_participant, df_data_participant$count_TRUE > 0)

# remove the observations with more than 100% detection
data_30s <- subset(df_data_without_empty_quadrats, df_data_without_empty_quadrats$prop_30s <= 1 & !is.nan(df_data_without_empty_quadrats$prop_30s))
data_1m <- subset(df_data_without_empty_quadrats, df_data_without_empty_quadrats$prop_1m <= 1 & !is.nan(df_data_without_empty_quadrats$prop_1m))
data_cells <- subset(df_data_without_empty_quadrats, df_data_without_empty_quadrats$prop_cells <= 1 & !is.nan(df_data_without_empty_quadrats$prop_cells))

# dim(df_data_participant)
# dim(df_data_without_empty_quadrats)
# dim(data_30s)
# dim(data_1m)
# dim(data_cells)

### export the dataframes for the supplementary material
write.csv2(data_30s, file = "./data/tidy/df_data_30s.csv", row.names = FALSE)
write.csv2(data_1m, file = "./data/tidy/df_data_1m.csv", row.names = FALSE)
write.csv2(data_cells, file = "./data/tidy/df_data_cells.csv", row.names = FALSE)



#### Make a data frame for modelling the 3 methods together ####

### Make a dataframe for the analysis of the false positives by keeping the empty quadrats

data_30s_0 <- df_data_participant
data_1m_0 <- df_data_participant
data_cells_0 <- df_data_participant

# keep only necessary columns for data_30s_0
data_30s_0_join <- data_30s_0[, c("species", "obs_id", "obs_letter", "quadrat_num", "count_30s", "prop_30s",
                                  "quadrat_order_30s", "count_TRUE", "Species_conspicuousness", "habitat_closure", "exp_bota")]

# rename columns for the future cbind
colnames(data_30s_0_join) <- c("species", "obs_id", "obs_letter", "quadrat_num", "count", "prop_detect",
                               "quadrat_order", "count_TRUE", "species_conspicuousness", "habitat_closure", "exp_bota")

# join a column with the counting time and another with the name of the counting method
counting_time_30s <- rep(30, times = nrow(data_30s_0_join))
counting_method_30s <- rep("count_30s", times = nrow(data_30s_0_join))
data_30s_0_join <- cbind(data_30s_0_join, counting_time = counting_time_30s, counting_method = counting_method_30s)

# reorder the columns
data_30s_0_join <- data_30s_0_join[, c("species", "obs_id", "obs_letter", "counting_method", "quadrat_num", "count", "counting_time", "prop_detect",
                                       "quadrat_order", "count_TRUE", "species_conspicuousness", "habitat_closure", "exp_bota")]

# keep only necessary columns for data_1m_0
data_1m_0_join <- data_1m_0[, c("species", "obs_id", "obs_letter", "quadrat_num", "count_1m", "counting_time_1m", "prop_1m",
                                "quadrat_order_1m", "count_TRUE", "Species_conspicuousness", "habitat_closure", "exp_bota")]
colnames(data_1m_0_join) <- c("species", "obs_id", "obs_letter", "quadrat_num", "count", "counting_time", "prop_detect",
                              "quadrat_order", "count_TRUE", "species_conspicuousness", "habitat_closure", "exp_bota")

# join a column with the name of the counting method
counting_method_1m <- rep("count_1m", times = nrow(data_1m_0_join))
data_1m_0_join <- cbind(data_1m_0_join, counting_method = counting_method_1m)
data_1m_0_join <- data_1m_0_join[, c("species", "obs_id", "obs_letter", "counting_method", "quadrat_num", "count", "counting_time", "prop_detect",
                                     "quadrat_order", "count_TRUE", "species_conspicuousness", "habitat_closure", "exp_bota")]

# keep only necessary columns for data_cells_0
data_cells_0_join <- data_cells_0[, c("species", "obs_id", "obs_letter", "quadrat_num", "count_cells", "counting_time_cells", "prop_cells",
                                      "quadrat_order_cells", "count_TRUE", "Species_conspicuousness", "habitat_closure", "exp_bota")]
colnames(data_cells_0_join) <- c("species", "obs_id", "obs_letter", "quadrat_num", "count", "counting_time", "prop_detect",
                                 "quadrat_order", "count_TRUE", "species_conspicuousness", "habitat_closure", "exp_bota")

# join a column with the name of the counting method
counting_method_cells <- rep("count_cells", times = nrow(data_cells_0_join))
data_cells_0_join <- cbind(data_cells_0_join, counting_method = counting_method_cells)
data_cells_0_join <- data_cells_0_join[, c("species", "obs_id", "obs_letter", "counting_method", "quadrat_num", "count", "counting_time", "prop_detect",
                                           "quadrat_order", "count_TRUE", "species_conspicuousness", "habitat_closure", "exp_bota")]

# join the rows of the 3 dataframes
df_3_methods_0 <- rbind(data_30s_0_join, data_1m_0_join, data_cells_0_join)

# sort the dataframe
df_3_methods_0 <- df_3_methods_0[with(df_3_methods_0, order(species, obs_id, obs_letter, counting_method, quadrat_num)), ]

# # remove the NAs
# df_3_methods_0 <- subset(df_3_methods_0, !is.na(df_3_methods_0$count))

# create a column marking observations with excess detections
excess_detect <- rep(NA, times = nrow(df_3_methods_0))
df_3_methods_0 <- cbind(df_3_methods_0, excess_detect)

for (i in 1:nrow(df_3_methods_0)) {
  
  if (is.na(df_3_methods_0$count[i])) { next }
  
  if (!is.na(df_3_methods_0$prop_detect[i]) & df_3_methods_0$prop_detect[i] > 1) {df_3_methods_0$excess_detect[i] <- 1}
  else if (df_3_methods_0$count_TRUE[i] == 0 & df_3_methods_0$count[i] > 0) {df_3_methods_0$excess_detect[i] <- 1}
  else {df_3_methods_0$excess_detect[i] <- 0}
  
}

# nrow(unique(df_3_methods_0[, c(1,2)])) * 10 * 3 # the difference with nrow(df_3_methods_0) is due to the quadrats with > 300 individuals

### make the sub-dataframes needed for the analysis
# NOTE : in the dataframe df_3_methods_0, there are all observations except the 4 quadrats with more than 300 individuals

# remove the empty quadrats for the binomial regression analysis
df_3_methods_without_empty_quadrats <- subset(df_3_methods_0, df_3_methods_0$count_TRUE != 0)

# make a version of the dataframe by removing the observations with more than 100% detection
df_3_methods_without_excess_detect <- subset(df_3_methods_without_empty_quadrats, df_3_methods_without_empty_quadrats$prop_detect <= 1)

# make another version where these observations are set to 100%
df_3_methods_excess_detect_set_to_1 <- df_3_methods_without_empty_quadrats
df_3_methods_excess_detect_set_to_1$prop_detect[df_3_methods_excess_detect_set_to_1$prop_detect > 1] <- 1

# save the dataframes
write.csv2(df_3_methods_0, file = "./data/tidy/df_3_methods_1_with_excess_detections.csv", row.names = FALSE)
write.csv2(df_3_methods_without_empty_quadrats, file = "./data/tidy/df_3_methods_2_without_empty_quadrats.csv", row.names = FALSE)
write.csv2(df_3_methods_without_excess_detect, file = "./data/tidy/df_3_methods_3_without_excess_detections.csv", row.names = FALSE)
write.csv2(df_3_methods_excess_detect_set_to_1, file = "./data/tidy/df_3_methods_4_excess_detect_set_to_1.csv", row.names = FALSE)


# dim(df_3_methods_0) # 5055   14
# dim(df_3_methods_without_empty_quadrats) # 4485   14
# dim(df_3_methods_without_excess_detect) # 4319   14
# dim(df_3_methods_excess_detect_set_to_1) # 4485   14

# df_test <- subset(df_3_methods_without_empty_quadrats, df_3_methods_without_empty_quadrats$excess_detect == 1)
# dim(df_test) # ---> 140 observations with more than 100% detection are removed from the dataset after removing the empty quadrats.



#### FIT THE MODEL ####

# load packages
library(tidyverse)
library(lme4)
library(car)
library(DHARMa)
library(broom)
library(broom.mixed)
library(ggeffects)
library(AICcmodavg)
library(MuMIn)
library(patchwork)

# load data
df_3_methods <- read_csv2(file = "./data/tidy/df_3_methods_3_without_excess_detections.csv")

# convert counting time from seconds to minutes
df_3_methods$counting_time <- df_3_methods$counting_time/60

# add columns to code manually the interaction counting_time * counting_method without estimating a slope for count_30s
counting_time_1m <- df_3_methods$counting_time
counting_time_1m[which(df_3_methods$counting_method == "count_30s" | df_3_methods$counting_method == "count_cells")] <- 0
counting_time_cells <- df_3_methods$counting_time
counting_time_cells[which(df_3_methods$counting_method == "count_30s" | df_3_methods$counting_method == "count_1m")] <- 0
df_3_methods <- cbind(df_3_methods, counting_time_1m, counting_time_cells)

# set the counting method count_30s as reference level
df_3_methods$counting_method <- factor(df_3_methods$counting_method, levels = c("count_30s", "count_1m", "count_cells"))

# add a column with a unique quadrat identification
df_3_methods <- cbind(df_3_methods, quadrat_id = paste0(df_3_methods$species, "_", df_3_methods$quadrat_num))

# convert to tibble
df_3_methods <- tibble(df_3_methods)


### Fit the model in two versions : one with standardised predictors and one without standardising. It's just 
### for practicality that I fit the model two times. I need the betas from the standardised version for Fig. 2,
### but to avoid needing to un-standardise the estimates I make the predictions for Figs 3 and 4 by using the
### not standardised version of the model. Predictions are identical between the two versions of the model.
mod_final <- glmer(prop_detect ~ (1|species) + (1|species:quadrat_id) + (1|obs_id)
                   + species_conspicuousness * habitat_closure
                   + species_conspicuousness * counting_method
                   + habitat_closure * counting_method
                   + exp_bota * counting_method
                   + count_TRUE * counting_method
                   + quadrat_order * counting_method
                   + counting_time_1m * habitat_closure
                   + counting_time_1m * species_conspicuousness
                   + counting_time_cells * species_conspicuousness
                   + counting_time_cells * habitat_closure,
                   family = binomial, data = df_3_methods, weight = count_TRUE)

mod_final_s <- glmer(prop_detect ~ (1|species) + (1|species:quadrat_id) + (1|obs_id)
                     + scale(species_conspicuousness) * habitat_closure
                     + scale(species_conspicuousness) * counting_method
                     + habitat_closure * counting_method
                     + scale(exp_bota) * counting_method
                     + scale(count_TRUE) * counting_method
                     + scale(quadrat_order) * counting_method
                     + scale(counting_time_1m) * habitat_closure
                     + scale(counting_time_1m) * scale(species_conspicuousness)
                     + scale(counting_time_cells) * scale(species_conspicuousness)
                     + scale(counting_time_cells) * habitat_closure,
                     family = binomial, data = df_3_methods, weight = count_TRUE)


#### MAKE THE FIGURES ####

#### Figure 2 ####

# make the table with the betas
df_mod_final_s_betas <- broom.mixed::tidy(mod_final_s, conf.int = TRUE, effects = c("fixed", "ran_pars"))

# move the columns with the confidence interval to the left
df_mod_final_s_betas <- subset(df_mod_final_s_betas, select = c(effect:std.error, conf.low, conf.high, statistic, p.value))

# change presentation of random effects
df_mod_final_s_betas$effect[c(26, 27, 28)] <- "random"
df_mod_final_s_betas$term[c(26, 27, 28)] <- df_mod_final_s_betas$group[c(26, 27, 28)]
df_mod_final_s_betas <- subset(df_mod_final_s_betas, select = -group)

# clean the variable names
df_mod_final_s_betas$term <- gsub(pattern = "scale(", replacement = "", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = ")", replacement = "", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "(", replacement = "", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = ":", replacement = " x ", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "count_1m", replacement = " [unlimited count]", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "count_cells", replacement = " [cell count]", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "count_TRUE", replacement = "Density", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "exp_bota", replacement = "Botany experience", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "counting_time_1m", replacement = "Counting_time x counting_method [unlimited count]", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "counting_time_cells", replacement = "Counting_time x counting_method [cell count]", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "counting_method", replacement = "Method", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "_", replacement = " ", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "species conspicuousness", replacement = "Species conspicuousness", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "habitat closure", replacement = "Habitat closure", x = df_mod_final_s_betas$term, fixed = TRUE)
df_mod_final_s_betas$term <- gsub(pattern = "quadrat order", replacement = "Quadrat order", x = df_mod_final_s_betas$term, fixed = TRUE)


# make two separate tables for the fixed and the random effects
df_mod_final_s_betas_random_effects <- subset(df_mod_final_s_betas, df_mod_final_s_betas$effect == "random")
df_mod_final_s_betas_random_effects <- subset(df_mod_final_s_betas_random_effects, select = -effect)

df_mod_final_s_betas <- subset(df_mod_final_s_betas, df_mod_final_s_betas$effect == "fixed")
df_mod_final_s_betas <- subset(df_mod_final_s_betas, select = -effect)

# remove empty columns and show both the variance and the SD of the random effects
df_mod_final_s_betas_random_effects <- df_mod_final_s_betas_random_effects[, c(1:3)]
colnames(df_mod_final_s_betas_random_effects) <- c("term", "SD", "variance")
df_mod_final_s_betas_random_effects$variance <- df_mod_final_s_betas_random_effects$SD^2
df_mod_final_s_betas_random_effects$term[1] <- "species:quadrat_id"

# make the table for the plot
df_mod_betas_plot <- cbind(df_mod_final_s_betas, signif = NA)

# fill column signif for the color of points and confidence intervals
for (i in 1:nrow(df_mod_betas_plot)) {
  if (df_mod_betas_plot$estimate[i] > 0) {df_mod_betas_plot$signif[i] <- "positive"}
  if (df_mod_betas_plot$estimate[i] < 0) {df_mod_betas_plot$signif[i] <- "negative"}
  if (df_mod_betas_plot$p.value[i] > 0.05) {df_mod_betas_plot$signif[i] <- "NS"}
}

# make term an ordered factor
df_mod_betas_plot$term <- factor(df_mod_betas_plot$term, levels = rev(df_mod_betas_plot$term))

# make the plot
plot_figure_2 <- ggplot(df_mod_betas_plot, aes(x = estimate, y = term, color = signif)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(size = 2.3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0, size = 0.8) +
  theme_bw() +
  scale_color_manual(values = c("#e31a1c", "#525252", "#3690c0")) +
  theme(legend.position = "none") +
  xlab("Log-Odds") +
  ylab("")
# plot_figure_2

# save the plot
pdf(file = "./output/Figure2_model_betas.pdf", width = 9.5, height = 7)
print(plot_figure_2)
dev.off()
png(file = "./output/Figure2_model_betas.png", width = 9.5, height = 7, units = "in", res = 600)
print(plot_figure_2)
dev.off()



#### Figure 3 ####

mypalette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3')

# count_30s
val <- c(-1.5, 0.5, 2.5)
pred_30s <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("counting_time_1m [0]", "species_conspicuousness [all]", "habitat_closure [val]"), type = "fixed",
                                               condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                             count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5,
                                                             counting_method = "count_30s")))
pred_30s$x <- 0.5
colnames(pred_30s) <- c("counting_time", "prop_detect", "std.error", "conf.low", "conf.high", "species_conspicuousness", "habitat_closure")

# modify counting_time so the points don't overlap
pred_30s <- within(pred_30s, counting_time[species_conspicuousness == 1] <- 0.25)
pred_30s <- within(pred_30s, counting_time[species_conspicuousness == 2] <- 0.375)
pred_30s <- within(pred_30s, counting_time[species_conspicuousness == 3] <- 0.5)
pred_30s <- within(pred_30s, counting_time[species_conspicuousness == 4] <- 0.625)

# make the plot
plot_30s <- ggplot(data = pred_30s, aes(x = counting_time, y = prop_detect, group = species_conspicuousness, color = species_conspicuousness)) +
  facet_grid(as.factor(habitat_closure) ~ .) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group = species_conspicuousness, color = species_conspicuousness), size = 1, width = 0) +
  geom_point(size = 2) +
  ylim(c(0, 1)) +
  xlim(c(0, 0.875)) +
  theme_bw() +
  ggtitle("Quick count") +
  xlab("Completed in 30 seconds") +
  ylab("Individual detection probability") +
  scale_color_manual(values = mypalette) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), 
        strip.background = element_blank(), strip.text.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  # guides(color = guide_legend(title = "Species conspicuousness"), fill = guide_legend(title = "Species conspicuousness"))
  guides(color = "none")

# count_1m
val <- c(-1.5, 0.5, 2.5)
pred_1m <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("counting_time_1m [all]", "species_conspicuousness [all]", "habitat_closure [val]"), type = "fixed",
                                              condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                            count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5,
                                                            counting_method = "count_1m")))
colnames(pred_1m) <- c("counting_time", "prop_detect", "std.error", "conf.low", "conf.high", "species_conspicuousness", "habitat_closure")

# make the plot
plot_1m <- ggplot(data = pred_1m, aes(x = counting_time, y = prop_detect, group = species_conspicuousness, color = species_conspicuousness)) +
  facet_grid(as.factor(habitat_closure) ~ .) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = species_conspicuousness, fill = species_conspicuousness), alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  ylim(c(0, 1)) +
  theme_bw() +
  ggtitle("Unlimited count") +
  xlab("Time spent counting (min)") +
  scale_color_manual(values = mypalette) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        strip.background = element_blank(), strip.text.y = element_blank()) +
  guides(color = guide_legend(title = "Species conspicuousness"), fill = guide_legend(title = "Species conspicuousness"))


# predicted detection rate for the mean values of all variables
df_1m <- subset(df_3_methods, df_3_methods$counting_method == "count_1m")
mean_time_1m <- mean(df_1m$counting_time_1m)
pred_mean_1m <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("counting_time_1m [mean_time_1m]", "species_conspicuousness [2.5]", "habitat_closure [0]"), type = "fixed",
                                                   condition = c(exp_bota = 2.5, count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5,
                                                                 counting_method = "count_1m")))

# count_cells
val <- c(-1.5, 0.5, 2.5)
pred_cells <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("counting_time_cells [all]", "species_conspicuousness [all]", "habitat_closure [val]"), type = "fixed",
                                                 condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                               count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5,
                                                               counting_method = "count_cells")))
colnames(pred_cells) <- c("counting_time", "prop_detect", "std.error", "conf.low", "conf.high", "species_conspicuousness", "habitat_closure")

facet_names <- as_labeller(c(`-1.5` = "Habitat closure = -1.5", `0.5` = "Habitat closure = 0.5",`2.5` = "Habitat closure = 2.5"))

# make the plot
plot_cells <- ggplot(data = pred_cells, aes(x = counting_time, y = prop_detect, group = species_conspicuousness, color = species_conspicuousness)) +
  facet_grid(as.factor(habitat_closure) ~ ., labeller = facet_names) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = species_conspicuousness, fill = species_conspicuousness), alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  ylim(c(0, 1)) +
  theme_bw() +
  ggtitle("Cell count") +
  xlab("Time spent counting (min)") +
  scale_color_manual(values = mypalette) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        strip.text.y = element_text(size = 11)) +
  guides(color = guide_legend(title = "Species conspicuousness"), fill = guide_legend(title = "Species conspicuousness"))

# combine the plots with the package patchwork
plot_figure_3 <- plot_30s + plot_1m + plot_cells + 
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
# plot_figure_3


# save the plot
pdf(file = "./output/Figure3_predict_sp_hab_time.pdf", width = 9, height = 7)
print(plot_figure_3)
dev.off()
png(file = "./output/Figure3_predict_sp_hab_time.png", width = 9, height = 7, units = "in", res = 600)
print(plot_figure_3)
dev.off()



#### Figure 4 ####

### effect of the experience in botany
pred_30s <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("exp_bota [all]", "counting_method [all]"), type = "fixed",
                                               condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                             count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5)))
pred_30s <- subset(pred_30s, pred_30s$group == "count_30s")

pred_1m <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("exp_bota [all]", "counting_method [all]"), type = "fixed",
                                              condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                            count_TRUE = 100, counting_time_1m = 3, counting_time_cells = 0, quadrat_order = 5)))
pred_1m <- subset(pred_1m, pred_1m$group == "count_1m")

pred_cells <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("exp_bota [all]", "counting_method [all]"), type = "fixed",
                                                 condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                               count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 3, quadrat_order = 5)))
pred_cells <- subset(pred_cells, pred_cells$group == "count_cells")

pred <- tibble(rbind(pred_30s, pred_1m, pred_cells))
colnames(pred) <- c("exp_bota", "prop_detect", "std.error", "conf.low", "conf.high", "counting_method")

# change the name of the counting methods
pred$counting_method <- gsub(pattern = "count_30s", replacement = "Quick count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- gsub(pattern = "count_1m", replacement = "Unlimited count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- gsub(pattern = "count_cells", replacement = "Cell count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- factor(pred$counting_method, levels = c("Quick count", "Unlimited count", "Cell count"))

plot_exp_bota <- ggplot(data = pred, aes(x = exp_bota, y = prop_detect, group = counting_method, color = counting_method)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = counting_method, fill = counting_method), alpha = 0.2, colour = NA) +
  ylim(c(0, 1)) +
  theme_bw() +
  xlab("Experience in botany") +
  ylab("Individual detection probability") +
  guides(color = guide_legend(title = "Counting method"), fill = guide_legend(title = "Counting method")) +
  annotate("text", x = 4.75, y = 0.97, label = "A", size = 5, fontface = 2)


### effect of the density of individuals
pred_30s <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("count_TRUE [all]", "counting_method [all]"), type = "fixed",
                                               condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                             count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5)))
pred_30s <- subset(pred_30s, pred_30s$group == "count_30s")

pred_1m <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("count_TRUE [all]", "counting_method [all]"), type = "fixed",
                                              condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                            count_TRUE = 100, counting_time_1m = 3, counting_time_cells = 0, quadrat_order = 5)))
pred_1m <- subset(pred_1m, pred_1m$group == "count_1m")

pred_cells <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("count_TRUE [all]", "counting_method [all]"), type = "fixed",
                                                 condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                               count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 3, quadrat_order = 5)))
pred_cells <- subset(pred_cells, pred_cells$group == "count_cells")

pred <- tibble(rbind(pred_30s, pred_1m, pred_cells))
colnames(pred) <- c("count_TRUE", "prop_detect", "std.error", "conf.low", "conf.high", "counting_method")

# change the name of the counting methods
pred$counting_method <- gsub(pattern = "count_30s", replacement = "Quick count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- gsub(pattern = "count_1m", replacement = "Unlimited count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- gsub(pattern = "count_cells", replacement = "Cell count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- factor(pred$counting_method, levels = c("Quick count", "Unlimited count", "Cell count"))

plot_count_TRUE <- ggplot(data = pred, aes(x = count_TRUE, y = prop_detect, group = counting_method, color = counting_method)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = counting_method, fill = counting_method), alpha = 0.2, colour = NA) +
  ylim(c(0, 1)) +
  theme_bw() +
  xlab("Individuals in the quadrat") +
  ylab("") +
  guides(color = guide_legend(title = "Counting method"), fill = guide_legend(title = "Counting method")) +
  annotate("text", x = 285, y = 0.97, label = "B", size = 5, fontface = 2)


### effect of fatigue
pred_30s <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("quadrat_order [all]", "counting_method [all]"), type = "fixed",
                                               condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                             count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 0, quadrat_order = 5)))
pred_30s <- subset(pred_30s, pred_30s$group == "count_30s")

pred_1m <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("quadrat_order [all]", "counting_method [all]"), type = "fixed",
                                              condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                            count_TRUE = 100, counting_time_1m = 3, counting_time_cells = 0, quadrat_order = 5)))
pred_1m <- subset(pred_1m, pred_1m$group == "count_1m")

pred_cells <- as.data.frame(ggeffects::ggpredict(mod_final, terms = c("quadrat_order [all]", "counting_method [all]"), type = "fixed",
                                                 condition = c(species_conspicuousness = 2, habitat_closure = 0, exp_bota = 2.5,
                                                               count_TRUE = 100, counting_time_1m = 0, counting_time_cells = 3, quadrat_order = 5)))
pred_cells <- subset(pred_cells, pred_cells$group == "count_cells")

pred <- tibble(rbind(pred_30s, pred_1m, pred_cells))
colnames(pred) <- c("quadrat_order", "prop_detect", "std.error", "conf.low", "conf.high", "counting_method")

# change the name of the counting methods
pred$counting_method <- gsub(pattern = "count_30s", replacement = "Quick count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- gsub(pattern = "count_1m", replacement = "Unlimited count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- gsub(pattern = "count_cells", replacement = "Cell count", x = pred$counting_method, fixed = TRUE)
pred$counting_method <- factor(pred$counting_method, levels = c("Quick count", "Unlimited count", "Cell count"))

plot_quadrat_order <- ggplot(data = pred, aes(x = quadrat_order, y = prop_detect, group = counting_method, color = counting_method)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = counting_method, fill = counting_method), alpha = 0.2, colour = NA) +
  ylim(c(0, 1)) +
  scale_x_continuous(limits = c(1, 10), breaks = c(2, 4, 6, 8, 10)) +
  theme_bw() +
  xlab("Quadrat order") +
  ylab("") +
  guides(color = guide_legend(title = "Counting method"), fill = guide_legend(title = "Counting method")) +
  annotate("text", x = 9.5, y = 0.97, label = "C", size = 5, fontface = 2)

# combine the plots
plot_figure_4 <- plot_exp_bota + plot_count_TRUE + plot_quadrat_order +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
# plot_figure_4


# save the plot
pdf(file = "./output/Figure4_predict_3_other_variables.pdf", width = 9, height = 3.6)
print(plot_figure_4)
dev.off()
png(file = "./output/Figure4_predict_3_other_variables.png", width = 9, height = 3.6, units = "in", res = 600)
print(plot_figure_4)
dev.off()



# #### Figures for a talk at SFE2 conference ####
# 
# ### version of Fig. 4 with only the effects of the experience in botany and true density
# plot_figure_4_SFE <- plot_exp_bota + plot_count_TRUE +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')
# # plot_figure_4_SFE
# png(file = "./output/SFE_Figure4_predict_3_other_variables.png", width = 7, height = 3.6, units = "in", res = 600)
# print(plot_figure_4_SFE)
# dev.off()
# 
# ### version of Fig. 4 with only the effect of quadrat order
# plot_figure_4_SFE_quadrat_order <- plot_quadrat_order + 
#   plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')
# # plot_figure_4_SFE_quadrat_order
# png(file = "./output/SFE_Figure4_predict_3_other_variables_quadrat_order.png", width = 5, height = 4.6, units = "in", res = 600)
# print(plot_figure_4_SFE_quadrat_order)
# dev.off()
# 
# ### original figure to show the raw detection rate averaged by session for each counting method
# df_mean_detect_rate <- df_3_methods %>% 
#   group_by(counting_method, species) %>% 
#   summarise(mean_detection_rate = mean(prop_detect))
# 
# df_mean_from_means <- df_mean_detect_rate %>% 
#   group_by(counting_method) %>% 
#   summarise(mean_detection_rate = round(mean(mean_detection_rate), digits = 2))
# 
# my_x_labs <- c("Quick count", "Unlimited count", "Cell count")
# 
# plot_raw_detection_rate <- ggplot(df_mean_detect_rate, aes(x = counting_method, y = mean_detection_rate, color = counting_method)) +
#   geom_point(size = 3, position = position_jitter(w = 0.1, h = 0)) +
#   geom_point(data = df_mean_from_means, mapping = aes(x = counting_method, y = mean_detection_rate), col = "black", size = 4, shape = 17) +
#   ylim(c(0, 1)) +
#   geom_hline(yintercept = 1, linetype = 2, col = 'black')+
#   theme_bw() +
#   xlab("Counting method") +
#   ylab("Detection rate") +
#   theme(legend.position = "none", text = element_text(size = 12)) +
#   scale_x_discrete(labels = my_x_labs) +
#   geom_text(data = df_mean_from_means, label = df_mean_from_means$mean_detection_rate, nudge_x = 0.2, nudge_y = 0.05, check_overlap = TRUE, size = 5, fontface = 'bold', col = "black")
# 
# 
# png(file = "./output/SFE_plot_raw_detection_rate.png", width = 5.5, height = 3.5, units = "in", res = 600)
# print(plot_raw_detection_rate)
# dev.off()
