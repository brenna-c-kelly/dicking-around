
# the foulest data cleaning I've ever done.
# I'm sure there's a more elegant solution

# this script will:
#     • create and clean copies of vi variables
#           - non-positive variables are coded as NA
#     • rank the values of columns in descending order based on cleaned vi variables
#     • add these rankings as variables to the original dataframe
#     • recode columns for observations with missing data appropriately (NA/negative/zero)


library(dplyr)
library(matrixStats)

toy_data <- read.csv("/Users/brenna/Documents/School/Research/dicking around/CEO_fiveyear.csv")

summary(toy_data$diff_na)

summary(toy_data)

# create copies of the variable importance columns
#     we don't want to remove information (code nonpositive values as NA)
#     but rowRanks needs to treat anything nonpositive or NA as NA
toy_data <- toy_data %>%
  mutate(vi_SEX_copy = vi_SEX) %>%
  mutate(vi_RACE_copy = vi_RACE) %>%
  mutate(vi_diff_copy = vi_diff_flag) %>%
  mutate(vi_SEX_copy = ifelse(vi_SEX_copy == 0, NA, vi_SEX_copy)) %>%
  mutate(vi_RACE_copy = ifelse(vi_RACE_copy == 0, NA, vi_RACE_copy)) %>%
  mutate(vi_diff_copy = ifelse(vi_diff_copy == 0, NA, vi_diff_copy)) %>%
  mutate(vi_SEX_copy = ifelse(vi_SEX_copy < 0, NA, vi_SEX_copy)) %>%
  mutate(vi_RACE_copy = ifelse(vi_RACE_copy < 0, NA, vi_RACE_copy)) %>%
  mutate(vi_diff_copy = ifelse(vi_diff_copy < 0, NA, vi_diff_copy)) %>%
  #  rowRanks uses ascending order; but we want descending order.
  #  this is ugly — but we can get descending order if we invert these values
  mutate(vi_SEX_copy = vi_SEX_copy^-1) %>%
  mutate(vi_RACE_copy = vi_RACE_copy^-1) %>%
  mutate(vi_diff_copy = vi_diff_copy^-1)
  
# convert to matrix for rowRanks
m_toy_data <- as.matrix(toy_data)

rank <- rowRanks(m_toy_data, cols = m_toy_data[12:14])
rank <- as.data.frame(rank)

# adding the ranking columns to the data
ranked_data <- cbind(toy_data, rank)

# renaming (e.g., V1 to rank_sex)
names(ranked_data)[15] = "rank_sex"
names(ranked_data)[16] = "rank_race"
names(ranked_data)[17] = "rank_diff"

# removing the superfluous
ranked_data <- select(ranked_data, -c("vi_SEX_copy",
                                      "vi_RACE_copy",
                                      "vi_diff_copy"))

##  before running this, double-check with Simon that you don't need to code anything nonpositive/NA as NA
#       if NA/nonpositive can be treated the same in the mapping (grayed out), run this section only:
# coding NAs
ranked_data$rank_sex <- ifelse(is.na(ranked_data$vi_SEX), "NA", ranked_data$rank_sex)
ranked_data$rank_race <- ifelse(is.na(ranked_data$vi_RACE), "NA", ranked_data$rank_race)
ranked_data$rank_diff <- ifelse(is.na(ranked_data$vi_diff_flag), "NA", ranked_data$rank_diff)

#       if NA/nonpositive must be treated differently in the mapping (grayed out in different ways), also run these two sections:
# coding negatives
ranked_data$rank_sex <- ifelse(ranked_data$vi_SEX < 0, "negative", ranked_data$rank_sex)
ranked_data$rank_race <- ifelse(ranked_data$vi_RACE < 0, "negative", ranked_data$rank_race)
ranked_data$rank_diff <- ifelse(ranked_data$vi_diff_flag < 0, "negative", ranked_data$rank_diff)

# coding 0s
ranked_data$rank_sex <- ifelse(ranked_data$vi_SEX == 0, "zero", ranked_data$rank_sex)
ranked_data$rank_race <- ifelse(ranked_data$vi_RACE == 0, "zero", ranked_data$rank_race)
ranked_data$rank_diff <- ifelse(ranked_data$vi_diff_flag == 0, "zero", ranked_data$rank_diff)

