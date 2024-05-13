# ==============================================================================
# R-Code - my first R Script
# date of creation: XXX
# authors: XXX
# ==============================================================================
rm(list=ls()) # clean environment
# dev.off() # clean plots


getwd() # points to your current working directory
# sets the directory of location of this script as the current directory
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # not needed if you use an R project!


############################################################################
# load packages, data
############################################################################

################
# load packages
################
# if packages are not already installed, the function will install and activate them
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
## Error handling Install packages in R base
# options(repos="https://CRAN.R-project.org")

usePackage("tidyverse")
usePackage("psych")
usePackage("afex")
usePackage("ggstatsplot")


rm(usePackage)

################
# simulate data
################
### reaction time data:
# simulate reaction time data with person ID, block ID, reaction time and error, whereby there are significant
# mean differences between blocks in reaction time and error rate

set.seed(123)
# Set the number of persons and blocks
num_persons <- 10
num_blocks <- 5

# Create an empty data frame to store the data
dat_reactionTime <- data.frame(PersonID = integer(),
                      BlockID = integer(),
                      ReactionTime = numeric(),
                      Error = logical(),
                      stringsAsFactors = FALSE)

# Simulate the data
for (person in 1:num_persons) {
  for (block in 1:num_blocks) {
    # Simulate reaction time with mean differences between blocks
    mean_rt <- 10 + block * 2
    reaction_time <- rnorm(n = 1, mean = mean_rt, sd = 1)

    # Simulate error rate with mean differences between blocks
    mean_error <- 0.1 + block * 0.02
    error <- rbinom(n = 1, size = 1, prob = mean_error)

    # Add the data to the data frame
    dat_reactionTime <- rbind(dat_reactionTime, data.frame(PersonID = person,
                                         BlockID = block,
                                         ReactionTime = reaction_time,
                                         Error = error,
                                         stringsAsFactors = FALSE))
  }
}


### long data format
head(dat_reactionTime)
table(dat_reactionTime$PersonID)
table(dat_reactionTime$BlockID)


### wide data format
dat_reactionTime_wide_blockID <- dat_reactionTime %>%
  pivot_wider(names_from = PersonID, values_from = c(ReactionTime, Error))

head(dat_reactionTime_wide_blockID)
dim(dat_reactionTime_wide_blockID)
colnames(dat_reactionTime_wide_blockID)


dat_reactionTime_wide_personID <- dat_reactionTime %>%
  pivot_wider(names_from = BlockID, values_from = c(ReactionTime, Error))
head(dat_reactionTime_wide_personID)
dim(dat_reactionTime_wide_personID)
colnames(dat_reactionTime_wide_personID)



### reaction time data:
# simulate two data sets, which can be matched by a unique ID, which contains multiple likert scales.
# The rows of the data sets should not match
# Simulate the first data set

# https://r4ds.had.co.nz/relational-data.html?q=left_join#relational-data
dat_1 <- data.frame(ID = 1:12,
                        Likert1 = sample(1:5, 12, replace = TRUE),
                        Likert2 = sample(1:5, 12, replace = TRUE),
                        Likert3 = sample(1:5, 12, replace = TRUE))

# Simulate the second data set
dat_2 <- data.frame(ID = 1:10,
                        Likert4 = sample(1:5, 10, replace = TRUE),
                        Likert5 = sample(1:5, 10, replace = TRUE),
                        Likert6 = sample(1:5, 10, replace = TRUE))

# Randomly shuffle the rows of the second data set
dat_2 <- dat_2[sample(x = nrow(dat_2)), ]

# Merge the two data sets by the ID column
dat_merged <- merge(dat_1, dat_2, by = "ID")
dim(dat_merged)


dat_leftJoin <- left_join(x = dat_1, y = dat_2, by = c("Vp" = "Vpn"))
dim(dat_leftJoin)
dat_rightJoin <- right_join(x = dat_1, y = dat_2, by = c("ID" = "ID"))
dim(dat_rightJoin)
dat_fullJoin <- full_join(x = dat_1, y = dat_2, by = c("ID" = "ID"))
dim(dat_fullJoin)


# Print the merged data
dat_fullJoin


############################################################################
# analyses - reaction time data
############################################################################
psych::corPlot(r = cor(dat_merged[,
                                  str_subset(string = colnames(dat_merged), pattern = "^Likert")]))

############################################################################
# analyses - reaction time data
############################################################################

################
# mean differences
################
dat_reactionTime %>%
  group_by(PersonID) %>%
  summarise(N = n(), mean = mean(ReactionTime))
rowMeans(x = dat_reactionTime_wide_personID[, str_subset(string = colnames(dat_reactionTime_wide_personID),
                                                         pattern = "^ReactionTime")])

dat_reactionTime %>%
  group_by(BlockID) %>%
  summarise(N = n(), mean = mean(ReactionTime))
rowMeans(x = dat_reactionTime_wide_blockID[, str_subset(string = colnames(dat_reactionTime_wide_blockID),
                                                        pattern = "^ReactionTime")])





################
# hypothesis test - ANOVA
################
library(afex)

# Perform ANOVA
m1 <- afex::aov_car(formula = ReactionTime ~ BlockID + Error(PersonID/BlockID), data=dat_reactionTime)
m1


m1a <- afex::aov_ez(id = "PersonID", dv = "ReactionTime", data = dat_reactionTime,
       within = c("BlockID"))
m1a




# simple function call with the defaults
library(ggstatsplot)
ggstatsplot::ggwithinstats(
  data = dat_reactionTime,
  x = BlockID,
  y = ReactionTime,
  type = "parametric",
  title = "Parametric test Reaction Times within blocks"
)

