# ==============================================================================
# R-Code - my first R Script
# date of creation: XXX
# authors: XXX
# ==============================================================================
rm(list=ls()) # clean environment
# dev.off() # clean plots


# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # not needed if you use an R project!
getwd() # get your current working directory


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
num_persons <- 10
num_blocks <- 5

# Generate random intercepts and slopes for each person
person_intercepts <- rnorm(num_persons, mean = 10, sd = 2)   # baseline RT
person_slopes <- rnorm(num_persons, mean = 2, sd = 2)         # allow negative slopes

# Create data frame
dat_reactionTime <- data.frame()

for (person in 1:num_persons) {
  for (block in 1:num_blocks) {
    # Person-specific mean RT depending on their slope
    mean_rt <- person_intercepts[person] + person_slopes[person] * block
    reaction_time <- rnorm(1, mean = mean_rt, sd = 1)

    # Simulate errors
    mean_error <- 0.1 + block * 0.02
    error <- rbinom(1, 1, prob = mean_error)

    dat_reactionTime <- rbind(dat_reactionTime, data.frame(
      PersonID = person,
      BlockID = block,
      ReactionTime = reaction_time,
      Error = error
    ))
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



### survey data:
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


dat_leftJoin <- left_join(x = dat_1, y = dat_2, by = c("ID" = "ID"))
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

################
# plot data and mixed model
################
# plot data:
dat_reactionTime$PersonID <- factor(dat_reactionTime$PersonID)

ggplot(dat_reactionTime, aes(x = BlockID, y = ReactionTime, col = PersonID)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  labs(
    title = "Reaction Time Across Blocks by Participant",
    x = "Block Number",
    y = "Reaction Time (s)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold", size = 16, hjust = 0.5
    ),
    axis.title.x = element_text(
      face = "bold", size = 14, margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      face = "bold", size = 14, margin = margin(r = 10)
    ),
    axis.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(size = 0.8),
    plot.margin = margin(15, 15, 15, 15)
  )


# Fit the linear mixed-effects model
model <- lme4::lmer(
  ReactionTime ~ BlockID + (1 + BlockID | PersonID),
  data = dat_reactionTime,
  REML = FALSE
)

# Summary of the model
summary(model)
lme4::ranef(object = model)

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
# Perform ANOVA
m1 <- afex::aov_car(formula = ReactionTime ~ BlockID + Error(PersonID/BlockID), data=dat_reactionTime)
m1


m1a <- afex::aov_ez(id = "PersonID", dv = "ReactionTime", data = dat_reactionTime,
                    within = c("BlockID"))
m1a




# simple function call with the defaults
ggstatsplot::ggwithinstats(
  data = dat_reactionTime,
  x = BlockID,
  y = ReactionTime,
  type = "parametric",
  title = "Parametric test Reaction Times within blocks"
)



############################################################################
# analyses - survey data
############################################################################
psych::corPlot(r = cor(dat_merged[,
                                  str_subset(string = colnames(dat_merged), pattern = "^Likert")]))

psych::fa.parallel(x = dat_merged[,
                                  str_subset(string = colnames(dat_merged), pattern = "^Likert")])
