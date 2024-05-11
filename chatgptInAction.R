set.seed(123)
# Set the number of persons and blocks
num_persons <- 10
num_blocks <- 10

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

library(tidyverse)

dat_wide <- dat_reactionTime %>%
  pivot_wider(names_from = BlockID, values_from = c(ReactionTime, Error))

head(dat_wide)

library(afex)

# Perform repeated measures ANOVA
anova_result <- aov_ez(id = "PersonID", dv = "ReactionTime", data = dat_reactionTime, within = "BlockID")

# Print the ANOVA table
print(anova_result)

# Check for mean differences between blocks
mean_diff <- mixed(dat_reactionTime, dv = "ReactionTime", between = "BlockID", within = "PersonID")
print(mean_diff)