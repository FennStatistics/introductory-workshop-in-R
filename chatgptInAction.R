# Create an empty list to store the data
data_list <- list()

# within the generated for loop show summary statistics and store the data in a list
for(i in 1:10){
    print(i)
    data <- rnorm(n = 10, mean = 10, sd = 1)
    summary_stats <- summary(data)
    data_list[[i]] <- list(data = data, summary_stats = summary_stats)
}

# Print the list of data and summary statistics
print(data_list)


# Set the number of persons and blocks
num_persons <- 10
num_blocks <- 5

# Create an empty data frame to store the data
data_df <- data.frame(PersonID = integer(),
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
    data_df <- rbind(data_df, data.frame(PersonID = person,
                                         BlockID = block,
                                         ReactionTime = reaction_time,
                                         Error = error,
                                         stringsAsFactors = FALSE))
  }
}

# Print the data frame
print(data_df)


# Set the seed for reproducibility
set.seed(123)

# Simulate the first data set
data_set1 <- data.frame(ID = 1:10,
                        Likert1 = sample(1:5, 10, replace = TRUE),
                        Likert2 = sample(1:5, 10, replace = TRUE),
                        Likert3 = sample(1:5, 10, replace = TRUE))

# Simulate the second data set
data_set2 <- data.frame(ID = 1:10,
                        Likert4 = sample(1:5, 10, replace = TRUE),
                        Likert5 = sample(1:5, 10, replace = TRUE),
                        Likert6 = sample(1:5, 10, replace = TRUE))

# Randomly shuffle the rows of the second data set
data_set2 <- data_set2[sample(nrow(data_set2)), ]

# Merge the two data sets by the ID column
merged_data <- merge(data_set1, data_set2, by = "ID")

# Print the merged data
print(merged_data)


