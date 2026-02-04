# Beans, beans, beans! 

# Packages ----
library(ggplot2)
library(dplyr)

#
#
#

# We wish to create code for simulating sampling done
# at different frequencies. First, you can see code 
# for individual functions, and at the end we put all 
# the information together. 

#
#
#

# Sampling ----

# sample function needs information about 
# 1. where to sample from
# 2. how many units it should draw
# 3. whether once one sampling unit is drawn, it gets returned back to the population before the second draw, or not
# 4. what the probabilities of sampling a certain value would be.

# Identify these elements in the code below
bean_sample <- sample(x = c("white", "black"),
                      size=10, 
                      replace = TRUE, 
                      prob = c(0.3, 0.7)) 
bean_sample

# Let's have R count up the number of black and white beans we sampled
bean_counts <-
  bean_sample |> table() 
bean_counts

# Looking at frequencies of either white or black beans separately.
# This is so that we can use the specific counts in calculations.
bean_counts["white"]
bean_counts["black"]

# If you didn't sample any black or white beans, R would count it as missing value (NA).
# But in reality it should be 0. We know there are white beans in the population.
# Let's make sure any NA's are 0's with ifelse function. 
# Different components of it are explained in the comments.

ifelse(is.na(bean_counts["white"])==TRUE, #Is it true that white bean count is NA?
       yes = 0, # If it is, then record that value as 0
       no = bean_counts["white"]) # If it is not, keep the count as it is

# That's the information we need from any one sample.
# But n=1 is not very good. We need to repeat this. And we don't want to do it manually.

#
#
#

# Putting it all together to sample multiple times

# First, determine how many beans are sampled on one go, and how many times the sample is taken
n_beans_in_sample <- 10
n_samples <- 15

# 

# Create an empty vector to save all proportions of white beans sampled from all samples
# So that we have a place to save each sampling information.
sampled_prop_white <- NULL

# A loop for sampling:
# In the following code, we run everything between {}-brackets the number of times determined by object n_samples

for(i in c(1:n_samples)){ 
  # Sampling
  bean_sample <- sample(x = c("white", "black"),
                        size=n_beans_in_sample, 
                        replace = TRUE, 
                        prob = c(0.3, 0.7)) |>
    table() 
  
  # Extract the number of white beans
  white_count <- ifelse(is.na(bean_sample["white"]), 0, bean_sample["white"])
  
  # Save the proportion within this loop run to the vector above
  sampled_prop_white[i] <- white_count / n_beans_in_sample
}

# Output of the loop
sampled_prop_white

# Turn the output into a dataframe
sampled_prop_white <- sampled_prop_white |>
  data.frame()
sampled_prop_white

# Histogram
ggplot(sampled_prop_white, aes(x=sampled_prop_white)) +
  theme_classic() +
  geom_histogram(bins = 5, # Change here to increase or decrease number of bars
                 fill="steelblue", 
                 colour="black")

# Boxplot
ggplot(sampled_prop_white, aes(x=sampled_prop_white)) +
  theme_classic() +
  geom_boxplot(fill="steelblue")

#
#
#

# Descriptive statistics ----

sampled_prop_white |>
  summarise(mean = mean(sampled_prop_white),
            median = median(sampled_prop_white),
            sd = sd(sampled_prop_white))

#
#
#

# TASK: Change the sample sizes (n_beans_in_sample and n_samples) and see what happens.
# HINT: Saving your output into R objects allows you to return to it.

#
#
#

# END ----