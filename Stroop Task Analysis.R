# This code relies on the pacman, tidyverse and jsonlite packages
require(pacman)
p_load('tidyverse', 'jsonlite')
# install.packages("ggpubr")
library("ggplot2")
library("ggpubr")

# We're going to assume that the data coming from
# the third-party tool has been loaded into R,
# for example from a CSV file.
data_raw <- read_csv('Final Data.csv')

# Please also check that any extraneous data that
# an external tool might introduce are stripped
# before the following steps. For example, Qualtrics
# introduces two extra rows of metadata after the
# header. Un-commenting the following command removes
# this line and re-checks all column data types.
data_raw <- data_raw[-c(1, 2),] %>% type_convert()

# Delete the extra set
# In this case digit span data are deleted
data_raw <- data_raw[-c(70,68)]

# One of the columns in this file contains the
# JSON-encoded data from lab.js
labjs_column <- 'labjs-data'

# Unpack the JSON data and discard the compressed version
data_raw %>%
  # Provide a fallback for missing data
  mutate(
    !!labjs_column := recode(.[[labjs_column]], .missing='[{}]')
  ) %>%
  # Expand JSON-encoded data per participant
  group_by_all() %>%
  do(
    fromJSON(.[[labjs_column]], flatten=T)
  ) %>%
  ungroup() %>%
  # Remove column containing raw JSON
  select(-matches(labjs_column)) -> data

# The resulting dataset, available via the 'data'
# variable, now contains both the experimental
# data collected by lab.js, as well as any other
# columns introduced by the software that collected
# the data. Values from the latter are repeated
# to fill added rows.

# As a final step, you might want to save the
# resulting long-form dataset
write_csv(data, 'labjs_data_output.csv')

# Delete other rows based on the value of 'correct responce'
data <- data[data$correctResponse!='NULL',]
data <- data[data$phase=='task',]

# Change Chinese into English
# Convert TRUE/FASLE to 1/0
data$word <- gsub('綠','green',data$word)
data$word <- gsub('橙','orange',data$word)
data$word <- gsub('藍','blue',data$word)
data$word <- gsub('紅','red',data$word)
data$correct <- gsub(TRUE,1,data$correct)
data$correct <- gsub(FALSE,0,data$correct)

# Construct a congurency list
congurence <- ifelse(data$word == data$color,1,0)
data$congurence <- congurence
congurence <- na.omit(congurence)

# Create a new dataframe
ID <- na.omit(data$Q3)
duration <- na.omit(data$duration)
correct <- na.omit(data$correct)
new_data <- tibble(ID,duration,correct,congurence)
new_data$correct <- as.numeric(new_data$correct)
new_data <- subset(new_data, duration <= 2000)

# Calculate the means
mean <- aggregate(new_data[2:3], list(new_data$ID), mean)

# Conducting t-test
t<- t.test(duration,congurence)
t

# Correlation test and graph
cor(mean$duration, mean$correct, method = c("pearson", "kendall", "spearman"))
ggscatter(mean, x = "duration", y = "correct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Duration", ylab = "Correct")

# Map the changes in duration
# First create a table of new_data
# Then calculate trial numbers
ND <- tibble(ND)
ND <- tibble(new_data)
ND <- ND[,trial:= 1:.N, by=ID]
ND %>%
  ggplot( aes(x=duration, y=trial, group=ID)) +
  geom_line()

