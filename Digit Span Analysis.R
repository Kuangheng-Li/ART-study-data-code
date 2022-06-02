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

# One of the columns in this file contains the
# JSON-encoded data from lab.js
labjs_column <- 'digit-span-2'

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
# write_csv(data_cut, 'labjs_data_output.csv')

# Delete other rows based on the value of 'responses'
data <- data[data$responses!='NULL',]

# Reverse the span
# Change data type from character to interger
n = length(data$span)
for (i in 1:n){if (is_empty(i) == FALSE){
  u = unlist(data$span[i])
  u = as.integer(u)
  u = rev(u)
  u = list(u)
  data$span[i] = u
}}

for (i in 1:n){if (is_empty(i) == FALSE){
  u = unlist(data$responses[i])
  u = as.integer(u)
  u = list(u)
  data$responses[i] = u
}}

# Construct a truth list
correct<-c(1)
for (i in seq(1:n)){
  correct <- c(correct,identical(data$responses[i],data$span[i]))
}
correct<-tibble(correct)
correct<-correct[-c(1),]

# Construck a duration list and ID list
duration <- data$duration
duration<- tibble(duration)
ID <- data$Q3
ID <- tibble(ID)

# Change to another digit span
# Change list name
correct2<-c(1)
for (i in seq(1:n)){
  correct2 <- c(correct2,identical(data$responses[i],data$span[i]))
}
correct2<-tibble(correct2)
correct2<-correct2[-c(1),]

# Change to another digit span
# Construct a duration list
duration2 <- data$duration
duration2<- tibble(duration2)

# Combine two results and compare
combines <- tibble(correct,correct2,duration,duration2,ID)

write_csv(combines,'additional.csv')


