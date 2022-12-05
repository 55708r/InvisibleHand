# Invisible Hand Experiment 2 Data Analysis

library(ggplot2)
library(ggpubr)
library(dplyr)
library(writexl)
library(readxl)
theme_set(theme_pubr())
options(scipen=999)

# Load data

setwd("/Users/izabelejonusaite/Documents/GitHub/InvisibleHand/data")
ihdata <- read.csv("IH_Experiment2_Data.csv", 
                   header = TRUE, 
                   sep = ",", 
                   stringsAsFactors = FALSE
                   )

# Delete first column (the first column has to be "Total_Time")

ihdata <- ihdata[,-1]

# Data cleaning

# Exclude participants who failed attention checks
# (Viewing the data, no participants failed attention checks, so this step is not necessary)

# Exclude participants who took less than 180 seconds to complete the experiment

ihdata[,1] <- as.numeric(as.character(ihdata[,1]))
exclude <- character(0)

for(i in 1:length(ihdata[,1])) {
  if(ihdata[i,1] <= 180){
    exclude <- c(exclude,i)}
}

if(length(exclude) > 0){
  exclude <- as.numeric(exclude)
  ihdata <- ihdata[-c(exclude),]
}

# Exclude participants with half or more responses given in =< 5 seconds

## Select last click data

row_lastclicks <- ihdata[,c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22)]

for(i in 1:length(row_lastclicks[,1])) {
  row_lastclicks[i,] <- gsub(",", ".", row_lastclicks[i,])
}

row_lastclicks <- as.data.frame(apply(row_lastclicks, 2, as.numeric))

row_lastclicks_eval <- array(0, dim=c(length(ihdata[,1]),10))

exclude <- character(0)

## Loop through each row and evaluate how many elements of that row are more (1) or less (0) than 5.01 sec

for(i in 1:length(ihdata[,1])) {
  for(j in 1:10) {
    if(row_lastclicks[i,j] > 5.01) {
      row_lastclicks_eval[i,j] <- 1
    } else {
      row_lastclicks_eval[i,j] <- 0
    }
  }
}

## Save each row number that is half or more <5s and will be excluded

for(i in 1:length(ihdata[,1])) {
  if(sum(row_lastclicks_eval[i,]) < 6) {
    exclude <- c(exclude,i)
  }
}

## Exclude rows

if(length(exclude) > 0){
  exclude <- as.numeric(exclude)
  ihdata <- ihdata[-c(exclude),]
}

# Print the number of participants who were excluded due to completing half of more items under 5 seconds each, and which rows they were in

if(length(exclude) > 0){
  exclude <- as.numeric(exclude)
  print(paste("The following", length(exclude), "participants were excluded due to completing half or more items under 5 seconds each:"))
  print(exclude)
} else {
  print("No participants were excluded due to completing half or more items under 5 seconds each.")
}

print(paste("The rows excluded are", exclude))

# Print valid sample size after exclusions

print(paste("Valid sample size:", nrow(ihdata)))

#################### Data analysis ####################

# Calculate average total time taken to complete the experiment

ihdata[,1] <- as.numeric(as.character(ihdata[,1]))
mean(ihdata[,1])

## Print the average total time taken to complete the experiment

print(paste("The average total time taken to complete the experiment was", round(mean(ihdata[,1]), 2), "seconds."))

# Total selections of Invisible Hand explanation (1)

ihdata_vignettes <- ihdata[,c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)]

## Create a vector that pools the selections

c_vignettes <- c(ihdata_vignettes[,1], ihdata_vignettes[,2], ihdata_vignettes[,3], ihdata_vignettes[,4], ihdata_vignettes[,5],
                 ihdata_vignettes[,6], ihdata_vignettes[,7], ihdata_vignettes[,8], ihdata_vignettes[,9], ihdata_vignettes[,10])

## Calculate the total number of selections of Invisible Hand explanation

for(i in 1:length(ihdata_vignettes[,1])){
  ihdata_vignettes$total_ih_selections[i] <- sum(ihdata_vignettes[i,1:10] == "1")
}

## Out of all selections (nrow(ihdata_vignettes) * 10), calculate the percentage of selections of Invisible Hand explanation

sum(c_vignettes == "1") / (nrow(ihdata_vignettes) * 10)

## Print the percentage of selections of Invisible Hand explanation

print(paste("The percentage of selections of Invisible Hand explanation was", round(sum(c_vignettes == "1") / (nrow(ihdata_vignettes) * 10), 2) * 100, "%."))

## Plot the number of selections of each explanation 

labels <- c(var1 = "Intentional Design Explanation", var2 = "Invisible Hand Explanation")
colors <- c("Invisible Hand" = "#FAAFAC", "Intentional Design" = "#A2E1E3")

plot_ihid_total_selections <- ggplot(data.frame(c_vignettes), aes(c_vignettes, fill = colors)) +
  geom_bar(width = 1.6,fill = colors) +
  labs(x = " ", y = "Total selections") + 
  scale_x_discrete(expand = c(0, 0.65), labels = labels) +
  scale_y_continuous(limits = c(0,1000), expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.ticks.x=element_blank())

## Print the plot

print(plot_ihid_total_selections)

## Export the plot as publication quality PDF

ggsave("plot_ihid_total_selections.pdf", plot = plot_ihid_total_selections, width = 5, height = 4)

## Plot a density plot of how many times out of 10 each participant selected the IH explanation

ggplot(ihdata_vignettes, aes(x = total_ih_selections)) +
  geom_density(fill = "#A2E1E3", alpha = 0.5) +
  labs(x = "Total selections of Invisible Hand explanation", y = "Density") +
  theme_pubr()