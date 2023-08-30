# Invisible Hand Experiment 1 Data Analysis

library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)
theme_set(theme_pubr())
options(scipen=999)

# Load data

ihdata_e1 <- read.csv("IH_Experiment1_Data.csv", 
                      header = TRUE, 
                      sep = ",", 
                      stringsAsFactors = FALSE
)

### Data cleaning ###

lastclicks <- ihdata_e1[,c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)]

for(i in 1:length(lastclicks[,1])) {
  lastclicks[i,] <- gsub(",", ".", lastclicks[i,])
}

lastclicks <- as.data.frame(apply(lastclicks, 2, as.numeric))

rows <- length(ihdata_e1[,1])
lastclicks_eval <- array(0, dim=c(rows,20))
exclude <- character(0)

## Loop through each row and evaluate how many elements of that row are more (1) or less (0) than 5.01 sec

for(i in 1:rows){
  for(y in 1:20){		   
    if(lastclicks[i,y] > 5.01){
      lastclicks_eval[i,y] = 0      
    } else {
      lastclicks_eval[i,y] = 1
    }
  }
  ### Save each row number that is half or more <5s and will be excluded
  if(sum(lastclicks_eval[i,]) > 10){
    exclude <- c(exclude,i)
  }
}

exclude <- as.numeric(exclude)

ihdata_e1 <- ihdata_e1[-c(exclude),]


## Print number of excluded participants and number of participants in the final dataset
## Print which rows were excluded

print(paste("Number of excluded participants:", length(exclude)))
print(paste("Number of participants in the final dataset:", length(ihdata_e1[,1])))
print(paste("Rows excluded:", exclude))


# Calculate average total time taken to complete the experiment

total_time <- ihdata_e1[,56]
total_time <- as.numeric(as.character(total_time))
total_time <- mean(total_time)

## Print the average total time taken to complete the experiment

print(paste("Average total time taken to complete the experiment:", total_time))

## Create a new dataframe with only the likelihood data

ihdata_e1_IH_Likelihoods <- ihdata_e1[,c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37)]
ihdata_e1_ID_Likelihoods <- ihdata_e1[,c(3, 7, 11, 15, 19, 23, 27, 31, 35, 39)]


## Convert the data to numeric

for(i in 1:length(ihdata_e1_IH_Likelihoods[,1])) {
  ihdata_e1_IH_Likelihoods[i,] <- gsub(",", ".", ihdata_e1_IH_Likelihoods[i,])
  ihdata_e1_ID_Likelihoods[i,] <- gsub(",", ".", ihdata_e1_ID_Likelihoods[i,])
}

ihdata_e1_IH_Likelihoods <- as.data.frame(apply(ihdata_e1_IH_Likelihoods, 2, as.numeric))
ihdata_e1_ID_Likelihoods <- as.data.frame(apply(ihdata_e1_ID_Likelihoods, 2, as.numeric))

## Create vectors with the means of IH and ID likelihoods

ihdata_e1_IH_Likelihoods_mean <- apply(ihdata_e1_IH_Likelihoods, 1, mean)
ihdata_e1_ID_Likelihoods_mean <- apply(ihdata_e1_ID_Likelihoods, 1, mean)

## Make a dataframe with the means of IH and ID likelihoods

ihdata_e1_IH_ID_Likelihoods_mean <- data.frame(ihdata_e1_IH_Likelihoods_mean, ihdata_e1_ID_Likelihoods_mean)

## Test the distribution of the means of IH and ID likelihoods for normality

shapiro.test(ihdata_e1_IH_ID_Likelihoods_mean$ihdata_e1_IH_Likelihoods_mean)
shapiro.test(ihdata_e1_IH_ID_Likelihoods_mean$ihdata_e1_ID_Likelihoods_mean)

## Test the difference between the means of IH and ID likelihoods, using a paired t-test

t.test(ihdata_e1_IH_ID_Likelihoods_mean$ihdata_e1_IH_Likelihoods_mean, ihdata_e1_IH_ID_Likelihoods_mean$ihdata_e1_ID_Likelihoods_mean, paired = TRUE, alternative = "two.sided")


##################### Data visualization #####################

# Plot overlapping IH and ID likelihood histograms

## Create a dataframe for plotting

ih_e1_plotdata <- data.frame(likelihood = c(ihdata_e1_IH_Likelihoods_mean, ihdata_e1_ID_Likelihoods_mean), 
                             expl_type = c(rep("IH", length(ihdata_e1_IH_Likelihoods_mean)), rep("ID", length(ihdata_e1_ID_Likelihoods_mean))))

## Plot overlapping histograms

colors <- c("Invisible-Hand" = "blue", "Intentional-Design" = "orange")
coeff = 0.004 #value to transform the data for plot overlay

ggplot(ih_e1_plotdata, aes(x=likelihood)) + 
  geom_histogram(data=subset(ih_e1_plotdata,expl_type == 'IH'), aes(fill = "Invisible-Hand"), bins = 50, alpha = 0.4) +
  geom_histogram(data=subset(ih_e1_plotdata,expl_type == 'ID'),aes(fill = "Intentional-Design"), bins = 50,  alpha = 0.5) +
  scale_color_manual(values = colors) +
  scale_y_continuous(name = "Count") +
  labs(x = "Likelihoods",
       y = "Count") +
  theme(legend.title=element_blank())

## Plot overlapping density plots

ggplot(ih_e1_plotdata, aes(x=likelihood)) + 
  geom_density(data=subset(ih_e1_plotdata,expl_type == 'IH'), aes(color = "Invisible-Hand")) +
  geom_density(data=subset(ih_e1_plotdata,expl_type == 'ID'), aes(color = "Intentional-Design")) +
  geom_vline(data=subset(ih_e1_plotdata,expl_type == 'IH'), aes(xintercept=mean(likelihood), color="Invisible-Hand"),
             linetype="dashed") +
  geom_vline(data=subset(ih_e1_plotdata,expl_type == 'ID'), aes(xintercept=mean(likelihood), color="Intentional-Design"),
             linetype="dashed") +
  labs(x = "Likelihoods",
       y = "Density") +
  theme(legend.title=element_blank())



# Create a plot panel with 10 plots, where each plot shows overlapping IH and ID likelihoods for each of the 10 vignettes. 

## Create a dataframe of likelihoods for each vignette. Bind the IH likelihoods for Vignettes 1-10 to ID likelihoods for Vignettes 1-10.

ihdata_e1_Likelihoods <- cbind(ihdata_e1_IH_Likelihoods, ihdata_e1_ID_Likelihoods)

## Create a dataframe for plotting with the likelihoods for each vignette

ih_e1_plotdata <- data.frame(likelihood = c(ihdata_e1_Likelihoods[,1], ihdata_e1_Likelihoods[,11]), 
                             expl_type = c(rep("IH", length(ihdata_e1_Likelihoods[,1])), rep("ID", length(ihdata_e1_Likelihoods[,11]))),
                             vignette = rep("Vignette 1", length(ihdata_e1_Likelihoods[,1])))
for(i in 2:10) { 
  ih_e1_plotdata <- rbind(ih_e1_plotdata, data.frame(likelihood = c(ihdata_e1_Likelihoods[,i], ihdata_e1_Likelihoods[,i+10]), 
                                                    expl_type = c(rep("IH", length(ihdata_e1_Likelihoods[,i])), rep("ID", length(ihdata_e1_Likelihoods[,i+10]))),
                                                    vignette = rep(paste("Vignette", i, sep = " "), length(ihdata_e1_Likelihoods[,i]))))
}

## Create a plot panel with IH and ID likelihoods for each vignette

ihe1_panelplot <- ggplot(ih_e1_plotdata, aes(x=likelihood)) + 
  geom_density(data=subset(ih_e1_plotdata,expl_type == 'IH'), aes(color = "Invisible-Hand")) +
  geom_density(data=subset(ih_e1_plotdata,expl_type == 'ID'), aes(color = "Intentional-Design")) +
  geom_vline(data=subset(ih_e1_plotdata,expl_type == 'IH'), aes(xintercept=mean(likelihood), color="Invisible-Hand"),
             linetype="dashed") +
  geom_vline(data=subset(ih_e1_plotdata,expl_type == 'ID'), aes(xintercept=mean(likelihood), color="Intentional-Design"),
             linetype="dashed") +
  facet_wrap(~ vignette, ncol = 2) +
  labs(x = "Likelihoods",
       y = "Density") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(breaks = seq(0, 100, by = 25))

## Export the plot panel to a pdf file

ggsave("ih_e1_panelplot.pdf", plot = ihe1_panelplot, width = 10, height = 5)

## Correlation test between average GCB score per participant and the difference between the means of IH and ID likelihoods per participant

# select GCB columns
ihdata_e1_gcb <- ihdata_e1[,41:55]

## Calculate the average GCB score per participant

ihdata_e1_gcb_mean <- apply(ihdata_e1_gcb, 1, mean)

## Calculate the difference between the means of IH and ID likelihoods per participant

ihdata_e1_IH_ID_Likelihoods_mean_diff <- ihdata_e1_IH_Likelihoods_mean - ihdata_e1_ID_Likelihoods_mean

## Create a dataframe with the average GCB score and the difference between the means of IH and ID likelihoods per participant

ihdata_e1_gcb_IH_ID_Likelihoods_mean_diff <- data.frame(ihdata_e1_gcb_mean, ihdata_e1_IH_ID_Likelihoods_mean_diff)

## Test the correlation between the average GCB score and the difference between the means of IH and ID likelihoods

cor.test(ihdata_e1_gcb_IH_ID_Likelihoods_mean_diff$ihdata_e1_gcb_mean, ihdata_e1_gcb_IH_ID_Likelihoods_mean_diff$ihdata_e1_IH_ID_Likelihoods_mean_diff, method = "pearson", alternative = "two.sided")

# print the correlation coefficient

print(paste("Correlation coefficient:", cor.test(ihdata_e1_gcb_IH_ID_Likelihoods_mean_diff$ihdata_e1_gcb_mean, ihdata_e1_gcb_IH_ID_Likelihoods_mean_diff$ihdata_e1_IH_ID_Likelihoods_mean_diff, method = "pearson", alternative = "two.sided")$estimate))

# create a scatterplot of the correlation between the average GCB score and the difference between the means of IH and ID likelihoods

ggscatter(ihdata_e1_gcb_IH_ID_Likelihoods_mean_diff, x = "ihdata_e1_gcb_mean", y = "ihdata_e1_IH_ID_Likelihoods_mean_diff", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Average GCB score", ylab = "Difference between the means of IH and ID likelihoods")

# export the correlation plot to a pdf file

ggsave("ih_e1_gcb_IH_ID_Likelihoods_mean_diff_correlation.pdf", plot = last_plot(), width = 5, height = 5)

# export the correlation plot to a png file

ggsave("ih_e1_gcb_IH_ID_Likelihoods_mean_diff_correlation.png", plot = last_plot(), width = 5, height = 5)