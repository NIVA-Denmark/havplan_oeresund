#       Loop read of uncorr summay files


#load libraries
library(readr)


# reading acs files- PART I
#####
#set dir from where to read the acs files 
#Reading corrected files with all channels

setwd("C:/Users/AU575801/Documents/KD_project/ac_processing_clean/20170929/time_depth_merge")


#Create a list of all file names in directory

#Save File names to a variable

names <- list.files(pattern="stn+.*csv")

## Read in all data frames using a loop


for(i in names){
  filepath <- file.path(paste(i,"",sep=""))
  assign(i, read.csv(filepath, sep = ","))
}

# manual read
# stn1_sum <- read_csv("~/KD_project/ac_processing_clean/20170929/time_depth_merge/stn1_sum.csv")

