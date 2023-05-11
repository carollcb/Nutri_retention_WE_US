#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to pull multiple files together 
# and ensure IDs are added 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)


list_files_TN <- paste0('data/results_TN/',list.files(path='data/results_TN/'),'/loadflex.csv') # list full paths
TN <- data.frame() # create empty dataframe


for(c in 1:length(list_files_TN)) {
    tmp <- read.csv(list_files_TN[c]) |>
      mutate(id = list_files_TN[c],
             id = gsub('data/results_TN/', '', id),
             id = gsub('/loadflex.csv','',id))
    
    TN <- bind_rows(TN, tmp) 
}




list_files_TP <- paste0('data/results_TP/',list.files(path='data/results_TP/'),'/loadflex.csv') # list full paths
TP <- data.frame() # create empty dataframe


for(c in 1:length(list_files_TP)) {
  tmp <- read.csv(list_files_TP[c]) |>
    mutate(id = list_files_TP[c],
           id = gsub('data/results_TP/', '', id),
           id = gsub('/loadflex.csv','',id))
  
  TP <- bind_rows(TP, tmp) 
}

