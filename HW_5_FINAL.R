#Used for homework week 5 for ZOO800
#Date: 10/2/25
#Group: Evan Peepo, Samantha Summerfield, Maggie Phillips

library(readxl)
library(tidyverse)
library(writexl)

##----------------------Problem 1-------------------------##

fish_csv <- read.csv('HW/Data/fish.csv')
head(fish_csv, n = 5)

fish_xlsx <- read_xlsx('HW/Data/fish.xlsx')
head(fish_xlsx, n = 5)

fish_rds <- readRDS('HW/Data/fish.rds')
head(fish_rds, n = 5)


##----------------------Problem 2-------------------------##

write.csv(fish_csv, 'HW/Output/fish.csv', row.names = FALSE)
write_xlsx(fish_csv, 'HW/Output/fish.xlsx')
saveRDS(fish_csv, 'HW/Output/fish.rds')

file.info('HW/Output/fish.csv')$size
file.info('HW/Output/fish.xlsx')$size
file.info('HW/Output/fish.rds')$size

#I think CSV is easiest for sharing because anyone can view it without using R and
#it is smaller than xlsx and can be read as a text file. For compact storage, RDS is best
#because these files are quite a bit smaller than CSV or XLSX. 


##----------------------Problem 3-------------------------##









##----------------------Problem 4-------------------------##







##----------------------Problem 5-------------------------##











