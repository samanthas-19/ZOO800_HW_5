#Used for homework week 5 for ZOO800
#Date: 10/2/25
#Group: Evan Peepo, Samantha Summerfield, Maggie Phillips

#library(readxl)
library(tidyverse)
library(writexl)

##----------------------Problem 1-------------------------##

#import fish.csv into the R environment using the read.csv function 
#Import the other two types of files using their specific function
#then use the head function to show the first five rows
fish_csv <- read.csv('HW/Data/fish.csv')
head(fish_csv, n = 5)

#fish_xlsx <- read_xlsx('HW/Data/fish.xlsx') - this function uses a package
#the function below does not need any packages to read the excel file
fish.xlsx = readxl::read_excel("HW/Data/fish.xlsx")
head(fish_xlsx, n = 5)

fish_rds <- readRDS('HW/Data/fish.rds')
head(fish_rds, n = 5)


##----------------------Problem 2-------------------------##

#save the files by using the 'write or save' function with the output name
write.csv(fish_csv, 'HW/Output/fish.csv', row.names = FALSE)
write_xlsx(fish_csv, 'HW/Output/fish.xlsx')
saveRDS(fish_csv, 'HW/Output/fish.rds')

#
file.info('HW/Output/fish.csv')$size
file.info('HW/Output/fish.xlsx')$size
file.info('HW/Output/fish.rds')$size

#I think CSV is easiest for sharing because anyone can view it without using R and
#it is smaller than xlsx and can be read as a text file. For compact storage, RDS is best
#because these files are quite a bit smaller than CSV or XLSX. 


##----------------------Problem 3-------------------------##









##----------------------Problem 4-------------------------##

#take all the .csv's from Multiple_files folder and put them all into one data frame\\
files <- list.files(
  path = "HW/Data/Multiple_files",  # directory
  pattern = "\\.csv$",              # only .csv files
  full.names = TRUE,                # keep full path for reading
  recursive = FALSE
)

# Read and combine into one data frame
fish_years_df <- do.call(rbind, lapply(files, function(file) { #this takes all the files together
  temp.df <- read.csv(file, header = TRUE, stringsAsFactors = FALSE) #make a temporary data frame to read the .csvs into
  temp.df$file_name <- basename(file)  # add file name as a new column
  return(temp.df)
}))

# Preview
head(fish_years_df)


##----------------------Problem 5-------------------------##

##most of this code was copied from the Bootstrap_parallel_computing.r script given in the HW
##just was asking for some code adjustment

# --- Setup: load base parallel, read data --------------------------------

library(parallel)                          # built-in; no install needed

fish <- read.csv("HW/Data/fish_bootstrap_parallel_computing.csv")   
species <- unique(fish$Species)            # list of species we'll loop over


# --- A tiny bootstrap function (no pipes, base R only) --------------------

boot_mean <- function(species_name, n_boot = 10000, sample_size = 200) {
  # Pull the weight vector for just this species
  x <- fish$Weight_g[fish$Species == species_name]
  
  # Do n_boot resamples WITH replacement; compute the mean each time
  # replicate(...) returns a numeric vector of bootstrap means
  means <- replicate(n_boot, mean(sample(x, size = sample_size, replace = TRUE)))
  
  # Return the average of those bootstrap means (a stable estimate)
  mean(means)
}


# --- SERIAL version: one core, one species after another ------------------

t_serial <- system.time({                   # time the whole serial run
  res_serial <- lapply(                     # loop over species in the main R process
    species,                                # input: vector of species names
    boot_mean,                              # function to apply
    n_boot = 10000,                           # number of bootstrap resamples per species
    sample_size = 200                       # bootstrap sample size
  )
})

# head(res_serial)


# --- PARALLEL version: many cores using a PSOCK cluster (portable) --------

n_cores <- max(1, detectCores() - 1)        # use all but one core (be nice to your laptop)
cl <- makeCluster(n_cores)                  # start worker processes

clusterSetRNGStream(cl, iseed = 123)        # make random numbers reproducible across workers

# Send needed objects to workers (function + data + species vector)
clusterExport(cl, varlist = c("fish", "boot_mean", "species"), envir = environment())

t_parallel <- system.time({                 # time the parallel run
  res_parallel <- parLapply(                # same API as lapply(), but across workers
    cl,                                     # the cluster
    species,                                # each worker gets one species (or more)
    boot_mean,                              # function to run
    n_boot = 10000,                           # same bootstrap settings as serial
    sample_size = 200
  )
})

stopCluster(cl)                             # always stop the cluster when done


# --- Compare runtimes & show speedup --------------------------------------

# Extract elapsed (wall) time and compute speedup = serial / parallel
elapsed_serial   <- unname(t_serial["elapsed"])
elapsed_parallel <- unname(t_parallel["elapsed"])
speedup <- elapsed_serial / elapsed_parallel

cat("Serial elapsed (s):   ", round(elapsed_serial, 3), "\n")
cat("Parallel elapsed (s): ", round(elapsed_parallel, 3), " using ", n_cores, " cores\n", sep = "")
cat("Speedup:               ", round(speedup, 2), "x\n", sep = "")










