# load libraries
library(dplyr)
library(here)
library(readxl)
library(lubridate)
library(ggplot2)
here <- here::here # resolve namespace conflict
# load function to get names of files within year folders 
source(here("scripts", "get_files_function.R"))

# set path to folder where the latest logger data files are kept
logger_path <- "D:/river/Logger_Data_dl_20220104/" 

# Read and compile HOBO data using logger_compile function
hobo2020 <- logger_compile("2020", type = "hobo")
hobo2021 <- logger_compile("2021", type = "hobo")

hydro2021 <- logger_compile("2021", type = "hydro")

### Generating data checking plots for each site/year, Temp and DO

# Plotting temperature by site for QC
logger_plot(hobo2020, "2020", "temp")
ggsave("figures/QC_plots/temp/hobo2020_temp.png", width = 9, height = 9, dpi = 100)


logger_plot(hydro2020, "2020", "temp")
ggsave("figures/QC_plots/temp/hobo2020_temp.png", width = 9, height = 9, dpi = 100)



