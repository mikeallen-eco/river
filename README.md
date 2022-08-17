# River Data Analysis
[![DOI](https://zenodo.org/badge/444492875.svg)](https://zenodo.org/badge/latestdoi/444492875)

Code to analyze and visualize river water quality monitoring data including data loggers (temperature, DO, turbidity) and macroinvertebrate/habitat quality indices. See "output" folder for examples.

## About the analysis
This analysis was performed in 2022 (with 2016-2021 data) in collaboration with The Nature Conservancy in support of their Paulinskill River restoration and monitoring efforts. For questions about the project or the data contact Michelle DiBlasio at michelle (d o t) diblasio (a t) tnc (d o t) org.

## Steps to run this code
0. Make sure you have R and RStudio installed.
1. Download the repository to your machine by clicking the green "code" button on the main github page and selecting "Download".
2. Unzip the folder. Put the river folder where you would like to keep it, but do not rename it or any of its sub-folders. Also do not move or rename any files in the main "river" folder or in the "scripts" or "data" sub-folders. This is important in order for the code (and RStudio) to look for files in the right places. 
3. The "output" subfolder contains all of the plots made for the 2016-2021 data. You can browse these and recreate them by running the scripts contained in the corresponding .Rmd file (see below) which reference the pre-compiled 2016-2021 data ("river/output/compiled_logger_data.rds"). Or, you can add new years of (similarly-formatted) data to the appropriate sub-folders within the "data" sub-folder and recreate the plots and analyses using the new data. To do the latter, you will need to re-compile all the logger data using the "Task0_logger_compile_QC.Rmd" script as described below. If you just want to use the data from 2016-2021, you can skip that step and go right to the "Task1" through "Task4" Rmd files.
4. OK, now it is time to open Rstudio and get started. Open the project by double clicking the river.Rproj file in the main "river" folder.
5. If you are compiling new data (from year 2022 or greater) then you'll need to run the "Task0_logger_compile_QC.Rmd" script. Open it by clicking "File -> Open..." and navigating to that file.
6. Add your new logger data to the appropriate subfolder within the "data" subfolder. It is important to make any new data you add have the same fields and formatting (tab naming convention, etc.) as the 2021 data.
7. Follow instructions in the "Task0_logger_compile_QC.Rmd" file to read and compile all logger files into a single database within the "output" subfolder that will be called "compiled_logger_data.rds".
8. After you successfully created this file and checked your data using the QC scripts in the "Task0_logger_compile_QC.Rmd" file, you do not need to open that Rmd file again. All future plots and analysis of logger data (i.e., the Task1-4 Rmd files) will refer to that newly-created "compiled_logger_data.rds" database.
9. To recreate the plots in the "output" folder, whether with updated data or the old 2016-2021 data, you will run the code in the corresponding .Rmd file. For example, if you see a plot or analysis you like in the "output/Task2_WQ_migratory_species" folder, you can find the code to recreate it in the "Task2_WQ_migratory_species.Rmd" file. First, open that file the same way you opened the "Task0" file above.
10. Next, read the information up top and follow the instructions to run the first "chunk" of code (i.e., the topmost gray box of code). It may help to clear your environment first by hitting the "broom" button in the Environment section, or by closing and reopening the river.Rproj project.
11. After you have run the first chunk of code you can scroll down to the plot or analysis you wish to recreate and run just that chunk of code. Each plot, table, or model is in its own self-contained (and numbered/labeled) chunk of code that should run as long as the first chunk was run as specified above.
12. If you are recreating a plot or table, after you run the corresponding code chunk the output will be saved in the corresponding subfolder of the "output" subfolder. It will overwrite the old (2016-2021) file that was in there, so be careful if you don't want to do that.
13. Repeat as needed for any other plot, table, or analysis. Note that closing and re-opening RStudio (as described in step 4 above) when switching between any of the different "Tasks" will help prevent excessive memory usage.
