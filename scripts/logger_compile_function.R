### function to read in and combine HOBO or HYDRO data from excel
logger_compile <- function(year = "2021",
                           type = "HOBO",
                           logger_path = "data/Logger_Data/"){
  
  ###############################################################
  ### 1. ### first create a sub-function to get file paths ######
  ###############################################################
  get_files <- function(year,
                        type,
                        logger_path){
    
    # define whether we are compiling HYDRO, HOBO, or PLDEUR files
    if (type %in% c("hydro","HYDRO")) {
      type2 = "HYDRO"
    } 
    
    if (type %in% c("hobo", "HOBO")) {
      type2 = "HOBO"
    }
    
    if (type %in% c("pldeur", "PLDEUR")) {
      type2 = "PLDEUR"
    }
    
    # define file path for data files based on the TNC folder naming convention
    file_path <- paste0(logger_path,
                        paste0(year, " data/"),
                        paste0(type2, "_", year, "/"))
    
    # make vector of the names of the files to compile
    file_list <- list.files(file_path)
    
    # make vector of the full paths of the files to compile
    full_paths <-
      sapply(file_list, function(x) {
        paste0(file_path, x)
      }, USE.NAMES = F)
    
    # return vector of full paths of the files to compile
    return(full_paths) 
  } # close get_files function
  
  ###############################################################
  ######## end sub-function to create vector of file paths ######
  ###############################################################

  ### get the file paths for the desired year and logger type
  logger_file_paths <- get_files(year, type, logger_path)
  
  ### if type == "hobo": read and compile all the HOBO data from excel
  if(type %in% c("hobo", "HOBO")){
    print(paste0("Compiling HOBO files from ", year))
    logger_data_list <- lapply(
      logger_file_paths,
      FUN = function(x) {
        logger_data_df = readxl::read_xlsx(path = x,
                                           sheet = year) %>%
          rename(
            year = 1,
            site = 2,
            logger = 3,
            num = 4,
            datetime = 5,
            do = 6,
            temp = 7,
            do_adj = 8,
            do_sat = 9,
            FlagsReviewed = 14,
            time_fix = 17,
            time_issue = 18
          ) %>%
          select(-20,-21,-22) %>%
          mutate(time_issue = case_when(is.na(time_issue) ~ "NA",
                                        TRUE ~ paste0("0:", minute(time_issue))))
        return(logger_data_df)
      } # close HOBO lapply
    )
  } # close HOBO if statement
  
  ### if type == "hydro": read and compile all the HYDRO data from excel
  if(type %in% c("hydro", "HYDRO")) {
    print(paste0("Compiling HYDRO files from ", year))
    
    logger_data_list <- lapply(
      logger_file_paths,
      FUN = function(x) {
        logger_data_df = readxl::read_xlsx(path = x,
                                           sheet = year) %>%
          rename(
            year = 1,
            site = 2,
            logger = 3,
            datetime = 4,
            do_sat = 5,
            spcon = 6,
            temp = 7,
            do = 8,
            turb = 9,
            FlagsReviewed = 16,
            time_fix = 21,
            time_issue = 22
          ) %>%
          select(-24,-25,-26,-27) %>%
          mutate(time_issue = case_when(is.na(time_issue) ~ "NA",
                                        TRUE ~ paste0("0:", minute(time_issue))))
        
        return(logger_data_df)
      } # close HYDRO lapply
    )
  } # close HYDRO if statement
  
  ### if type == "pldeur": read and compile all the PLDEUR data from excel
  if(type %in% c("pldeur", "PLDEUR")) {
    print(paste0("Compiling PLDEUR files from ", year))
    
    logger_data_list <- lapply(
      logger_file_paths,
      FUN = function(x) {
        logger_data_df = readxl::read_xlsx(path = x,
                                           sheet = year) %>%
          rename(
            year = 1,
            site = 2,
            logger = 3,
            date = 4,
            time = 5,
            temp = 6,
            turb = 7,
            FlagsReviewed = 12
          ) %>%
          mutate(datetime = ymd_hms(paste0(date, " ", hour(time), 
                                   ":", minute(time),
                                   ":", second(time)))) %>%
          select(-date, -time)
        
        return(logger_data_df)
      } # close PLDEUR lapply
    )
  } # close PLDEUR if statement
  
  # bind all data together into one dataframe
  logger_data_compiled <- do.call(rbind, logger_data_list)
  
  # return the compiled dataframe
  return(logger_data_compiled)
  
}