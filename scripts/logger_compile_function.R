### function to read in and combine HOBO or HYDRO data from excel
logger_compile <- function(year = "2021",
                           type = "HOBO",
                           logger_path = "D:/river/Logger_Data_dl_20220104/"){
  ### first create a sub-function to get file paths
  get_files <- function(year,
                        type,
                        logger_path){
    if (type == "hydro") {
      type2 = "HYDRO"
    } else{
      type2 = "HOBO"
    }
    file_path <- paste0(logger_path,
                        paste0(year, " data/"),
                        paste0(type2, "_", year, "/"))
    file_list <- list.files(file_path)
    full_paths <-
      sapply(file_list, function(x) {
        paste0(file_path, x)
      }, USE.NAMES = F)
    
    return(full_paths)
  } # close get_files function
  
  ### get the file paths for the desired year and logger type
  logger_file_paths <- get_files(year, type, logger_path)
  
  ### if type == "hobo": read and compile all the HOBO data from excel
  if(type == "hobo"){
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
  if(type == "hydro") {
    print(paste0("Compiling HYDRO files from ", year))
    
    logger_data_list <- lapply(
      logger_file_paths,
      FUN = function(x) {
        logger_data_df = readxl::read_xlsx(path = test_path,
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
  
  logger_data_compiled <- do.call(rbind, logger_data_list) %>%
    filter(as.numeric(gsub("TNC_", "", site)) < 30)
  
  return(logger_data_compiled)
  
}