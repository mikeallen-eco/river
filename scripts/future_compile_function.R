# function to compile future (2022+) data if it exists
future_compile <- function(year = "2022",
                            type = "HOBO",
                            logger_path = "data/Logger_Data/"){
  
# standardize names for HYDRO, HOBO, or PLDEUR
  if (type %in% c("hydro","HYDRO")) {
    type2 = "HYDRO"
  } 
  
  if (type %in% c("hobo", "HOBO")) {
    type2 = "HOBO"
  }
  
  if (type %in% c("pldeur", "PLDEUR")) {
    type2 = "PLDEUR"
  }

if(length(list.files(here(logger_path, 
                          paste0(year, " data"), 
                          paste0(type2, "_", year))))!=0){
future_df <- logger_compile(year, type = type) %>%
    filter(is.na(year) != TRUE,
           temp != -9999.00,
           temp != -888.88)
} # end compile if statement
  
} # end function