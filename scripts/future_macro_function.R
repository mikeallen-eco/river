# function to compile future (2022+) macroinvertebrate data if it exists
future_macro <- function(year = "2022",
                           mh_path = "data/Macro_Habitat/"){

# check if year is even or odd
  # this matters because the format for 2020 and 2021 tabs were different
  # and this script assumes identical data formatting to those years
  # in even-odd pairs (e.g., 2022-2023, 2024-2025, etc.)
  is.odd <- as.numeric(year) %% 2
  
  # make "inv" an empty data frame in case it doesn't exist
  inv <- data.frame()

# Check if macroinvertebrate data exists for that year. 
  # If so, then load it using the appropriate 2020 or 2021 format.
if(max(grepl(list.files(mh_path), pattern = year))==1){
  
  # get name of data file
  file <- list.files(mh_path)[grepl(list.files(mh_path), 
                                       pattern = year)]
  
  # if even year, load assuming 2020 format, otherwise 2021 format
  if(is.odd == 0){
  
  # Read in Macroinvertebrate data (2020 formatting)
  inv <- read_xlsx(here(mh_path,
                          file),
                     skip = 3, 
                     sheet = paste0("macro_", year)
  ) %>%
    select(c(3,4,32:40)) %>%
    rename(tnc_code = 1,
           date = 2,
           num_gen = 3,
           pct_noninsect = 4,
           pct_ept = 5,
           num_scraper = 6,
           hilsenhoff = 7,
           num_talu2 = 8,
           num_talu3 = 9,
           index = 10,
           rating = 11
    ) %>%
    filter(is.na(num_gen) != T)
  
} # end "is.odd == 0" if statement
  
if( is.odd == 1 ) {
  
  # Read in Macroinvertebrate data (2021 formatting)
  inv <- try(read_xlsx(here(mh_path,
                            file),
                     skip = 2, 
                     sheet = paste0("macro_", year)
  ) %>%
    select(c(3,4,32:40)+1) %>%
    rename(num_gen = 3,
           pct_noninsect = 4,
           pct_ept = 5,
           num_scraper = 6,
           hilsenhoff = 7,
           num_talu2 = 8,
           num_talu3 = 9,
           index = 10,
           rating = 11
    ), silent = T)
  
} # end "is odd" if statement

  if(class(inv)[1] == "try-error") {inv = data.frame()}
  
} # end "does file exist" if statement

  return(inv)
  
} # end function