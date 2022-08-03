# function to compile future (2022+) habitat quality score data if it exists
future_hab <- function(year = "2022",
                           mh_path = "data/Macro_Habitat/"){

# check if year is even or odd
  # this matters because the format for 2020 and 2021 tabs were different
  # and this script assumes identical data formatting to those years
  # in even-odd pairs (e.g., 2022-2023, 2024-2025, etc.)
  is.odd <- as.numeric(year) %% 2
  
  # make "inv" an empty data frame in case it doesn't exist
  hab <- data.frame()

# Check if habitat data exists for that year. 
  # If so, then load it using the appropriate 2020 or 2021 format.
if(max(grepl(list.files(mh_path), pattern = year))==1){
  
  # get name of data file
  file <- list.files(mh_path)[grepl(list.files(mh_path), 
                                       pattern = year)]
  
  # if even year, load assuming 2020 format, otherwise 2021 format
  if(is.odd == 0){
  
  # Read in habitat data (2020 formatting)
    hab <- read_xlsx(here(mh_path,
                            file),
                       skip = 0, 
                       sheet = paste0("Habitat_", year)
    ) %>%
      select(c(3:17,18,20)) %>%
      rename(epifaunal_sub = 3,
             pool_sub = 4,
             pool_var = 5,
             sed_dep = 6,
             channel_flow = 7,
             channel_alt = 8,
             channel_sin = 9,
             bank_stab_l = 10,
             bank_stab_r = 11,
             bank_veg_l = 12,
             bank_veg_r = 13,
             rip_veg_l = 14,
             rip_veg_r = 15,
             habitat_score = 16)
  
} # end "is.odd == 0" if statement
  
if( is.odd == 1 ) {
  
  # Read in habitat data (2021 formatting)
  hab <- try(read_xlsx(here(mh_path,
                            file),
                       skip = 0, 
                       sheet = paste0("Habitat_", year)
  ) %>%
    select(c(3:17,19:20)) %>%
    rename(epifaunal_sub = 3,
           pool_sub = 4,
           pool_var = 5,
           sed_dep = 6,
           channel_flow = 7,
           channel_alt = 8,
           channel_sin = 9,
           bank_stab_l = 10,
           bank_stab_r = 11,
           bank_veg_l = 12,
           bank_veg_r = 13,
           rip_veg_l = 14,
           rip_veg_r = 15), silent = T)
  
} # end "is odd" if statement

  if(class(hab)[1] == "try-error") {hab = data.frame()}
  
} # end "does file exist" if statement

  return(hab)
  
} # end function