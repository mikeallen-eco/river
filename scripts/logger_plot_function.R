### logger data plotting functions
logger_plot <- function(df, year, variable){
  
  year_num <- as.numeric(year)
  
  if(variable == "temp"){
  the_plot <- df %>%
    filter(year == year_num) %>%
    mutate(order = as.numeric(gsub("TNC_", "", site)),
           site = forcats::fct_reorder(site, order),
           site_logger = paste0(site," ", logger)) %>%
    ggplot() + 
    geom_line(aes(x = datetime, y = temp), size = 0.1) +
    facet_wrap(~site_logger) +
    theme_bw() +
    labs(x = "", y = "Temp. (C)", title = year)
  }
  
  if(variable == "do"){
    the_plot <- df %>%
    filter(year == year_num) %>%
      mutate(order = as.numeric(gsub("TNC_", "", site)),
             site = forcats::fct_reorder(site, order),
             site_logger = paste0(site," ", logger)) %>%
    ggplot() + 
    geom_line(aes(x = datetime, y = do), size = 0.1) +
    facet_wrap(~site_logger) +
    theme_bw() +
    labs(x = "", y = "DO", title = year)
  }
  
  # return the plot
  return(the_plot)
}
