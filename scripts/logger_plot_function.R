### logger data plotting functions
logger_plot <- function(df, year, variable){
  
  if(variable == "temp"){
  df %>%
    filter(temp > 0) %>%
    ggplot() + 
    geom_line(aes(x = datetime, y = temp), size = 0.1) +
    facet_wrap(~site) +
    theme_bw() +
    labs(x = "", y = "Temp. (C)", title = year)
  }
  
  if(variable == "do"){
    df %>%
    filter(temp > 0) %>%
    ggplot() + 
    geom_line(aes(x = datetime, y = do), size = 0.1) +
    facet_wrap(~site) +
    theme_bw() +
    labs(x = "", y = "DO", title = year)
  }
  
}
