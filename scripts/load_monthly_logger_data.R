### This script is used to compactly load monthly temperature/DO data
###   for inclusion in Task 4 (macroinvertebrate and habitat) analyses

# read in compiled logger data (including flagged data)
p <- readRDS("output/compiled_logger_data.rds") %>%
  mutate(order = as.numeric(substr(site, 5, 6)),
         site = fct_reorder(site, order))

# format final compiled temperature data
tdata <- p %>%
  filter(FLAG_Temp == 0) %>%
  mutate(day = yday(datetime),
         month = month(datetime),
         hour = hour(datetime),
         minute = minute(datetime),
         dtime = hour + minute/60) %>%
  # remove obvious outliers at start of time series
  filter(as.logical(1-(site == "TNC_25" & 
                         year == 2018 & 
                         day == 122))) %>%
  filter(as.logical(1-(site == "TNC_23" & 
                         year == 2018 & 
                         day == 122))) %>%
  filter(as.logical(1-(site == "TNC_23" & 
                         year == 2021 & 
                         day == 118))) %>%
  # right_join(expand.grid(site = unique(p$site), 
  #                        year = 2016:2021, day = 82:339)) %>%
  arrange(site, year, day) %>%
  # peak movement ~ 15 April - 15 May in Raritan per Anthony V.
  mutate(season = case_when(day %in% 105:135 ~ "migrate", #4/15-5/15
                            day %in% 136:151 ~ "spawn", #5/16-5/31
                            day %in% 152:181 ~ "larvae", #6/1-6/30
                            day %in% 182:258 ~ "juvenile", #7/1-9/15
                            day %in% 259:334 ~ "fall_mig"), #9/15-11
         yearf = as.factor(year),
         dayf = as.factor(day),
         hourf = as.factor(hour),
         season = as.factor(season))

# summarize data by day
tsum <- tdata %>%
  group_by(site, yearf, month, day, dayf) %>%
  summarise(min_temp = min(temp, na.rm = T),
            max_temp = max(temp, na.rm = T),
            mean_temp = mean(temp, na.rm = T),
            .groups = "drop") 

# summarize daily temp data by month
tsum_month <- tsum %>%
  group_by(site, yearf, month) %>%
  summarize(mean_mean = mean(mean_temp),
            mean_max = mean(max_temp),
            mean_min = mean(min_temp),
            n = length(mean_temp),
            .groups = "drop") %>%
  mutate(mid = case_when(month == 3 ~ yday("2021-03-15"),
                         month == 4 ~ yday("2021-04-15"),
                         month == 5 ~ yday("2021-05-15"),
                         month == 6 ~ yday("2021-06-15"),
                         month == 7 ~ yday("2021-07-15"),
                         month == 8 ~ yday("2021-08-15"),
                         month == 9 ~ yday("2021-09-15"),
                         month == 10 ~ yday("2021-10-15"),
                         month == 11 ~ yday("2021-11-15")
  )) %>%
  mutate(Month = case_when(month == 4 ~ "April",
                           month == 5 ~ "May",
                           month == 6 ~ "June",
                           month == 7 ~ "July",
                           month == 8 ~ "August",
                           month == 9 ~ "September",
                           month == 10 ~ "October",
                           month == 11 ~ "November",
                           month == 12 ~ "December"))

# remove unneeded objects
rm(tdata, tsum)

# format final compiled DO data
dodata <- p %>%
  filter(FLAG_DO == 0) %>%
  mutate(day = yday(datetime),
         month = month(datetime),
         hour = hour(datetime),
         minute = minute(datetime),
         dtime = hour + minute/60,
         do_adj2 = case_when((is.na(do)==F & is.na(do_adj)==T) ~
                               do,
                             TRUE ~ do_adj),
         adj = case_when(is.na(do_adj)==F ~ 1,
                         TRUE ~ 0)) %>%
  select(-do, -do_adj) %>%
  rename(do = do_adj2) %>%
  # right_join(expand.grid(site = unique(p$site), 
  #                        year = 2016:2021, day = 82:339)) %>%
  arrange(site, year, day) %>%
  # Dates based on Anthony V. and shad reference he provided
  mutate(season = case_when(day %in% 105:135 ~ "migrate", #4/15-5/15
                            day %in% 136:151 ~ "spawn", #5/16-5/31
                            day %in% 152:181 ~ "larvae", #6/1-6/30
                            day %in% 182:258 ~ "juvenile", #7/1-9/15
                            day %in% 259:334 ~ "fall_mig"), #9/15-11
         yearf = as.factor(year),
         dayf = as.factor(day),
         hourf = as.factor(hour))

# summarize DO data by day
dosum <- dodata %>%
  group_by(site, yearf, month, day, dayf) %>%
  summarise(min_do = min(do, na.rm = T),
            mean_do = mean(do, na.rm = T),
            max_do = max(do, na.rm = T),
            .groups = "drop") %>%
  arrange(site, yearf, day)

# summarize daily DO data by month
dosum_month <- dosum %>%
  group_by(site, yearf, month) %>%
  summarize(mean_mean = mean(mean_do),
            mean_max = mean(max_do),
            mean_min = mean(min_do),
            n = length(mean_do),
            .groups = "drop") %>%
  mutate(mid = case_when(month == 3 ~ yday("2021-03-15"),
                         month == 4 ~ yday("2021-04-15"),
                         month == 5 ~ yday("2021-05-15"),
                         month == 6 ~ yday("2021-06-15"),
                         month == 7 ~ yday("2021-07-15"),
                         month == 8 ~ yday("2021-08-15"),
                         month == 9 ~ yday("2021-09-15"),
                         month == 10 ~ yday("2021-10-15"),
                         month == 11 ~ yday("2021-11-15")
  ))

rm(dodata, dosum, p)