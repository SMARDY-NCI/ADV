library(dplyr)
library(tidyr)
library(lubridate)

dat <- rio::import("data/SCP3_raw_25oct_10feb.csv") %>%
  select(-V2) %>%
  
  separate(V1, sep = " ", into = c("date", "time"), remove = F) %>%
  # Replace problematic records taken on a specific minute by adding 1 second
  mutate(time = stringr::str_replace(time, ":00$", ":01$")) %>%
  # Round up to nearest minute
  mutate(time = lubridate::ceiling_date(strptime(time, "%H:%M:%S"),
                                        unit = "1 minute")) %>%
  # Round up/down to nearest 5 minutes
  mutate(time = lubridate::round_date(time,
                                      unit = "5 minutes")) %>%
  # Pivot to wide format
  pivot_wider(
    id = c(date, time),
    names_from = V3,
    values_from = V4
  ) %>%
  # Recreate datestamp variable with cleaned unique time, and add sensor
  # variable
  mutate(time = as.character(time),
         time = stringr::str_remove(time,"^.+ "),
         datestamp = paste(date,time),
         sensor = "SCP3") %>%
  #Reorder and select needed variables
  select(datestamp, sensor, everything(),-c(date,time))

# Check number of rows is as expected 
# Total number of rows in original data divided by 6 measurements
assertthat::are_equal(nrow(dat),73392/6)

# Write to file
write.csv(dat,"data/BEIA_wide_format.csv")
