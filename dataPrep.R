#install.packages("writexl")
#install.packages("Rtsne")

library(dplyr)
library(ggplot2)
library(scales)
library(hms)
library(tidyr)
library(patchwork) 
library(lubridate)
library(scales)
library(Rtsne)
library(ggrepel)
library(grid)
library(writexl)


data <- read.csv2('Elektrines_duomenys_2023-2024m.csv')


# Only summer days for 2023 and 2024 ------
data <- data %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))
duom_filtered_23 <- data %>% 
  filter(between(timestamp, as.POSIXct('2023-06-01 00:00:00'), as.POSIXct('2023-08-31 23:59:59')))
duom_filtered_24 <- data %>% 
  filter(between(timestamp, as.POSIXct('2024-06-01 00:00:00'), as.POSIXct('2024-08-31 23:59:59')))

# NA delete
fill_na_with_row_median <- function(df, current_columns, voltage_columns) {
  df %>%
    rowwise() %>%
    mutate(
      across(all_of(current_columns), 
             ~ ifelse(is.na(.), 
                      ifelse(all(is.na(c_across(all_of(current_columns)))), 0, 
                             median(c_across(all_of(current_columns)), na.rm = TRUE)), 
                      .)),
      across(all_of(voltage_columns), 
             ~ ifelse(is.na(.), 
                      ifelse(all(is.na(c_across(all_of(voltage_columns)))), 0, 
                             median(c_across(all_of(voltage_columns)), na.rm = TRUE)), 
                      .))
    ) %>%
    ungroup()
}  

current_columns <- c("Current_of_input_1_INV.1", "Current_of_input_2_INV.1", "Current_of_input_3_INV.1", 
                     "Current_of_input_4_INV.1", "Current_of_input_5_INV.1", "Current_of_input_6_INV.1", 
                     "Current_of_input_7_INV.1", "Current_of_input_8_INV.1", "Current_of_input_9_INV.1", 
                     "Current_of_input_10_INV.1", "Current_of_input_11_INV.1", "Current_of_input_12_INV.1")

voltage_columns <- c("DC_Voltage_1_INV.1", "DC_Voltage_2_INV.1", "DC_Voltage_3_INV.1", "DC_Voltage_4_INV.1")

duom_filtered_23 <- fill_na_with_row_median(duom_filtered_23, current_columns, voltage_columns)
duom_filtered_24 <- fill_na_with_row_median(duom_filtered_24, current_columns, voltage_columns)

#Strings computing
duom_filtered_23 <- duom_filtered_23 %>%
  mutate(
    energy_string_1 = Current_of_input_1_INV.1 * DC_Voltage_1_INV.1 * (1/12),
    energy_string_2 = Current_of_input_2_INV.1 * DC_Voltage_1_INV.1 * (1/12),
    energy_string_3 = Current_of_input_3_INV.1 * DC_Voltage_1_INV.1 * (1/12),
    energy_string_4 = Current_of_input_4_INV.1 * DC_Voltage_2_INV.1 * (1/12),
    energy_string_5 = Current_of_input_5_INV.1 * DC_Voltage_2_INV.1 * (1/12),
    energy_string_6 = Current_of_input_6_INV.1 * DC_Voltage_2_INV.1 * (1/12),
    energy_string_7 = Current_of_input_7_INV.1 * DC_Voltage_3_INV.1 * (1/12),
    energy_string_8 = Current_of_input_8_INV.1 * DC_Voltage_3_INV.1 * (1/12),
    energy_string_9 = Current_of_input_9_INV.1 * DC_Voltage_3_INV.1 * (1/12),
    energy_string_10 = Current_of_input_10_INV.1 * DC_Voltage_4_INV.1 * (1/12),
    energy_string_11 = Current_of_input_11_INV.1 * DC_Voltage_4_INV.1 * (1/12),
    energy_string_12 = Current_of_input_12_INV.1 * DC_Voltage_4_INV.1 * (1/12)
  )


duom_filtered_24 <- duom_filtered_24 %>%
  mutate(
    energy_string_1 = Current_of_input_1_INV.1 * DC_Voltage_1_INV.1 * (1/12),
    energy_string_2 = Current_of_input_2_INV.1 * DC_Voltage_1_INV.1 * (1/12),
    energy_string_3 = Current_of_input_3_INV.1 * DC_Voltage_1_INV.1 * (1/12),
    energy_string_4 = Current_of_input_4_INV.1 * DC_Voltage_2_INV.1 * (1/12),
    energy_string_5 = Current_of_input_5_INV.1 * DC_Voltage_2_INV.1 * (1/12),
    energy_string_6 = Current_of_input_6_INV.1 * DC_Voltage_2_INV.1 * (1/12),
    energy_string_7 = Current_of_input_7_INV.1 * DC_Voltage_3_INV.1 * (1/12),
    energy_string_8 = Current_of_input_8_INV.1 * DC_Voltage_3_INV.1 * (1/12),
    energy_string_9 = Current_of_input_9_INV.1 * DC_Voltage_3_INV.1 * (1/12),
    energy_string_10 = Current_of_input_10_INV.1 * DC_Voltage_4_INV.1 * (1/12),
    energy_string_11 = Current_of_input_11_INV.1 * DC_Voltage_4_INV.1 * (1/12),
    energy_string_12 = Current_of_input_12_INV.1 * DC_Voltage_4_INV.1 * (1/12)
  )

duom_strings_23 <- duom_filtered_23 %>%
  select(timestamp, starts_with("energy_string_"))

duom_strings_24 <- duom_filtered_24 %>%
  select(timestamp, starts_with("energy_string_"))



duom_strings_23 <- duom_strings_23 %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
duom_strings_24 <- duom_strings_24 %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


duom_strings_val_23 <- duom_strings_23 %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  mutate(hour_timestamp = floor_date(timestamp, "hour")) %>%
  group_by(hour_timestamp) %>%
  summarise(across(starts_with("energy_string"), sum, na.rm = TRUE), .groups = 'drop') %>%
  rename(timestamp = hour_timestamp)

duom_strings_val_24 <- duom_strings_24 %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  mutate(hour_timestamp = floor_date(timestamp, "hour")) %>%
  group_by(hour_timestamp) %>%
  summarise(across(starts_with("energy_string"), sum, na.rm = TRUE), .groups = 'drop') %>%
  rename(timestamp = hour_timestamp)



# Overall energy computing
daily_energy_23 <- duom_strings_val_23 %>%
  mutate(
    date = make_date(year(timestamp), month(timestamp), day(timestamp))
  ) %>%
  group_by(date) %>%
  summarise(
    total_energy = sum(across(starts_with("energy_string")), na.rm = TRUE), 
    .groups = 'drop'
  )

daily_energy_24 <- duom_strings_val_24 %>%
  mutate(
    date = make_date(year(timestamp), month(timestamp), day(timestamp))
  ) %>%
  group_by(date) %>%
  summarise(
    total_energy = sum(across(starts_with("energy_string")), na.rm = TRUE), 
    .groups = 'drop'
  )


# Delete days, when energy=0
zero_energy_days_23 <- daily_energy_23 %>%
  filter(total_energy == 0) %>%
  pull(date)

zero_energy_days_24 <- daily_energy_24 %>%
  filter(total_energy == 0) %>%
  pull(date)


daily_energy_23 <- daily_energy_23 %>%
  filter(total_energy != 0)

daily_energy_24 <- daily_energy_24 %>%
  filter(total_energy != 0)

print(zero_energy_days_23)
print(zero_energy_days_24)



min_23 <- min(daily_energy_23$total_energy, na.rm = TRUE)
max_23 <- max(daily_energy_23$total_energy, na.rm = TRUE)
step_23 <- (max_23 - min_23) / 3

low_threshold_23 <- min_23 + step_23
high_threshold_23 <- min_23 + 2 * step_23

min_24 <- min(daily_energy_24$total_energy, na.rm = TRUE)
max_24 <- max(daily_energy_24$total_energy, na.rm = TRUE)
step_24 <- (max_24 - min_24) / 3

low_threshold_24 <- min_24 + step_24
high_threshold_24 <- min_24 + 2 * step_24

daily_energy_23 <- daily_energy_23 %>%
  mutate(group = case_when(
    total_energy <= low_threshold_23 ~ "Cloudy",
    total_energy > low_threshold_23 & total_energy <= high_threshold_23 ~ "Average",
    total_energy > high_threshold_23 ~ "Sunny"
  ))

daily_energy_24 <- daily_energy_24 %>%
  mutate(group = case_when(
    total_energy <= low_threshold_24 ~ "Cloudy",
    total_energy > low_threshold_24 & total_energy <= high_threshold_24 ~ "Average",
    total_energy > high_threshold_24 ~ "Sunny"
  ))

table(daily_energy_23$group)
table(daily_energy_24$group)
