# load_city_data: load any mdt data file in the repository, can be applied ot a list
# input of function:
# city: name of city

load_city_data <- function(city) {
  # Construct the URL for the city data
  url <- paste0("https://raw.githubusercontent.com/TrevorKap/MUSA810-Marine-Pollution/main/Data/mdt-data", city, ".csv")
  
  # Load the CSV file from the URL as a data frame
  data <- read.csv(url)
  
  # Return the data frame
  return(data)
}
#test_data <- load_city_data("Melaka")

# List of cities
cities <- c("Bangkok", "Can_Tho", "Chennai", "Melaka", "Mumbai", "Panama_City", 
            "Pune", "Salvador", "Santa_Fe", "Santiago", "Semarang", "Surat")


#city_data_list <- lapply(cities, load_city_data)
names(city_data_list) <- cities
# Create a list of data frames with city names as data frame names
city_data_list <- lapply(cities, function(city) {
  data <- load_city_data(city)
  data$City <- city
  return(data)
})

# Convert the list of data frames to separate objects in the global environment
list2env(city_data_list, envir = .GlobalEnv)
#library(dplyr)
combined_data <- do.call(rbind, city_data_list)
unique(combined_data$master_item_name)
unique(combined_data$master_material)
unique(combined_data$material)

combined_data <- combined_data %>%
  mutate(Type = case_when(
    master_item_name == "CIGARETTES/CIGARS" ~ "Cigarette",
    master_material %in% c("FISHING GEAR", "RUBBER") ~ "Plastic",
    master_material == "PLASTIC" ~ "Plastic",
    master_material == "GLASS" ~ "Glass",
    master_material == "CLOTH" ~ "Cloth",
    master_material == "PAPER & LUMBER" ~ "Paper",
    master_material == "METAL" ~ "Metal",
    master_material %in% c("LANDMARKS", "NON-LITTER ITEM", "MIXED MATERIALS", "PERSONAL HYGEINE", "OTHER") ~ "Other",
    TRUE ~ NA_character_ # Default value if none of the conditions are met
  ))

sum(is.na(combined_data$Type))
print(table(combined_data$Type))
install.packages("rlang")
library(rlang)
library(tidyr)
tidy_combined <- combined_data %>%
  group_by(City, Type) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Type, values_from = count, values_fill = 0)


tidy_combined <- combined_data %>%
  group_by(City) %>%
  summarize(Type = n(Type))