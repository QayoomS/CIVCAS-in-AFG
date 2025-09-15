library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(table1)
library(tidyr)
library(dplyr)
library(readxl)
library(forcats)
library(sf)
library(tmap)
library(reshape2)
library(viridis)

# Reading our data sets and have first glance our data:
SecDB <- read.csv("C:\\Users\\Abdul Qayoom\\Desktop\\GitHub Projects\\CIVCAS in Afg\\CIVCAS-in-AFG\\Cleaned Data\\conflict_data_afg.csv")

# View the first few rows of the data to ensure it's read correctly and then explore in different way:
head(SecDB)
names(SecDB)
str(SecDB)
unique(SecDB$adm_1)


## Exploratory Data Analysis (EDA):


# I will group by year to have a better picture.

Conflict_by_year <- SecDB %>%
  group_by(year) %>%
  summarize(TotalCivCaus = sum(`deaths_civilians`, na.rm = TRUE))

# Checking if it is correctly filtered:
print(Conflict_by_year)


# I will create different plots to explore our data and see which one fits better to visualize our data:

#1.1. Civilian causalities per Year bars:

ggplot(Conflict_by_year, aes(x = year, y = TotalCivCaus)) +
  geom_col(color = "blue", fill = 'maroon') +
  labs(title = "Civilain Death per Year",
       x = "Year",
       y = "Number of Civilan Death") +
  theme_gray()

# Ensure year is treated as a numeric or Date variable (if necessary)
Conflict_by_year$year <- as.numeric(Conflict_by_year$year)

ggplot(Conflict_by_year, aes(x = year, y = TotalCivCaus)) + 
  geom_line(color = "maroon") +   # Line plot connecting the points
  geom_point(color = "white", fill = "#0072B2", shape = 21, size = 3) +  # Add points at each year
  geom_text(aes(label = TotalCivCaus), vjust = -1, size = 3) +  # Adds labels above points
  scale_y_continuous(
    limits = c(0, max(Conflict_by_year$TotalCivCaus, na.rm = TRUE) * 1.05),  # Adjust y-axis limit dynamically
    name = "Number of Civilian Deaths"  # Update y-axis label
  ) +
  ggtitle("Civilian Deaths per Year: 2002-2024") +  # Update title
  theme_classic() +  # Change to classic theme
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Bold plot title and center it
  )



# Now let group civilian deaths by province:

Conflict_by_province <- SecDB %>%
  group_by(adm_1) %>%
  summarize(TotalCivCaus = sum(`deaths_civilians`, na.rm = TRUE))

Conflict_by_province <- Conflict_by_province %>%
  mutate(adm_1 = fct_reorder(adm_1, TotalCivCaus))

# Visualize in bar graph:

ggplot(Conflict_by_province, aes(x = adm_1, y = TotalCivCaus)) +
  geom_col(color = "blue", fill = 'maroon') +
  geom_point(color = "red") +
  labs(title = "Civilain Death by Province",
       x = "Province",
       y = "Number of Civilan Death") +
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Group by province and year, then summarize total civilian deaths
Conflict_by_province_year <- SecDB %>%
  group_by(adm_1, year) %>%
  summarize(TotalCivCaus = sum(`deaths_civilians`, na.rm = TRUE)) %>%
  mutate(adm_1 = fct_reorder(adm_1, TotalCivCaus))  # Reorder provinces based on total civilian deaths

# Plot the data with a line graph
ggplot(Conflict_by_province_year, aes(x = year, y = TotalCivCaus, group = adm_1, color = adm_1)) + 
  geom_line(size = 1) +  # Line plot for each province
  geom_point(color = "red", size = 2) +  # Red points at each year
  labs(title = "Civilian Deaths by Province per Year",
       x = "Year",
       y = "Number of Civilian Deaths") +
  theme_minimal() +  # Use a minimal theme for clarity
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "right")  # Place the legend on the right


# The graph too busy. Let filter only for top 10 province.
# Summarize total civilian deaths by province and year
Conflict_by_province_year <- SecDB %>%
  group_by(adm_1, year) %>%
  summarize(TotalCivCaus = sum(`deaths_civilians`, na.rm = TRUE)) 

# Calculate the total deaths per province across all years
total_deaths_by_province <- Conflict_by_province_year %>%
  group_by(adm_1) %>%
  summarize(TotalCivDeathAllYears = sum(TotalCivCaus, na.rm = TRUE))

# Get the top 10 provinces by total civilian deaths
top_10_provinces <- total_deaths_by_province %>%
  top_n(10, TotalCivDeathAllYears) %>%
  pull(adm_1)  

# Filter the data to keep only the top 10 provinces
Conflict_by_province_year_top10 <- Conflict_by_province_year %>%
  filter(adm_1 %in% top_10_provinces)

# Plot the data with a line graph for the top 10 provinces
ggplot(Conflict_by_province_year_top10, aes(x = year, y = TotalCivCaus, group = adm_1, color = adm_1)) + 
  geom_line(size = 1) +  # Line plot for each province
  geom_point(color = "red", size = 2) +  # Red points at each year
  labs(title = "Top 10 Provinces for Civilian Deaths per Year",
       x = "Year",
       y = "Number of Civilian Deaths") +
  scale_color_discrete(name = "Province") +  # Change legend title to "Province"
  theme_minimal() +  # Use a minimal theme for clarity
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "right")  # Place the legend on the right



# We do a similar analysis for civilian death by district

Conflict_by_district <- SecDB %>%
  group_by(adm_2, year) %>%
  summarize(TotalCivCaus = sum(`deaths_civilians`, na.rm = TRUE))

ggplot(Conflict_by_district, aes(x = adm_2, y = TotalCivCaus)) +
  geom_col(color = "blue", fill = 'orange') +
  geom_point(color = "red") +
  labs(title = "Civilain Death per Year",
       x = "Year",
       y = "Number of Civilan Death") +
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Conflict_by_district_ordered <- Conflict_by_district %>%
  mutate(adm_2 = fct_reorder(adm_2, TotalCivCaus))

ggplot(Conflict_by_district_ordered, aes(x = adm_2, y = TotalCivCaus)) +
  geom_col(color = "blue", fill = 'orange') +
  geom_point(color = "red") +
  labs(title = "Civilain Death per Year",
       x = "Year",
       y = "Number of Civilan Death") +
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(Conflict_by_district_ordered)

#let's filter only top 5 districts this time.

top_districts <- Conflict_by_district_ordered %>%
  top_n(5, TotalCivCaus) %>%
  arrange(desc(TotalCivCaus))


ggplot(top_districts, aes(x = adm_2, y = TotalCivCaus)) +
  geom_col(color = "blue", fill = 'maroon') +
  geom_point(color = "red") +
  labs(title = "Civilain Death by District (Top 30)",
       x = "District",
       y = "Number of Civilan Death") +
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter top 5 districts based on total civilian casualties
top_5_districts <- top_districts %>%
  group_by(adm_2) %>%
  summarize(TotalCivCaus = sum(TotalCivCaus, na.rm = TRUE)) %>%
  top_n(5, TotalCivCaus)  # Select top 5 districts based on civilian casualties

# Filter the original data to include only these top 5 districts
top_5_districts_data <- top_districts %>%
  filter(adm_2 %in% top_5_districts$adm_2)

# Plot the top 5 districts
ggplot(top_5_districts_data, aes(x = year, y = TotalCivCaus, group = adm_2, color = adm_2)) + 
  geom_line(size = 1) +  # Line plot for each district
  geom_point(color = "red", size = 2) +  # Red points at each year
  labs(title = "Civilian Deaths by District (Top 5)",
       x = "Year",
       y = "Number of Civilian Deaths") +
  scale_color_discrete(name = "District") +  # Change legend title to "District"
  theme_minimal() +  # Use a minimal theme for clarity
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "right")  # Place the legend on the right



# a closer look to Mazar civilian casualities. 
Balkh_1998 <- SecDB %>%
  filter(adm_1 == "Balkh province", year == 1998)

names(Balkh_1998) 



Conflict_by_year <- SecDB %>%
  group_by(year) %>%
  summarize(
    TotalCivCaus = sum(deaths_civilians, na.rm = TRUE),
    TotalASideCaus = sum(deaths_a, na.rm = TRUE),
    TotalBSideCaus = sum(deaths_b, na.rm = TRUE)
  )


# Reshape the data to a long format
Conflict_long <- Conflict_by_year %>%
  pivot_longer(cols = c(TotalCivCaus, TotalASideCaus, TotalBSideCaus), 
               names_to = "DeathType", values_to = "Deaths")

# Create the line graph with smoother
ggplot(Conflict_long, aes(x = year, y = Deaths, color = DeathType)) +
  geom_line(size = 1.2) +         
  geom_point(size = 2) +          
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +  
  labs(title = "Civilian, A Side, and B Side Deaths per Year with Trend Lines",
       x = "Year",
       y = "Number of Deaths") +
  scale_color_manual(values = c("TotalCivCaus" = "orange", 
                                "TotalASideCaus" = "blue", 
                                "TotalBSideCaus" = "green"),
                     labels = c("Civilian Deaths", "A Side Deaths", "B Side Deaths")) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#lets look to who caused civilian deaths: 
# Make sure our year is date
Conflict_by_year$year <- as.Date(as.character(Conflict_by_year$year), format = "%Y")

# Reshape the data to a long format
Conflict_long <- Conflict_by_year %>%
  pivot_longer(cols = c(TotalCivCaus, TotalASideCaus, TotalBSideCaus), 
               names_to = "DeathType", values_to = "Deaths")

# Create the time series graph with smoothers
ggplot(Conflict_long, aes(x = year, y = Deaths, color = DeathType)) +
  geom_line(size = 1.2) +        
  geom_point(size = 2) +         
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +  
  labs(title = "Civilian, A Side, and B Side Deaths Over Time with Trend Lines",
       x = "Year",
       y = "Number of Deaths") +
  scale_color_manual(values = c("TotalCivCaus" = "orange", 
                                "TotalASideCaus" = "blue", 
                                "TotalBSideCaus" = "green"),
                     labels = c("Civilian Deaths", "A Side Deaths", "B Side Deaths")) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




