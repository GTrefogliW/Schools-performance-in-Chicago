library(sf)
library(tidyverse)
library(stringr)
library(spData)
library(scales)
library(RColorBrewer)
library(shiny)
library(plotly)

#path <- "C:/Users/guill/OneDrive/Documents/Data and Programming II/HW2/Boundaries - ZIP Codes"
#chi_shape <- st_read(file.path(path, "geo_export_fbe7a12f-9b8f-4ed9-b1eb-9b5cc7027af5.shp"))

# choropleths for dataset 1 - Education
df_educ <- read_csv("Chicago_Public_Schools_-_Progress_Report_Cards__2011-2012_.csv")
# Merge
df_educ$`ZIP Code` <- as.character(df_educ$`ZIP Code`)
educ_il <- inner_join(chi_shape, df_educ, by = c("zip"="ZIP Code"))
educ_il <- educ_il$`CPS Performance Policy Level`
# Plot 1
map_peformance <- ggplot() +
  geom_sf(data = educ_il, aes(fill = `CPS Performance Policy Level`)) +
  theme_minimal() +
  labs(title = "Chicago Public Schools - Performance Level",
       subtitle = "School Year 2011")

map_peformance

# choropleths for dataset 2 - Summer Program
df_program <- read_csv("Libraries_-_2011_Children_s_Summer_Reading_Program.csv")
# Merge
df_program$`ZIP CODE` <- as.character(df_program$`ZIP CODE`)
program_il <- inner_join(chi_shape, df_program, by = c("zip"="ZIP CODE"))
# Plot 2
map_programs <- ggplot() +
  geom_sf(data = program_il, aes(fill = `2011 PARTICIPANTS`)) +
  theme_minimal() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Participation in The Chicago Public Library Summer Program",
       subtitle = "Year 2011")

map_programs

# I found it interesting exploring these two dataset because they allow me to get initial
# insights around the targeting criteria and coverage of educational interventions in the 
# city Chicago. 

# Based on Plot 1, it seems that the south side of the city is the one with the lowest 
# education outcomes: their schools are ranked at Level 3 (lowest performance). In spite
# of this fact, Plot 2 shows that the participation in the educational program run in 
# the Summer of 2011 by The Chicago Public Library was the lowest for neighborhoods located
# in that part of the city.
