# This script outlines the first step in cleaning data from hydrolytic enzyme assays

# Load packages
library(tidyverse) # ver 2.0.0


#########################################################################################
# STEP 1: Read data ####
# Read data (adapt for file format)
dat <- readxl::read_xlsx("Data/test_data.xlsx", range = "A14:L21", col_names = FALSE)

# Define new column and row names for raw enzyme data and change names in df
cols <- c(1:12)
rows <- LETTERS[1:8]
colnames(dat)[1:12] = cols 
dat$row <- rows

# Restructure df into long format, concatenate row and column info into well_ID
dat_long <- dat |> 
  pivot_longer(cols = 1:12, names_to = "num", values_to = "fluo") |> 
  unite("well_ID", c(num, row))

# Read sample IDs
samples <- readxl::read_xlsx("Data/samples.xlsx")

samples_long <- samples |> 
  pivot_longer(cols = 4:7, names_to = "rep", values_to = "row") |> 
  unite("well_ID", c(col, row)) |> 
  select(c(sampleID, well_ID))

# Join sample list with raw data
data <- full_join(samples_long, dat_long, by = "well_ID")

########################################################################################
# Step 2: Data validation ####
# Calculate mean fluorescence and check for outliers

data |> 
  ggplot(aes(fluo))+
  geom_boxplot()+
  facet_grid(sampleID ~. )

# Calculate mean fluorescence
mean <- data |> 
  group_by(sampleID) |> 
  summarise(n = n(),
            mean = mean(fluo, na.rm = TRUE),
            sd = sd(fluo, na.rm = TRUE)) |> 
  mutate(se = sd/sqrt(n))

