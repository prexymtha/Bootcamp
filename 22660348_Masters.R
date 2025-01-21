# Set working directory
getwd() # Check current working directory

# Create a new R script in RStudio named [yourstudentnumber].R
# Save all subsequent code in this script.

# Load necessary packages using pacman
install.packages("pacman") # Install pacman if not already installed
library(pacman)

# Load all required packages
p_load(tidyverse, huxtable, fixest, readxl, tsibble, mFilter)

# Explanation:
# - pacman::p_load() installs and loads packages in one step.
# - These packages are used for data wrangling, visualization, econometric modeling, and time series analysis.

# Create a folder named 'data' in the current working directory
dir.create("data")

# Explanation:
# - `dir.create()` creates a new folder for storing data files, ensuring organized workflows.
# Define the URL and file path
url <- "https://www.columbia.edu/~mu2166/book/empirics/usg_data_annual.xls"
file_path <- file.path(getwd(), "data", "usg_data_annual.xls")

# Download the file
download.file(url, file_path, mode = "wb")

# Load the data into a dataframe
library(readxl)
usg_data <- read_excel(file_path)

# Explanation:
# - `download.file()` fetches the dataset from the internet.
# - `read_excel()` reads the Excel file into R as a dataframe.
# Extract unique country names
countries <- unique(usg_data$`Country Name`)

# Function to assign random countries
my_countries <- function(studentnumber, array) {
  set.seed(studentnumber)
  country_index <- c(204, 174)
  repeat {
    third_number <- sample(1:214, 1)
    if (third_number != 204 && third_number != 174) break
  }
  country_index <- c(country_index, third_number)
  selected_countries <- array[country_index]
  return(data.frame(Index = country_index, Country = selected_countries))
}

# Generate the selected countries
keep_countries <- my_countries(123456789, countries)

# Merge with the main dataset
raw_countries <- merge(usg_data, keep_countries, by.x = "Country Name", by.y = "Country")

# Explanation:
# - This task narrows down the dataset to specific countries using random selection.
# - The merging aligns the filtered country list with the dataset.
# Reshape the data into a long format
long_data <- raw_countries %>%
  pivot_longer(cols = `1960`:`2011`, names_to = "Year", values_to = "Value") %>%
  select(-`Indicator Name`) %>%
  distinct() %>%
  pivot_wider(names_from = `Indicator Code`, values_from = Value)

# Convert Year to numeric
long_data$Year <- as.numeric(long_data$Year)

# Convert to time-series tibble
long_data <- long_data %>% as_tsibble(index = Year, key = `Country Name`)

# Rename columns and create new variables
long_data <- long_data %>%
  rename(gdp_pcap = NY.GDP.PCAP.KN) %>%
  mutate(
    cons_pcap = `NY.CON.PCAP.CD`,
    gdi_pcap = `NY.GDI.PCAP.CD`
  )

# Explanation:
# - `pivot_longer()` transforms the dataset to a long format for panel analysis.
# - `pivot_wider()` reshapes it back to a wide format with indicator codes as columns.
# - Variables are renamed and additional per capita measures are created.
library(huxtable)
summary_stats <- long_data %>%
  group_by(`Country Name`) %>%
  summarise(
    Mean = mean(gdp_pcap, na.rm = TRUE),
    StdDev = sd(gdp_pcap, na.rm = TRUE),
    Observations = n()
  )

# Display the table
hux(summary_stats)
library(fixest)

# Filter data for the United States
us_data <- filter(long_data, `Country Name` == "United States")

# Run regressions
model_1 <- feols(l_gdp_pcap ~ Year, data = us_data)
model_2 <- feols(l_gdp_pcap ~ Year + lag(l_gdp_pcap, 1), data = us_data)
model_3 <- feols(d(l_gdp_pcap) ~ Year + lag(l_gdp_pcap, 1), data = us_data)

# Combine tables
huxreg(model_1, model_2, model_3)
data_to_use <- long_data %>%
  group_by_key() %>%
  mutate(l_gdp_pcap_ldt = residuals(lm(l_gdp_pcap ~ poly(Year, 2), data = cur_data()))) %>%
  ungroup()
hp_data <- data_to_use %>%
  mutate(across(
    c(l_gdp_pcap, l_cons_pcap),
    ~ mFilter::hpfilter(.x, freq = 6.25)$cycle
  ))
library(tseries)
adf.test(hp_data$l_gdp_pcap)
