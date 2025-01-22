# 22660348 ASSIGNMENT

# Clear the console and the environment
rm(list = ls())  # Remove all objects from the environment
graphics.off()  # Turn off all graphics devices
cat('\f')  # Clear the console

# Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Set the working directory to the script's location
getwd()  # Get the current working directory

# Git reminder
message("Ensure Git is initialized and linked to https://github.com/prexymtha/Bootcamp")  # Reminder to initialize Git

# Install & load the pacman package
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")  # Install pacman if not already installed
}
library(pacman)  # Load pacman library
p_load(urca, fixest, tidyverse, huxtable, readxl, tsibble, mFilter)  # Load required packages using pacman

# TASK 3: Create a folder named 'data' in the current working directory
folder <- file.path(getwd(), "data")  # Define the folder path
if (!dir.exists(folder)) {
  dir.create(folder)  # Create the folder if it doesn't exist
}

# TASK 4: Download the data from the provided link
url <- "https://www.columbia.edu/~mu2166/book/empirics/usg_data_annual.xls"  # URL of the data file
usg_data_path <- file.path(folder, "usg_data_annual.xls")  # Define the path to save the downloaded file
download.file(url, usg_data_path, mode = "wb")  # Download the file
print("File downloaded successfully!")  # Print success message

# Step 4: Read the data into a dataframe using the read_xls function from the readxl package
usg_data <- read_xls(usg_data_path, sheet = 1)  # Read the data from the Excel file
str(usg_data)  # Display the structure of the data
cat("This data contains", ncol(usg_data), "variables, ", nrow(usg_data), " rows.\nThe columns are: ", paste(colnames(usg_data), collapse = ", "), "\nThe types of the data are: ", paste(sapply(usg_data, class), collapse = ", "), "\n")  # Print data summary

# TASK 5: Getting Unique Values and Creating Your Dataset
countries <- usg_data %>% 
  pull(`Country Name`) %>%  # Extract the 'Country Name' column
  unique() %>%  # Get unique country names
  sort()  # Sort the country names
print(countries)  # Print the list of countries

# Step 3: Function to assign you a random country based on my SU (22660348)
my_countries <- function(s22660348, countries) {
  set.seed(s22660348)  # Set seed for reproducibility
  country_index <- c(204, 174)  # Predefined country indices
  repeat {
    third_number <- sample(1:214, 1)  # Randomly select a third country index
    if (third_number != 204 && third_number != 174) break  # Ensure the third index is unique
  }
  country_index <- c(country_index, third_number)  # Combine indices
  selected_countries <- countries[country_index]  # Select countries based on indices
  return(data.frame(Index = country_index, Country = selected_countries))  # Return the selected countries
}

keep_countries <- my_countries(22660348, countries)  # Get the selected countries

# Step 4: Merge keep_countries with the original data
raw_countries <- usg_data %>% 
  filter(`Country Name` %in% keep_countries$Country)  # Filter the data for the selected countries

# TASK 6: MAKING THE DATA USABLE
long_data <- raw_countries %>%
  pivot_longer(
    cols = `1960`:`2011`,  # Convert wide data to long format
    names_to = "Year",  # Name the new column 'Year'
    values_to = "Value"  # Name the new column 'Value'
  ) %>%
  select(-`Indicator Name`) %>%  # Remove the 'Indicator Name' column
  distinct() %>%  # Remove duplicate rows
  pivot_wider(
    names_from = `Indicator Code`,  # Convert long data back to wide format
    values_from = Value  # Use 'Value' column for the values
  ) %>%
  mutate(across(everything(), ~ {
    code <- cur_column()  # Get the current column name
    label <- unique(raw_countries$`Indicator Name`[raw_countries$`Indicator Code` == code])  # Get the label for the column
    attr(., "label") <- label  # Set the label as an attribute
    .
  })) %>%
  mutate(
    Year = as.numeric(Year),  # Convert 'Year' to numeric
    gdp_pcap = `NY.GDP.PCAP.PP.KD`,  # Create 'gdp_pcap' column
    cons_pcap = (`NY.GDP.PCAP.KN` * `NE.CON.PETC.ZS`) / 100,  # Create 'cons_pcap' column
    gd1_pcap = (`NY.GDP.PCAP.KN` * `NE.GDI.TOTL.ZS`) / 100  # Create 'gd1_pcap' column
  ) %>%
  as_tsibble(index = Year, key = `Country Name`)  # Convert to tsibble

data_to_use <- long_data %>%
  select(`Country Name`, `Country Code`, Year, gdp_pcap, cons_pcap, gd1_pcap) %>%  # Select relevant columns
  drop_na() %>%  # Drop rows with NA values
  mutate(
    l_gdp_pcap = log(gdp_pcap),  # Create log of 'gdp_pcap'
    l_cons_pcap = log(cons_pcap),  # Create log of 'cons_pcap'
    l_gd1_pcap = log(gd1_pcap)  # Create log of 'gd1_pcap'
  )

# TASK 7: USING YOUR DATA 
summary_stats <- data_to_use %>%
  as_tibble() %>%  # Convert to tibble
  group_by(`Country Name`) %>%  # Group by 'Country Name'
  summarise(
    Mean = mean(gdp_pcap, na.rm = TRUE),  # Calculate mean of 'gdp_pcap'
    StdDev = sd(gdp_pcap, na.rm = TRUE),  # Calculate standard deviation of 'gdp_pcap'
    Observations = n()  # Count the number of observations
  ) %>%
  huxtable::hux() %>%  # Convert to huxtable
  huxtable::set_caption("Table 1: Descriptive Statistics of GDP per Capita by Country")  # Set table caption

print(summary_stats)  # Print the summary statistics table

# Step 2: Run a regression model on the US only 
us_data <- data_to_use %>% filter(`Country Name` == "United States")  # Filter data for the United States

# 2.1 Regression: l_gdp_pcap ~ Year
model_2_1 <- feols(l_gdp_pcap ~ Year, data = us_data)  # Run regression model

# 2.2 Regression: l_gdp_pcap ~ Year + lag(l_gdp_pcap, 1)
model_2_2 <- feols(l_gdp_pcap ~ Year + lag(l_gdp_pcap, 1), data = us_data)  # Run regression model with lagged variable

# 2.3 Regression: diff(l_gdp_pcap, differences = 1) ~ Year + lag(l_gdp_pcap, 1)
us_data <- us_data %>%
  mutate(
    diff_l_gdp_pcap = c(NA, diff(l_gdp_pcap, differences = 1)),  # Create differenced variable
    lag_l_gdp_pcap = lag(l_gdp_pcap, 1)  # Create lagged variable
  )

model_2_3 <- feols(diff_l_gdp_pcap ~ Year + lag_l_gdp_pcap, data = us_data)  # Run regression model with differenced variable

# 2.4 Combine all tables into a huxreg
combined_models <- huxreg(
  "(1)" = model_2_1,  # First model
  "(2)" = model_2_2,  # Second model
  "(3)" = model_2_3,  # Third model
  coefs = c(
    "Intercept" = "(Intercept)",  # Rename coefficients
    "Year" = "Year",
    "Lagged GDP per Capita" = "lag_l_gdp_pcap"
  ),
  statistics = c("N" = "nobs", "R2" = "r.squared", "Adj. R2" = "adj.r.squared", "logLik", "AIC")  # Include statistics
) %>%
  set_bold(1, everywhere, TRUE) %>%  # Set bold for the first row
  set_bottom_border(1, everywhere, 1) %>%  # Set bottom border for the first row
  set_caption("Table 2: Regression Models Summary") %>%  # Set table caption
  set_width(1.0)  # Set table width

print(combined_models)  # Print the combined models table

# Test for stationarity using the Augmented Dickey-Fuller test
adf_test <- urca::ur.df(us_data$l_gdp_pcap, type = "drift", selectlags = "AIC")  # Perform ADF test
adf_summary <- summary(adf_test)  # Get summary of ADF test

stationarity_result <- if (adf_test@teststat[1] < adf_test@cval[1, "5pct"]) {
  "Stationary"  # Check if the series is stationary
} else {
  "Not Stationary"
}

adf_table <- hux(
  Statistic.tau2 = adf_summary@teststat[1],  # ADF test statistic
  Statistic.phi1 = adf_summary@teststat[2],  # ADF test statistic
  `Critical Value (1%)` = adf_summary@cval[1, "1pct"],  # Critical value at 1%
  `Critical Value (5%)` = adf_summary@cval[1, "5pct"],  # Critical value at 5%
  `Critical Value (10%)` = adf_summary@cval[1, "10pct"]  # Critical value at 10%
) %>%
  rbind(
    hux("Table 3: Augmented Dickey-Fuller Test Results", "", "", "", "") %>%  # Add table caption
      set_bold(1, everywhere, TRUE) %>%  # Set bold for the first row
      set_align(1, everywhere, "center")  # Center align the first row
  ) %>%
  rbind(
    hux("", paste0("The series is ", stationarity_result, 
                   " because the test statistic ", 
                   if (stationarity_result == "Stationary") "is less than" else "is not less than", 
                   " the 5% critical value."), "", "", "") %>%  # Add interpretation
      set_font(1, everywhere, "italic")  # Set italic font for the interpretation
  )

print(adf_table)  # Print the ADF test results table

if (stationarity_result == "Stationary") {
  cat("The series is stationary because the test statistic is less than the 5% critical value.\n")  # Print result
} else {
  cat("The series is not stationary because the test statistic is not less than the 5% critical value.\n")  # Print result
}
