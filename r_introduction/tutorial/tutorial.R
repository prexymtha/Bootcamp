# Clear environment
rm(list = ls())

# Setup ---------------------------------------------------------------

# Observe the location of your working directory, the location of your project
getwd()
#setwd("./tutorial")

# Packages ----------------------------------------------------------------

# First install the package from CRAN
install.packages("pacman")

# Load the installed package into your workspace
library(pacman)

# From CRAN
p_load(fixest, tidyverse, huxtable, modelsummary, glue, skimr, MetBrewer, devtools, labelled) # or
pacman::p_load(fixest, tidyverse, huxtable, modelsummary, glue, skimr, MetBrewer, devtools, labelled)

# Environment -------------------------------------------------------------
  # object name <- (or =) value(s)
    a <- 10
    hello <- "Hello world!"
    test <- TRUE

  # Determine the class of an object
    class(a)
    class(hello)
    class(test)

  # Report these variables in your output by running the following:
    a
  # or
    print(hello)
  # or with the glue package for something more fancy
    glue::glue("It's {test}. I saved a variable which contains {hello} and I stored the number {a}.")

# Arrays ------------------------------------------------------------------
  # An array object is equivalent to a vector of values from the same class.
  # Arrays can be created by concatenating values using the function `c()`.
    x <- c(1, 2, 3, 4)
    y <- c(4, 5, 6, 7)
    z <- c(7, 8, 9, 10)

  # Useful functions to perform on arrays/vectors
    sum(x)
    min(x)
    median(x)

  # summary() provides a summary of the functions above
    summary(x)

  # Missing values denoted by NA
    x_with_missing <- c(1, 2, 3, NA)

  # Take care to properly treat missing values:
    # This function returns an NA because a missing number is included
      sum(x_with_missing)
    # This function does not, the na.rm=T - says: remove any value for which na is tru.
      sum(x_with_missing, na.rm = T)

# Data frames -------------------------------------------------------------

  # data.frame() can create columns from arrays and assign column names
    # Here we create a data frame, we give names to each array
    # It is generally good practice to keep your variable names consistent
      # I prefer using underscores, for example an expenditure variable on wages will be x_wages
      # whereas expenditure on electricity would be x_electricity
      # When working with large scale data, this simplifies variable search
      # as I only need to return variables with the prefix [x_] should I be looking
      # for expenditure data

      df_1 <- data.frame(A = x, B = y, C = z)

  # Some useful operations
    # as expected, the below provides the column names
      colnames(df_1)
    # if you want to create a dataset that looks the same as the previous one but with a new name (and taking up different memory):
      df_1_copy <- df_1
    # If you want to change the names of your variables
      colnames(df_1_copy) <- c("col1", "col2", "col3")
      colnames(df_1_copy)
  # To count the number of rows and columns
  #(sometimes you'll need this if you do  manual transformation)
    nrow(df_1)
    ncol(df_1)

  # Return column "A" as a vector
    df_1$A

  # df_1[row no., column no.] - empty implies all (before the comma)
   df_1[, 1]

  # Using tidyverse's pipe operator %>%
    # the below pulls all value from the column A
    # it does not replace the values of df_1 with column A
      df_1 %>% pull(A)
    # to do that we can write
      col_A <- df_1 %>% pull(A)
    # Note that tideverse works with tibble's instead of dataframe,
    # we'll elaborate on the difference as the course goes on.
    # for now, the main advantages are more informative errors

  # Similarly with rows
    # Return row 2 as a single row data frame
      df_1[2, ]

  # Return row 2-3 as a two row data frame
    df_1[2:3, ]

  # Return cell in row 2 column 1
    df_1[2, 1]

  # Create a new column "D" that is the sum of A and B
    df_1$D <- df_1$A + df_1$B

  # in tidyverse the code is:
    # The mutate function is where we do our data manipulation
    df_1 <- df_1 %>% mutate(D = A + B)


  # Conditional selection:
    # you can permanently remove missing data from a dataframe using the na.omit code below
      df_missing <- data.frame(A = x_with_missing, B = y, C = z)

      df_missing_clean <- na.omit(df_missing)
    # count the rows and columns, you will see there is one less row now
      nrow(df_missing_clean)
      ncol(df_missing_clean)

    # You can also remove rows or columns with specific conditions
    # if we want to drop all rows if the the column value in C is above 9:
      df_missing_9 <- df_missing[df_missing$C < 9, ]

    # count the rows and columns, you will see there is one less row now
      nrow(df_missing_9)
      ncol(df_missing_9)


# Managing data, directories, and importing ------------------------------------------------
  ####################################################
  # From here some of the comments become long.      #
  # I'll keep all code on the same whitespace level  #
  # unless they are part of a program or looping     #
  # structure                                        #
  ####################################################
  # note, your present working directory matters here
  getwd()

  # Reading data ###############################################################

  ire_energy <- read.csv(file = "../data/Ireland_energy.csv", header = TRUE)
    # The "file" argument refers to the relative file path from your root directory
    # The "header" argument is set to true because the .csv file contains column headings

  ire_pop <- read.csv("../data/Ireland_population.csv")

  # Sometimes it's unnecessary to spell out the arguments
    # in the read.csv file section, we specified the file and header section
    # in the head argument below we print the first 3 rows
  head(ire_energy,n=3)
    # We don't always need this:
  head(ire_energy, 3) # same as ire_energy[1:3, ]
  tail(ire_pop, 5) # same as ire_pop[(nrow(ire_pop) - 4): nrow(ire_pop), ]

    # skim() can provide useful overviews of data frames

  skimr::skim(ire_pop)


# Manipulating dataframes -------------------------------------------------------
    # We have population data and energy data, now we will merge the data
    # to examine both together
    # The merge command as below, where x and y are the dataframe names that we imported
  ireland_df <- merge(x = ire_energy, y = ire_pop, by.x = "Year", by.y = "Year")
    # merge() merges data frames x and y on the basis of some column
    # by.x for x's column and by.y for y's column that we merge on.
    # If both exist in the two datasets we can write:
  ireland_df_a <- merge(x = ire_energy, y = ire_pop, by = c("Year"))
    # As you can see both are the same
  skimr::skim(ireland_df)
  skimr::skim(ireland_df_a)
    # Dropping a dataframe:
  rm(ireland_df_a)
    # Let's say year in the one table is called data_year instead, we can rename a variable as below, lets copy the
    # load the df first:
  irepop_difname <- ire_pop
    # Change the column name
  colnames(irepop_difname)[colnames(irepop_difname) == "Year"] <- "data_year"
    # We can still merge the data, but now we have to change the by.y variable
  ireland_df_a <- merge(x = ire_energy, y = irepop_difname, by.x = "Year", by.y = "data_year")
    # As you can see both are the same
  skimr::skim(ireland_df)
  skimr::skim(ireland_df_a)


  # Transformations:

  ireland_df <- ireland_df %>%
    mutate(ln_energy_pc = log(GJ / Population))
      # You should recognise mutate() from before
      # log()'s default setting implies natural logarithmic transformation (ln)

    # Instead of tidyverse piping, you could have done this:
  ireland_df <- mutate(.data = ireland_df, ln_energy_pc = log(GJ / Population))

    # But piping is more useful when you require multiple consecutive operations
    # For example, the read, merge, and mutate commands we've done thus far could've been condensed:
  ireland_df <- read.csv("../data/Ireland_energy.csv") %>%                # Read CSV for ireland energy from the subfolder (as I am in code-in-r, replace with single dot if fails)
    merge(
      x = ., # full stop represents the result of all previous operations
      y = read.csv("../data/Ireland_population.csv"),
      by.x = "Year",
      by.y = "Year"
    ) %>%
    mutate(ln_energy_pc = log(GJ / Population))

    # Changing type of the variable:
    # `ireland_df`'s `Year` column is of the class `integer`.
  ireland_df$Year %>%
    class(.)

    # Transform the `Year` column from an integer to a date.
    # note that we protect the value of the variable year with {}
    # if we didn't, the glue command would change every value to the word [Year-01-01]
  ireland_df <- ireland_df %>%
    mutate(Year = glue::glue("{Year}-01-01"))
    # but the result is of class `character`:
    # sometimes the base script is faster than piping for quick commands:
  class(ireland_df$Year)

    # We want to transform the year variable to a date
    # The as.Date() below renders characters of a given format into dates
    # We set that format as: "%Y-%m-%d" means yyyy-mm-dd
  ireland_df <- ireland_df %>%
    mutate(Year = as.Date(x = Year, format = "%Y-%m-%d"))

    # Finally it is in date format
  ireland_df$Year %>%
    class(.)

    # Confirm that data is chronological
  ireland_df <- ireland_df %>%
    arrange(Year) # this is equivalent to sorting by Year

    # Let's see what our new data frame `ireland_df` looks like.
  #view(ireland_df)

    # Create a table using the huxtable package - observations after 2013
      # a huxtable package is a easy to use table that makes document creation easier
      # note that even though there are gaps in the pipe
      # the fact that we continue using %>% means that it will continue with the settings

  ireland_df %>%
    filter(Year > as.Date("2013-01-01")) %>%
      # subsets data for entries after 2013

    as_hux() %>%
      # or huxtable::as_hux() to transform data frame into huxtable object
      # hereafter code to define certain aesthetic qualities of our table

    theme_basic() %>%
      # use a theme to make tables more presentable, e.g. theme_article() or theme_compact()

    set_number_format(col = c(2, 4), value = 2) %>%
     # set number of decimals to 2 in the 2nd and 4th column

    set_font_size(10) %>%
    set_caption("Ireland's energy consumption after 2013")


# Writing data ------------------------------------------------------------
  # We can now write this data frame back to a `.csv` file
    # Remember I am still in the parent directory, if you are in the tutorial directory, use .. before the /data part
    # remember you can check your working directory with getwd()
  ireland_df %>% # data frame to be written to csv
    write.csv(
      x = ., # ireland_df is piped into "."
      file = "../data/ireland_complete.csv", # file path and file name we choose
      row.names = TRUE
    )

# Time series analysis ----------------------------------------------------

  # Visualising time series -------------------------------------------------

  # plot()'s default is a scatterplot
  # inputting a single vector
    # note the index comes from the fact that no x axis variable has been defined
  ireland_df$ln_energy_pc %>%
    plot(.)

  # undefined x-axis
  # inputting a data frame with two columns
  ireland_df %>%
    select(Year, ln_energy_pc) %>%
    plot(.)

  # lines instead of points
  ireland_df %>%
    select(Year, ln_energy_pc) %>%
    plot(., type = "l")

  # ggplotting
    # this gives you a cleanerg graph with more options; the labs section handels labels.
  ireland_df %>%
    ggplot(aes(x = Year, y = ln_energy_pc)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    labs(
      title = "Ireland's Primary Energy Consumption",
      y = "ln(GJs Per Capita)",
      x = "Date"
    ) +
    scale_x_date(
      date_labels = "`%y",
      date_breaks = "2 year"
    ) +
    scale_y_continuous(limits = c(4, 5.5))

  # and customise as you please
    # I am not going over this procedure
    brewercolor = "Austria"
  ireland_df %>%
    ggplot(aes(x = Year, y = ln_energy_pc)) +
    theme_bw() +
    geom_point(
      aes(color = ifelse(Year < as.Date("2000-01-01"), "Before 2000",
        ifelse(Year > as.Date("2000-01-01"), "After 2000", "2000")
      )),
      size = 1.5
    ) +
    geom_line(
      alpha = 0.5,
      color = "lightgrey",
      size = 1
    ) +
    labs(
      title = "Ireland's Primary Energy Consumption",
      y = "ln(GJs Per Capita)",
      x = "Date"
    ) +
    scale_x_date(
      date_labels = "`%y",
      date_breaks = "2 year"
    ) +
    scale_y_continuous(limits = c(4, 5.5)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0),
        size = 10
      ),
      axis.title.x = element_text(
        margin = margin(t = 0, r = 0, b = 0, l = 0),
        size = 10
      ),
      axis.text.x = element_text(angle = 45),
      legend.position = "bottom",
      legend.margin = margin(t = -10, r = 0, b = 0, l = 0),
      legend.title = element_blank()
    ) +
    geom_label(
      data = . %>% filter(Year == as.Date("2000-01-01")),
      aes(label = round(ln_energy_pc, 1)),
      nudge_y = 0.15,
      size = 3,
      color = met.brewer(brewercolor, type = "discrete")[1]
    ) +
    geom_hline(aes(color = "Mean", yintercept = mean(ln_energy_pc)),
      size = 1,
      linetype = "dashed",
      show.legend = F
    ) +
    scale_color_manual(values = met.brewer(brewercolor, type = "discrete"))

  # You can also define your own colours:
    # These are part of Paul Tol's pallette
    friendly_blue <- '#0072B2'
    friendly_orange <- '#D55E00'
    friendly_green <- '#009E73'




  # Autocorrelation ---------------------------------------------------------
    # note the acf(.) command is a default package in R - it does not need to be loaded
    # remember ACF is the autocorrelation function
      # Remember mathematically the autocorrelation function plots the correlation rho_j = cov(y_t,y_{t-j})/Var(y_t)
  ireland_df %>%
    select(ln_energy_pc) %>% # isolate ln_energy_pc in data frame
    acf(
      plot = T, # create a plot
      type = "correlation"
    ) # standard ACF

    # For the partial autocorrelation
  pacf_result <- ireland_df %>%
    select(ln_energy_pc) %>%
    acf(
      plot = T,
      type = "partial"
    ) # PACF option

    # Extract PACF values
  pacf_values_function <- pacf_result$acf

    # Print the PACF values (if you want to)
  print("PACF Values:")
  print(pacf_values_function)


  # Regressions and Detrending -------------------------------------------------------------

    #-------------------------------------------
    # OLS BY HAND
    #------------------------------------------
      # Remember, OLS Coefficients are: (β̂ = (X'X)^(-1) * X'Y)
      # Load data
  y <- ireland_df$ln_energy_pc  # Dependent variable

    # Convert 'Year' column to numeric
  year_numeric <- as.numeric(format(ireland_df$Year, "%Y"))

    # Construct the matrix representing the data (including intercept)
  X <- cbind(1, year_numeric) # cbind combines the the vector of 1 to the year vector

    # Convert y to a column matrix
  y_matrix <- matrix(y, ncol = 1)

    # Step 1: Compute X'X (X transpose times X)
  XtX <- t(X) %*% X  # note that t(X) transposes the matrix X and %*% does the matrix multiplication
  print("X'X:")
  print(XtX)

    # Step 2: Compute the inverse of X'X manually using Gaussian elimination (it is really difficult to invert matrices by hand)
  XtX_inv <- solve(XtX)


    # Step 3: Compute X'Y (X transpose times Y)
  XtY <- t(X) %*% y_matrix

    # Step 4: Compute the coefficients (β̂ = (X'X)^(-1) * X'Y)
  beta_hat <- XtX_inv %*% XtY
  print("Estimated Coefficients (β̂):")
  print(beta_hat)

    # Step 5: Compute predicted values (ŷ = X * β̂)
  y_hat <- X %*% beta_hat

    # Step 6: Calculate residuals (ε = y - ŷ)
  residuals <- y_matrix - y_hat

    # ------------------------------------------
    # Run the OLS regression for a linear trend
    # ------------------------------------------

    # Extract the dependent variable
  y <- ireland_df$ln_energy_pc

    # Extract the year as a numeric variable from the 'Year' column
  year_numeric <- as.numeric(format(ireland_df$Year, "%Y"))

    # Combine into a data frame
  detrended_example <- data.frame(
    y = y,
    year = year_numeric,
    year_squared = year_numeric^2  # Add quadratic term for future use
  )
    # note that the y ~ year part is the part that specifies the trend
      # Note that the lm(.) means that we are running a linear model
  model_linear <- lm(y ~ year, data = detrended_example)

    # View the regression summary
  summary(model_linear)
    # compare to betas from matrix multiplication
  print(beta_hat)


    # Predict the trend (fitted values from the model)
  detrended_example$trend <- predict(model_linear)

    # Create the detrended series
  detrended_example$detrended_y_b <- detrended_example$y - detrended_example$trend
    # Note you can do the same with:
  detrended_example$detrended_lin <- residuals(model_linear)

    # You can confirm that the detrended series and the residuals are
    # the same thing here:
  check_detrend <- lm(detrended_lin ~ detrended_y_b, data=detrended_example)
    # As you can see this leads to exactly the same result
  summary(check_detrend)

      # ------------------------------------------
      # Run the OLS regression for a quadratic trend
      # ------------------------------------------
      # note that the y ~ year part is the part that specifies the trend
  model_quad <- lm(y ~ year + year_squared, data = detrended_example)

    # View the regression summary
  summary(model_quad)

    # Predict the trend (fitted values from the model)
  detrended_example$trend_quad <- predict(model_quad)

  detrended_example$detrended_quad <- residuals(model_quad)

    # Create the ggplot
  ggplot(detrended_example, aes(x = year)) +
    geom_line(aes(y = y, color = "Log Gigajoules per Capita"), size = 1) + # Original series
    geom_line(aes(y = trend, color = "Linear Trend"), size = 1, linetype = "solid") + # Linear trend
    geom_line(aes(y = trend_quad, color = "Quadratic Trend"), size = 1, linetype = "dashed") + # Quadratic trend
    labs(
      title = "Linear and Quadratic Trends",
      x = "Year",
      y = "Value",
      color = "Legend"
    ) +
    scale_color_manual(
      values = c("Log Gigajoules per Capita" = friendly_blue, "Linear Trend" = friendly_orange, "Quadratic Trend" = friendly_green)
    ) +
    theme_minimal() + # Clean theme
    theme(
      legend.position = "top", # Adjust legend position (e.g., "top", "bottom", "left", "right")
      legend.text = element_text(size = 10), # Adjust legend text size
      plot.title = element_text(hjust = 0.5, size = 14) # Center and size the title
    )

  # Using stored results ----------------------------------
    # Sometimes we want to use stored results
    # to View the object that we want to obtain values from
  View(model_linear)

    # We can do the below if we want to get only the values from a specific
    # part of the object - but also keep the names of the variables

  lin_year_coef = model_linear$coefficients["year"]
  print(lin_year_coef)

   # More often, if we want to do this - we'll need the coefficients as vectors
  coef_values <- as.numeric(model_linear$coefficients)
  print(coef_values)

    # I often find myself needing only specific coefficients from models in order to do the transformation of interest
    # suppose I only need the year coefficient from both equations above (as unrealistic as that is)


    # set up data-frame for coefficients:
  example_coefs <- data.frame(model_type = character(), year_coef = numeric(), stringsAsFactors = FALSE)

    # run the loop over model names
      # note when running large scale data on sub_groups of different sizes
      # it is often better to dynamically populate this
      # with files stored on csv or txt data.
      # This especially counts for secure data facilities.

  for (model_name in c("model_linear","model_quad")) {
    model <- get(model_name)
    coef_value <- model$coefficients["year"]  # Extract the coefficient of 'year'

      # Handle cases where 'year' might not exist in the model
    if (is.na(coef_value)) {
      coef_value <- NA
    }

      # Append to results data frame
    example_coefs <- rbind(example_coefs, data.frame(
      model_type = sub("model_", "", model_name),  # Extract suffix
      year_coef = coef_value
    ))
  }

  # Print results
  print(example_coefs)


# Unit root tests ---------------------------------------------------------

    # load the urca package
  p_load(urca)

    # ur.df() requires a vector/array
    # you should recognise pull() from before
  test_vector <- ireland_df %>%
    pull(ln_energy_pc)

  my_adf1 <- ur.df(
    y = test_vector, # vector
    type = "trend", # type  of ADF - trend + constant
    lags = 5, # max number of lags
    selectlags = "AIC"
  ) # lag selection criteria

    # use summary() to present the saved ADF object
    # summary() wraps many different kinds of objects
  summary(my_adf1)


  # what about using drift only?

  my_adf2 <- ur.df(
    y = test_vector,
    type = "drift", # type  of ADF - with drift
    lags = 5,
    selectlags = "AIC"
  )
  summary(my_adf2)



#--------------------------------------------------------------------------
# Cross section analysis --------------------------------------------------
#---------------------------------------------------------------------------
    # Replace a local file path with a web address
    # Subset the data to only those observations in 1974
    # To restrict memory usage, select only the relevant columns
  cs_df <- read.csv("https://raw.githubusercontent.com/stata2r/stata2r.github.io/main/data/cps_long.csv") %>%
    filter(year == 1974) %>%
    select(wage, educ, age, marr)

# Descriptive statistics --------------------------------------------------

    # Get an overview of the sample
    # Do you notice any issues?
  skimr::skim(cs_df)

    # Get an impression of wage by marital status
  cs_df %>%                 # call the cross-section dataframe
    select(wage, marr) %>%  # Select the variables wage and marriage from the dataframe
    group_by(marr) %>%      # group the selected variables from the dataframe
    skimr::skim()           # skim the data grouped by mariage limiting only to the selected variables from the dataframe




# Regressions -------------------------------------------------------------

    # There are some zero wages, but we should probably log wages in any case:
  cs_df <- cs_df %>% mutate(lwage = log(wage))

    # Our first model
  model1 <- feols(fml = lwage ~ educ, data = cs_df)

    # Adding an explanatory continuous variable: age
  model2 <- feols(lwage ~ educ + age, cs_df)

    # Adding a categorical variable
  model3 <- feols(lwage ~ educ + age + factor(marr), cs_df)

    # As before, use summary() to display the results of model1
  summary(model1)

# Visualising results -----------------------------------------------------

    # we can display the results with huxreg
  huxreg(model1, model2, model3)

    # but sometimes we want to make some aesthetic adjustments
  huxreg(
    "Model 1" = model1, "Model 2" = model2, "Model 3" = model3,
    statistics = c("N" = "nobs", "R-squared" = "r.squared"),
    stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01, `****` = 0.001),
    number_format = 2,
    coefs = c(
      "Education" = "educ",
      "Age" = "age",
      "Married" = "factor(marr)1"
    )
  ) %>%
    set_font_size(8) %>%
    set_caption("Regression of Log Wages on Education, Age, and Marital Status")

    # In fixest; Notice that models need to be entered as a list() object
    # Generate the coefficient plot with explicit inclusion of desired coefficients

  coefplot(list(model1, model2, model3), drop = "Constant")


# Interaction effects -----------------------------------------------------
   # Same as before, but "*" denotes an interaction
  model4 <- feols(lwage ~ educ * factor(marr), data = cs_df)

   # What do our results say?
  summary(model4)
    # maybe this is easier:
  huxreg(model4)


    # plot the interaction effects
  cs_df %>%
    ggplot(aes(x = educ, y = lwage)) +
    theme_bw() +
    geom_point(alpha = 0.5) + # creates a scatterplot
    geom_smooth(
      formula = y ~ x, # x, y inherited from aes()
      method = "lm", # specifies linear model
      aes(color = factor(marr)), # creates two regression lines
      se = TRUE, # display confidence interval,
      level = 0.95
    ) + # confidence level to 95%
    theme(legend.position = "bottom") +
    labs(
      y = "Wage", x = "Years of Education", color = "Married",
      title = "The effect of education on wage by marital status"
    )
