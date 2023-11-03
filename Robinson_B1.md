Robinson_B1
================
Daniel
2023-11-02

### Excercise 1 and 2: Building a Function and Documenting Function with roxygen

In my thesis data, I have several categories of repeatedly sampled field
observations. For example, I recorded canopy density at the same 50
locations every week over the course of 4 months. For one of my thesis
chapters, I assess spatial differences in forest structure, and am not
really interested in the week to week change in data, as much as the
site to site differences. For this chapter I need to average the data
for each location across each observation period (i.e. I average all 12
observations of canopy cover at fixed point = A from Jan to Apr to get a
seasonal average of canopy cover for point A). I will perform this
operation on several different variables at various sample sizes and
with varying observation amounts, but with the same grouping structure.
**In this project, I will make a function consisting of the dplyr
commands necessary to group and summarize repeated measure data at
different sample sizes and different observation scales.**

``` r
#' Average repeated observations: this function groups data = data by a categorical group variable = group_var to determine the average of a given numeric variable = measure_var
#'
#' @param data a dataframe or tibble (I designited this param as "data" as that seems to be industry standard)
#' @param group_var a categorical variable (I designated this param "group_var" as opposed to var1 or something less descriptive as group_var helps to clarify the variables type as categorical and its role in the function)
#' @param measure_var a numeric variable (I designated this param "measure_var" as opposed to var2 or something less descriptive as measure_var helps to clarify the variables type as numeric and its role in the function)
#'
#' @return a dataframe or tibble of measured variable averages grouped by group variable
#' @export
#'
#' @examples
avg_obs <- function(data, group_var, measure_var, na.rm = TRUE, ...) { #added na.rm = TRUE to ignore NA as the default
  result <- data %>%
    group_by({{ group_var }}) %>%  # Group the data by the specified variable
    summarize(mean_value = mean({{ measure_var }}, na.rm = na.rm, ...))  # Calculate the mean of the specified measure variable
  
  return(result)
}
```

### Excercise 3: Examples from Mock Dataframes and Penguins Dataset

We will run a few examples with simple mock dataframes to check the
function by eye.

``` r
#Creating sample dataframe 
data <- data.frame(
  id = c(1, 1, 2, 2, 3, 3), #Sample categorical variable for group_var
  value = c(10, 15, 20, 25, 30, 35) #Sample numeric variable for measure_var
)

result <- avg_obs(data, group_var = id, measure_var = value)
result
```

    ## # A tibble: 3 × 2
    ##      id mean_value
    ##   <dbl>      <dbl>
    ## 1     1       12.5
    ## 2     2       22.5
    ## 3     3       32.5

``` r
#Creating sample dataframe with NAs to assess robustness
data <- data.frame(
  id = c(1, 1, 2, 2, 3, 3, 3, NA, NA), #Sample categorical variable for group_var
  value = c(10, 15, 20, 25, 30, 35, NA, NA, 40) #Sample numeric variable for measure_var
)

result <- avg_obs(data, group_var = id, measure_var = value)
result
```

    ## # A tibble: 4 × 2
    ##      id mean_value
    ##   <dbl>      <dbl>
    ## 1     1       12.5
    ## 2     2       22.5
    ## 3     3       32.5
    ## 4    NA       40

And now we will test our function on the penguins dataset.

``` r
#Checking out the penguin data 
head(penguins)
```

    ## # A tibble: 6 × 8
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           NA            NA                  NA          NA
    ## 5 Adelie  Torgersen           36.7          19.3               193        3450
    ## 6 Adelie  Torgersen           39.3          20.6               190        3650
    ## # ℹ 2 more variables: sex <fct>, year <int>

``` r
#Island and species are good candidates for out grouping variable and bill_depth_mm or any of the numeric variables can be averaged:
result <- avg_obs(penguins, species, bill_length_mm)
result
```

    ## # A tibble: 3 × 2
    ##   species   mean_value
    ##   <fct>          <dbl>
    ## 1 Adelie          38.8
    ## 2 Chinstrap       48.8
    ## 3 Gentoo          47.5

### Excercise 4: Formally Testing the Function

The function looks like its working okay with the mock tests, but now we
will write formal tests for the function using expect\_() family of
functions and test_that() from the testthat package.

``` r
test_that("Data input and output classes are correct", {
  # Sample dataframe from above
  penguins

  # Call the function from above
  result <- avg_obs(penguins, species, bill_length_mm)

  # Use expect_ family of functions to define expectations for variable class
  expect_true(is.numeric(result$mean_value))
  expect_true(is.factor(penguins$species))
  expect_true(is.numeric(penguins$bill_length_mm))
})
```

    ## Test passed 🥇

Now we will run a test to make sure our mean values are making sense
given out original data.

``` r
#Use expect_equal to test that the number of rows matches number of species and test that the mean values of the result make sense given the values from the original data 
test_that("Output makes sense numerically", {
  expect_equal(nrow(result), n_distinct(penguins$species))  # Expect the number of rows in the result to match the number of species
  for (species_group in unique(penguins$species)) {   # Creating loop to check that mean_value of result is less than the maximum value for each group
    max_value_group <- max(penguins$bill_length_mm[penguins$species == species_group], na.rm = TRUE)
    expect_true(all(result$mean_value[result$species == species_group] < max_value_group))
  }
})
```

    ## Test passed 🥳

Lastly, we will run an easy test just to makes sure there are no NAs in
mean_value data

``` r
test_that("No NAs in mean_value", {
  expect_false(anyNA(result$mean_value))
})
```

    ## Test passed 🥳
