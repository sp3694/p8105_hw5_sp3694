hw5
================
Stephen Powers
11/8/2019

## Problem 1

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

``` r
fill_missing = function(x) {
  if (is.numeric(x)) 
    {replace_na(x, mean(x, na.rm = TRUE))} 
  else if (is.character(x)) 
    {replace_na(x, "virginica")}}

iris_fill = map_df(iris_with_missing, fill_missing)
```

## Problem 2

``` r
p2_data = 
  list.files("./data/", pattern = ".csv", full.names = TRUE) %>% view
```

``` r
p2_tidy_data = 
  p2_data %>% map_df(read.csv) %>% 
  mutate(id = tools::file_path_sans_ext(basename(p2_data))) %>% view
```

## Problem 3
