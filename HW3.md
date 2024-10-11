P8105_EZ2384_HW3
================

``` r
# Load the required dataset
  library(p8105.datasets)
      data("ny_noaa")
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
```

### Problem 1

- Dataset `ny_noaa` is of size 2595176 x 7, with variables
  - **id** \[char\]: Weather station ID
  - **date** \[date\]: Date of observation
  - **prcp** \[int\] : Precipitation (tenths of mm)
  - **snow** \[int\] : Snowfall (mm)
  - **snwd** \[int\] : Snow depth (mm)
  - **tmax** \[char\]: Maximum temperature (tenths of degrees C)
  - **tmin** \[char\]: Minimum temperature (tenths of degrees C)
- Missing Data
  - `tmin` and `tmax` contains a great deal of missing values
  - `prcp` and `snow` contains some missing values
  - `id` and `date` are complete
- Data Cleaning Procedures
  - preliminary cleaning (give reasonable column names, rename column
    names to be more straightforward)
  - created new columns year,month,day
  - dropped all NA values
  - changed all column types except for id and date into integer type to
    facilitate future conversions
  - changed column values for temperature, precipitation, snowfall so
    that are given in integer units

<!-- -->

    ## # A tibble: 248 × 2
    ## # Groups:   snowfall(mm) [248]
    ##    `snowfall(mm)`   count
    ##             <int>   <int>
    ##  1              0 1112758
    ##  2             25   15809
    ##  3             13   12460
    ##  4             51    9252
    ##  5              5    5669
    ##  6              8    5380
    ##  7             76    5296
    ##  8              3    5276
    ##  9             38    5050
    ## 10            102    3386
    ## # ℹ 238 more rows

- For snowfall, the most commonly observed values are 0, which has a
  total occurrence of 1112758

### Problem 2

- Load, tidy, merge, and otherwise organize the data sets. Exclusion of
  participants less than 21 yrs and those with missing demographc data.
  Encoded dataset so that all columns converted into character type.

``` r
  accelerometers =  read.csv("../../Datasets/accelerometers.csv", skip=4) %>% janitor::clean_names() %>% filter(age>=21) %>%
                    mutate(across(seqn:education, as.character)) %>%  
                    mutate( sex=recode(sex, "1"="male", "2"="female"), 
                            education=recode(education, "1"="lower than High School", "2"="equivalent to High School", 
                                                        "3"="above High School"))  %>% drop_na()
  head(accelerometers)
```

    ##    seqn    sex age  bmi                 education
    ## 1 62161   male  22 23.3 equivalent to High School
    ## 2 62164 female  44 23.2         above High School
    ## 3 62169   male  21 20.1 equivalent to High School
    ## 4 62174   male  80 33.9         above High School
    ## 5 62177   male  51 20.1 equivalent to High School
    ## 6 62178   male  80 28.5 equivalent to High School

- Produce a reader-friendly table for the number of men and women in
  each education category create a visualization of the age
  distributions for men and women in each education category Comment on
  these items.

``` r
  # Table
  table_by_education =  accelerometers %>% group_by(education, sex) %>% count(name="count")
  table_by_education
```

    ## # A tibble: 6 × 3
    ## # Groups:   education, sex [6]
    ##   education                 sex    count
    ##   <chr>                     <chr>  <int>
    ## 1 above High School         female    59
    ## 2 above High School         male      56
    ## 3 equivalent to High School female    23
    ## 4 equivalent to High School male      35
    ## 5 lower than High School    female    28
    ## 6 lower than High School    male      27

``` r
  # Plot
  ggplot(accelerometers, aes(x = sex, fill = education)) +
    geom_bar(position = "dodge") +
    labs(title = "Distribution of Sex by Education Level",
         x = "Sex",
         y = "Count",
         fill = "Education Level") +
    theme_light()
```

![](HW3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

- From the bar graph we can see that

  - Females have higher counts of education level above high school
  - Males have an education level more evenly distributed

- Using your tidied dataset, aggregate across minutes to create a total
  activity variable for each participant. Plot these total
  activities(y-axis) against age(x-axis). Your plot should compare men
  to women and have separate panels for each education level. Include a
  trend line or a smooth line to illustrate differences. Comment on your
  plot.

- Make a three-panel plot that shows the 24-hour activity time courses
  for each education level and use color to indicate sex. Describe in
  words any patterns or conclusions you can make based on this graph;
  including smooth trends may help identify differences.

### Problem 3
