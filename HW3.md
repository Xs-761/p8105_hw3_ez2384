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
  participants less than 21 yrs and those with missing demographic data.
  Encoded dataset so that all columns in the demographics dataset are
  converted into character type.

``` r
  demographics =  read.csv("../../Datasets/participant_demographics.csv", skip=4) %>% janitor::clean_names() %>% filter(age>=21) %>%
                  mutate(across(seqn:education, as.character)) %>%  
                  mutate(sex=recode(sex, "1"="male", "2"="female"), 
                         education=recode(education, "1"="lower than High School", "2"="equivalent to High School", "3"="above High School")) %>%                   rename(id=seqn) %>% drop_na()

  accelerometers= read.csv("../../Datasets/accelerometers.csv") %>% janitor::clean_names() %>% rename(id=seqn) %>% mutate(id=as.character(id))
  
  merged = left_join(demographics, accelerometers, "id")
```

- Produce a reader-friendly table for the number of men and women in
  each education category create a visualization of the age
  distributions for men and women in each education category Comment on
  these items.

``` r
  # Table
  table_by_education =  demographics %>% group_by(education, sex) %>% count(name="count")
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
  barplot = ggplot(demographics, aes(x = sex, fill = education)) +
            geom_bar(position = "dodge") +
            labs(title = "Distribution of Sex by Education Level", x = "Sex", y = "Count", fill = "Education Level") +
            theme_light() + theme(plot.title=element_text(hjust=0.5))
  barplot
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

``` r
  # Aggregated Plot
  aggregated = merged %>% mutate(total_minutes=rowSums(select(.,starts_with("min")))) %>% 
                          relocate("id", "sex", "age", "bmi", "education", "total_minutes")
  
  # Plot Total_Minutes against Age
  scatterplot_total = aggregated %>% mutate(age=as.integer(age)) %>%
                      ggplot(., mapping=aes(x=age, y=total_minutes, color=sex)) + geom_point(na.rm=TRUE, size=1) + theme_light() +
                      ggtitle("Scatterplot of Age against Total Minutes") + xlab("Age") + ylab("Total Minutes")+
                      scale_x_continuous(expand=c(0,0), limits=c(0,100)) + theme(plot.title = element_text(hjust=0.5)) + geom_smooth() +
                      facet_grid( . ~ education )
  scatterplot_total
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](HW3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
  # Plot 24-H Minutes against Age
  scatterplot_24h = merged %>% mutate(age=as.integer(age)) %>%
                    ggplot(., mapping=aes(x=age, y=))
```

- From the scatterplot we can see that
  - Overall, participants have lower total minutes as age increases,
    regardless of their sex and education levels.
  - Overall, males tend to have higher total minutes compared to
    females, regardless of their age and education levels.
  - For those with education level above high school, the total minutes
    for subjects is relatively smooth and even. For those with education
    level equivalent to high school, the total minutes for subjects has
    a peak around middle ages. For those with education level lower than
    high school, the total minutes for subjects drops for all age
    intervals as age increases.
- Make a three-panel plot that shows the 24-hour activity time courses
  for each education level and use color to indicate sex. Describe in
  words any patterns or conclusions you can make based on this graph;
  including smooth trends may help identify differences.

### Problem 3

- Import, clean, and tidy these data, and describe the resulting
  dataset.

``` r
  city_bike1 = read.csv("../../Datasets/CityBike/CityBike_Jan2020.csv") %>% janitor::clean_names()
  city_bike2 = read.csv("../../Datasets/CityBike/CityBike_July2020.csv") %>% janitor::clean_names()
  city_bike3 = read.csv("../../Datasets/CityBike/CityBike_Jan2020.csv") %>% janitor::clean_names()
  city_bike4 = read.csv("../../Datasets/CityBike/CityBike_July2024.csv") %>% janitor::clean_names()
  
  head(city_bike1)
```

    ##            ride_id rideable_type  weekdays  duration         start_station_name
    ## 1 4BE06CB33B037044  classic_bike   Tuesday 15.333267     Columbus Ave & W 95 St
    ## 2 26886E034974493B  classic_bike Wednesday  5.309467            2 Ave & E 96 St
    ## 3 24DC56060EBE6260  classic_bike    Friday  9.691800 Columbia St & Rivington St
    ## 4 EEDC1053582D02E5  classic_bike    Sunday  6.996183     W 84 St & Columbus Ave
    ## 5 2CD4BD4CEE2E50A9  classic_bike    Friday  2.849500     Forsyth St & Broome St
    ## 6 E18682F9A4E501BB  classic_bike    Sunday 25.523467       Allen St & Hester St
    ##           end_station_name member_casual
    ## 1    E 53 St & Madison Ave        member
    ## 2         1 Ave & E 110 St        member
    ## 3  Grand St & Elizabeth St        member
    ## 4   Columbus Ave & W 72 St        member
    ## 5  Suffolk St & Stanton St        member
    ## 6 Atlantic Ave & Furman St        member

``` r
  head(city_bike2)
```

    ##            ride_id rideable_type  weekdays  duration
    ## 1 A7503F194A7CB244  classic_bike    Sunday  9.862550
    ## 2 B47EBE0EA71E3275  classic_bike    Monday  8.289867
    ## 3 8146F6C6855338C8  classic_bike Wednesday  5.390200
    ## 4 D49560E3308D2128  classic_bike  Saturday 19.203617
    ## 5 87687AAE400824DE  classic_bike   Tuesday 26.420533
    ## 6 E30DFCD98462C9F9  classic_bike    Sunday 51.902067
    ##            start_station_name                 end_station_name member_casual
    ## 1  Franklin Ave & Empire Blvd Grand Army Plaza & Plaza St West        member
    ## 2             E 33 St & 1 Ave                  E 33 St & 5 Ave        member
    ## 3      George St & Wilson Ave     Willoughby Ave & Wyckoff Ave        member
    ## 4 St. Nicholas Ave & W 126 St            Willis Ave & E 143 St        member
    ## 5           Front St & Jay St          Grand St & Elizabeth St        member
    ## 6   Clinton St & Joralemon St           Myrtle Ave & Linden St        casual

``` r
  head(city_bike3)
```

    ##            ride_id rideable_type  weekdays  duration         start_station_name
    ## 1 4BE06CB33B037044  classic_bike   Tuesday 15.333267     Columbus Ave & W 95 St
    ## 2 26886E034974493B  classic_bike Wednesday  5.309467            2 Ave & E 96 St
    ## 3 24DC56060EBE6260  classic_bike    Friday  9.691800 Columbia St & Rivington St
    ## 4 EEDC1053582D02E5  classic_bike    Sunday  6.996183     W 84 St & Columbus Ave
    ## 5 2CD4BD4CEE2E50A9  classic_bike    Friday  2.849500     Forsyth St & Broome St
    ## 6 E18682F9A4E501BB  classic_bike    Sunday 25.523467       Allen St & Hester St
    ##           end_station_name member_casual
    ## 1    E 53 St & Madison Ave        member
    ## 2         1 Ave & E 110 St        member
    ## 3  Grand St & Elizabeth St        member
    ## 4   Columbus Ave & W 72 St        member
    ## 5  Suffolk St & Stanton St        member
    ## 6 Atlantic Ave & Furman St        member

``` r
  head(city_bike4)
```

    ##            ride_id rideable_type  weekdays  duration      start_station_name
    ## 1 86AE148E36FBF035  classic_bike    Sunday 19.661183            Picnic Point
    ## 2 FCF07A30F66B9B07 electric_bike  Thursday  7.676433         W 54 St & 9 Ave
    ## 3 D8397E843C06644D  classic_bike  Thursday 24.465950        12 Ave & W 40 St
    ## 4 E575690C13424E8C electric_bike   Tuesday  3.528600 Grand St & Havemeyer St
    ## 5 184AABED46DCE11A electric_bike Wednesday 24.126050     Broadway & Kent Ave
    ## 6 ACA61A92B5EA0D11  classic_bike  Saturday  7.825750          E 1 St & 1 Ave
    ##          end_station_name member_casual
    ## 1   Yankee Ferry Terminal        casual
    ## 2         W 42 St & 8 Ave        casual
    ## 3 W 84 St & Amsterdam Ave        member
    ## 4      S 4 St & Rodney St        member
    ## 5    Henry St & Degraw St        casual
    ## 6   Mercer St & Spring St        member

- Description of CityBike Datasets

- Produce a reader-friendly table showing the total number of rides in
  each combination of year and month separating casual riders and Citi
  Bike members. Comment on these results.

  Make a table showing the 5 most popular starting stations for July
  2024; include the number of rides originating from these stations.

  Make a plot to investigate the effects of day of the week, month, and
  year on median ride duration. This plot can include one or more
  panels, but should facilitate comparison across all variables of
  interest. Comment on your observations from this plot.

  There were relatively few electric Citi Bikes in 2020, but many more
  are available now. For data in 2024, make a figure that shows the
  impact of month, membership status, and bike type on the distribution
  of ride duration. Comment on your results.
