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
  city_bike1 = read.csv("../../Datasets/CityBike/CityBike_Jan2020.csv") %>% janitor::clean_names() %>% mutate("year"="2020", "month"="01")  %>%
               relocate("ride_id", "member_casual", "year", "month", "weekdays", "rideable_type", "start_station_name", "end_station_name")  %>%
               rename("id"="ride_id", "type"="member_casual", "bike"="rideable_type", "start_station"="start_station_name", 
                      "end_station"="end_station_name")
  city_bike2 = read.csv("../../Datasets/CityBike/CityBike_July2020.csv") %>% janitor::clean_names() %>% mutate("year"="2020", "month"="07")  %>%
               relocate("ride_id", "member_casual", "year", "month", "weekdays", "rideable_type", "start_station_name", "end_station_name")  %>%
               rename("id"="ride_id", "type"="member_casual", "bike"="rideable_type", "start_station"="start_station_name", 
                      "end_station"="end_station_name")
  city_bike3 = read.csv("../../Datasets/CityBike/CityBike_Jan2024.csv") %>% janitor::clean_names() %>% mutate("year"="2024", "month"="01")  %>%
               relocate("ride_id", "member_casual", "year", "month", "weekdays", "rideable_type", "start_station_name", "end_station_name")  %>%
               rename("id"="ride_id", "type"="member_casual", "bike"="rideable_type", "start_station"="start_station_name", 
                      "end_station"="end_station_name")
  city_bike4 = read.csv("../../Datasets/CityBike/CityBike_July2024.csv") %>% janitor::clean_names() %>% mutate("year"="2024", "month"="07")  %>%
               relocate("ride_id", "member_casual", "year", "month", "weekdays", "rideable_type", "start_station_name", "end_station_name")  %>%
               rename("id"="ride_id", "type"="member_casual", "bike"="rideable_type", "start_station"="start_station_name", 
                      "end_station"="end_station_name")
  
  head(city_bike1)
```

    ##                 id   type year month  weekdays         bike
    ## 1 4BE06CB33B037044 member 2020    01   Tuesday classic_bike
    ## 2 26886E034974493B member 2020    01 Wednesday classic_bike
    ## 3 24DC56060EBE6260 member 2020    01    Friday classic_bike
    ## 4 EEDC1053582D02E5 member 2020    01    Sunday classic_bike
    ## 5 2CD4BD4CEE2E50A9 member 2020    01    Friday classic_bike
    ## 6 E18682F9A4E501BB member 2020    01    Sunday classic_bike
    ##                start_station              end_station  duration
    ## 1     Columbus Ave & W 95 St    E 53 St & Madison Ave 15.333267
    ## 2            2 Ave & E 96 St         1 Ave & E 110 St  5.309467
    ## 3 Columbia St & Rivington St  Grand St & Elizabeth St  9.691800
    ## 4     W 84 St & Columbus Ave   Columbus Ave & W 72 St  6.996183
    ## 5     Forsyth St & Broome St  Suffolk St & Stanton St  2.849500
    ## 6       Allen St & Hester St Atlantic Ave & Furman St 25.523467

``` r
  head(city_bike2)
```

    ##                 id   type year month  weekdays         bike
    ## 1 A7503F194A7CB244 member 2020    07    Sunday classic_bike
    ## 2 B47EBE0EA71E3275 member 2020    07    Monday classic_bike
    ## 3 8146F6C6855338C8 member 2020    07 Wednesday classic_bike
    ## 4 D49560E3308D2128 member 2020    07  Saturday classic_bike
    ## 5 87687AAE400824DE member 2020    07   Tuesday classic_bike
    ## 6 E30DFCD98462C9F9 casual 2020    07    Sunday classic_bike
    ##                 start_station                      end_station  duration
    ## 1  Franklin Ave & Empire Blvd Grand Army Plaza & Plaza St West  9.862550
    ## 2             E 33 St & 1 Ave                  E 33 St & 5 Ave  8.289867
    ## 3      George St & Wilson Ave     Willoughby Ave & Wyckoff Ave  5.390200
    ## 4 St. Nicholas Ave & W 126 St            Willis Ave & E 143 St 19.203617
    ## 5           Front St & Jay St          Grand St & Elizabeth St 26.420533
    ## 6   Clinton St & Joralemon St           Myrtle Ave & Linden St 51.902067

``` r
  head(city_bike3)
```

    ##                 id   type year month  weekdays          bike
    ## 1 644A0105ACA27B15 member 2024    01 Wednesday electric_bike
    ## 2 A5A8C0AD18EDA2C0 member 2024    01 Wednesday electric_bike
    ## 3 B392CE3496831A89 member 2024    01   Tuesday electric_bike
    ## 4 33756EDC77800B6A member 2024    01 Wednesday electric_bike
    ## 5 29D9AF64D6593D9B member 2024    01  Thursday electric_bike
    ## 6 C7E61191A30649D5 member 2024    01    Sunday electric_bike
    ##                start_station                   end_station  duration
    ## 1   Lafayette St & Jersey St               W 50 St & 9 Ave 16.472933
    ## 2    Clinton St & Tillary St   Duffield St & Willoughby St  6.294983
    ## 3     West End Ave & W 94 St           W 116 St & Broadway  6.119750
    ## 4    Grand St & Elizabeth St             Front St & Jay St 10.875667
    ## 5           12 Ave & W 40 St Washington St & Gansevoort St  8.416267
    ## 6 7 Ave & Central Park South           44 Dr & Jackson Ave 18.540983

``` r
  head(city_bike4)
```

    ##                 id   type year month  weekdays          bike
    ## 1 86AE148E36FBF035 casual 2024    07    Sunday  classic_bike
    ## 2 FCF07A30F66B9B07 casual 2024    07  Thursday electric_bike
    ## 3 D8397E843C06644D member 2024    07  Thursday  classic_bike
    ## 4 E575690C13424E8C member 2024    07   Tuesday electric_bike
    ## 5 184AABED46DCE11A casual 2024    07 Wednesday electric_bike
    ## 6 ACA61A92B5EA0D11 member 2024    07  Saturday  classic_bike
    ##             start_station             end_station  duration
    ## 1            Picnic Point   Yankee Ferry Terminal 19.661183
    ## 2         W 54 St & 9 Ave         W 42 St & 8 Ave  7.676433
    ## 3        12 Ave & W 40 St W 84 St & Amsterdam Ave 24.465950
    ## 4 Grand St & Havemeyer St      S 4 St & Rodney St  3.528600
    ## 5     Broadway & Kent Ave    Henry St & Degraw St 24.126050
    ## 6          E 1 St & 1 Ave   Mercer St & Spring St  7.825750

``` r
  city_bike_binded = bind_rows(city_bike1,city_bike2,city_bike3,city_bike4) %>% arrange("id")
  head(city_bike_binded)
```

    ##                 id   type year month  weekdays         bike
    ## 1 4BE06CB33B037044 member 2020    01   Tuesday classic_bike
    ## 2 26886E034974493B member 2020    01 Wednesday classic_bike
    ## 3 24DC56060EBE6260 member 2020    01    Friday classic_bike
    ## 4 EEDC1053582D02E5 member 2020    01    Sunday classic_bike
    ## 5 2CD4BD4CEE2E50A9 member 2020    01    Friday classic_bike
    ## 6 E18682F9A4E501BB member 2020    01    Sunday classic_bike
    ##                start_station              end_station  duration
    ## 1     Columbus Ave & W 95 St    E 53 St & Madison Ave 15.333267
    ## 2            2 Ave & E 96 St         1 Ave & E 110 St  5.309467
    ## 3 Columbia St & Rivington St  Grand St & Elizabeth St  9.691800
    ## 4     W 84 St & Columbus Ave   Columbus Ave & W 72 St  6.996183
    ## 5     Forsyth St & Broome St  Suffolk St & Stanton St  2.849500
    ## 6       Allen St & Hester St Atlantic Ave & Furman St 25.523467

``` r
  table_by_YearMonthType =  city_bike_binded %>% group_by(year, month, type) %>% count(name="count")
  table_by_YearMonthType
```

    ## # A tibble: 8 × 4
    ## # Groups:   year, month, type [8]
    ##   year  month type   count
    ##   <chr> <chr> <chr>  <int>
    ## 1 2020  01    casual   984
    ## 2 2020  01    member 11436
    ## 3 2020  07    casual  5637
    ## 4 2020  07    member 15411
    ## 5 2024  01    casual  2108
    ## 6 2024  01    member 16753
    ## 7 2024  07    casual 10894
    ## 8 2024  07    member 36262

``` r
  top5_starting_positions = city_bike4 %>% group_by(start_station) %>% count(name="count")
  head(top5_starting_positions, n=5) # top 5 originating stations and the corresponding number of rides in July.2024
```

    ## # A tibble: 5 × 2
    ## # Groups:   start_station [5]
    ##   start_station    count
    ##   <chr>            <int>
    ## 1 1 Ave & E 110 St    33
    ## 2 1 Ave & E 118 St    21
    ## 3 1 Ave & E 16 St     73
    ## 4 1 Ave & E 18 St     75
    ## 5 1 Ave & E 30 St     57

- Description of CityBike Datasets

- Comment on table_by_YearMonthType

  - The total number of riders rise considerably each year from 2020 to
    2024, and has risen from 12480 riders in Jan.2020 to 47156 riders in
    July.2024.
  - The total number of members rise steadily and considerably each time
    interval from Jan.2020 to July.2020 to Jan.2024 to July.2024
  - Overall, for each timeslot, the number of members are considerably
    lower compared to the number of casual riders in that event.

  Make a plot to investigate the effects of day of the week, month, and
  year on median ride duration. This plot can include one or more
  panels, but should facilitate comparison across all variables of
  interest. Comment on your observations from this plot.

  There were relatively few electric Citi Bikes in 2020, but many more
  are available now. For data in 2024, make a figure that shows the
  impact of month, membership status, and bike type on the distribution
  of ride duration. Comment on your results.
