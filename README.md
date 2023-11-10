Transport and Travel - Scottish Household Survey
================

The goal of this repository is to analyse the results of the Scottish
Household Survey from 2014 to 2019 \[1\],\[2\],\[3\],\[4\],\[5\],\[6\]
to estimate the overall mode splits and temporal distribution of trips
made by bicycle. Some details on the weighting methodology of the survey
can be found
[here](https://www.gov.scot/publications/scottish-household-survey-2021-methodology-fieldwork-outcomes/pages/7/)

All the zip files have been obtained from the [UK data
service](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8775)

First of all, a copy of the original files can be obtained with the code
below. This code is to be run just once.

``` r
dir.create("raw_data")
system("gh release download 1 --dir raw_data")
```

We can list the files with the following code:

``` r
zip_files = list.files("raw_data/","\\.zip$",full.names = T)
zip_files
```

    ## [1] "raw_data/7964spss_bcc98090d92c0cad9d0e65e37ddc0591.zip"   
    ## [2] "raw_data/8168spss_7E71FE4E91FBD90B6A22931DBFB9444C_V1.zip"
    ## [3] "raw_data/8333spss_2119F1608B6E9B643BBCADDFC5E865D5_V1.zip"
    ## [4] "raw_data/8463spss_27B8A9C6A2988942DA5DF9EB6D9CCE35_V1.zip"
    ## [5] "raw_data/8617spss_EB73235EFE70CDB92AAAFBDA4A4BDBE7_V1.zip"
    ## [6] "raw_data/8775spss_647772365F41501FC26EA57EDF2A7077_V1.zip"

All the zipped files are extracted with the following code:

``` r
for (file in zip_files){  
  unzip(file,exdir = "raw_data")
  }
```

Once the files have been unzipped, the `*.sav` files containing the
journey diaries are listed as follows:

``` r
SPSS_files = list.files(pattern = "journey.*\\.sav$",
                        recursive = T,full.names = T)
SPSS_files
```

    ## [1] "./raw_data/UKDA-7964-spss/spss/spss19/shs2014_td_journey_public.sav"
    ## [2] "./raw_data/UKDA-8168-spss/spss/spss24/shs2015_td_journey_public.sav"
    ## [3] "./raw_data/UKDA-8333-spss/spss/spss24/shs2016_td_journey_public.sav"
    ## [4] "./raw_data/UKDA-8463-spss/spss/spss24/shs2017_td_journey_public.sav"
    ## [5] "./raw_data/UKDA-8617-spss/spss/spss25/shs2018_td_journey_public.sav"
    ## [6] "./raw_data/UKDA-8775-spss/spss/spss25/shs2019_td_journey_public.sav"

All files are imported using the `haven` library with this code:

``` r
library(haven)
library(tidyverse)
library(kableExtra)

data = do.call(bind_rows,lapply(SPSS_files,read_sav))

data |> head()
```

    ## # A tibble: 6 × 34
    ##   UNIQIDNEW Journey dyear       IND_WT trav_wt randsex randage randecon mainmode
    ##       <dbl>   <dbl> <dbl+lbl>    <dbl>   <dbl> <dbl+l> <dbl+l> <dbl+lb> <dbl+lb>
    ## 1   671908.       1 16 [2014 d…   1.28    1.03 1 [Mal… 44      2 [Empl… 1 [Walk…
    ## 2   671908.       2 16 [2014 d…   1.28    1.03 1 [Mal… 44      2 [Empl… 1 [Walk…
    ## 3  1513778.       1 16 [2014 d…   2.24    2.91 2 [Fem… 42      3 [Empl… 2 [Car/…
    ## 4  1513778.       2 16 [2014 d…   2.24    2.91 2 [Fem… 42      3 [Empl… 2 [Car/…
    ## 5  1513778.       3 16 [2014 d…   2.24    2.91 2 [Fem… 42      3 [Empl… 3 [Car/…
    ## 6  1513778.       4 16 [2014 d…   2.24    2.91 2 [Fem… 42      3 [Empl… 1 [Walk…
    ## # ℹ 25 more variables: purpose_new <dbl+lbl>, purpose_old <dbl+lbl>,
    ## #   journeystart_lacode <chr>, journeyend_lacode <chr>,
    ## #   JourStart_UR13 <dbl+lbl>, JourEnd_UR13 <dbl+lbl>, journeystart_time <time>,
    ## #   journeystart_hh <dbl>, journeystart_mm <dbl>, journeyend_time <time>,
    ## #   journeyend_hh <dbl>, journeyend_mm <dbl>, journey_duration <dbl>,
    ## #   journey_distance <dbl>, journeystart_sum <dbl>, journeyend_sum <dbl>,
    ## #   keepjourney <dbl+lbl>, imp_dist <dbl+lbl>, roadnet_time <dbl>, …

We are only interested in trips made by bicycle. According to the data
dictionaries, the corresponding code is `4`.

``` r
data_bicycle = data |> filter(mainmode == 4)
data_bicycle |> head()
```

    ## # A tibble: 6 × 34
    ##   UNIQIDNEW Journey dyear       IND_WT trav_wt randsex randage randecon mainmode
    ##       <dbl>   <dbl> <dbl+lbl>    <dbl>   <dbl> <dbl+l> <dbl+l> <dbl+lb> <dbl+lb>
    ## 1  1327998.       1 16 [2014 d…   1.61    1.36 1 [Mal… 46       2 [Emp… 4 [Bicy…
    ## 2  1327998.       2 16 [2014 d…   1.61    1.36 1 [Mal… 46       2 [Emp… 4 [Bicy…
    ## 3   257230.       1 16 [2014 d…   1.87    1.27 1 [Mal… 58      10 [Per… 4 [Bicy…
    ## 4   257230.       2 16 [2014 d…   1.87    1.27 1 [Mal… 58      10 [Per… 4 [Bicy…
    ## 5  1041311.       1 16 [2014 d…   1.44    1.03 1 [Mal… 77       5 [Per… 4 [Bicy…
    ## 6  1041311.       2 16 [2014 d…   1.44    1.03 1 [Mal… 77       5 [Per… 4 [Bicy…
    ## # ℹ 25 more variables: purpose_new <dbl+lbl>, purpose_old <dbl+lbl>,
    ## #   journeystart_lacode <chr>, journeyend_lacode <chr>,
    ## #   JourStart_UR13 <dbl+lbl>, JourEnd_UR13 <dbl+lbl>, journeystart_time <time>,
    ## #   journeystart_hh <dbl>, journeystart_mm <dbl>, journeyend_time <time>,
    ## #   journeyend_hh <dbl>, journeyend_mm <dbl>, journey_duration <dbl>,
    ## #   journey_distance <dbl>, journeystart_sum <dbl>, journeyend_sum <dbl>,
    ## #   keepjourney <dbl+lbl>, imp_dist <dbl+lbl>, roadnet_time <dbl>, …

We can calculate the purpose split for the bicycle trips using the
weight variables as follows:

``` r
summary_purpose = data_bicycle |>
  summarise(Trips = sum(trav_wt),
            .by = c(purpose_old)) |> 
  mutate(Split = Trips/sum(Trips))
summary_purpose
```

    ## # A tibble: 15 × 3
    ##    purpose_old                                 Trips    Split
    ##    <dbl+lbl>                                   <dbl>    <dbl>
    ##  1  1 [Place of work]                         555.   0.361   
    ##  2  4 [Shopping]                              132.   0.0858  
    ##  3 14 [Other - not coded]                      42.1  0.0274  
    ##  4 28 [Go home]                               188.   0.123   
    ##  5 11 [Partcipipating in sport]               285.   0.185   
    ##  6 10 [Entertainment/other public activities]   7.1  0.00462 
    ##  7  7 [visiting Friends or relatives]         106.   0.0692  
    ##  8  6 [Other personal business]                48.3  0.0315  
    ##  9 29 [Just go for a walk]                      8.44 0.00550 
    ## 10  3 [Educational establishment]             116.   0.0754  
    ## 11  5 [Visit hospital or other health]          8.9  0.00580 
    ## 12  2 [In course of work]                      15.9  0.0104  
    ## 13  9 [Easting/drinking other occasions]       14.1  0.00917 
    ## 14 13 [Day trip]                                7.7  0.00501 
    ## 15 27 [Escort - other]                          1.15 0.000749

The `summary_purpose` object has coded variables which are stored as
labelled vectors. The following code allows us to extract the labels
from the `purpose_old` column.

``` r
summary_purpose = summary_purpose |> 
  mutate(purpose_old = haven::as_factor(purpose_old))
summary_purpose
```

    ## # A tibble: 15 × 3
    ##    purpose_old                            Trips    Split
    ##    <fct>                                  <dbl>    <dbl>
    ##  1 Place of work                         555.   0.361   
    ##  2 Shopping                              132.   0.0858  
    ##  3 Other - not coded                      42.1  0.0274  
    ##  4 Go home                               188.   0.123   
    ##  5 Partcipipating in sport               285.   0.185   
    ##  6 Entertainment/other public activities   7.1  0.00462 
    ##  7 visiting Friends or relatives         106.   0.0692  
    ##  8 Other personal business                48.3  0.0315  
    ##  9 Just go for a walk                      8.44 0.00550 
    ## 10 Educational establishment             116.   0.0754  
    ## 11 Visit hospital or other health          8.9  0.00580 
    ## 12 In course of work                      15.9  0.0104  
    ## 13 Easting/drinking other occasions       14.1  0.00917 
    ## 14 Day trip                                7.7  0.00501 
    ## 15 Escort - other                          1.15 0.000749

![](README_files/figure-gfm/mode_split-1.png)<!-- -->

Similarly, the split by year can be calculated with the following code:

``` r
summary_purpose_year = data_bicycle |>
  summarise(Trips = sum(trav_wt),
            .by = c(purpose_old,dyear)) |> 
  mutate(Split = Trips/sum(Trips),.by = c(dyear))
```

The years are also a coded variable.

``` r
summary_purpose_year = summary_purpose_year |>
  mutate(purpose_old = haven::as_factor(purpose_old),
         dyear = haven::as_factor(dyear))
summary_purpose_year
```

    ## # A tibble: 74 × 4
    ##    purpose_old                           dyear                Trips   Split
    ##    <fct>                                 <fct>                <dbl>   <dbl>
    ##  1 Place of work                         2014 dataset/script 102.   0.352  
    ##  2 Shopping                              2014 dataset/script  32.3  0.112  
    ##  3 Other - not coded                     2014 dataset/script  21.7  0.0753 
    ##  4 Go home                               2014 dataset/script  31.7  0.110  
    ##  5 Partcipipating in sport               2014 dataset/script  58    0.201  
    ##  6 Entertainment/other public activities 2014 dataset/script   0.75 0.00260
    ##  7 visiting Friends or relatives         2014 dataset/script  15.3  0.0529 
    ##  8 Other personal business               2014 dataset/script   1.04 0.00360
    ##  9 Just go for a walk                    2014 dataset/script   1.44 0.00499
    ## 10 Educational establishment             2014 dataset/script  21.7  0.0753 
    ## # ℹ 64 more rows

We can see how the splits for the five most common purposes have changed
from year to year. For this purpose, we extract the top 5 purposes from
the previous analysis.

``` r
top_5_purposes = summary_purpose |> slice_max(Split,n = 5) |> pull(purpose_old)
top_5_purposes
```

    ## [1] Place of work             Partcipipating in sport  
    ## [3] Go home                   Shopping                 
    ## [5] Educational establishment
    ## attr(,"label")
    ## [1] Purpose of journey (Old codes)
    ## 24 Levels: Not stated Place of work ... Just go for a walk

We need to remove the `dataset/script` or extract the numerical part of
the dyear column for plotting.

``` r
summary_purpose_year |> 
  filter(purpose_old %in% top_5_purposes) |> 
  mutate(Year = as.integer(str_extract(dyear,"\\d*")), 
         purpose_old = fct_reorder(purpose_old, Split)) |> 
  ggplot(aes(x = Year, y = Split, col = purpose_old)) + 
  geom_line(linewidth = 1)+
  scale_color_viridis_d()+
  scale_y_continuous(labels = scales::percent)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The high variation of the splits might be linked to the different
samples for each year’s survey.

## Temporal Distribution of trips

Using the start time when the recorded trips (`journeystart_hh`), we can
build an hourly profile of the trips.

``` r
hourly_summary = data_bicycle |> 
  summarise(Trips = sum(trav_wt),
            .by = c(journeystart_hh))
hourly_summary
```

    ## # A tibble: 25 × 2
    ##    journeystart_hh Trips
    ##              <dbl> <dbl>
    ##  1               8 144. 
    ##  2              17 166. 
    ##  3               9  77.0
    ##  4               6  37.2
    ##  5               7 109. 
    ##  6              15 120. 
    ##  7              16 130. 
    ##  8              12  80.7
    ##  9              13  71.6
    ## 10              11  85.8
    ## # ℹ 15 more rows

The following code is used to plot the hourly trips profile.

``` r
hourly_summary |>
  ggplot(aes(x=journeystart_hh,y = Trips))+
    geom_line(linewidth = 0.7,col = "blue")+
  geom_hline(yintercept = 0)+
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

We can also produce the same analysis by trip purpose.

``` r
hourly_summary_purpose = data_bicycle |> 
  summarise(Trips = sum(trav_wt),
            .by = c(journeystart_hh,purpose_old)) |> 
  mutate(purpose_old = haven::as_factor(purpose_old))
  
hourly_summary_purpose
```

    ## # A tibble: 184 × 3
    ##    journeystart_hh purpose_old              Trips
    ##              <dbl> <fct>                    <dbl>
    ##  1               8 Place of work           102.  
    ##  2              17 Place of work            81.2 
    ##  3               9 Shopping                 10.3 
    ##  4               6 Other - not coded         3.86
    ##  5               7 Other - not coded         3.86
    ##  6              15 Other - not coded         2.13
    ##  7              16 Go home                  21.1 
    ##  8              12 Partcipipating in sport  36.7 
    ##  9              13 Go home                   8.89
    ## 10              11 Place of work            11.0 
    ## # ℹ 174 more rows

As shown in the plot below, the *commuting* and *education* trips have
two clear peaks; for all other purposes the temporal patterns are less
clear.

``` r
hourly_summary_purpose |>
  filter(purpose_old %in% top_5_purposes) |>
  mutate(purpose_old = fct_reorder(purpose_old, Trips)) |> 
  ggplot(aes(x=journeystart_hh,y = Trips, col = purpose_old))+
  geom_line(linewidth = 0.7)+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_colour_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Trip Length Distribution

The following section includes a brief analysis of the trip length
distribution. For this analysis, the following distance bands are
defined (in km)

``` r
tld_bands = c(0,5,10,15,25,50,100,500,1000,2500)
```

### By mode

``` r
TLD_mode = data |> 
  mutate(dist_band = cut(roadnet_kms,
                         breaks = tld_bands,
                         include.lowest = T),
         ) |>
  summarise(Trips = sum(trav_wt,na.rm = T),
            .by = c(mainmode,dist_band)) |> 
  mutate(perc = Trips/sum(Trips),
         .by = c(mainmode))
```

``` r
TLD_mode |> 
  drop_na(dist_band) |> 
  ggplot(aes(x=dist_band,y=perc,fill=haven::as_factor(mainmode)))+
  geom_col()+
  facet_wrap(haven::as_factor(mainmode)~.)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### By purpose

The following exercise uses the `purpose_old` categories. A finer
analysis is possible if the `purpose_new` and `purpose_new2`

``` r
TLD_purpose = data |>
  mutate(dist_band = cut(roadnet_kms,
                         breaks = tld_bands,
                         include.lowest = T),
         ) |>
  drop_na(dist_band) |> 
  summarise(Trips = sum(trav_wt,na.rm = T),
            .by = c(purpose_old,dist_band)) |> 
  mutate(perc_band = Trips/sum(Trips),
         .by = c(purpose_old)) |> 
  mutate(perc_purp = Trips/sum(Trips),
         .by = c(dist_band))
```

``` r
TLD_purpose |> 
  ggplot(aes(x=dist_band,y=perc_purp,fill=haven::as_factor(purpose_old)))+
  geom_col(position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
TLD_purpose |> 
  ggplot(aes(x=dist_band,y=perc_purp,fill=haven::as_factor(purpose_old)))+
  geom_col()+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(haven::as_factor(purpose_old)~.)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### By mode and purpose

``` r
TLD_mode_purp = data |> 
  mutate(dist_band = cut(roadnet_kms,
                         breaks = tld_bands,
                         include.lowest = T),
         ) |>
  summarise(Trips = sum(trav_wt,na.rm = T),
            .by = c(mainmode,purpose_old,dist_band)) |> 
  mutate(perc = Trips/sum(Trips),
         .by = c(mainmode,purpose_old))
```

``` r
TLD_mode_purp |> 
  drop_na(dist_band, mainmode) |> 
  filter(mainmode<9) |> 
  ggplot(aes(x=dist_band,y=perc,fill=haven::as_factor(purpose_old)))+
  geom_col()+
  facet_wrap(haven::as_factor(mainmode)~.)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## Annualisation factors calculation

In order to estimate the trip frequency by purpose, the stage tables
from the survey are used. The following code loads the data into the
environment.

``` r
SPSS_files_stage = list.files(pattern = "stage.*\\.sav$",
                        recursive = T,full.names = T)

data.stage = do.call(bind_rows,lapply(SPSS_files_stage,read_sav))
```

As in the previous analysis, we are interested only in the bicycle trips
(`mode` = `4`). Although bike might not be the main mode for all the
trip, all bike stages are considered for this analysis. Subsequently, we
select the columns of interest and discard duplicated records

``` r
bike_stages <- data.stage |> 
  filter(mode == 4) |> 
  select(UNIQIDNEW,dyear,IND_WT,trav_wt,purpose_new,travday) |> 
  unique()
```

Firstly, we explore the overall trip frequency splits by day of the week

``` r
bike_stages |> 
  summarise(Total = sum(trav_wt,na.rm = T),
            .by = c(dyear,travday)) |> 
  mutate(Perc = Total/sum(Total),
         .by = c(dyear)) |> 
  select(-Total) |>
  mutate(across(c("dyear",travday),as_factor)) |> 
  pivot_wider(names_from = dyear,values_from = Perc) |> 
  arrange(travday) |>
  kable(digits = 3) |> 
  kable_classic() |>
  as_image(width = 10,file = "README_files/figure-gfm/wday_splits.png")
```

<img src="README_files/figure-gfm/wday_splits.png" width="960" />

``` r
bike_stages |> 
  summarise(Total = sum(trav_wt,na.rm = T),
            .by = c(dyear,travday)) |> 
  mutate(Perc = Total/sum(Total),
         .by = c(dyear)) |> 
  select(-Total) |> 
  ggplot(aes(x=haven::as_factor(dyear), y = Perc, fill = haven::as_factor(travday)))+
  scale_y_continuous(
    # limits = c(0,0.3),
    labels = scales::percent)+
  geom_col()+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30,vjust = 1,hjust = 1))+
  labs(x= "year/dataset",y=NULL,fill = "Day",title = "Portion of trips by day of travel")
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- --> Since travel
patters and frequency would vary among trip purposes, we need to produce
a more dissagregated analysis. Thus, we first define groups of trip
purposes from the `new_purpose` column. The following code identifies
the different trip purposes that people logged in their trip diaries of
the survey when using the bike.

``` r
SHS_purposes_bike <- bike_stages |>
  select(purpose_new) |>
  unique() |> 
  mutate(p.label = as_factor(purpose_new))
```

, A correspondence between trip purposes for the NPT project has been
defined as follows:

``` r
NPT_purposes_equiv <- tribble(
  ~ purpose_new,  ~ NPT_purpose,
  3, 'Commute',
  103, 'Commute',
  4, 'Shopping',
  104, 'Shopping',
  15, 'Workout',
  29, 'Other',
  129, 'Other',
  1, 'Other',
  115, 'Workout',
  34, 'Social',
  6, 'Social',
  19, 'Workout',
  24, 'Other',
  106, 'Social',
  5, 'Social',
  105, 'Social',
  12, 'Other',
  112, 'Other',
  11, 'School',
  111, 'School',
  30, 'Workout',
  130, 'Workout',
  23, 'Other',
  123, 'Other',
  7, 'School',
  107, 'School',
  2, 'Other',
  102, 'Other',
  36, 'Social',
  31, 'Other',
  131, 'Other',
  119, 'Workout',
  35, 'Social',
  26, 'Social',
  126, 'Social',
  25, 'Social',
  125, 'Social',
  17, 'Other',
  22, 'Other',
  122, 'Other',
  135, 'Social',
  124, 'Other',
  33, 'Other',
  136, 'Social',
  117, 'Other',
  226, 'Social',
  18, 'Other',
  118, 'Other',
  13, 'Workout',
  113, 'Workout',
  133, 'Other',
  114, 'Workout',
  134, 'Social'
)
```

The following table shows how the `new_purpose` values have been grouped
into 6 wider categories:

``` r
tbl_purposes <- SHS_purposes_bike |>
  left_join(NPT_purposes_equiv,by = "purpose_new") |>
  arrange(NPT_purpose) 

kable(tbl_purposes |>
                       select(-NPT_purpose)) |>
  pack_rows(index = table(tbl_purposes$NPT_purpose)) |> 
  kable_classic() |>
  as_image(width = 10,file = "README_files/figure-gfm/purpose_table.png")
```

<img src="README_files/figure-gfm/purpose_table.png" width="960" />

Based on this purposes, we can produce a plot for proportion of trips by
day of travel

``` r
bike_stages |> 
left_join(NPT_purposes_equiv,by = "purpose_new") |>    
  summarise(Total = sum(trav_wt,na.rm = T),
            .by = c(dyear,travday,NPT_purpose)) |> 
  mutate(Perc = Total/sum(Total),
         .by = c(dyear,NPT_purpose)) |> 
  select(-Total) |> 
  ggplot(aes(x=haven::as_factor(dyear), y = Perc, fill = haven::as_factor(travday)))+
  scale_y_continuous(
    # limits = c(0,0.3),
    labels = scales::percent)+
  geom_col()+
  facet_wrap(NPT_purpose~.)+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30,vjust = 1,hjust = 1))+
  labs(x= "year/dataset",y=NULL,fill = "Day",title = "Portion of trips by day of travel")
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- --> In order to
produce the annualisation factors, we have to estimate the trip
frequency by purpose from the survey. First the number of trips by day
of the week is calculated for each respondent of the survey. Then, we
calculated the weighted mean and median across all respondents using the
travel diary weight (`trav_wt`). This calculations are applied for each
category of the NTP purposes.

``` r
purpose_factors = bike_stages |>
  left_join(NPT_purposes_equiv,by = "purpose_new") |> 
  summarise(N_trips = n(),
            .by = c(dyear,travday,UNIQIDNEW,NPT_purpose,trav_wt)) |> 
  # mutate(N_trips_weighted = N_trips*trav_wt) |> 
  summarise(N_trips_mean = weighted.mean(N_trips,trav_wt),
            N_trips_median=matrixStats::weightedMedian(N_trips,trav_wt),
            .by = c(travday,NPT_purpose)) |> 
  mutate(wd.type = case_when(travday<6~"weekday",
                            travday==6~"saturday",
                            TRUE~"sunday/bankholiday"))
```

In order to consider the different weights of each type of day in a year
i.e., the total number of Mondays, Tuesdays, etc; days are grouped into
weekdays, Saturdays and Sundays/bank holidays. Finally, assuming that
every year in Scotland has 250 weekdays, 52 Saturdays and 63
Sundays/Bank holidays, we calculate the annualisation factor for each
trip purpose.

``` r
purpose_factors_dtype = purpose_factors |>
  summarise(across(c(N_trips_mean,N_trips_median),
                   mean),
            .by = c(wd.type,NPT_purpose)) |> 
  mutate(wd.wt = case_when(wd.type=="weekday"~250,
                           wd.type=="saturday"~52,
                           wd.type=="sunday/bankholiday"~63)) |> 
  summarise(factor_AADT = weighted.mean(N_trips_mean,wd.wt),
            .by = NPT_purpose)

purpose_factors_dtype |> 
  kbl(digits=4) |>
  kable_classic_2() |>
  as_image(width = 10,file = "README_files/figure-gfm/AADT_factors.png")
```

<img src="README_files/figure-gfm/AADT_factors.png" width="960" />

<div id="refs" class="references csl-bib-body">

<div id="ref-SHS14" class="csl-entry">

<span class="csl-left-margin">\[1\]
</span><span class="csl-right-inline">I. MORI and S. Government,
“Scottish household survey, 2014.” UK Data Service, 2017. doi:
[10.5255/UKDA-SN-7964-2](https://doi.org/10.5255/UKDA-SN-7964-2).</span>

</div>

<div id="ref-SHS15" class="csl-entry">

<span class="csl-left-margin">\[2\]
</span><span class="csl-right-inline">I. MORI and S. Government,
“Scottish household survey, 2015.” UK Data Service, 2020. doi:
[10.5255/UKDA-SN-8168-2](https://doi.org/10.5255/UKDA-SN-8168-2).</span>

</div>

<div id="ref-SHS16" class="csl-entry">

<span class="csl-left-margin">\[3\]
</span><span class="csl-right-inline">I. MORI and S. Government,
“Scottish household survey, 2016.” UK Data Service, 2020. doi:
[10.5255/UKDA-SN-8333-2](https://doi.org/10.5255/UKDA-SN-8333-2).</span>

</div>

<div id="ref-SHS17" class="csl-entry">

<span class="csl-left-margin">\[4\]
</span><span class="csl-right-inline">S. Government and I. MORI,
“Scottish household survey, 2017.” UK Data Service, 2020. doi:
[10.5255/UKDA-SN-8463-1](https://doi.org/10.5255/UKDA-SN-8463-1).</span>

</div>

<div id="ref-SHS18" class="csl-entry">

<span class="csl-left-margin">\[5\]
</span><span class="csl-right-inline">S. Government and I. MORI,
“Scottish household survey, 2018.” UK Data Service, 2020. doi:
[10.5255/UKDA-SN-8617-1](https://doi.org/10.5255/UKDA-SN-8617-1).</span>

</div>

<div id="ref-SHS19" class="csl-entry">

<span class="csl-left-margin">\[6\]
</span><span class="csl-right-inline">S. Government and I. MORI,
“Scottish household survey, 2019.” UK Data Service, 2021. doi:
[10.5255/UKDA-SN-8775-1](https://doi.org/10.5255/UKDA-SN-8775-1).</span>

</div>

</div>
