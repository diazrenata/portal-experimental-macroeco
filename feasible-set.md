Feasible sets sampling
================
Renata Diaz
3/25/2019

get plant data
--------------

``` r
# load winter and summer annual plant data and subset out bad data
winter = read.csv("PortalWinterAnnuals_1995_2009.csv")
winter = subset(winter, plot!=1 & plot!=9 & plot!=24)   #omit spectabs removal (1,9) and misshapen plot (24)

summer = read.csv("PortalSummerAnnuals_1995_2009.csv")
summer = subset(summer, plot!=1 & plot!=9 & plot!=24)   #omit spectabs removal (1,9) and misshapen plot (24)

# add spatial areas needed to get averages for SARs
winter_new = add_areas(winter)
summer_new = add_areas(summer)
  summer_new = subset(summer_new, year != 1997 & year != 1998) # yrs where Unid Spp > 10% community
```

SADs from data
--------------

``` r
########## MACROECOLOGICAL PATTERNS ##########
##### The below functions summarize the plant community data, construct the three macroecological patterns,
##### and get the parameters used to characterize each pattern as output for analysis

##### RADs
# reshape data for analysis
winter_wide = reshape_data(winter_new)
summer_wide = reshape_data(summer_new)

#get descriptive parameters for SADs for each plot-year combination
years = c(1995:2009)
winter_rad = RAD_data(winter_wide, years) #returns all the parameters
summer_rad = RAD_data(summer_wide, years) 

str(winter_rad)
```

    ## 'data.frame':    189 obs. of  6 variables:
    ##  $ year : num  1995 1995 1995 1995 1995 ...
    ##  $ plot : num  2 3 4 5 6 7 8 10 11 12 ...
    ##  $ S    : num  21 20 23 20 24 22 23 21 22 23 ...
    ##  $ N    : num  2981 1309 1357 908 1057 ...
    ##  $ mu   : num  3.1 2.68 2.03 1.02 2.47 ...
    ##  $ sigma: num  2.22 1.95 2.13 2.29 1.81 ...

``` r
str(summer_rad)
```

    ## 'data.frame':    215 obs. of  6 variables:
    ##  $ year : num  1995 1995 1995 1995 1995 ...
    ##  $ plot : num  2 3 4 5 7 8 10 13 16 18 ...
    ##  $ S    : num  10 8 8 8 5 8 8 6 13 5 ...
    ##  $ N    : num  71 99 228 216 73 323 80 212 48 107 ...
    ##  $ mu   : num  -1.7999 0.2919 -3.1103 -1.8956 -0.0383 ...
    ##  $ sigma: num  2.27 2.08 3.58 3.02 2.46 ...

Extract real S and Ns
---------------------

``` r
winter_y <- winter_rad$year
winter_s <- winter_rad$S
winter_n <- winter_rad$N

summer_y <- summer_rad$year
summer_s <- summer_rad$S
summer_n <- summer_rad$N

winter_sn <- data.frame(year = winter_y, s = winter_s, n = winter_n, season = rep('w', length(winter_y)))
summer_sn <- data.frame(year = summer_y, s = summer_s, n = summer_n,
                        season = rep('s', length(summer_y)))

all_sn <- rbind(winter_sn, summer_sn)

str(all_sn)
```

    ## 'data.frame':    404 obs. of  4 variables:
    ##  $ year  : num  1995 1995 1995 1995 1995 ...
    ##  $ s     : num  21 20 23 20 24 22 23 21 22 23 ...
    ##  $ n     : num  2981 1309 1357 908 1057 ...
    ##  $ season: Factor w/ 2 levels "w","s": 1 1 1 1 1 1 1 1 1 1 ...

``` r
unique_sns <- all_sn %>%
  dplyr::select(s, n) %>%
  distinct()

str(unique_sns)
```

    ## 'data.frame':    401 obs. of  2 variables:
    ##  $ s: num  21 20 23 20 24 22 23 21 22 23 ...
    ##  $ n: num  2981 1309 1357 908 1057 ...

``` r
sn_space_plot <- ggplot(data = unique_sns, aes(x = s, y = n)) +
   geom_point(data = unique_sns, aes(x = s, y = n))

min(unique_sns$n)
```

    ## [1] 32

``` r
unique_sns[which(unique_sns$n == 32), 's']
```

    ## [1] 7 8

How long does it take to get 100 vectors of length 7 that sum to 32?

``` r
get_partition <- function(n ,s){
  
  if(s > n) return(NULL)
  
  sums_correct <- FALSE
  partition <- NULL
  
  while(!sums_correct) {
  partition <- sample.int(n-s+1, size = s, replace = T)
  if(sum(partition) == n) {
    sums_correct <- TRUE
  }
  }
  
  return(sort(partition))

}  

npartitions <- 100
n <- 32
s <- 7
many_partitions <- as.data.frame(t(replicate(npartitions, get_partition(n = n, s = s))))

str(many_partitions)
```

    ## 'data.frame':    100 obs. of  7 variables:
    ##  $ V1: int  1 1 1 1 1 1 1 1 2 1 ...
    ##  $ V2: int  2 1 3 2 1 1 2 1 2 2 ...
    ##  $ V3: int  3 2 4 5 2 3 3 3 4 3 ...
    ##  $ V4: int  4 3 4 5 5 4 4 4 4 5 ...
    ##  $ V5: int  5 4 5 6 5 4 4 5 6 5 ...
    ##  $ V6: int  8 8 6 6 7 8 8 8 6 8 ...
    ##  $ V7: int  9 13 9 7 11 11 10 10 8 8 ...

``` r
distinct_partitions <- many_partitions %>%
  distinct()

distinct_partitions
```

    ##    V1 V2 V3 V4 V5 V6 V7
    ## 1   1  2  3  4  5  8  9
    ## 2   1  1  2  3  4  8 13
    ## 3   1  3  4  4  5  6  9
    ## 4   1  2  5  5  6  6  7
    ## 5   1  1  2  5  5  7 11
    ## 6   1  1  3  4  4  8 11
    ## 7   1  2  3  4  4  8 10
    ## 8   1  1  3  4  5  8 10
    ## 9   2  2  4  4  6  6  8
    ## 10  1  2  3  5  5  8  8
    ## 11  1  1  3  5  5  7 10
    ## 12  1  2  3  4  5  7 10
    ## 13  1  1  1  5  7  8  9
    ## 14  2  2  2  3  4  6 13
    ## 15  1  1  2  3  6  8 11
    ## 16  1  2  2  4  6  7 10
    ## 17  1  2  3  3  4  8 11
    ## 18  1  1  1  2  7  9 11
    ## 19  1  1  4  4  5  8  9
    ## 20  1  1  4  4  5  5 12
    ## 21  1  1  2  2  2  6 18
    ## 22  1  2  2  3  4  5 15
    ## 23  1  1  1  3  4  5 17
    ## 24  1  2  2  3  3  7 14
    ## 25  1  1  2  4  5  6 13
    ## 26  1  1  1  3  3  8 15
    ## 27  1  1  2  2  3  8 15
    ## 28  1  2  2  3  5  8 11
    ## 29  1  2  5  5  5  6  8
    ## 30  2  2  2  3  3  3 17
    ## 31  1  1  1  4  7  8 10
    ## 32  1  1  1  4  5  6 14
    ## 33  1  2  3  4  6  7  9
    ## 34  1  1  1  1  3 12 13
    ## 35  1  1  1  5  6  7 11
    ## 36  2  2  2  2  3  6 15
    ## 37  1  2  2  2  7  9  9
    ## 38  1  1  2  3  4  4 17
    ## 39  1  2  4  5  5  6  9
    ## 40  1  1  1  2  3  5 19
    ## 41  1  1  1  2  2 11 14
    ## 42  1  2  4  5  6  7  7
    ## 43  1  3  3  4  5  6 10
    ## 44  1  2  2  4  4  8 11
    ## 45  1  1  2  2  5  7 14
    ## 46  1  2  2  6  7  7  7
    ## 47  1  1  1  3  4  8 14
    ## 48  1  2  3  4  7  7  8
    ## 49  2  2  2  2  8  8  8
    ## 50  1  3  4  5  5  5  9
    ## 51  1  1  2  5  6  8  9
    ## 52  1  1  3  4  5  9  9
    ## 53  1  1  4  6  6  7  7
    ## 54  1  2  2  3  6  9  9
    ## 55  1  2  2  2  2 10 13
    ## 56  1  1  2  2  5  6 15
    ## 57  2  2  2  2  6  9  9
    ## 58  1  1  1  2  3  7 17
    ## 59  1  3  4  6  6  6  6
    ## 60  1  2  2  5  5  8  9
    ## 61  2  4  4  5  5  5  7
    ## 62  1  1  1  2  4 10 13
    ## 63  1  2  2  3  3  8 13
    ## 64  1  1  1  5  7  7 10
    ## 65  2  3  4  4  5  6  8
    ## 66  1  1  1  2  4  8 15
    ## 67  2  2  3  5  5  5 10
    ## 68  1  1  2  5  7  8  8
    ## 69  1  2  2  3  5  9 10
    ## 70  1  1  3  4  6  6 11
    ## 71  2  2  3  4  5  6 10
    ## 72  1  1  1  3  4  7 15
    ## 73  1  2  3  3  5  8 10
    ## 74  1  1  3  3  4  9 11
    ## 75  2  3  3  5  5  6  8
    ## 76  2  2  3  3  5  8  9
    ## 77  2  2  3  4  5  8  8
    ## 78  2  2  2  3  6  8  9
    ## 79  1  2  3  3  5  6 12
    ## 80  1  1  1  4  7  9  9
    ## 81  1  1  2  3  4  6 15
    ## 82  1  1  2  2  5  9 12
    ## 83  1  2  2  3  4  9 11
    ## 84  1  1  2  4  5  5 14
    ## 85  1  1  1  1  4  6 18
    ## 86  1  1  3  5  6  7  9
    ## 87  1  2  3  4  5  5 12
    ## 88  1  1  2  3  3  5 17
