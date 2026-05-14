Chad’s 2026 MLB Report
================

*Interested in the underlying code that builds this report?* Check it
out on GitHub:
<a href="https://github.com/chadallison/mlb26" target="_blank">mlb26</a>

------------------------------------------------------------------------

### Contents

- [Team Standings](#team-standings)
- [Run Differentials](#run-differentials)
- [Runs Scored and Allowed per Game](#runs-scored-and-allowed-per-game)
- [Pythagorean Wins](#pythagorean-wins)
- [Adjusted Run Differentials](#adjusted-run-differentials)
- [Team NPR](#team-npr)
- [Adjusted Pythagorean Wins](#adjusted-pythagorean-wins)
- [Scaled Team Ratings](#scaled-team-ratings)
- [Cumulative Run Differentials](#cumulative-run-differentials)
- [Win Percentage by Run
  Differential](#win-percentage-by-run-differential)
- [One Run Games](#one-run-games)
- [True vs Adjusted Run
  Differentials](#true-vs-adjusted-run-differentials)
- [Blowout-Adjusted Cumulative Run
  Differentials](#blowout-adjusted-cumulative-run-differentials)
- [Records vs Winning Record Teams](#records-vs-winning-record-teams)

------------------------------------------------------------------------

### Team Standings

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

### Run Differentials

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

------------------------------------------------------------------------

### Runs Scored and Allowed per Game

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

------------------------------------------------------------------------

### Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

------------------------------------------------------------------------

### Adjusted Run Differentials

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

------------------------------------------------------------------------

### Team NPR

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

------------------------------------------------------------------------

### Adjusted Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

------------------------------------------------------------------------

### Scaled Team Ratings

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

------------------------------------------------------------------------

### Cumulative Run Differentials

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

------------------------------------------------------------------------

### Win Percentage by Run Differential

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

------------------------------------------------------------------------

### One Run Games

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

------------------------------------------------------------------------

### True vs Adjusted Run Differentials

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

------------------------------------------------------------------------

### Blowout-Adjusted Cumulative Run Differentials

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

------------------------------------------------------------------------

### Records vs Winning Record Teams

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

------------------------------------------------------------------------

``` r
all_results |>
  mutate(diff = score - opp_score) |>
  group_by(diff) |>
  count(team, name = "cnt") |>
  slice_max(cnt, n = 1, with_ties = T) |>
  summarise(cnt = first(cnt), team = paste(team, collapse = ", ")) |>
  mutate(clean_diff = ifelse(diff > 0, paste0(diff, "-run victory"), paste0(abs(diff), "-run defeat")),
         str = paste0(clean_diff, ": ", team)) |>
  distinct(str) |>
  pull(str)
```

    ##  [1] "16-run defeat: Baltimore Orioles"                                                                                                                                
    ##  [2] "15-run defeat: Arizona Diamondbacks"                                                                                                                             
    ##  [3] "13-run defeat: Los Angeles Angels, Minnesota Twins"                                                                                                              
    ##  [4] "12-run defeat: Arizona Diamondbacks, Chicago White Sox, Cleveland Guardians, New York Mets, Toronto Blue Jays"                                                   
    ##  [5] "11-run defeat: Philadelphia Phillies"                                                                                                                            
    ##  [6] "10-run defeat: Cincinnati Reds"                                                                                                                                  
    ##  [7] "9-run defeat: Colorado Rockies, Philadelphia Phillies"                                                                                                           
    ##  [8] "8-run defeat: Houston Astros, Washington Nationals"                                                                                                              
    ##  [9] "7-run defeat: Boston Red Sox"                                                                                                                                    
    ## [10] "6-run defeat: San Diego Padres"                                                                                                                                  
    ## [11] "5-run defeat: Washington Nationals"                                                                                                                              
    ## [12] "4-run defeat: Minnesota Twins, San Francisco Giants"                                                                                                             
    ## [13] "3-run defeat: Toronto Blue Jays"                                                                                                                                 
    ## [14] "2-run defeat: Houston Astros"                                                                                                                                    
    ## [15] "1-run defeat: Seattle Mariners"                                                                                                                                  
    ## [16] "1-run victory: Chicago Cubs, Philadelphia Phillies, Tampa Bay Rays"                                                                                              
    ## [17] "2-run victory: Baltimore Orioles, Cleveland Guardians"                                                                                                           
    ## [18] "3-run victory: Texas Rangers"                                                                                                                                    
    ## [19] "4-run victory: Tampa Bay Rays"                                                                                                                                   
    ## [20] "5-run victory: Atlanta Braves"                                                                                                                                   
    ## [21] "6-run victory: Los Angeles Dodgers, Minnesota Twins"                                                                                                             
    ## [22] "7-run victory: Baltimore Orioles, Houston Astros, New York Yankees"                                                                                              
    ## [23] "8-run victory: Chicago Cubs, Cleveland Guardians, Los Angeles Angels, New York Mets, New York Yankees, Seattle Mariners"                                         
    ## [24] "9-run victory: Arizona Diamondbacks, Atlanta Braves, Chicago Cubs, Colorado Rockies, Los Angeles Dodgers, New York Mets, New York Yankees, Philadelphia Phillies"
    ## [25] "10-run victory: Pittsburgh Pirates"                                                                                                                              
    ## [26] "11-run victory: Athletics, Colorado Rockies, Houston Astros, Kansas City Royals, Milwaukee Brewers, New York Yankees, Pittsburgh Pirates, Washington Nationals"  
    ## [27] "12-run victory: Milwaukee Brewers"                                                                                                                               
    ## [28] "13-run victory: Toronto Blue Jays, Washington Nationals"                                                                                                         
    ## [29] "15-run victory: Atlanta Braves"                                                                                                                                  
    ## [30] "16-run victory: Boston Red Sox"
