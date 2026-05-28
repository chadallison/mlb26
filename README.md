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
         str = paste0(clean_diff, ": ", team, " (", cnt, ")")) |>
  distinct(str) |>
  pull(str)
```

    ##  [1] "16-run defeat: Baltimore Orioles (1)"                                                                                             
    ##  [2] "15-run defeat: Arizona Diamondbacks (1)"                                                                                          
    ##  [3] "14-run defeat: Kansas City Royals, Washington Nationals (1)"                                                                      
    ##  [4] "13-run defeat: Los Angeles Angels, Minnesota Twins (2)"                                                                           
    ##  [5] "12-run defeat: Arizona Diamondbacks, Atlanta Braves, Chicago White Sox, Cleveland Guardians, New York Mets, Toronto Blue Jays (1)"
    ##  [6] "11-run defeat: Philadelphia Phillies (2)"                                                                                         
    ##  [7] "10-run defeat: Baltimore Orioles, Cincinnati Reds, San Francisco Giants (2)"                                                      
    ##  [8] "9-run defeat: Colorado Rockies (3)"                                                                                               
    ##  [9] "8-run defeat: Houston Astros (4)"                                                                                                 
    ## [10] "7-run defeat: Boston Red Sox (6)"                                                                                                 
    ## [11] "6-run defeat: San Diego Padres (6)"                                                                                               
    ## [12] "5-run defeat: Houston Astros, Washington Nationals (6)"                                                                           
    ## [13] "4-run defeat: New York Mets, San Francisco Giants, St. Louis Cardinals (6)"                                                       
    ## [14] "3-run defeat: Chicago Cubs, San Francisco Giants, Toronto Blue Jays (8)"                                                          
    ## [15] "2-run defeat: Boston Red Sox, Colorado Rockies, Detroit Tigers, Houston Astros, Kansas City Royals, Minnesota Twins (8)"          
    ## [16] "1-run defeat: Seattle Mariners (12)"                                                                                              
    ## [17] "1-run victory: Arizona Diamondbacks, Philadelphia Phillies (11)"                                                                  
    ## [18] "2-run victory: Baltimore Orioles, Cleveland Guardians (11)"                                                                       
    ## [19] "3-run victory: Texas Rangers (9)"                                                                                                 
    ## [20] "4-run victory: Arizona Diamondbacks, Atlanta Braves, Tampa Bay Rays (5)"                                                          
    ## [21] "5-run victory: Cincinnati Reds (7)"                                                                                               
    ## [22] "6-run victory: Los Angeles Dodgers, Milwaukee Brewers, Philadelphia Phillies (5)"                                                 
    ## [23] "7-run victory: New York Yankees (4)"                                                                                              
    ## [24] "8-run victory: Seattle Mariners (3)"                                                                                              
    ## [25] "9-run victory: Los Angeles Dodgers (3)"                                                                                           
    ## [26] "10-run victory: Pittsburgh Pirates (2)"                                                                                           
    ## [27] "11-run victory: Pittsburgh Pirates (2)"                                                                                           
    ## [28] "12-run victory: Milwaukee Brewers (2)"                                                                                            
    ## [29] "13-run victory: Chicago White Sox, Los Angeles Dodgers, Toronto Blue Jays, Washington Nationals (1)"                              
    ## [30] "14-run victory: Cincinnati Reds, New York Yankees (1)"                                                                            
    ## [31] "15-run victory: Atlanta Braves (1)"                                                                                               
    ## [32] "16-run victory: Boston Red Sox (1)"

------------------------------------------------------------------------

### True vs Adjusted Strength of Schedule

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

*Adjusted strength of schedule removes a team from its opponent’s pool
of games. For example, when calculating the Cubs’ adjusted SOS, it is
the average win percentage of their opponents with all games against the
Cubs removed.*

------------------------------------------------------------------------
