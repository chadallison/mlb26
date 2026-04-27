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

``` r
window_size = 7

all_results |>
  arrange(team, date, game_pk) |>
  mutate(roll_score = rollapply(score, width = window_size, align = "right", FUN = "sum", fill = NA),
         roll_allow = rollapply(opp_score, width = window_size, align = "right", FUN = "sum", fill = NA),
         .by = "team") |>
  filter(!is.na(roll_score)) |>
  mutate(game_num = row_number(), .by = "team") |>
  mutate(roll_py = roll_score ^ 2 / (roll_score ^ 2 + roll_allow ^ 2)) |>
  inner_join(teams_info, by = "team") |>
  inner_join(team_divisons, by = "team") |>
  ggplot(aes(game_num, roll_py)) +
  geom_line(aes(col = hex), linewidth = 1.25) +
  scale_color_identity() +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  facet_wrap(vars(division))
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
window_size = 7

all_results |>
  arrange(team, date, game_pk) |>
  mutate(diff_sr = sqrt(score) - sqrt(opp_score)) |>
  mutate(game_num = row_number(),
         roll_diff = rollapply(diff_sr, FUN = "sum", width = window_size, align = "right", partial = T),
         .by = "team") |>
  inner_join(teams_info, by = "team") |>
  inner_join(team_divisons, by = "team") |>
  ggplot(aes(game_num, roll_diff)) +
  geom_line(aes(col = hex), linewidth = 1.25) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.25) +
  scale_color_identity() +
  facet_wrap(vars(division))
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
