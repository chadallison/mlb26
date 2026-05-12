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
all_results |>
  mutate(diff = abs(score - opp_score),
         real_diff = score - opp_score,
         log_diff = log(diff + 1),
         adj_diff = ifelse(score > opp_score, log_diff, -1 * log_diff)) |>
  group_by(team) |>
  summarise(adj_diff = mean(adj_diff),
            real_diff = mean(real_diff)) |>
  arrange(desc(adj_diff)) |>
  inner_join(teams_info, by = "team") |>
  ggplot(aes(real_diff, adj_diff)) +
  geom_point(aes(col = hex), shape = "square", size = 4) +
  scale_color_identity() +
  geom_line(
    stat = "smooth",
    formula = y ~ x,
    method = "lm",
    linetype = "dashed",
    alpha = 0.5
  ) +
  ggrepel::geom_text_repel(aes(label = abb), size = 3, max.overlaps = 30) +
  scale_x_continuous(breaks = seq(-5, 5, by = 0.25)) +
  scale_y_continuous(breaks = seq(-5, 5, by = 0.25)) +
  labs(x = "True Run Differential",
       y = "Adjusted Run Differential",
       title = glue("True vs. Adjusted Run Differentials as of {today_nice}"))
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
max_diff = end_games |>
  mutate(diff = abs(win_score - lose_score)) |>
  summarise(avg_diff = mean(diff),
            sd_diff = sd(diff),
            max_diff = round(mean(diff) + sd(diff), 0)) |>
  pull(max_diff)

all_results |>
  filter(date >= Sys.Date() - 30) |>
  mutate(diff = score - opp_score,
         diff = ifelse(diff > max_diff, max_diff, diff)) |>
  arrange(team, date, game_pk) |>
  mutate(game_num = row_number(),
         cum_adj_diff = cumsum(diff),
         .by = "team") |>
  inner_join(teams_info, by = "team") |>
  inner_join(team_divisons, by = "team") |>
  ggplot(aes(game_num, cum_adj_diff)) +
  geom_line(aes(col = hex), linewidth = 1.25) +
  geom_text_repel(
    data = ~ slice_max(.x, game_num, by = team),
    aes(label = abb, col = hex),
    nudge_x = 2,
    direction = "y",
    hjust = 0,
    size = 3,
    segment.size = 0,
    min.segment.length = 100
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.25) +
  scale_color_identity() +
  facet_wrap(vars(division)) +
  coord_cartesian(clip = "off") +
  labs(x = "Game number",
       y = "Cumulative run differential",
       title = glue("Cumulative run differentials (blowout-adj.) by team as of {today_nice}"))
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
records_vs_above_fh = all_results |>
  inner_join(team_records, by = c("opp" = "team")) |>
  filter(win_pct >= 0.5) |>
  group_by(team) |>
  summarise(gp = n(),
            wins = sum(is_win),
            losses = sum(1 - is_win),
            pct = mean(is_win),
            record = paste0(sum(is_win), "-", sum(1 - is_win)))

totals = all_results |>
  group_by(team) |>
  summarise(total_wins = sum(is_win), total_gp = n())

h2h = all_results |>
  group_by(team, opp) |>
  summarise(h2h_wins = sum(is_win), h2h_gp = n(), .groups = "drop")

adj_wp_lookup = h2h |>
  left_join(
    h2h |> rename(opp_wins_vs_team = h2h_wins, opp_gp_vs_team = h2h_gp),
    by = c("opp" = "team", "team" = "opp")
  ) |>
  left_join(totals, by = c("opp" = "team")) |>
  mutate(
    adj_wins = total_wins - opp_wins_vs_team,
    adj_gp   = total_gp   - opp_gp_vs_team,
    adj_pct  = adj_wins / adj_gp
  ) |>
  select(team, opp, adj_pct)

adj_wp_vs_fh = all_results |>
  left_join(adj_wp_lookup, by = c("team", "opp")) |>
  filter(adj_pct >= 0.5) |>
  group_by(team) |>
  summarise(pct = mean(is_win)) |>
  arrange(desc(pct))

records_vs_above_fh |>
  rename(true_pct = pct) |>
  inner_join(adj_wp_vs_fh, by = "team") |>
  inner_join(teams_info, by = "team") |>
  ggplot(aes(true_pct, pct)) +
  geom_point(aes(col = hex), shape = "square", size = 4) +
  scale_color_identity() +
  ggrepel::geom_text_repel(aes(label = abb), size = 3, max.overlaps = 30) +
  geom_abline(linetype = "dashed", alpha = 0.25) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  labs(x = "True Win Pct. vs. .500+ Teams",
       y = "Win Pct. vs. .500+ Teams w/ Games vs. Self Removed",
       title = glue("Win Percentage vs. .500+ Teams with/without Self Included as of {today_nice}"))
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
