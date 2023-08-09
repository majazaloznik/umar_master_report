#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/030.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("BDP" = "value",
         "BDPraw" = "raw") |>
  left_join(data2, by = "period")  |>
  as_tibble() -> data

data |>
  mutate(bpp = lag(raw.x/BDPraw, 4)*value.x,
         szz = lag(raw.y/BDPraw, 4)*value.y,
         dpu = lag(raw.x.x/BDPraw, 4)*value.x.x,
         snp = lag(raw.y.y/BDPraw, 4)*value.y.y) -> data

updated <- max(prep_l$updated, prep_l2$updated)


# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_mp(y = ~`BDP`, name = "BDP, medletna rast (v %)", color = I("black")) |>
  add_bars_m(y = ~`bpp`, name = "Bruto poslovni prese\u017eek/raznovrstni dohodek",  color = I(umar_cols()[1])) |>
  add_bars_m(y = ~`szz`, name = "Sredstva za zaposlene",  color = I(umar_cols()[2])) |>
  add_bars_m(y = ~`dpu`, name = "Davki na proizvodnjo in uvoz",  color = I(umar_cols()[3])) |>
  add_bars_m(y = ~-`snp`,  name = "Subvencije na proizvodnjo",  color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  umar_layout(slider_w, m,
              barmode = "relative",
              yaxis = umar_yaxis("Prispevki k medletni rasti BDP, v o.t"),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("NaTJ"))
