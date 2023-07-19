#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/017.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
prep_l3 <- prep_multi_line(spl[[3]], con)
prep_l4 <- prep_multi_line(spl[[4]], con)
prep_l5 <- prep_multi_line(spl[[5]], con)

library(lubridate)
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  as_tibble() |>
  mutate(period = floor_date( period, "year")) |>
  rename("ddv" = "value") -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  mutate(period = floor_date( period, "year")) |>
  as_tibble() |>
  rename(pred.d = value.x,
         gradb = value.y) -> data2

purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  mutate(period = floor_date( period, "year")) |>
  as_tibble() |>
  rowwise() |>
  transmute(period, javne = sum(c_across(where(is.numeric)))) -> data3


  purrr::reduce(prep_l4$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
    mutate(period = floor_date( period, "year")) |>
    as_tibble() |>
  rowwise() |>
  transmute(period, trzne = sum(c_across(where(is.numeric)))) -> data4


  purrr::reduce(prep_l5$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
    mutate(period = floor_date( period, "year")) |>
    as_tibble() |>
  rowwise() |>
  transmute(period, ostalo = sum(c_across(where(is.numeric)))) -> data5


updated <- max(prep_l$updated, prep_l2$updated )


# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_ap(y = ~ddv, name = "Dodana vrednost (v %)", color = I("black")) |>
  add_bars_a(data = data2, y = ~pred.d, name = "Predelovalne dejavnosti (C)",  color = I(umar_cols()[8])) |>
  add_bars_a(data = data2, y = ~gradb, name = "Gradbeni\u0161tvo (F)",  color = I(umar_cols()[5])) |>
  add_bars_a(data = data3, y = ~javne, name = "Javne storitve (O-Q)",  color = I(umar_cols()[6])) |>
  add_bars_a(data = data4, y = ~trzne, name = "Tr\u017ene storitve (G-N, R, S, T)",  color = I(umar_cols()[1])) |>
  add_bars_a(data = data5, y = ~ostalo, name = "Ostalo (A, B, D, E)",  color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2013-01-01"), max(data$period)+200) |>
  umar_layout(barmode = "relative",
              yaxis = umar_yaxis("Prispevki k medletni rasti BDP, v o.t"),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle("UMAR"),
              annotations = initials("NaTJ"))

