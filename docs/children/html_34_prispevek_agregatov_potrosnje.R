# get data
df <- read.csv2(here::here("data/019.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))

prep_l2 <- invisible(prep_multi_line(spl[[2]], con))

prep_l2$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data2
updated <- max(prep_l$updated, prep_l2$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  left_join(data2, by = "period") -> data


data |>
  plot_ly(x = ~period,  width = 1000) |>
  add_bars_q(y = ~`value.x`, name = "Doma\u010da potro\u0161nja",  color = I(umar_cols()[1])) |>
  add_bars_q(y = ~`value.y`, name = "Neto izvoz",  color = I(umar_cols()[2])) |>
  add_lines_q(y = ~`value`, name = "Realna rast BDP (v %)",  color = I("black")) |>
  umar_layout(slider_w, m, barmode = "relative",
              yaxis = umar_yaxis("Prispevki k medletni rasti BDP, v o.t"),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated, ),
              annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)



