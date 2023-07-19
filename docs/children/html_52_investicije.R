#### Investicije
# get data
df <- read.csv2(here::here("data/038.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rowwise() |>
  mutate("sz" = coalesce(value.x, 0)+
           coalesce(value.y, 0), .keep = "unused") -> data
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("bios" = "value") |>
  left_join(data, by = "period")  -> data

updated <- max(prep_l$updated, prep_l2$updated)


# plot
fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_qp(y = ~bios, name = "Bruto investicije v osnovna sredstva", color = I(umar_cols()[1]))

fig1 <- add_empty_lines(fig1, 10)

fig2 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_bars_q(y = ~`sz`, name = "Sprememebe zalog",  color = I(umar_cols()[3]))

subplot(fig1,  fig2, nrows = 2, shareX = TRUE) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  umar_layout(yaxis = umar_yaxis("Medletna rast, v %"),
              yaxis2 = umar_yaxis("Prispevki k medletni rasti BDP, v o.t"),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle(),
              annotations = initials("NaTJ"))


