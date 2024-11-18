
# get data
df <- read.csv(here::here("data/114.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

updated <- max(prep_l$updated)

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_m(y = ~`value.x`, name = "Realna masa neto plač",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~`value.y`, name = "Realni socialni transferji",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (100 = 2010)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo")) |>
  layout( shapes = emph_line(100, data$period) ) |>

  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)


