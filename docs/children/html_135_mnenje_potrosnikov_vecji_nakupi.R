# get data
df <- read.csv(here::here("data/113.csv"), encoding = "UTF-8")
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
  add_lines_q(y = ~`value`, name = "Izboljšave v stanovanju v prihodnjih 12 m",  color = I(umar_cols()[5])) |>
  add_lines_q(y = ~`value.x`, name = "Nakup avtomobila v prihodnjih 12 m",  color = I(umar_cols()[3])) |>
  add_lines_q(y = ~`value.y`, name = "Nakup ali gradnja stanovanja v prihodnjih 12 m",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Ravnotežje, v o.t.'),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)



