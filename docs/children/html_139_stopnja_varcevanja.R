# get data
df <- read.csv(here::here("data/117.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period"))  |>
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  arrange(period) -> data

updated <- max(prep_l$updated)

# hardcoded y-lims, just so you know..
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_ap(y = ~`value`, name = "Stopnja varčevanja gospodinjstev",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Stopnja, v %'),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle(updated, prep_l$transf_txt),
              annotations = initials("MoKo")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 50)
