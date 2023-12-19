# get data
df <- read.csv2(here::here("data/105.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data

data <- data |>
  mutate(across(starts_with("value"),
                ~ . - 100))

plot_ly(data, x = ~period, width = 1000) |>
  add_lines_mp(y = ~value.x,  name = "Javni sektor",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "Zasebni sektor",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value,  name = "Skupaj",  color = I(umar_cols()[3])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Medletna rast, v %'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MiPe")) |>
  rangeslider(as.Date("2015-01-01"), max(data$period))
