# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[9]], con)
updated <- prep_l$updated

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

plot_ly(data, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~value.x,  name = "Kazalnik zaupanja potro\u0161nikov",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Finan\u010dno stanje v gospodinjstvu v prihodnjih 12 mesecih",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Gospodarsko stanje v Sloveniji v prihodnjih 12 mesecih",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y,   name = "Gibanje cen v prihodnjih 12 mesecih",color = I(umar_cols()[4])) |>
  add_lines(y = ~value.x.x.x,  name = "Raven brezposelnosti v prihodnjih 12 mesecih",  color = I(umar_cols()[5])) |>
  add_lines(y = ~value.y.y.y,   name = "Var\u010devanje v prihodnjih 12 mesecih",color = I(umar_cols()[6])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Dele\u017e podjetij, v %"),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated, ),
              annotations = initials("MaHr")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))
