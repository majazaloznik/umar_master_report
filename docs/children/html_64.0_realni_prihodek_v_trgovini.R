# get data
df <- read.csv2(here::here("data/050.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
updated <- prep_l$updated

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

plot_ly(data, x = ~period, width = 1000) |>
  add_lines(y = ~value.x,  name = "Trgovina skupaj",  color = I("black"),
            hovertemplate="%{x|%b-%Y} %{y:.2f}") |>
  add_lines(y = ~value.y,   name = "Trgovina z motornimi vozili",color = I(umar_cols()[1]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}") |>
  add_lines(y = ~value.x.x,  name = "Trgovina na debelo",  color = I(umar_cols()[2]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}") |>
  add_lines(y = ~value.y.y, name = "Trgovina na drobno", color = I(umar_cols()[4]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}") |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Indeks (povpre\u010dje 2015)"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period))|>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))





