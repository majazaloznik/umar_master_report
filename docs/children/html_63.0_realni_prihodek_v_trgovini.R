# get data
df <- read.csv2(here::here("data/049.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
updated <- prep_l$updated


purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data



plot_ly(data, x = ~period, width = 1000) |>
  add_lines(y = ~value.x,  name = "Trgovina skupaj",  color = I(umar_cols()[3]), fill = "tozeroy",
           hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Trgovina z motornimi vozili",color = I(umar_cols()[1]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.x.x,  name = "Trgovina na debelo",  color = I(umar_cols()[2]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y.y, name = "Trgovina na drobno", color = I(umar_cols()[4]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Medletna sprememba, v %"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))





