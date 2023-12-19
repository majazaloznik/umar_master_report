# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
df <- df |> mutate(year_on_year = ifelse(year_on_year == "y", TRUE, year_on_year))
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[10]][1,], con)
prep_l2 <- prep_multi_line(spl[[10]][2:3,], con)

prep_l$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

updated <- max(prep_l$updated, prep_l2$updated)

data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",
           name = "Realni indeks opravljenih del v gradbeni\u0161tvu", color = I(umar_cols()[2])) |>
  add_lines(data = data2, y = ~`value.x`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",
            name = "Indeks ind.p.: pridobivanje rudnin in kamnin ", color = I(umar_cols()[1])) |>
  add_lines(data = data2, y = ~`value.y`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",
            name = "Indeks ind.p.: proizv. nekovinskih mineral. izdelkov ", color = I(umar_cols()[5])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Medletna sprememba, v %"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("JaKu")) |>
  rangeslider(as.Date("2020-01-01"), max(data$period)+10) |>
  config(modeBarButtonsToAdd = list(dl_button))
