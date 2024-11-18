# get data
df <- read.csv2(here::here("data/045.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
updated <- prep_l$updated

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> raw_data

raw_data |>
  mutate(year = year(period)) -> data

avg_2019 <- data |>
  filter(year == 2019) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

numeric_cols <- names(data)[sapply(data, is.numeric)][1:6]


# Use a loop to mutate each numeric column separately
for (col in numeric_cols) {
  data <- data %>%
    mutate(!!col := 100* !!sym(col) / avg_2019[[col]])
}


data |>
  plot_ly(x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}%", width = 1000) |>
  add_lines(y = ~`value.x`, name = "Tr\u017ene storitve (H+I+J+L+M+N)",  color = I("black")) |>
  add_lines(y = ~`value.y`, name = "Promet in skladi\u0161\u010denje (H)",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x.x`, name = "Gostinstvo (I)",  color = I(umar_cols()[2])) |>
  add_lines(y = ~`value.y.y`, name = "Informacijske in komunikacijske dejavnosti (J)", color = I(umar_cols()[4])) |>
  add_lines(y = ~`value.x.x.x`, name = "Strokovne, znanstvene in tehni\u010dne dejavnosti (M)",  color = I(umar_cols()[5])) |>
  add_lines(y = ~`value.y.y.y`, name = "Druge raznovrstne poslovne dejavnosti (N)",  color = I(umar_cols()[6])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Indeks (povpre\u010dje 2019)"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated),
              annotations = initials("JuPo"),
              shapes = emph_line(100, data$period))|>
  rangeslider(as.Date("2019-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



