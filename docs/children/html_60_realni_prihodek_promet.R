# get data
df <- read.csv2(here::here("data/046.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
updated <- prep_l$updated

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> raw_data

raw_data |>
  mutate(year = year(period)) -> df

avg_2019 <- df |>
  filter(year == 2019) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

numeric_cols <- names(df)[sapply(df, is.numeric)][1:4]


# Use a loop to mutate each numeric column separately
for (col in numeric_cols) {
  df <- df %>%
    mutate(!!col := 100* !!sym(col) / avg_2019[[col]])
}


df |>
  plot_ly(x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}%", width = 1000) |>
  add_lines(y = ~`value.x`, name = "Promet in skladi\u0161\u010denje (H)",  color = I("black")) |>
  add_lines(y = ~`value.y`, name = "Kopenski promet, cevovodni transport (H49)",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x.x`, name = "Skladi\u0161\u010denje in spremlj. prometne dej. (H52)",  color = I(umar_cols()[4])) |>
  add_lines(y = ~`value.y.y`, name = "Po\u0161tna in kurirska dejavnost (H53)",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Indeks (povpre\u010dje 2019)"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: sprememba osnove"),
              annotations = initials("JuPo"),
              shapes = emph_line(100, df$period)) |>
  rangeslider(as.Date("2019-01-01"), max(df$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



