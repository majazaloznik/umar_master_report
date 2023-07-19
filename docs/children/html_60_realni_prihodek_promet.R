# get data
df <- read.csv2(here::here("data/046.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> raw_data

raw_data |>
  mutate(year = year(period)) -> data

avg_2019 <- data |>
  filter(year == 2019) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

numeric_cols <- names(data)[sapply(data, is.numeric)][1:4]


# Use a loop to mutate each numeric column separately
for (col in numeric_cols) {
  data <- data %>%
    mutate(!!col := 100* !!sym(col) / avg_2019[[col]])
}


data |>
  plot_ly(x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}%", width = 1000) |>
  add_lines(y = ~`value.x`, name = "Promet in skladi\u0161\u010denje (H)",  color = I("black")) |>
  add_lines(y = ~`value.y`, name = "Kopenski promet, cevovodni transport (H49)",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x.x`, name = "Skladi\u0161\u010denje in spremljajou010de prometne dejavnosti (H52)",  color = I(umar_cols()[4])) |>
  add_lines(y = ~`value.y.y`, name = "Po\u0161tna in kurirska dejavnost (H53)",  color = I(umar_cols()[2])) |>
  umar_layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Indeks (povpre\u010dje 2019)",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                      font = list(size = 12),
                      x = 0),
         shapes = list(
           list(
             type = "line",
             x0 = min(data$period), x1 = max(data$period),
             y0 = 100, y1 = 100,
             line = list(color = umar_cols("emph"), width = 1)
           )),
         annotations = list(
           x = 1, y = 1, text = "JuPo", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2019-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



