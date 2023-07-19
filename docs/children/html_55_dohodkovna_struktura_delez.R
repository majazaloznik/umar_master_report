# get data
df <- read.csv2(here::here("data/041.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[2]], con))


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data



data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_qp(y = ~`value.x`, name = "Bruto poslovni prese\u017eek in razn. doh.",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`value.y`, name = "Sredstva za zaposlene",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~`value.x.x`, name = "Davki na proizvodnjo in uvoz",  color = I(umar_cols()[4])) |>
  add_lines_qp(y = ~`value.y.y`, name = "Subvencije na proizvodnjo",  color = I(umar_cols()[1])) |>
  umar_layout(
    showlegend = TRUE,
    autosize = F, margin = m,
    font=list(family = "Myriad Pro"),
    yaxis = list(title = list(text="Dele\u017e, v %",
                              font = list(size =12))),
    xaxis = list(title = "",
                 tickformatstops = list(
                   list(dtickrange = list(NULL, "M6"),
                        value = "%Y"),
                   list(dtickrange = list("M6", NULL),
                        value = "%Y"))),
    title = list(text = paste("Posodobljeno:", prep_l$updated, "(Vir: SURS)"),
                 font = list(size = 12),
                 x = 0),
    annotations = list(
      x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
      xref='paper', yref='paper', xanchor='right', yanchor='top',
      font=list(size=10, color = umar_cols()[3])
    )) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))

