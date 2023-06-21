# get data
df <- read.csv2(here::here("data/041.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

fig1 <- data |>
  plot_ly(x = ~period, hovertemplate="%{x|%Y} %{y:.2f}%", width = 1000, height = 600) |>
  add_lines(y = ~`value.x`, name = "Nominalni BDP",  color = I("black")) |>
  add_lines(y = ~`value.y`, name = "Bruto poslovni prese\u017eek in razn. doh.",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x.x`, name = "Sredstva za zaposlene",  color = I(umar_cols()[2])) |>
  add_lines(y = ~`value.y.y`, name = "Davki na proizvodnjo in uvoz",  color = I(umar_cols()[3]))

for(i in 1:8) {
  fig1 <- fig1 |>
    add_lines(data = data, y = ~value.x,  name = "\u200A",  color = I('rgba(0,0,0,0)'))
}

fig2 <- data |>
  plot_ly(x = ~period, hovertemplate="%{x|%Y} %{y:.2f}%", width = 1000, height = 600) |>
  add_lines(y = ~`value.x`, name = "Nominalni BDP",  color = I("black")) |>
  add_lines(y = ~`value`, name = "Subvencije na proizvodnjo",  color = I(umar_cols()[4]))

subplot(fig1,  fig2, nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = T, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         yaxis2 = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated, "(Vir: SURS & prera\u010dun UMAR)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))


