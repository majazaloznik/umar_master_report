# get data
df <- read.csv2(here::here("data/003.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
# dygraph_plotter_mixed(prep_l)
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

prep_l2 <- invisible(prep_multi_line(spl[[2]], con))
# dygraph_plotter_mixed(prep_l)
prep_l2$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  left_join(data, by = "period") |>
  as_tibble() -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~`value.y`, name = "BDP - stalne cene (2010)",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x`, name = "BDP - teko\u010de cene",  color = I(umar_cols()[2])) |>
  layout(barmode = "relative",
         showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Znesek, v mio EUR",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0))|>
  rangeslider(as.Date("2012-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))



