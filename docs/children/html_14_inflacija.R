# inflacija
df <- read.csv2(here::here("data/015.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
# dygraph_plotter_mixed(prep_l)

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  as_tibble() |>
  mutate(value = value - 100) -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|%b %Y} %{y:.2f}%", width = 1000) |>
  add_bars(y = ~`value`, name = "Medletna rast cen \u017eivljenskih potreb\u0161\u010din",  color = I(umar_cols()[1])) |>
  umar_layout(barmode = "relative",
         showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))



