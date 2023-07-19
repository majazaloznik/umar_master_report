# get data
df <- read.csv2(here::here("data/004.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))
# dygraph_plotter_mixed(prep_l)

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|Q%q-%Y}  %{y:.2f}", width = 1000) |>
  add_bars(y = ~`value`, name = "Prispevek k rasti obsega BDP",  color = I(umar_cols()[1])) |>
  umar_layout(barmode = "relative",
         showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Prispevek, v o.t.",
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
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100)|>
  config(modeBarButtonsToAdd = list(dl_button))



