# get data
df <- read.csv2(here::here("data/020.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

prep_l2 <- invisible(prep_multi_line(spl[[2]], con))
prep_l2$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  left_join(data, by = "period") |>
  as_tibble() -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~`value.y`, name = "Nominalni BDP",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x`, name = "Realni BDP",  color = I(umar_cols()[2])) |>
  layout(annotations = list(x = -0.23 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                                                     prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)\n
                                                                     Posodobljeno:", prep_l2$updated,
                                                                     prep_l2$transf_txt, "(Vir: SURS)                                                                                                                                                  "),
                            font = list(size = 12))) |>
  layout(barmode = "relative",
         showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))))|>
   rangeslider(as.Date("2012-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))



