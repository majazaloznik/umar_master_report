# get data
df <- read.csv2(here::here("data/019.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))

prep_l2 <- invisible(prep_multi_line(spl[[2]], con))

prep_l2$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data2

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  left_join(data2, by = "period") -> data



data |>
  plot_ly(x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000) |>
  add_bars(y = ~`value.x`, name = "Doma\u010da potro\u0161nja",  color = I(umar_cols()[1])) |>
  add_bars(y = ~`value.y`, name = "Neto izvoz",  color = I(umar_cols()[2])) |>
  add_lines(y = ~`value`, name = "Realna rast BDP (v %)",  color = I("black")) |>
  layout(barmode = "relative",
         showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Prispevek k medletni rasti BDP, v o.t",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



