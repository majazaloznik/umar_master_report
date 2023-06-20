# get data
df <- read.csv2(here::here("data/040.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|%Y} %{y:.2f}%", width = 1000) |>
  add_lines(y = ~`value.x`, name = "Izvoz blaga in storitev",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.y`, name = "Izvoz blaga",  color = I(umar_cols()[2])) |>
  add_lines(y = ~`value`, name = "Izvoz storitev",  color = I(umar_cols()[3])) |>
  layout(barmode = "relative",
         showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



