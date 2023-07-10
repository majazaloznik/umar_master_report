# get data
df <- read.csv2(here::here("data/043.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data

prep_l <- invisible(prep_multi_line(spl[[1]], con))



purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

data |>
  plot_ly(x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}%", width = 1000) |>
  add_bars(y = ~`value.x`, name = "Tr\u017ene storitve (H+I+J+L+M+N)",  color = I(umar_cols()[3])) |>
  add_lines(y = ~`value.y`, name = "Promet in skladi\u0161\u010denje (H)",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`value.x.x`, name = "Gostinstvo (I)",  color = I(umar_cols()[2])) |>
  add_lines(y = ~`value.y.y`, name = "Informacijske in komunikacijske dejavnosti (J)", color = I(umar_cols()[4])) |>
  add_lines(y = ~`value.x.x.x`, name = "Strokovne, znanstvene in tehni\u010dne dejavnosti (M)",  color = I(umar_cols()[5])) |>
  add_lines(y = ~`value.y.y.y`, name = "Druge raznovrstne poslovne dejavnosti (N)",  color = I(umar_cols()[6])) |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
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
                      x = 0)) |>
  rangeslider(as.Date("2019-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



