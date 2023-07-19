# get data
df <- read.csv2(here::here("data/063.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data


updated <- max(prep_l$updated)

x <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_qp(y = ~`value.x`,
            name = "Informacijske in komunikacijske dejavnosti", color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`value.y`,
            name = "Poslovanje z nepremi\u010dninami", color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`value.x.x`,
            name = "Strokovne, znanstvene in tehni\u010dne dejavnosti", color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~`value.y.y`,
            name = "Druge raznovrstne poslovne dejavnosti", color = I(umar_cols()[4])) |>
  umar_layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                                                     prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                            font = list(size = 12))) |>
  umar_layout(
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12)),
                                   fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         annotations = list(
           x = 0.95, y = 1.05, text = "MaHa", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         ))|>
  config(modeBarButtonsToAdd = list(dl_button)) |>
  rangeslider(as.Date("2020-01-01"), max(data$period)+10)
