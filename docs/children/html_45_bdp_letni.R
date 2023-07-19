# get data
df <- read.csv2(here::here("data/031.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)

# prepare data
prep_l <- invisible(prep_multi_line(spl[[1]], con))
prep_l2 <- invisible(prep_multi_line(spl[[2]], con))

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename( stalne = value) |>
  as_tibble() -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  rename(tekoce = value) |>
  select(-raw) |>
  left_join(data, by = "period") |>
  as_tibble() -> data2

data2 |>
  plot_ly(x = ~period, hovertemplate="%{x|%Y} %{y:.2f}%", width = 1000) |>
  add_lines(y = ~`stalne`, name = "BDP - stalne cene",  color = I(umar_cols()[1])) |>
  add_lines(y = ~`tekoce`, name = "BDP - teko\u010de cene",  color = I(umar_cols()[2])) |>
  umar_layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list(NULL, "M6"),
                             value = "%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y")),
                      rangemode = "tozero"),
         title = list(text = paste("Posodobljeno:", prep_l$updated, "(Vir: SURS & prera\u010duni UMAR))"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2012-01-01"), max(data$period) + 100) |>
  config(modeBarButtonsToAdd = list(dl_button))



