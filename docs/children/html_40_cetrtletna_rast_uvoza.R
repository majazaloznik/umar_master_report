# get data
df <- read.csv2(here::here("data/025.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data


updated <- max(prep_l$updated, prep_l2$updated)

data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`value.x`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Uvoz blaga in storitev", color = I(umar_cols()[2])) |>
  add_lines(y = ~`value.y`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Uvoz blaga", color = I(umar_cols()[1])) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Uvoz storitev", color = I(umar_cols()[4])) |>
  layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                                                     prep_l$transf_txt, "(Vir: SURS)"),
                            font = list(size = 12))) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+10) |>
  layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="\u010cetrtletna sprememba, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.1),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))))|>
  config(modeBarButtonsToAdd = list(dl_button))
