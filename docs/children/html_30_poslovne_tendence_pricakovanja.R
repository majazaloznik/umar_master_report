# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[6]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

plot_ly(data, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~value.x,  name = "Pri\u010dakovana proizvodnja",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Pri\u010dakovan izvoz",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Pri\u010dakovano zaposlovanje v gradbeni\u0161tvu",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y,   name = "Pri\u010dakovano povpra\u0161evanje v trgovini na drobno",color = I(umar_cols()[4])) |>
  add_lines(y = ~value,   name = "Pri\u010dakovano povpra\u0161evanje v storitvenih dejavnostih",color = I(umar_cols()[5])) |>
  umar_layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Ravnote\u017eje, v o.t.",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "MaHr", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))
