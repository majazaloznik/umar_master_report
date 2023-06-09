# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l2 <- prep_multi_line(spl[[15]], con)

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

plot_ly(data2, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~value.x,  name = "Kazalnik zaupanja",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Poslovni polo\u017eaj",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Povpra\u0161evanje",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y,   name = "Pričakovano povpra\u0161evanje",color = I(umar_cols()[4])) |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Ravnote\u017eje, v o.t.",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l2$updated,
                                   prep_l2$transf_txt, "(Vir: SURS & prera\u010duni UMAR)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))


