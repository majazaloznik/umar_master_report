# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[2]], con)
prep_l2 <- prep_multi_line(spl[[12]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2


plot_ly(data, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~value.x,  name = "Kazalnik zaupanja",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Zaloge kon\u010dnih izdelkov",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Pri\u010dakovana proizvodnja",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y, name = "Izvozna naro\u010dila", color = I(umar_cols()[4])) |>
  add_lines(y = ~value,  name = "Pri\u010dakovan izvoz",  color = I(umar_cols()[5])) |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Ravnote\u017eje, v o.t.",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010duni UMAR)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))


fig2 <- plot_ly(data2, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000,
                height = 600) |>
  add_lines(y = ~value.x,  name = "Kazalnik zaupanja",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Zaloge kon\u010dnih izdelkov",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Pri\u010dakovana proizvodnja",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y, name = "Izvozna naro\u010dila", color = I(umar_cols()[4])) |>
  add_lines(y = ~value,  name = "Pri\u010dakovan izvoz",  color = I(umar_cols()[5])) |>
  layout(annotations = list(x = 0. , y = 1, text = "Poslovne tendence v predelovalnih dejavnostih - trimese\u010dne drse\u010de sredine (desne)", showarrow = F,
                            xref='paper', yref='paper'))|>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Ravnote\u017eje, v o.t.",
                                   font = list(size =12)), fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))



# subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
#   layout(showlegend = TRUE,
#          autosize = F, margin = m,
#          font=list(family = "Myriad Pro"),
#          yaxis = list(title = list(text="Ravnote\u017eje, v o.t.",
#                                    font = list(size =12))),
#          yaxis2 = list(title = list(text="Ravnote\u017eje, v o.t.",
#                                     font = list(size =12))),
#          xaxis = list(title = "",
#                       rangeslider = list(thickness = 0.05),
#                       tickformatstops = list(
#                         list(dtickrange = list("M1", "M6"),
#                              value = "Q%b %Y"),
#                         list(dtickrange = list("M6", NULL),
#                              value = "%Y"))),
#          title = list(text = paste("Posodobljeno:", prep_l$updated,
#                                    prep_l$transf_txt, "(Vir: SURS & prera\u010duni UMAR)"),
#                       font = list(size = 12),
#                       x = 0)) |>
#   rangeslider(as.Date("2018-01-01"), max(data$period))|>
#   config(modeBarButtonsToAdd = list(dl_button))
