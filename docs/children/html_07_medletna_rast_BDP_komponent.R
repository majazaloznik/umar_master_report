# get data
df <- read.csv2(here::here("data/007.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data


fig1 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                height = 1000) |>
  add_lines(y = ~value.x.x,  name = "Bruto investicije v o.s.",  color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y.y,   name = "Izvoz blaga in storitev",color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.x.x.x,  name = "Uvoz blaga in storitev",  color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y.y.y, name = "BDP", color = I("black")) |>
  add_lines(y = ~value.y,  name = "Kon\u010dna potro\u0161nja dr\u017eave",  color = I(umar_cols()[4])) |>
  add_lines(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev",  color = I(umar_cols()[1])) |>
  layout(annotations = list(x = 0.2 , y = .95, text = "Kon\u010dna potro\u0161nja", showarrow = F,
                            xref='paper', yref='paper'))

fig3 <-  plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                 height = 1000) |>
  add_lines(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev", color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y, name = "Kon\u010dna potro\u0161nja dr\u017eave",  color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y.y,   name = "Izvoz blaga in storitev", color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.x.x.x,   name = "Uvoz blaga in storitev",  color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y.y.y, name = "BDP",  color = I("black"), showlegend = FALSE) |>
  add_lines(y = ~value.x.x, name = "Bruto investicije v o.s.",  color = I(umar_cols()[5])) |>
  layout(annotations = list(x = 0.2 , y = .95, text = "Bruto investicije v o.s.", showarrow = F,
                            xref='paper', yref='paper'))

fig4 <-  plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                 height = 1000) |>
  add_lines(y = ~value.x.x,  name = "Bruto investicije v o.s.",  color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y, name = "Kon\u010dna potro\u0161nja dr\u017eave", color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev", color = I("light gray"), showlegend = FALSE) |>
  add_lines(y = ~value.y.y.y, name = "BDP", color = I("black"), showlegend = FALSE) |>
  add_lines(y = ~value.y.y, name = "Izvoz blaga in storitev",  color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x.x, name = "Uvoz blaga in storitev",  color = I(umar_cols()[6])) |>
  layout(annotations = list(x = 0.2 , y = .95, text = "Zunanja trgovina", showarrow = F,
                            xref='paper', yref='paper'))



subplot(fig1,  fig3, fig4,  nrows = 3, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))




