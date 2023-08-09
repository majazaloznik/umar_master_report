# get data
df <- read.csv2(here::here("data/007.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
updated <- prep_l$updated


fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 800) |>
  add_lines(y = ~value.x.x,  name = "Bruto investicije v o.s.",  color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",) |>
  add_lines(y = ~value.y.y,   name = "Izvoz blaga in storitev",color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",) |>
  add_lines(y = ~value.x.x.x,  name = "Uvoz blaga in storitev",  color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",) |>
  add_lines(y = ~value.y.y.y, name = "BDP", color = I("black"),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",) |>
  add_lines(y = ~value.y,  name = "Kon\u010dna potro\u0161nja dr\u017eave",  color = I(umar_cols()[4]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",) |>
  add_lines(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev",  color = I(umar_cols()[1]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",) |>
  umar_layout(slider_w, m, annotations = list(x = 0.2 , y = .95, text = "Kon\u010dna potro\u0161nja", showarrow = F,
                            xref='paper', yref='paper'))

fig1 <- add_empty_lines(fig1, 9)

fig3 <-  plot_ly(data, x = ~period, width = 1000,
                 height = 800) |>
  add_lines(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev", color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y, name = "Kon\u010dna potro\u0161nja dr\u017eave",  color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y.y,   name = "Izvoz blaga in storitev", color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.x.x.x,   name = "Uvoz blaga in storitev",  color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y.y.y, name = "BDP",  color = I("black"),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.x.x, name = "Bruto investicije v o.s.",  color = I(umar_cols()[5]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  umar_layout(slider_w, m,annotations = list(x = 0.2 , y = .95, text = "Bruto investicije v o.s.", showarrow = F,
                            xref='paper', yref='paper'))

fig3 <- add_empty_lines(fig3, 9)

fig4 <-  plot_ly(data, x = ~period, width = 1000,
                 height = 800) |>
  add_lines_qp(y = ~value.x.x,  name = "Bruto investicije v o.s.",  color = I("light gray"), showlegend = FALSE) |>
  add_lines_qp(y = ~value.y, name = "Kon\u010dna potro\u0161nja dr\u017eave", color = I("light gray"), showlegend = FALSE) |>
  add_lines_qp(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev", color = I("light gray"), showlegend = FALSE) |>
  add_lines_qp(y = ~value.y.y.y, name = "BDP", color = I("black")) |>
  add_lines_qp(y = ~value.y.y, name = "Izvoz blaga in storitev",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~value.x.x.x, name = "Uvoz blaga in storitev",  color = I(umar_cols()[6])) |>
  umar_layout(slider_w, m,annotations = list(x = 0.2 , y = .95, text = "Zunanja trgovina", showarrow = F,
                            xref='paper', yref='paper'))



subplot(fig1,  fig3, fig4,  nrows = 3, shareX = TRUE) |>
  umar_layout(slider_w, m,
      yaxis = umar_yaxis('Medletna rast, v %', range = c(-30, 40)),
      yaxis2 = umar_yaxis('Medletna rast, v %', range = c(-30, 40)),
      yaxis3 = umar_yaxis('Medletna rast, v %', range = c(-30, 40)),
      xaxis = umar_xaxis("Q"),
      title = umar_subtitle(updated, "UMAR"),
      annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))



