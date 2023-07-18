# get data
df <- read.csv2(here::here("data/007.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data


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
  layout(annotations = list(x = 0.2 , y = .95, text = "Kon\u010dna potro\u0161nja", showarrow = F,
                            xref='paper', yref='paper'))
for(i in 1:9) {
  fig1 <- fig1 |>
    add_lines(y = ~c,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}

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
  layout(annotations = list(x = 0.2 , y = .95, text = "Bruto investicije v o.s.", showarrow = F,
                            xref='paper', yref='paper'))

for(i in 1:9) {
  fig3 <- fig3 |>
    add_lines(y = ~c,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}


fig4 <-  plot_ly(data, x = ~period, width = 1000,
                 height = 800) |>
  add_lines(y = ~value.x.x,  name = "Bruto investicije v o.s.",  color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y, name = "Kon\u010dna potro\u0161nja dr\u017eave", color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.x, name = "Kon\u010dna potro\u0161nja gospodinjstev", color = I("light gray"), showlegend = FALSE,
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y.y.y, name = "BDP", color = I("black"),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y.y, name = "Izvoz blaga in storitev",  color = I(umar_cols()[2]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.x.x.x, name = "Uvoz blaga in storitev",  color = I(umar_cols()[6]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  layout(annotations = list(x = 0.2 , y = .95, text = "Zunanja trgovina", showarrow = F,
                            xref='paper', yref='paper'))



subplot(fig1,  fig3, fig4,  nrows = 3, shareX = TRUE) |>
  umar_layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12)),
                      range = c(-30, 40), fixedrange = FALSE),
         yaxis2 = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12)),
                       range = c(-30, 40), fixedrange = FALSE),
         yaxis3 = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12)),
                       range = c(-30, 40), fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = slider_w),
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))




