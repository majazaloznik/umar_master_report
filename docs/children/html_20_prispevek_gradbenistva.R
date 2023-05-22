# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
df <- df |> mutate(year_on_year = ifelse(year_on_year == "y", TRUE, year_on_year))
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[8]][1,], con)
prep_l2 <- prep_multi_line(spl[[8]][2,], con)
prep_l3 <- prep_multi_line(spl[[8]][3,], con)



prep_l$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

prep_l2$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

prep_l3$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data3


updated <- max(prep_l$updated, prep_l2$updated, prep_l3$updated)

fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_bars(y = ~`value`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}",
           name = "Prispevek gradbeni\u0161tva k rasti BDP", color = I(umar_cols()[2])) |>
  layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                                                     prep_l$transf_txt, "(Vir: SURS)"),
                            font = list(size = 12)))

fig2 <- data2 |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Bruto Investicije: zgradbe in objekti ", color = I(umar_cols()[1])) |>
  layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l2$updated,
                                                                     prep_l2$transf_txt, "(Vir: SURS)"),
                            font = list(size = 12)))

fig3 <- data3 |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Realni indeks opravljenih del v gradbeni\u0161tvu", color = I(umar_cols()[5])) |>
  layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l3$updated,
                                                                     prep_l3$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                            font = list(size = 12)))



subplot( fig1, fig2, fig3, nrows = 3, shareX = TRUE) |>
  rangeslider(as.Date("2015-01-01"), max(data$period)+10) |>
  layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="Prispevek k rasti, v o.t.",
                                   font = list(size =12))),
         yaxis2 = list(title = list(text="Medletna sprememba, v %",
                                    font = list(size =12))),
         yaxis3 = list(title = list(text="Medletna sprememba, v %",
                                    font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.1),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))))


