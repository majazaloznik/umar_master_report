#### Investicijska dejavnost - option 2
# get data
df <- read.csv2(here::here("data/008.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
colnames(data)[2:4] <- c("Oprema in stroji", "Zgradbe in objekti", "Bruto investicije v osnovna sredstva")

data <- data |> left_join(prep_l2$data_points[[1]]) |>
  select(-period_id) |>
  rename("Spremembe zalog" = value)

updated <- prep_l$updated

fig1 <- data |>
  plot_ly(x = ~period,  width = 1000, height = 600) |>
  add_lines_qp(y = ~`Oprema in stroji`,
            name = "Oprema in stroji", color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`Zgradbe in objekti`,
            name = "Zgradbe in objekti", color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`Bruto investicije v osnovna sredstva`,
            name = "Bruto investicije v osnovna sredstva", color = I(umar_cols()[4])) |>
  umar_layout(font=list(family = "Myriad Pro"))

for(i in 1:9) {
  fig1 <- fig1 |>
    add_lines(y = ~c,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}

fig2 <-  data |>
  plot_ly(x = ~period,  width = 1000,
          height = 600) |>
  add_bars_q( x = data$period,  y = data$`Spremembe zalog`,
            name = "Spremembe zalog",
            color = I(umar_cols()[3])) |>
  umar_layout(font=list(family = "Myriad Pro"))

subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(showlegend = TRUE,
         autosize = F, margin =  m,
         font=list(family = "Myriad Pro"),
         yaxis = list(fixedrange = FALSE,
                      title = list(text = 'Medletna sprememba, v %', font = list(size =12))),
         yaxis2 = list(fixedrange = FALSE,
                       title = list(text = 'Prispevek h rasti BDP, v o.t.', font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2018-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))

