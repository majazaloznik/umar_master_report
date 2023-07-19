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
  as_tibble() |>
  mutate(period = period %m+% months(2)) -> data

prep_l2$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  |>
  mutate(period = period %m+% months(2)) -> data2

prep_l3$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id)  |>
  as_tibble()    -> data3

updated <- max(prep_l$updated, prep_l2$updated, prep_l3$updated)

data3 |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Realni indeks opravljenih del v gradbeni\u0161tvu", color = I(umar_cols()[5])) |>
  add_lines(data = data2, x = ~period, y = ~`value`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}",
           name = "Dodana vrednost gradbeni\u0161tva", color = I(umar_cols()[2])) |>
  add_lines(data = data, x = ~period, y = ~`value`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",#showlegend = FALSE,
            name = "Bruto Investicije: zgradbe in objekti ", color = I(umar_cols()[1])) |>
  umar_layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l3$updated,
                                                                     prep_l3$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                            font = list(size = 12))) |>
  umar_layout(annotations = list(x = 0 , y = .95, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                                                     prep_l$transf_txt, "(Vir: SURS)"),
                            font = list(size = 12))) |>
  rangeslider(as.Date("2013-01-01"), max(data$period)+10) |>
  umar_layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="Medletna sprememba, v %",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         annotations = list(
           x = 0.95, y = 1.05, text = "JaKu", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  config(modeBarButtonsToAdd = list(dl_button))


