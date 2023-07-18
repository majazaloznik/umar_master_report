# get data
df <- read.csv2(here::here("data/050.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

plot_ly(data, x = ~period, width = 1000) |>
  add_lines(y = ~value.x,  name = "Trgovina skupaj",  color = I("black"),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Trgovina z motornimi vozili",color = I(umar_cols()[1]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.x.x,  name = "Trgovina na debelo",  color = I(umar_cols()[2]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y.y, name = "Trgovina na drobno", color = I(umar_cols()[4]),
            hovertemplate="%{x|%b-%Y} %{y:.2f}%") |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Indeks (povpre\u010dje 2015)",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010duni UMAR))"),
                      font = list(size = 12),
                      x = 0),
         shapes = list(
           list(
             type = "line",
             x0 = min(data$period), x1 = max(data$period),
             y0 = 100, y1 = 100,
             line = list(color = umar_cols("emph"), width = 1)
           )),
         annotations = list(
           x = 1, y = 1, text = "MoKo", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))





