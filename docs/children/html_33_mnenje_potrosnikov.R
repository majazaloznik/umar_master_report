# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[9]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

plot_ly(data, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000) |>
  add_lines(y = ~value.x,  name = "Kazalnik zaupanja potro\u0161nikov",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Finan\u010dno stanje v gospodinjstvu v prihodnjih 12 mesecih",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Gospodarsko stanje v Sloveniji v prihodnjih 12 mesecih",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y,   name = "Gibanje cen v prihodnjih 12 mesecih",color = I(umar_cols()[4])) |>
  add_lines(y = ~value.x.x.x,  name = "Raven brezposelnosti v prihodnjih 12 mesecih",  color = I(umar_cols()[5])) |>
  add_lines(y = ~value.y.y.y,   name = "Var\u010devanje v prihodnjih 12 mesecih",color = I(umar_cols()[6])) |>
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
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))
