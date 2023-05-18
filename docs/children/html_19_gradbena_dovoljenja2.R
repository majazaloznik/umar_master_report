#### stevilo stanovanj
# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[7]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(pravna = value.x,
         pravnar = raw.x,
         fizicna = value.y,
         fizicnar = raw.y) |>
  dplyr::arrange(period) -> data


updated <- prep_l$updated

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~`pravnar`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Pravne osebe - originalni", color = I(umar_cols()[3])) |>
  add_lines(y = ~`pravna`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Pravne osebe", color = I(umar_cols()[5])) |>
  add_lines(y = ~`fizicnar`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Fizi\u010dne osebe - originalni", color = I(umar_cols()[2]),
            opacity = 0.5) |>
  add_lines(y = ~`fizicna`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Fizi\u010dne osebe", color = I(umar_cols()[1])) |>
  rangeslider(as.Date("2013-01-01"), max(data$period)+10) |>
  layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(fixedrange = FALSE,
                      range = c(0, 350),
                      title = list(text="\u0160tevilo stanovanj v stavbah",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0))

