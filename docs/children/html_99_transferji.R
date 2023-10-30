# get data
df <- read.csv2(here::here("data/mojca_test.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l5 <- prep_multi_line(spl[[5]], con)

updated <- max(prep_l5$updated)

purrr::reduce(prep_l5$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data5

plot_ly(data5, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~raw,  name = "Socialni transferji (nezglajeni)",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value,  name = "Socialni transferji",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (povprečje 2010)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: 3-m drseča sredina"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data5$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data5$period))

