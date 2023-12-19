# get data
df <- read.csv2(here::here("data/mojca_test.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l3 <- prep_multi_line(spl[[3]], con)

updated <- max(prep_l3$updated)

purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data3

plot_ly(data3, x = ~period, width = 1000) |>
  add_lines_mp(y = ~raw,  name = "Realna masa neto plač (nezglajena)",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value,  name = "Realna masa neto plač",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (povprečje 2010)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: 3-m drseča sredina"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data3$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data3$period))

