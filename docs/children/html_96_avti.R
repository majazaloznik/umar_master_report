# get data
df <- read.csv2(here::here("data/mojca_test.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data

prep_l2 <- prep_multi_line(spl[[2]], con)

updated <- max(prep_l2$updated)

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

plot_ly(data2, x = ~period, width = 1000) |>
  add_lines_mp(y = ~value.x,  name = "Fizične osebe",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "Pravne osebe",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value,  name = "Skupaj prodaja avtov",  color = I(umar_cols()[3])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (povprečje 2010)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: 3-m drseča sredina"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data2$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data2$period))




