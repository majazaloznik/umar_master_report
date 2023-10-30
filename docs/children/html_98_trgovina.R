# get data
df <- read.csv2(here::here("data/mojca_test.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l4 <- prep_multi_line(spl[[4]], con)

updated <- max(prep_l4$updated)

purrr::reduce(prep_l4$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data4

plot_ly(data4, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Trg. na debelo z živili",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "Trg. na debelo z neživili",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.x.x,  name = "Posredništvo",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value.y.y,  name = "Skupaj trgovina na debelo",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (povprečje 2010)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: 3-m drseča sredina"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data4$period)) |>
  rangeslider(as.Date("2012-01-01"), max(data4$period))

