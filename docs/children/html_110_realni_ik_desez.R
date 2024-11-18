# get data
df <- read.csv2(here::here("data/091.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data


plot_ly(data, x = ~period, width = 1000) |>
  add_lines_m(y = ~value.x,  name = "Informacijsko-komunikacijske dejavnosti (J)",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.y,  name = "Telekomunikacijske dejavnosti (J 61)",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.x.x,  name = "Računalniške storitve (J 62)",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~value.y.y,  name = "Druge informacijske dejavnosti (J 63)",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (100 = povprečje 2015)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("AnVi"),
              shapes = emph_line(100, data4$period)) |>
  rangeslider(as.Date("2011-01-01"), max(data$period))
