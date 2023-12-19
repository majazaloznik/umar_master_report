# get data
df <- read.csv2(here::here("data/093.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data


plot_ly(data, x = ~period, width = 1000,
        height = 600) |>
  add_lines_m(y = ~value.x,  name = "Strokovno-tehnične dejavnosti (M)",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.y,  name = "Pravno-računovodske dejavnosti (M 69)",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.x.x,  name = "Svetovalne storitve (M 70.2)",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~value.y.y,  name = "Arhitekturno-projektantske storitve (M 71)",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (100 = povprečje 2015)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("AnVi"),
              shapes = emph_line(100, data$period)) |>
  rangeslider(as.Date("2011-01-01"), max(data$period))
