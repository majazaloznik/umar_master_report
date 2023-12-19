# get data
df <- read.csv2(here::here("data/099.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  |>
  rowwise() |>
  dplyr::mutate(ostalo = sum(value.x, value.x.x, value.y.y, value.x.x.x.x.x,
                          value.y.y.y.y.y, value.x.x.x.x.x.x, value.y.y.y.y.y.y,
                            value.x.x.x.x.x.x.x, value.y.y.y.y.y.y.y,
                            value.x.x.x.x.x.x.x.x, value.y.y.y.y.y.y.y.y,
                            value.y.y.y.y.y.y.y.y.y, na.rm = TRUE), .keep = "unused") -> data_prosta


plot_ly(data_prosta, x = ~period, width = 1000) |>
  add_lines_q(y = ~value.y,  name = "C predelovalne dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_q(y = ~value.x.x.x,  name = "F gradbeništvo",  color = I(umar_cols()[2])) |>
  add_lines_q(y = ~value.y.y.y,  name = "G trgovina, vzdrževanje in popravila motornih vozil",  color = I(umar_cols()[3])) |>
  add_lines_q(y = ~value.x.x.x.x,  name = "H promet in skladiščenje",  color = I(umar_cols()[4])) |>
  add_lines_q(y = ~value.y.y.y.y,  name = "I gostinstvo",  color = I(umar_cols()[5])) |>
  add_lines_q(y = ~value.x.x.x.x.x.x.x.x.x,  name = "R kulturne, razvedrilne in rekreacijske dej.",  color = I(umar_cols()[6])) |>
  add_lines_q(y = ~ostalo,  name = "Ostalo",  color = I(umar_cols()[7])) |>
  add_lines_q(y = ~value,  name = "Skupaj",  color = I("black")) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Število prostih delovnih mest'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MiPe")) |>
  rangeslider(as.Date("2011-01-01"), max(data_prosta$period))
