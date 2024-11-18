# get data
df <- read.csv2(here::here("data/099.csv"), encoding = "UTF-8")
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
  add_lines_m(y = ~value.x,  name = "B rudarstvo",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.y,  name = "C predelovalne dejavnosti",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.x.x,  name = "D oskrba z električno energijo, plinom in paro",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~value.y.y,  name = "E oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja",  color = I(umar_cols()[4])) |>
  add_lines_m(y = ~value.x.x.x,  name = "F gradbeništvo",  color = I(umar_cols()[5])) |>
  add_lines_m(y = ~value.y.y.y,  name = "G trgovina, vzdrževanje in popravila motornih vozil",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.x.x.x.x,  name = "H promet in skladiščenje",  color = I(umar_cols()[6])) |>
  add_lines_m(y = ~value.y.y.y.y,  name = "I gostinstvo",  color = I(umar_cols()[7])) |>
  add_lines_m(y = ~value.x.x.x.x.x,  name = "J informacijske in komunikacijske dejavnosti",  color = I(umar_cols()[8])) |>
  add_lines_m(y = ~value.y.y.y.y.y,  name = "K finančne in zavarovalniške dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.x.x.x.x.x.x,  name = "L poslovanje z nepremičninami",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.y.y.y.y.y.y,  name = "M strokovne, znanstvene in tehnične dejavnosti",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~value.x.x.x.x.x.x.x,  name = "N druge raznovrstne poslovne dejavnosti",  color = I(umar_cols()[4])) |>
  add_lines_m(y = ~value.y.y.y.y.y.y.y,  name = "O dejavnost javne uprave in obrambe, dejavnost obvezne socialne varnosti",  color = I(umar_cols()[5])) |>
  add_lines_m(y = ~value.x.x.x.x.x.x.x.x,  name = "P izobraževanje",  color = I(umar_cols()[6])) |>
  add_lines_m(y = ~value.y.y.y.y.y.y.y.y,  name = "Q zdravstvo in socialno varstvo",  color = I(umar_cols()[7])) |>
  add_lines_m(y = ~value.x.x.x.x.x.x.x.x.x,  name = "R kulturne, razvedrilne in rekreacijske dejavnosti",  color = I(umar_cols()[8])) |>
  add_lines_m(y = ~value.y.y.y.y.y.y.y.y.y,  name = "S druge dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value,  name = "Skupaj -- skd dejavnost - skupaj [B do S]",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Števi'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MiPe")) |>
  rangeslider(as.Date("2011-01-01"), max(data$period))









