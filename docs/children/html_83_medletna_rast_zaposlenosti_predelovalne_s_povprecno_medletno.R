# get data
df <- read.csv2(here::here("data/069.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l0 <- prep_multi_line(spl[[1]], con)
prep_l <- prep_multi_line(spl[[2]], con)
prep_l2 <- prep_multi_line(spl[[3]], con)

prep_l0$data_points[[1]] |>
  as_tibble()  |>
  select(-period_id) |>
  relocate(period) |>
  select(-raw) |>
  rename(value_skupaj = value) -> skupaj

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()   |>
  left_join(skupaj) -> data

data |>
mutate(year = year(period),
       month = month(period)) |>
  group_by(year) |>
  mutate(across(starts_with("raw"), ~cummean(.), .names = "cumavg_{.col}")) |>
  ungroup() |>
  mutate(across(starts_with("cumavg_raw"), ~(. /lag(., n = 12) -1)*100, .names = "yoy_{.col}"))   |>
 group_by(year) |>
  mutate(across(starts_with("yoy_cumavg_raw"), ~last(.), .names = "last_{.col}"))  -> data_g
data_g |>
  ungroup() -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  |>
  left_join(skupaj) -> data2

data2 |>
  mutate(year = year(period),
         month = month(period)) |>
  group_by(year) |>
  mutate(across(starts_with("raw"), ~cummean(.), .names = "cumavg_{.col}")) |>
  ungroup() |>
  mutate(across(starts_with("cumavg_raw"), ~(. /lag(., n = 12) -1)*100, .names = "yoy_{.col}"))   |>
  group_by(year) |>
  mutate(across(starts_with("yoy_cumavg_raw"), ~last(.), .names = "last_{.col}"))  -> data_g2
data_g2 |>
  ungroup() -> data2


updated <- max(prep_l$updated)

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Proizvodnja \u017eivil",  color = I(umar_cols()[1])) |>
  add_lines_mp(data = data_g, y = ~last_yoy_cumavg_raw.x,  name = "Proizvodnja \u017eivil - povpre\u010dna medletna",
               color = I(umar_cols()[1]), line = list(dash = "dot")) |>
  add_lines_mp(data = data, y = ~value.y,  name = "Proizvodnja pija\u010d",  color = I(umar_cols()[2])) |>
    add_lines_mp(data = data_g, y = ~last_yoy_cumavg_raw.y,  name = "Proizvodnja pija\u010d - povpre\u010dna medletna",
                 color = I(umar_cols()[2]), line = list(dash = "dot")) |>
  add_lines_mp(data = data, y = ~value.x.x,  name = "Proizvodnja tekstilij",  color = I(umar_cols()[3])) |>
  add_lines_mp(data = data_g, y = ~last_yoy_cumavg_raw.x.x,  name = "Proizvodnja tekstilij - povpre\u010dna medletna",
               color = I(umar_cols()[3]), line = list(dash = "dot")) |>
  add_lines_mp(data = data, y = ~value.y.y,  name = "Proizvodnja obla\u010dil",  color = I(umar_cols()[4])) |>
  add_lines_mp(data = data_g, y = ~last_yoy_cumavg_raw.y.y,  name = "Proizvodnja obla\u010dil - povpre\u010dna medletna",
               color = I(umar_cols()[4]), line = list(dash = "dot")) |>
  add_lines_mp(data = data, y = ~value,  name = "Usnjarstvo",  color = I(umar_cols()[5])) |>
  add_lines_mp(data = data_g, y = ~last_yoy_cumavg_raw,  name = "Usnjarstvo - povpre\u010dna medletna",
               color = I(umar_cols()[4]), line = list(dash = "dot")) |>
  umar_layout(annotations = list(x = 0 , y = 1,
                                 text = "Rast zaposlenosti dodane vrednosti v nizko tehnolo\u0161ko zahtevnih dejavnostih", showarrow = F,
                                 xref='paper', yref='paper'))

fig1 <- add_empty_lines(fig1, 4)


fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(data = data2, y = ~value.x,  name = "Lesna industrija",  color = I(umar_cols()[6])) |>
  add_lines_mp(data = data_g2, y = ~last_yoy_cumavg_raw.x,  name = "Lesna industrija - povpre\u010dna medletna",
               color = I(umar_cols()[6]), line = list(dash = "dot")) |>
  add_lines_mp(data = data2,y = ~value.y,  name = "Papirna industrija",  color = I(umar_cols()[7])) |>
  add_lines_mp(data = data_g2, y = ~last_yoy_cumavg_raw.y,  name = "Papirna industrija - povpre\u010dna medletna",
               color = I(umar_cols()[7]), line = list(dash = "dot")) |>
  add_lines_mp(data = data2, y = ~value.x.x,  name = "Tiskarstvo",  color = I(umar_cols()[8])) |>
  add_lines_mp(data = data_g2, y = ~last_yoy_cumavg_raw.x.x,  name = "Tiskarstvo - povpre\u010dna medletna",
               color = I(umar_cols()[8]), line = list(dash = "dot")) |>
  add_lines_mp(data = data2, y = ~value.y.y,  name = "Pohi\u0161tvena industrija",  color = I(umar_cols()[1])) |>
  add_lines_mp(data = data_g2, y = ~last_yoy_cumavg_raw.y.y,  name = "Pohi\u0161tvena industrija - povpre\u010dna medletna",
               color = I(umar_cols()[1]), line = list(dash = "dot")) |>
  add_lines_mp(data = data2, y = ~value,  name = "Dr. razno. pred. dej.",  color = I(umar_cols()[2])) |>
  add_lines_mp(data = data_g2, y = ~last_yoy_cumavg_raw,  name = "Dr. razno. pred. dej. - povpre\u010dna medletna",
               color = I(umar_cols()[2]), line = list(dash = "dot"))



subplot(fig1, fig2,   nrows = 2, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis('Medletna rast, v %'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle("UMAR"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


