# get data
df <- read.csv2(here::here("data/067.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

prep_l2 <- prep_multi_line(spl[[2]], con)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data


purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

updated <- max(prep_l$updated, prep_l2$updated)

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_ap(y = ~value.x,  name = "Proizvodnja \u017eivil",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~value.y,  name = "Proizvodnja pija\u010d",  color = I(umar_cols()[2])) |>
  add_lines_ap(y = ~value.x.x,  name = "Proizvodnja tekstilij",  color = I(umar_cols()[3])) |>
  add_lines_ap(y = ~value.y.y,  name = "Proizvodnja obla\u010dil",  color = I(umar_cols()[4])) |>
  add_lines_ap(y = ~value,  name = "Usnjarstvo",  color = I(umar_cols()[5])) |>
  my_panel_subtitle("Rast dodane vrednosti v nizko tehnolo\u0161ko zahtevnih dejavnostih")

fig1 <- add_empty_lines(fig1, 9)


fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_ap(y = ~value.x,  name = "Lesna industrija",  color = I(umar_cols()[6])) |>
  add_lines_ap(y = ~value.y,  name = "Papirna industrija",  color = I(umar_cols()[7])) |>
  add_lines_ap(y = ~value.x.x,  name = "Tiskarstvo",  color = I(umar_cols()[8])) |>
  add_lines_ap(y = ~value.y.y,  name = "Pohi\u0161tvena industrija",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~value,  name = "Dr. razno. pred. dej.",  color = I(umar_cols()[2]))


subplot(fig1, fig2,   nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Medletna sprememba, v %'),
    yaxis2 = umar_yaxis('Medletna sprememba, v %'),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


