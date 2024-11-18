# get data
df <- read.csv2(here::here("data/067.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[3]], con)

prep_l2 <- prep_multi_line(spl[[4]], con)

prep_l3 <- prep_multi_line(spl[[5]], con)
updated <- max(prep_l$updated, prep_l2$updated, prep_l3$transf_txt)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data3



fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 800) |>
  add_lines_ap(y = ~value.x,  name = "Gumarska industrija",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~value.y,  name = "Proizvodnja nekovin. mineral. izd.",  color = I(umar_cols()[2])) |>
  add_lines_ap(y = ~value.x.x,  name = "Proizvodnja kovin",  color = I(umar_cols()[3])) |>
  add_lines_ap(y = ~value.y.y,  name = "Proizvodnja kovin. izd.",  color = I(umar_cols()[4])) |>
  add_lines_ap(y = ~value,  name = "Popravila in monta\u017ea",  color = I(umar_cols()[5])) |>
  my_panel_subtitle("Rast dodane vrednosti v srednje nizko tehnolo\u0161ko zahtevnih dejavnostih")

fig1 <- add_empty_lines(fig1, 7)

fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 800) |>
  add_lines_ap(y = ~value.x,  name = "Kemi\u010d industrija",  color = I(umar_cols()[6])) |>
  add_lines_ap(y = ~value.y,  name = "Proiz. el. naprav",  color = I(umar_cols()[7])) |>
  add_lines_ap(y = ~value.x.x,  name = "Proiz. dr. strojev in naprav",  color = I(umar_cols()[8])) |>
  add_lines_ap(y = ~value.y.y,  name = "Proiz. mot. vozil in plovil",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~value,  name = "Proiz. dr. vozil in plovil.",  color = I(umar_cols()[2])) |>
  my_panel_subtitle("Rast dodane vrednosti v srednje visoko tehnolo\u0161ko zahtevnih dejavnostih")

fig2 <- add_empty_lines(fig2, 7)

fig3 <- plot_ly(data3, x = ~period, width = 1000,
                height = 800) |>
  add_lines_ap(y = ~value.x,  name = "Farmacevtska industrija",  color = I(umar_cols()[3])) |>
  add_lines_ap(y = ~value.y,  name = "Proiz. IKT opreme",  color = I(umar_cols()[4]))  |>
  my_panel_subtitle("Rast dodane vrednosti v visoko tehnolo\u0161ko zahtevnih dejavnostih")


subplot(fig1, fig2, fig3,  nrows = 3, shareX = TRUE) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Medletna sprememba, v %'),
    yaxis2 = umar_yaxis('Medletna sprememba, v %'),
    yaxis3 = umar_yaxis('Medletna sprememba, v %'),
    xaxis = umar_xaxis("A"),
    title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


