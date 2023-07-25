# get data
df <- read.csv2(here::here("data/071.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()   -> data2

updated <- max(prep_l$updated)

fig1 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_qp(y = ~value.x,  name = "Ustreznost proizvodnih zmogljivosti",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~value.y,  name = "Konkurenčni položaj na domačem trgu",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~value.x.x,  name = "Konkurenčni položaj na trgih držav EU",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~value.y.y,  name = "Konkurenčni položaj na trgih zunaj EU",  color = I(umar_cols()[4])) |>
  add_lines_qp(y = ~value,  name = "Obseg novih naročil",  color = I(umar_cols()[6])) |>
  umar_layout(annotations = list(x = 0 , y = 1,
                                 text = "Rast zaposlenosti dodane vrednosti v nizko tehnolo\u0161ko zahtevnih dejavnostih", showarrow = F,
                                 xref='paper', yref='paper'))

fig1 <- add_empty_lines(fig1, 9)



fig2 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_qp(y = ~value,  name = "Izkoriščenost proizvodnih zmogljivosti",  color = I(umar_cols()[5]))


subplot(fig1, fig2,   nrows = 2, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis('Ravnotežje, v o.t'),
    yaxis2 = umar_yaxis('Delež, v %'),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle("UMAR"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))

