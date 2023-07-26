# get data
df <- read.csv2(here::here("data/078.csv"), encoding = "UTF-8")
df <- df |>  arrange(sub_chart)
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)



purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  -> data2

updated <- max(prep_l$updated)

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(y = ~value.x,  name = "B RUDARSTVO",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y,  name = "B+C+D Industrija",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.x.x,  name = "C PREDELOVALNE DEJAVNOSTI",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value.y.y,  name = "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO",  color = I(umar_cols()[4])) |>
  my_panel_subtitle("Rast industrijske proizvodnje")


fig1 <- add_empty_lines(fig1, 10)


fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(y = ~value.x,  name = "A Proizvodi za vmesno porabo",  color = I(umar_cols()[6])) |>
  add_lines_mp(y = ~value.y,  name = "AE Energenti",  color = I(umar_cols()[7])) |>
  add_lines_mp(y = ~value.x.x,  name = "AI Surovine",  color = I(umar_cols()[8])) |>
  add_lines_mp(y = ~value.y.y,  name = "B Proizvodi za investicije",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.x.x.x,  name = "C Proizvodi za široko porabo",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.y.y.y,  name = "CD Trajni proizvodi za široko porabo",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value,  name = "CN Netrajni proizvodi za široko porabo",  color = I(umar_cols()[4])) |>
  my_panel_subtitle("Rast industrijske proizvodnje po namenskih skupinah")


subplot(fig1, fig2,   nrows = 2, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis('Medletna rast, v %'),
    yaxis2 = umar_yaxis('Medletna rast, v %'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle("UMAR"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2012-01-01"), max(data$period))


