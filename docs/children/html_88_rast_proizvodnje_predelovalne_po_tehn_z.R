# get data
df <- read.csv2(here::here("data/074.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()  |>
  mutate(across(starts_with("value"), ~ (. / lag(.) - 1)*100, .names = "monthly_{.col}")) -> data

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()   -> data2

updated <- max(prep_l$updated)



fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(y = ~monthly_value.x,  name = "Predelovalne dejavnosti",  color = I(umar_cols()[3]), fill = "tozeroy") |>
  add_lines_mp(y = ~monthly_value.y,  name = "Nizko teh. zaht. dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~monthly_value.x.x,  name = "Srednje nizko teh. zaht. dejavnosti",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~monthly_value.y.y,  name = "Srednje visoko teh. zaht. dejavnosti",  color = I(umar_cols()[8])) |>
  add_lines_mp(y = ~monthly_value,  name = "Visoko teh. zaht. dejavnosti",  color = I(umar_cols()[4])) |>
  my_panel_subtitle("Rast proizvodnje po tehnološki zahtevnosti predelovalnih dejavnosti, mesečno")

fig1 <- add_empty_lines(fig1, 9)



fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_mp(y = ~value.x,  name = "Predelovalne dejavnosti",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~value.y,  name = "Nizko teh. zaht. dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.x.x,  name = "Srednje nizko teh. zaht. dejavnosti",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.y.y,  name = "Srednje visoko teh. zaht. dejavnosti",  color = I(umar_cols()[8])) |>
  add_lines_mp(y = ~value,  name = "Visoko teh. zaht. dejavnosti",  color = I(umar_cols()[4])) |>
  my_panel_title("UMAR", prep_l2$transf_txt) |>
  my_panel_subtitle("Proizvodnja predelovalnih dejavnosti po tehnološki zahtevnosti") |>
  layout(shapes = emph_line())



subplot(fig1, fig2,   nrows = 2, shareX = TRUE) |>
  umar_layout(
    yaxis = umar_yaxis('Mesečna rast, v %'),
    yaxis2 = umar_yaxis('Indeks (povprečje 2015)'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle("UMAR"),
    annotations = initials("TiNe")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))

