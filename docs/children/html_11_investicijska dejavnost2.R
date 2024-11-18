#### Investicijska dejavnost - option 2
# get data
df <- read.csv2(here::here("data/008.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)


purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
colnames(data)[2:4] <- c("Oprema in stroji", "Zgradbe in objekti", "Bruto investicije v osnovna sredstva")

data <- data |> left_join(prep_l2$data_points[[1]]) |>
  select(-period_id) |>
  rename("Spremembe zalog" = value)

updated <- max(prep_l$updated, prep_l2$updated)

fig1 <- data |>
  plot_ly(x = ~period,  width = 1000, height = 600) |>
  add_lines_qp(y = ~`Oprema in stroji`,
            name = "Oprema in stroji", color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`Zgradbe in objekti`,
            name = "Zgradbe in objekti", color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`Bruto investicije v osnovna sredstva`,
            name = "Bruto investicije v osnovna sredstva", color = I(umar_cols()[4]))

fig1 <- add_empty_lines(fig1, 9)

fig2 <-  data |>
  plot_ly(x = ~period,  width = 1000,
          height = 600) |>
  add_bars_q( x = data$period,  y = data$`Spremembe zalog`,
            name = "Spremembe zalog",
            color = I(umar_cols()[3]))

subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis("Medletna rast, v %"),
    yaxis2 = umar_yaxis("Prispevek k rasti BDP, v o.t."),
    xaxis = umar_xaxis("Q"),
    title = umar_subtitle(updated),
    annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period) + 100)

