# get data
df <- read.csv2(here::here("data/052.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2
prep_l3<- prep_multi_line(spl[[3]], con)
purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data3

updated <- max(prep_l$updated,
               prep_l2$updated,
               prep_l3$updated)



fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 800) |>
  add_lines(y = ~value.x,  name = "Skupaj",  color = I(umar_cols()[3]), fill = "tozeroy",
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Nove stanovanjske nepremi\u010dnine ",color = I(umar_cols()[1]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value,  name = "Rabljene stanovanjske nepremi\u010dnine",  color = I(umar_cols()[2]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  my_panel_subtitle("\u0160tevilo prodaj stanovanjskih nepremi\u010dnin")


fig1 <- add_empty_lines(fig1, 10)

fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 800) |>
  add_lines(y = ~value.x,  name = "Skupaj",  color = I(umar_cols()[3]), fill = "tozeroy",
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Nova stanovanja",color = I(umar_cols()[4]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value,   name = "Nove dru\u017einske hi\u0161e",color = I(umar_cols()[5]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  my_panel_subtitle("Nove stanovanjske nepremi\u010dnine")

fig2 <- add_empty_lines(fig2, 8)

fig3 <- plot_ly(data3, x = ~period, width = 1000,
                height = 800) |>
  add_lines(y = ~value.x,  name = "Skupaj",  color = I(umar_cols()[3]), fill = "tozeroy",
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value.y,   name = "Rabljena stanovanja",color = I(umar_cols()[6]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  add_lines(y = ~value,  name = "Rabljene dru\u017einske hi\u0161e",  color = I(umar_cols()[7]),
            hovertemplate="%{x|Q%q-%Y} %{y:.2f}%") |>
  my_panel_subtitle("Rabljene stanovanjske nepremi\u010dnine")


subplot(fig1, fig2, fig3,   nrows = 3, shareX = TRUE) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Medletna sprememba, v %"),
              yaxis2 = umar_yaxis("Medletna sprememba, v %"),
              yaxis3 = umar_yaxis("Medletna sprememba, v %"),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated, prep_l$transf_txt, "UMAR"),
              annotations = initials("MoKo"))  |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))





