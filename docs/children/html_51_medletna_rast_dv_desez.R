# get data
df <- read.csv2(here::here("data/037.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
prep_l3 <- prep_multi_line(spl[[3]], con)
prep_l4 <- prep_multi_line(spl[[4]], con)
updated <- max(prep_l$updated,
               prep_l2$updated,
               prep_l3$updated,
               prep_l4$updated)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2
purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data3
prep_l4$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename( dv = value) |>
  as_tibble() -> data4

data4 |>
  left_join(data) |>
  rename(bcde = value.x,
         c = value.y) |>
  left_join(data2) |>
  rename(ghi = value.x,
         J = value.y,
         K = value.x.x,
         l = value.y.y,
         mn = value) |>
  left_join(data3) |>
  rename(a = value.x,
         f = value.y,
         opq = value.x.x,
         rst = value.y.y) -> data

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 800) |>
  add_lines_qp(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_qp(y = ~bcde,   name = "Industrija (BCDE)",color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~c,  name = "Predelovalne dejavnosti (C)",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Industrija", showarrow = F,
                                 xref='paper', yref='paper'))

fig1 <- add_empty_lines(fig1, 9)

fig2 <- plot_ly(data, x = ~period,  width = 1000,
                height = 800) |>
  add_lines_qp(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_qp(y = ~ghi,   name = "Trgovina, promet, gostinstvo (GHI)",color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~J,  name = "IKT dejavnosti (J)",  color = I(umar_cols()[4])) |>
  add_lines_qp(y = ~K,  name = "Finan\u010dne in zavaroval. Dej. (K)",  color = I(umar_cols()[5])) |>
  add_lines_qp(y = ~l,  name = "Poslovanje z neprem. (L)",  color = I(umar_cols()[6])) |>
  add_lines_qp(y = ~mn,  name = "MN dejavnosti",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~mn,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
               hoverinfo = "none") |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Tr\u017ene storitve", showarrow = F,
                                 xref='paper', yref='paper'))

fig2 <- add_empty_lines(fig2, 5)

fig3 <- plot_ly(data, x = ~period,  width = 1000,
                height = 800) |>
  add_lines_qp(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_qp(y = ~a,  name = "Kmetijstvo (A)",color = I(umar_cols()[8])) |>
  add_lines_qp(y = ~f, name = "Gradbeni\u0161tvo (F)",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~opq,   name = "Javne storitve (OPQ)",color = I(umar_cols()[7])) |>
  add_lines_qp(y = ~rst,  name = "Druge storitve (RST)",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~rst,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
               hoverinfo = "none") |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Ostalo", showarrow = F,
                                 xref='paper', yref='paper'))

subplot(fig1,  fig2, fig3,  nrows = 3, shareX = TRUE) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("\u010cetrtletna rast, v %", range = c(-30, 35)),
              yaxis2 = umar_yaxis("\u010cetrtletna rast, v %", range = c(-30, 35)),
              yaxis3 = umar_yaxis("\u010cetrtletna rast, v %", range = c(-30, 35)),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))
