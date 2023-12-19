# get data
df <- read.csv2(here::here("data/042.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
prep_l3 <- prep_multi_line(spl[[3]], con)
prep_l4 <- prep_multi_line(spl[[4]], con)
prep_l5 <- prep_multi_line(spl[[5]], con)
prep_l6 <- prep_multi_line(spl[[6]], con)

updated <- max(prep_l$updated, prep_l2$updated, prep_l3$updated,prep_l4$updated,prep_l5$updated,prep_l6$updated )


prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename( dv = value) |>
  as_tibble() -> data1

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2
purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data3
purrr::reduce(prep_l4$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data4
purrr::reduce(prep_l5$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data5
purrr::reduce(prep_l6$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data6

data1 |>
  left_join(data2) |>
  rename(b = value.x,
         c = value.y,
         d = value.x.x,
         e = value.y.y) |>
  left_join(data3) |>
  rename(g = value.x,
         h = value.y,
         i = value.x.x,
         j = value.y.y,
         k = value.x.x.x,
         l = value.y.y.y,
         m = value.x.x.x.x,
         n = value.y.y.y.y) |>
  left_join(data4) |>
  rename(o = value.x,
         p = value.y,
         q = value) |>
  left_join(data5) |>
  rename(a = value.x,
         f = value.y,
         u = value) |>
  left_join(data6) |>
  rename(r = value.x,
         s = value.y,
         t = value) -> data

fig1 <- plot_ly(data, x = ~period, width = 1000,
                height = 1400) |>
  add_lines_qp(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_qp(y = ~b,   name = "Rudarstvo (B)",color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~c,  name = "Predelovalne dejavnosti (C)",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~d,  name = "Oskrba z elektriko, plinom in paro (D)",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~e,  name = "Oskrba z vodo (E)",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Industrija", showarrow = F,
                                 xref='paper', yref='paper'))

fig1 <- add_empty_lines(fig1, 8)

fig2 <- plot_ly(data, x = ~period,  width = 1000,
                height = 1400) |>
  add_lines_qp(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_qp(y = ~g,   name = "Trgovina, vzdr\u017eevanje in popravila vozil (G)",color = I(umar_cols()[5])) |>
  add_lines_qp(y = ~h,  name = "Promet in skladi\u0161\u010denje (H)",  color = I(umar_cols()[6])) |>
  add_lines_qp(y = ~i,  name = "Gostinstvo (I)",  color = I(umar_cols()[7])) |>
  add_lines_qp(y = ~j,  name = "IKT dejavnosti (J)",  color = I(umar_cols()[8])) |>
  add_lines_qp(y = ~k,  name = "Finan\u010dne in zavarovalni\u0161ke dejavnosti (K)",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~l,  name = "Poslovanje z nepremi\u010dninami (L)",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~m,  name = "Strokovne, znanstvene in tehni\u010dne dejavnosti (M)",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~n,  name = "Druge raznovrstne poslovne dejavnosti (N)",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Tr\u017ene storitve", showarrow = F,
                                 xref='paper', yref='paper'))

fig2 <- add_empty_lines(fig2, 5)

fig3 <- plot_ly(data, x = ~period,  width = 1000,
                height = 1400) |>
  add_lines_ap(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_ap(y = ~o,  name = "Dejavnosti uprave, obvezna socialna varnost (O)",color = I(umar_cols()[5])) |>
  add_lines_ap(y = ~p, name = "Izobra\u017eevanje (P)",  color = I(umar_cols()[6])) |>
  add_lines_ap(y = ~q,   name = "Zdravstvo in socialno varstvo (Q)",color = I(umar_cols()[7])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Javne storitve", showarrow = F,
                                 xref='paper', yref='paper'))
fig3 <- add_empty_lines(fig3, 9)

fig4 <- plot_ly(data, x = ~period, width = 1000,
                height = 1400) |>
  add_lines_ap(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_ap(y = ~a,  name = "Kmetijstvo (A)",color = I(umar_cols()[8])) |>
  add_lines_ap(y = ~f, name = "Gradbeni\u0161tvo (F)",  color = I(umar_cols()[1])) |>
  add_lines_ap(y = ~u,   name = "Dejavnost eksteritorialnih organizacij in teles  (u)",color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Ostalo", showarrow = F,
                                 xref='paper', yref='paper'))
fig4 <- add_empty_lines(fig4, 10)

fig5 <- plot_ly(data, x = ~period, width = 1000,
                height = 1400) |>
  add_lines_ap(y = ~dv,  name = "Dodana vrednost",  color = I("black") ) |>
  add_lines_ap(y = ~r,  name = " Kulturne, razvedrilne in rekreacijske dejavnosti (R)",color = I(umar_cols()[2])) |>
  add_lines_ap(y = ~s, name = "Druge dejavnosti (S)",  color = I(umar_cols()[3])) |>
  add_lines_ap(y = ~t,   name = "Dejavnost gospodinjstev z zaposlenim osebjem (T)",color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "RST tr\u017ene storitve", showarrow = F,
                                 xref='paper', yref='paper'))


subplot(fig1,  fig2, fig3, fig4, fig5,  nrows = 5, shareX = TRUE) |>
  umar_layout(slider_w, m,
              barmode = "relative",
              yaxis = umar_yaxis("Medletna rast, v %",  range = c(-40, 30)),
              yaxis2 = umar_yaxis("Medletna rast, v %",  range = c(-40, 30)),
              yaxis3 = umar_yaxis("Medletna rast, v %",  range = c(-40, 30)),
              yaxis4 = umar_yaxis("Medletna rast, v %",  range = c(-40, 30)),
              yaxis5 = umar_yaxis("Medletna rast, v %",  range = c(-40, 30)),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle(updated),
              annotations = initials("NaTJ")) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))

