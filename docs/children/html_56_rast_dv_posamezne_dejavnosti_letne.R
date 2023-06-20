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

fig1 <- plot_ly(data, x = ~period, hovertemplate="%{x|%Y} %{y:.2f}", width = 1000,
                height = 1600) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '1') |>
  add_lines(y = ~b,   name = "Rudarstvo (B)",color = I(umar_cols()[1]),  legendgroup = '1') |>
  add_lines(y = ~c,  name = "Predelovalne dejavnosti (C)",  color = I(umar_cols()[2]),  legendgroup = '1') |>
  add_lines(y = ~d,  name = "Oskrba z elektriko, plinom in paro (D)",  color = I(umar_cols()[3]),  legendgroup = '1') |>
  add_lines(y = ~e,  name = "Oskrba z vodo (E)",  color = I(umar_cols()[4]),  legendgroup = '1') |>

  layout(annotations = list(x = 0. , y = 1, text = "Industrija", showarrow = F,
                            xref='paper', yref='paper'))


fig2 <- plot_ly(data, x = ~period, hovertemplate="%{x|%Y} %{y:.2f}", width = 1000,
                height = 1600) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '2') |>
  add_lines(y = ~g,   name = "Trgovina, vzdr\u017eevanje in popravila vozil (G)",color = I(umar_cols()[5]),  legendgroup = '2') |>
  add_lines(y = ~h,  name = "Promet in skladi\u0161\u010denje (H)",  color = I(umar_cols()[6]),  legendgroup = '2') |>
  add_lines(y = ~i,  name = "Gostinstvo (I)",  color = I(umar_cols()[7]),  legendgroup = '2') |>
  add_lines(y = ~j,  name = "IKT dejavnosti (J)",  color = I(umar_cols()[8]),  legendgroup = '2') |>
  add_lines(y = ~k,  name = "Finan\u010dne in zavarovalni\u0161ke dejavnosti (K)",  color = I(umar_cols()[1]),  legendgroup = '2') |>
  add_lines(y = ~l,  name = "Poslovanje z nepremi\u010dninami (L)",  color = I(umar_cols()[2]),  legendgroup = '2') |>
  add_lines(y = ~m,  name = "Strokovne, znanstvene in tehni\u010dne dejavnosti (M)",  color = I(umar_cols()[3]),  legendgroup = '2') |>
  add_lines(y = ~n,  name = "Druge raznovrstne poslovne dejavnosti (N)",  color = I(umar_cols()[4]),  legendgroup = '2') |>
  layout(annotations = list(x = 0. , y = 1, text = "Tr\u017ene storitve", showarrow = F,
                            xref='paper', yref='paper'))


fig3 <- plot_ly(data, x = ~period, hovertemplate="%{x|%Y} %{y:.2f}", width = 1000,
                height = 1600) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '3') |>
  add_lines(y = ~o,  name = "Dejavnosti uprave, obvezna socialna varnost (O)",color = I(umar_cols()[5]),  legendgroup = '3') |>
  add_lines(y = ~p, name = "Izobra\u017eevanje (P)",  color = I(umar_cols()[6]),  legendgroup = '3') |>
  add_lines(y = ~r,   name = "Zdravstvo in socialno varstvo (R)",color = I(umar_cols()[7]),  legendgroup = '3') |>
  layout(annotations = list(x = 0. , y = 1, text = "Javne storitve", showarrow = F,
                            xref='paper', yref='paper'))

fig4 <- plot_ly(data, x = ~period, hovertemplate="%{x|%Y} %{y:.2f}", width = 1000,
                height = 1600) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '4') |>
  add_lines(y = ~a,  name = "Kmetijstvo (A)",color = I(umar_cols()[8]),  legendgroup = '4') |>
  add_lines(y = ~f, name = "Gradbeni\u0161tvo (F)",  color = I(umar_cols()[1]),  legendgroup = '4') |>
  add_lines(y = ~u,   name = "Dejavnost eksteritorialnih organizacij in teles  (u)",color = I(umar_cols()[2]),  legendgroup = '3') |>
  layout(annotations = list(x = 0. , y = 1, text = "Ostalo", showarrow = F,
                            xref='paper', yref='paper'))

fig5 <- plot_ly(data, x = ~period, hovertemplate="%{x|%Y} %{y:.2f}", width = 1000,
                height = 1600) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '5') |>
  add_lines(y = ~r,  name = " Kulturne, razvedrilne in rekreacijske dejavnosti (R)",color = I(umar_cols()[2]),  legendgroup = '5') |>
  add_lines(y = ~s, name = "Druge dejavnosti (S)",  color = I(umar_cols()[3]),  legendgroup = '5') |>
  add_lines(y = ~t,   name = "Dejavnost gospodinjstev z zaposlenim osebjem (T)",color = I(umar_cols()[4]),  legendgroup = '5') |>
  layout(annotations = list(x = 0. , y = 1, text = "RST tr\u017ene storitve", showarrow = F,
                            xref='paper', yref='paper'))


subplot(fig1,  fig2, fig3, fig4, fig5,  nrows = 5, shareX = TRUE) |>
  layout(showlegend = TRUE,
         legend = list(tracegroupgap = 200),
         autosize = T, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12)), range = c(-40, 30)),
         yaxis2 = list(title = list(text="Medletna rast, v %",
                                    font = list(size =12)), range = c(-40, 30)),
         yaxis3 = list(title = list(text="Medletna rast, v %",
                                    font = list(size =12)), range = c(-40, 30)),
         yaxis4 = list(title = list(text="Medletna rast, v %",
                                    font = list(size =12)), range = c(-40, 30)),
         yaxis5 = list(title = list(text="Medletna rast, v %",
                                    font = list(size =12)), range = c(-40, 30)),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))
