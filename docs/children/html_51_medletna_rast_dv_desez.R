# get data
df <- read.csv2(here::here("data/037.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
prep_l3 <- prep_multi_line(spl[[3]], con)
prep_l4 <- prep_multi_line(spl[[4]], con)

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

fig1 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000,
                height = 1000) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '1') |>
  add_lines(y = ~bcde,   name = "Industrija (BCDE)",color = I(umar_cols()[1]),  legendgroup = '1') |>
  add_lines(y = ~c,  name = "Predelovalne dejavnosti (C)",  color = I(umar_cols()[2]),  legendgroup = '1') |>
  layout(annotations = list(x = 0. , y = 1, text = "Industrija", showarrow = F,
                            xref='paper', yref='paper'))


fig2 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000,
                height = 1000) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '2') |>
  add_lines(y = ~ghi,   name = "Trgovina, promet, gostinstvo (GHI)",color = I(umar_cols()[3]),  legendgroup = '2') |>
  add_lines(y = ~J,  name = "IKT dejavnosti (J)",  color = I(umar_cols()[4]),  legendgroup = '2') |>
  add_lines(y = ~K,  name = "Finan\u010dne in zavaroval. Dej. (K)",  color = I(umar_cols()[5]),  legendgroup = '2') |>
  add_lines(y = ~l,  name = "Poslovanje z neprem. (L)",  color = I(umar_cols()[6]),  legendgroup = '2') |>
  add_lines(y = ~mn,  name = "MN dejavnosti",  color = I(umar_cols()[2]),  legendgroup = '2') |>
  layout(annotations = list(x = 0. , y = 1, text = "Tr\u017ene storitve", showarrow = F,
                            xref='paper', yref='paper'))


fig3 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000,
                height = 1000) |>
  add_lines(y = ~dv,  name = "Dodana vrednost",  color = I("black") ,  legendgroup = '3') |>
  add_lines(y = ~a,  name = "Kmetijstvo (A)",color = I(umar_cols()[8]),  legendgroup = '3') |>
  add_lines(y = ~f, name = "Gradbeni\u0161tvo (F)",  color = I(umar_cols()[1]),  legendgroup = '3') |>
  add_lines(y = ~opq,   name = "Javne storitve (OPQ)",color = I(umar_cols()[7]),  legendgroup = '3') |>
  add_lines(y = ~rst,  name = "Druge storitve (RST)",  color = I(umar_cols()[3]),  legendgroup = '3') |>
  layout(annotations = list(x = 0. , y = 1, text = "Ostalo", showarrow = F,
                            xref='paper', yref='paper'))




subplot(fig1,  fig2, fig3,  nrows = 3, shareX = TRUE) |>
  layout(showlegend = TRUE,
         legend = list(tracegroupgap = 200),
         autosize = T, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="\u010cetrtletna sprememba, v %",
                                   font = list(size =12)), range = c(-30, 35)),
         yaxis2 = list(title = list(text="\u010cetrtletna sprememba, v %",
                                    font = list(size =12)), range = c(-30, 35)),
         yaxis3 = list(title = list(text="\u010cetrtletna sprememba, v %",
                                    font = list(size =12)), range = c(-30, 35)),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))
