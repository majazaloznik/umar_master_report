# get data
df <- read.csv2(here::here("data/027.csv"), encoding = "UTF-8")
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
prep_l4<- prep_multi_line(spl[[4]], con)
purrr::reduce(prep_l4$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data4

fig1 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                height = 1000) |>
  add_lines(y = ~value.x,  name = "Ni omejitev",  color = I(umar_cols()[1]),  legendgroup = '1') |>
  add_lines(y = ~value.y,   name = "Negotove gospodarske razmere",color = I(umar_cols()[2]), legendgroup = '1') |>
  add_lines(y = ~value.x.x,  name = "Nezadostno doma\u010de povpra\u0161evanje",  color = I(umar_cols()[3]), legendgroup = '1') |>
  add_lines(y = ~value.y.y, name = "Nezadostno tuje povpra\u0161evanje", color = I(umar_cols()[4]), legendgroup = '1') |>
  add_lines(y = ~value,  name = "Konkuren\u010den uvoz",  color = I(umar_cols()[5]), legendgroup = '1')

fig2 <- plot_ly(data2, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                height = 1000) |>
  add_lines(y = ~value.x,  name = "Pomanjkanje delavcev na splo\u0161no",  color = I(umar_cols()[6]), legendgroup = '2') |>
  add_lines(y = ~value.y,   name = "Pomanjkanje usposobljenih delavcev",color = I(umar_cols()[7]), legendgroup = '2')

fig3 <- plot_ly(data3, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                height = 1000) |>
  add_lines(y = ~value.x,  name = "Pomanjkanje surovin",  color = I(umar_cols()[8]), legendgroup = '3') |>
  add_lines(y = ~value.y,   name = "Pomanjkanje polizdelkov",color = I(umar_cols()[1]), legendgroup = '3') |>
  add_lines(y = ~value,  name = "Pomanjkanje ustrezne opreme",  color = I(umar_cols()[2]), legendgroup = '3')

fig4 <- plot_ly(data4, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
                height = 1000) |>
  add_lines(y = ~value.x,  name = "Neporavnane obveznosti iz poslovanja",  color = I(umar_cols()[3]), legendgroup = '4') |>
  add_lines(y = ~value.y,   name = "Finan\u010dni problemi",color = I(umar_cols()[4]), legendgroup = '4') |>
  add_lines(y = ~value.x.x,  name = "Nejasna gospodarska zakonodaja",  color = I(umar_cols()[5]), legendgroup = '4') |>
  add_lines(y = ~value.y.y,  name = "Drugo",  color = I(umar_cols()[6]), legendgroup = '4')




subplot(fig1, fig2, fig3, fig4,  nrows = 4, shareX = TRUE) |>
  layout(showlegend = TRUE,
         legend = list(tracegroupgap = 150),
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Dele\u017e podjetij, v %",
                                   font = list(size =12)), range = c(0,70)),
         yaxis2 = list(title = list(text="Dele\u017e podjetij, v %",
                                    font = list(size =12)), range = c(0,70)),
         yaxis3 = list(title = list(text="Dele\u017e podjetij, v %",
                                    font = list(size =12)), range = c(0,70)),
         yaxis4 = list(title = list(text="Dele\u017e podjetij, v %",
                                    font = list(size =12)), range = c(0,70)),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))





