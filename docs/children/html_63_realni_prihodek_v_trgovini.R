# get data
df <- read.csv2(here::here("data/049.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
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

updated <- max(prep_l$updated,
               prep_l2$updated,
               prep_l3$updated,
               prep_l4$updated)


fig2 <- plot_ly(data2, x = ~period,  width = 1000,
                height = 800) |>
  add_lines_mp(y = ~value.x,  name = "Skupaj trgovina z motornimi vozili in njihovimi popravili",  color = I(umar_cols()[3]), fill = "tozeroy") |>
  add_lines_mp(y = ~value.y,   name = "Motorna vozila, motorna kolesa, rezervni deli, oprema",color = I(umar_cols()[5])) |>
  add_lines_mp(y = ~value,   name = "Vzdr\u017eevanje in popravila motornih vozil",color = I(umar_cols()[6])) |>
  umar_layout(slider_w, m,
              annotations = list(x = 0. , y = 1, text = "Trgovina z motornimi vozili", showarrow = F,
                            xref='paper', yref='paper'))


for(i in 1:9) {
  fig2 <- fig2 |>
    add_lines(y = ~value.x,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}

fig3 <- plot_ly(data3, x = ~period, width = 1000,
                height = 800) |>
  add_lines_mp(y = ~value.x,  name = "Skupaj trgovina na drobno",  color = I(umar_cols()[3]), fill = "tozeroy") |>
  add_lines_mp(y = ~value.y,   name = "Skupaj trgovina na drobno, brez motornih goriv",color = I(umar_cols()[8])) |>
  add_lines_mp(y = ~value.x.x,  name = "Trgovina z \u017eivili, pija\u010dami in toba\u010dnimi izdelki",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.y.y,   name = "Motorna goriva v specializiranih prodajalnah",color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value,  name = "Trgovina z ne\u017eivili, brez motornih goriv",  color = I(umar_cols()[7])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Trgovina na drobno", showarrow = F,
                            xref='paper', yref='paper'))
for(i in 1:8) {
  fig3 <- fig3 |>
    add_lines(y = ~value.x,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}


fig4 <- plot_ly(data4, x = ~period,  width = 1000,
                height = 800) |>
  add_lines_mp(y = ~value.x,  name = "Skupaj trgovina na drobno z ne\u017eivili",  color =I(umar_cols()[3]), fill = "tozeroy") |>
  add_lines_mp(y = ~value.y,   name = "Nespec. prodajalne, prete\u017eno z ne\u017eivili",color = I(umar_cols()[5])) |>
  add_lines_mp(y = ~value.x.x,  name = "Ra\u010dunalni\u061ke, telek. naprave, knjige, \u061portna oprema, igra\u010de ",  color = I(umar_cols()[6])) |>
  add_lines_mp(y = ~value.x.x.x,   name = "Gospodinjske naprave, avdio in video zapisi",color = I(umar_cols()[8])) |>
  add_lines_mp(y = ~value.y.y.y,  name = " Tekstil, obla\u010dila, obutev in usnjeni izdelki",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.x.x.x.x,  name = "Pohi\u061tvo, gradbeni material",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.y.y.y.y,   name = "Farmacevtski, medicinski, kozmeti\u010dni in toaletni\nizdelki",color = I(umar_cols()[4])) |>
  add_lines_mp(y = ~value,   name = "Trgovina na drobno po po\u0161ti ali po internetu",color = I(umar_cols()[3])) |>
  umar_layout(slider_w, m,annotations = list(x = 0. , y = 1, text = "Trgovina na drobno z ne\u017eivili", showarrow = F,
                            xref='paper', yref='paper'))


subplot(fig2, fig3, fig4,  nrows = 3, shareX = TRUE) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Medletna sprememba, v %"),
              yaxis2 = umar_yaxis("Medletna sprememba, v %"),
              yaxis3 = umar_yaxis("Medletna sprememba, v %"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo"))|>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))





