# get data
df <- read.csv2(here::here("data/028.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

fig1 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000, height=600) |>
  add_lines(y = ~value.x,  name = "Nominalni BDP",  color = I(umar_cols()[1])) |>
  add_lines(y = ~value.y,   name = "Bruto poslovni prese\u017eek/raznovrstni dohodek",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Sredstva za zaposlene",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y, name = "Davki na proizvodnjo in uvoz", color = I(umar_cols()[4]))

for(i in 1:9) {
  fig1 <- fig1 |>
    add_lines(y = ~`value.x`,  name = "\u200A",  color = I('rgba(0,0,0,0)'))
}

fig2 <- plot_ly(data, x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000, height=600) |>
  add_lines(y = ~value.x,  name = "Nominalni BDP",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value,  name = "Subvencije na proizvodnjo",  color = I(umar_cols()[5]))

  subplot(fig1,  fig2, nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         yaxis2 = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l2$updated,
                                   prep_l2$transf_txt, "(Vir: SURS & prera\u010duni UMAR)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))


