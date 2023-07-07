# get data
df <- read.csv2(here::here("data/018.csv"), encoding = "UTF-8")
df$group <- interaction(df$chart_no, df$sub_chart, drop = TRUE)
spl <- split(df, df$group)
# prepare data

prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[11]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2


fig1 <- plot_ly(data, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000,
                height = 600) |>
  add_lines(y = ~value.x,  name = "Kazalnik gospodarske klime",  color = I(umar_cols()[1]) ) |>
  add_lines(y = ~value.y,   name = "Zaupanje v predelovalnih dejavnostih",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Zaupanje v trgovini na drobno",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y, name = "Zaupanje potro\u0161nikov", color = I(umar_cols()[4])) |>
  add_lines(y = ~value.x.x.x,  name = "Zaupanje v storitvenih dejavnostih",  color = I(umar_cols()[5])) |>
  add_lines(y = ~value.y.y.y, name = "Zaupanje v gradbeni\u0161tvu",  color = I(umar_cols()[6])) |>
  layout(annotations = list(x = 0. , y = 1, text = "Poslovne tendence in mnenje potro\u0161nikov", showarrow = F,
                            xref='paper', yref='paper'))

for(i in 1:7) {
  fig1 <- fig1 |>
    add_lines(y = ~value.x,  name = "\u200A",  color = I('rgba(0,0,0,0)'))
}
fig2 <- plot_ly(data2, x = ~period, hovertemplate="%{x|%b-%Y} %{y:.2f}", width = 1000,
                height = 600) |>
  add_lines(y = ~value.x,  name = "Kazalnik gospodarske klime",  color = I(umar_cols()[1])) |>
  add_lines(y = ~value.y,   name = "Zaupanje v predelovalnih dejavnostih",color = I(umar_cols()[2])) |>
  add_lines(y = ~value.x.x,  name = "Zaupanje v trgovini na drobno",  color = I(umar_cols()[3])) |>
  add_lines(y = ~value.y.y, name = "Zaupanje potro\u0161nikov", color = I(umar_cols()[4])) |>
  add_lines(y = ~value.x.x.x,  name = "Zaupanje v storitvenih dejavnostih",  color = I(umar_cols()[5])) |>
  add_lines(y = ~value.y.y.y, name = "Zaupanje v gradbeni\u0161tvu",  color = I(umar_cols()[6])) |>
  layout(annotations = list(x = 0. , y = 1, text = "Poslovne tendence in mnenje potro\u0161nikov - trimese\u010dne drse\u010de sredine (desne)", showarrow = F,
                            xref='paper', yref='paper'))



subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin = m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = list(text="Ravnote\u017eje, v o.t.",
                                   font = list(size =12)),
                      range = c(-60, 50), fixedrange = FALSE),
         yaxis2 = list(title = list(text="Ravnote\u017eje, v o.t.",
                                    font = list(size =12)),
                       range = c(-60, 50), fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.05),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010duni UMAR)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period))|>
  config(modeBarButtonsToAdd = list(dl_button))
