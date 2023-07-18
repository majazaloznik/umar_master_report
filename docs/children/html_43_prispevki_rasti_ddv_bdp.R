#### Prispevki rasti dodane vrednosti posameznih dejavnosti k rasti BDP
# get data
df <- read.csv2(here::here("data/029.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)

purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename("predelovalne" = "value.x",
         "gradbenistvo" = "value.y",
         "javne" = "value")-> data2

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("Dodana vrednost" = "value") |>
  left_join(data2, by = "period")  |>
  as_tibble()-> data

prep_l3 <- prep_multi_line(spl[[3]], con)

purrr::reduce(prep_l3$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rowwise() |>
  transmute(period, trzne = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> data3


prep_l4 <- prep_multi_line(spl[[4]], con)

purrr::reduce(prep_l4$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rowwise() |>
  transmute(period, ostalo = sum(value.x, value.y, -value, na.rm = TRUE)) -> data4

data |>
  left_join(data3, by = "period") |>
  left_join(data4, by = "period") -> data


updated <- max(prep_l$updated, prep_l2$updated)


# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~`Dodana vrednost`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", name = "Dodana vredost (v %)", color = I("black")) |>
  add_bars(y = ~`predelovalne`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Predelovalne dejavnosti (C)",  color = I(umar_cols()[8])) |>
  add_bars(y = ~`gradbenistvo`, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Gradbeni\u0161tvo (F)",  color = I(umar_cols()[5])) |>
  add_bars(y = ~`javne`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}",name = "Javne storitve (O-Q)",  color = I(umar_cols()[6])) |>
  add_bars(y = ~`trzne`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Tr\u017ene storitve (G-N, R, S, T)",  color = I(umar_cols()[1])) |>
  add_bars(y = ~`ostalo`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Ostalo (A, B, D, E)",  color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  layout(barmode = "relative", font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="Prispevki k medletni rasti BDP, v o.t",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated, "(Vir: SURS & prera\u010duni UMAR)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         ))|>
  config(modeBarButtonsToAdd = list(dl_button))
