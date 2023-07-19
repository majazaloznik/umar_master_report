#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/001.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  mutate("Spremembe zalog in v.p" = coalesce(value.y.y, 0)+
                                             coalesce(value.x.x.x.x, 0), .keep = "unused") -> data
colnames(data)[2:7] <- c("Državna potrošnja", "Zasebna potrošnja", "Bruto investicije v o.s.",
                         "Izvoz proizvodov in storitev", "Uvoz proizvodov in storitev", "Neto izvoz")
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("BDP, realna rast (v %)" = "value") |>
  left_join(data, by = "period") |>
  mutate("Uvoz proizvodov in storitev" = -`Uvoz proizvodov in storitev`) -> data

updated <- max(prep_l$updated, prep_l2$updated)


# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_qp(y = ~`BDP, realna rast (v %)`, name = "BDP, realna rast (v %)", color = I("black")) |>
  add_bars_q(y = ~`Uvoz proizvodov in storitev`,  name = "Uvoz proizvodov in storitev",  color = I(umar_cols()[3])) |>
  add_bars_q(y = ~`Izvoz proizvodov in storitev`,  name = "Izvoz proizvodov in storitev",  color = I(umar_cols()[2])) |>
  add_bars_q(y = ~`Bruto investicije v o.s.`,  name = "Bruto investicije v o.s.",  color = I(umar_cols()[5])) |>
  add_bars_q(y = ~`Spremembe zalog in v.p`,name = "Spremembe zalog in v.p",  color = I(umar_cols()[6])) |>
  add_bars_q(y = ~`Državna potrošnja`,   name = "Dr\u017eavna potro\u0161nja",  color = I(umar_cols()[1])) |>
  add_bars_q(y = ~`Zasebna potrošnja`,   name = "Potro\u0161nja gospodinjstev",  color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  umar_layout(barmode = "relative",
         yaxis = list(title = list(text="Prispevki k medletni rasti BDP, v o.t",
                                   font = list(size =12)),
                      fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated, "(Vir: SURS & prera\u010dun UMAR)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         ))|>
  config(modeBarButtonsToAdd = list(dl_button))
