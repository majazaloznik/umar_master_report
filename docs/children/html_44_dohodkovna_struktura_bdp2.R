#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/030.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("BDP" = "value",
         "BDPraw" = "raw") |>
  left_join(data2, by = "period")  |>
  as_tibble() -> data

data |>
  mutate(bpp = lag(raw.x/BDPraw, 4)*value.x,
         szz = lag(raw.y/BDPraw, 4)*value.y,
         dpu = lag(raw.x.x/BDPraw, 4)*value.x.x,
         snp = lag(raw.y.y/BDPraw, 4)*value.y.y) -> data
write.csv2(data, "1.19.csv")
updated <- max(prep_l$updated, prep_l2$updated)


# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~`BDP`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", name = "BDP, medletna rast (v %)", color = I("black")) |>
  add_bars(y = ~`bpp`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Bruto poslovni prese\u017eek/raznovrstni dohodek",  color = I(umar_cols()[1])) |>
  add_bars(y = ~`szz`, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Sredstva za zaposlene",  color = I(umar_cols()[2])) |>
  add_bars(y = ~`dpu`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}",name = "Davki na proizvodnjo in uvoz",  color = I(umar_cols()[3])) |>
  add_bars(y = ~-`snp`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Subvencije na proizvodnjo",  color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  layout(barmode = "relative", font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="Prispevki k medletni rasti BDP, v o.t",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list(NULL, "M12"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated, "(Vir: SURS & prera\u010duni UMAR)"),
                      font = list(size = 12),
                      x = 0))|>
  config(modeBarButtonsToAdd = list(dl_button))
