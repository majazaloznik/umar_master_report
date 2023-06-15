#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/038.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rowwise() |>
  mutate("sz" = coalesce(value.x, 0)+
           coalesce(value.y, 0), .keep = "unused") -> data
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("bios" = "value") |>
  left_join(data, by = "period")  -> data

updated <- max(prep_l$updated, prep_l2$updated)


# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~bios,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", name = "Bruto investicije v osnovna sredstva (v %)", color = I(umar_cols()[1])) |>
  add_bars(y = ~`sz`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}", name = "Sprememebe zalog",  color = I(umar_cols()[3])) |>
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
         title = list(text = paste("Posodobljeno:", updated, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0))|>
  config(modeBarButtonsToAdd = list(dl_button))
