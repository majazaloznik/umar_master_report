#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/032.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  mutate(spzvp = coalesce(value.x.x.x, 0)+
           coalesce(value.y.y.y, 0),
         zsp = coalesce(value.x, 0)+
           coalesce(value.y, 0)  , .keep = "unused") -> data
colnames(data)[2:5] <- c("kpd", "bios", "ups", "ips")
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) |>
  rename("BDP" = "value") |>
  left_join(data, by = "period") |>  mutate(ups = -ups) |>
  as_tibble()-> data

updated <- max(prep_l$updated, prep_l2$updated)

# plot
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_ap(y = ~BDP, name = "BDP, realna rast (v %)", color = I("black")) |>
  add_bars_a(y = ~ups, name = "Uvoz proizvodov in storitev",  color = I(umar_cols()[3])) |>
  add_bars_a(y = ~ips, name = "Izvoz proizvodov in storitev",  color = I(umar_cols()[2])) |>
  add_bars_a(y = ~bios,  name = "Bruto investicije v o.s.",  color = I(umar_cols()[5])) |>
  add_bars_a(y = ~spzvp, name = "Spremembe zalog in v.p",  color = I(umar_cols()[6])) |>
  add_bars_a(y = ~kpd, name = "Dr\u017eavna potro\u0161nja",  color = I(umar_cols()[1])) |>
  add_bars_a(y = ~zsp, name = "Zasebna potro\u0161nja",  color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  umar_layout(slider_w, m,
              barmode = "relative",
              yaxis = umar_yaxis("Prispevki k medletni rasti BDP, v o.t"),
              xaxis = umar_xaxis("A"),
              title = umar_subtitle(updated, "UMAR"),
              annotations = initials("NaTJ"))
