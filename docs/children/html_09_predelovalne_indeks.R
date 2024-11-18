#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/010.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- prep_l$updated

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id, -raw.x, -raw.y) |>
  as_tibble() -> data
colnames(data)[2:3] <- prep_l$legend_labels


data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~`B+C+D Industrija`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "B+C+D Industrija", color = I(umar_cols()[1])) |>
  add_lines(y = ~`C PREDELOVALNE DEJAVNOSTI`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "C PREDELOVALNE DEJAVNOSTI", color = I(umar_cols()[2])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+10) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Medletna sprememba, v %"),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated),
              annotations = initials("MaHr"))|>
  config(modeBarButtonsToAdd = list(dl_button))
