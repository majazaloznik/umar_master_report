#### Trg dela
# get data
df <- read.csv2(here::here("data/012.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)

data <- prep_l$data_points[[1]] |>
  select(-period_id)

prep_l2$data_points[[1]] |>
  as_tibble() |>
  select(-period_id) |>
  mutate(value = value / 1000) -> data2
updated <- max(prep_l2$updated, prep_l$updated)

# prepare charts
fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_q(y = ~value,
            name = "Zaposlenost, desezonirana", color = I(umar_cols()[1]))

for(i in 1:11) {
  fig1 <- fig1 |>
    add_lines(y = mean(data$value, na.rm = TRUE),  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")}

fig2 <-  data2 |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines_m(y = ~value,name = "Brezposelnost", color = I(umar_cols()[2]))

subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
         yaxis = umar_yaxis('Zaposlenost, v 1000'),
         yaxis2 = umar_yaxis('\u0160tevilo registriranih brezposelnih, v 1000'),
         xaxis = umar_xaxis("M"),
         title = umar_subtitle(updated,  add = "ZRSZ"),
         annotations = initials("DeRo")) |>
  rangeslider(as.Date("2000-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))
