#### Trg dela
# get data
df <- read.csv2(here::here("data/012.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)

prep_l2$data_points[[1]] |>
  as_tibble() |>
  select(-period_id) |>
  mutate(value = value / 1000) |>
  # mutate(quarter = lubridate::quarter(period), year = lubridate::year(period)) |>
  # group_by(year, quarter) |>
  # summarise(value = mean(value)) |>
  # ungroup() |>
  # mutate(period = as.Date(paste0(year, "-", (quarter*3-2), "-01"),  format = "%Y-%m-%d")) |>
  # select(-year, -quarter) |>
  rename("Brezposelnost" = value) -> data2


data <- prep_l$data_points[[1]] |>
  select(-period_id) |>
  rename("Zaposlenost" = value)

updated <- max(prep_l2$updated, prep_l$updated)

fig1 <- data |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`Zaposlenost`,  hovertemplate="%{x|Q%q-%Y} %{y:.}",
            name = "Zaposlenost, desezonirana", color = I(umar_cols()[1])) |>
  layout(font=list(family = "Myriad Pro"))

for(i in 1:11) {
  fig1 <- fig1 |>
    add_lines(y = ~`Zaposlenost`,  name = "\u200A",  color = I('rgba(0,0,0,0)'))
}

fig2 <-  data2 |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`Brezposelnost`,  hovertemplate="%{x|%m-%Y} %{y:.}",
            name = "Brezposelnost", color = I(umar_cols()[2])) |>
  layout(font=list(family = "Myriad Pro"))

subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin =  m,
         font=list(family = "Myriad Pro"),
         yaxis = list(fixedrange = FALSE,
                      title = list(text = 'Zaposlenost, v 1000', font = list(size =12))),
         yaxis2 = list(fixedrange = FALSE,
                       title =list(text =  '\u0160tevilo registriranih brezposelnih, v 1000',
                                   font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.1),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:",updated,
                                   prep_l$transf_txt, "(Vir: SURS & ZRSZ)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2000-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))

