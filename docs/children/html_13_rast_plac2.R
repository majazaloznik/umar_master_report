#### rast povpr plače
# get data
df <- read.csv2(here::here("data/013.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
roll3 <- function(x) zoo::rollmean(x, k = 3,fill= NA,align = "c")
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(javni = value.x,
         zasebni = value.y,
         skupaj = value) |>
  dplyr::arrange(period) |>
  dplyr::mutate_if(is.numeric, function(x) x/dplyr::lag(x, n = 12)*100 - 100) |>
  dplyr::mutate_if(is.numeric, list(roll = roll3))-> data


updated <- prep_l$updated

fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_mp(y = ~`javni`,
            name = "Javni sektor ", color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~`javni_roll`,
            name = "Javni sektor (3-m drse\u010da sredina)", color = I(umar_cols()[1]))

for(i in 1:6) {
  fig1 <- fig1 |>
    add_lines(y = mean(data$javni, na.rm = TRUE),  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")
}

fig2 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_mp(y = ~`zasebni`,
            name = "Zasebni sektor", color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~`zasebni_roll`,
            name = "Zasebni sektor (3-m drse\u010da sredina)", color = I(umar_cols()[2]))

for(i in 1:6) {
  fig2 <- fig2 |>
    add_lines(y =mean(data$zasebni, na.rm = TRUE),  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")}


fig3 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines_mp(y = ~`skupaj`,
            name = "Skupaj ", color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~`skupaj_roll`,
            name = "Skupaj (3-m drse\u010da sredina)", color = I(umar_cols()[4]))

subplot(fig1,  fig2,  fig3, nrows = 3, shareX = TRUE) |>
  rangeslider(as.Date("2015-01-01"), max(data$period)+10) |>
  umar_layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = umar_yaxis("", range = c(-10, 20)),
         yaxis2 =umar_yaxis("Medletna rast, v %", range = c(-10, 20)),
         yaxis3 = umar_yaxis("", range = c(-10, 20)),
         xaxis = umar_xaxis("M"),
         title = umar_subtitle(),
         annotations = initials("DeRo")) |>
  config(modeBarButtonsToAdd = list(dl_button))
