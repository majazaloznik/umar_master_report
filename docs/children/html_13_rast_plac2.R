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
  add_lines(y = ~`javni`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Javni sektor - originalni", color = I(umar_cols()[3])) |>
  add_lines(y = ~`javni_roll`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Javni sektor  zglajeni", color = I(umar_cols()[1]))

fig2 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`zasebni`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Zasebni sektor - originalni", color = I(umar_cols()[3])) |>
  add_lines(y = ~`zasebni_roll`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Zasebni sektor  zglajeni", color = I(umar_cols()[2]))
fig3 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`skupaj`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Skupaj - originalni", color = I(umar_cols()[3])) |>
  add_lines(y = ~`skupaj_roll`,  hovertemplate="%{x|%m-%Y} %{y:.2f}%",
            name = "Skupaj  zglajeni", color = I(umar_cols()[4]))

subplot(fig1,  fig2,  fig3, nrows = 3, shareX = TRUE) |>
  rangeslider(as.Date("2015-01-01"), max(data$period)+10) |>
  layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis2 = list(title = list(text="3-m ds medletne spremembe, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.07),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0))
