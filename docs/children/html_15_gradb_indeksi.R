#### rast povpr plače
# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(stavbe = value.x,
         inzobj = value.y,
         spec = value.x.x,
         skupaj = value.y.y)  -> data
####
# prepare data
prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(stavbe = value.x,
         stan = value.y,
         nestan = value)  -> data2

updated <- max(prep_l$updated, prep_l2$updated)

fig1 <- data |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`stavbe`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradnja stavb", color = I(umar_cols()[1])) |>
  add_lines(y = ~`inzobj`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradnja in\u017eenirskih objektov", color = I(umar_cols()[2])) |>
  add_lines(y = ~`spec`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Specializirana gradbena dela", color = I(umar_cols()[4])) |>
  add_lines(y = ~`skupaj`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradbeni\u0161tvo - SKUPAJ", color = I(umar_cols()[5])) |>
  umar_layout(slider_w, m,
    shapes = emph_line(100, data$period))

for(i in 1:8) {
  fig1 <- fig1 |>
    add_lines(y = ~c,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")}
fig2 <- data2 |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`stavbe`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradnja stavb", color = I(umar_cols()[1])) |>
  add_lines(y = ~`stan`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Stanovanjske stavbe", color = I(umar_cols()[6])) |>
  add_lines(y = ~`nestan`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Nestanovanjske stavbe", color = I(umar_cols()[7])) |>
  umar_layout(slider_w, m,
              shapes = emph_line(100, data$period))


subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Indeks (2015)"),
              yaxis2 = umar_yaxis("Indeks (2015)"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("JaKu")) |>
  rangeslider(as.Date("2007-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))

