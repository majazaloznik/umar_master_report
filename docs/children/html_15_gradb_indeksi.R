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
  layout(
    shapes = list(
      list(
        type = "line",
        x0 = min(data$period), x1 = max(data$period),
        y0 = 100, y1 = 100,
        line = list(color = umar_cols("emph"), width = 1)
      )
    ))


fig2 <- data2 |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`stavbe`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradnja stavb", color = I(umar_cols()[1])) |>
  add_lines(y = ~`stan`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Stanovanjske stavbe", color = I(umar_cols()[6])) |>
  add_lines(y = ~`nestan`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Nestanovanjske stavbe", color = I(umar_cols()[7])) |>
  layout(
         shapes = list(
           list(
             type = "line",
             x0 = min(data$period), x1 = max(data$period),
             y0 = 100, y1 = 100,
             line = list(color = umar_cols("emph"), width = 1)
           )
         ))


subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin =  m,
         font=list(family = "Myriad Pro"),
         yaxis = list(
           title = list(text="3-m drsea\u010da sredina (desna) indeksa (2015)",
                        font = list(size =12)),
           fixedrange = FALSE),
         yaxis2 = list(
           title = list(text="3-m drsea\u010da sredina (desna) indeksa (2015)",
                        font = list(size =12)), fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.1),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:",updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2007-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))

