#### rast povpr plače
# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
df <- df |> mutate(year_on_year = ifelse(year_on_year == "y", TRUE, year_on_year))
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[3]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(stavbe = value.x,
         inzobj = value.y,
         spec = value.x.x,
         skupaj = value.y.y)  -> data


updated <- prep_l$updated

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
            name = "Gradbeni\u0161tvo - SKUPAJ", color = I(umar_cols()[5]))
for(i in 1:8) {
  fig1 <- fig1 |>
    add_lines(y = ~skupaj,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")}
# prepare data
prep_l <- prep_multi_line(spl[[4]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(stavbe = value.x,
         stan = value.y,
         nestan = value)  -> data


updated <- prep_l$updated

fig2 <- data |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`stavbe`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradnja stavb", color = I(umar_cols()[1])) |>
  add_lines(y = ~`stan`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Stanovanjske stavbe", color = I(umar_cols()[6])) |>
  add_lines(y = ~`nestan`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Nestanovanjske stavbe", color = I(umar_cols()[7]))


subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(showlegend = TRUE,
         autosize = F, margin =  m,
         font=list(family = "Myriad Pro"),
         yaxis = list(
           title = list(text="Medletna sprememba, v %",
                        font = list(size =12)),
           fixedrange = FALSE),
         yaxis2 = list(
           title = list(text="Medletna sprememba, v %",
                        font = list(size =12)), fixedrange = FALSE),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%b %Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:",updated,
                                   prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "JaKu", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         )) |>
  rangeslider(as.Date("2007-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))
