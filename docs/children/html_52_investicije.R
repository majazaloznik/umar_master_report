#### Investicije
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
fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~bios,  hovertemplate="%{x|%Y} %{y:.2f}%", name = "Bruto investicije v osnovna sredstva", color = I(umar_cols()[1]))

for(i in 1:10) {
  fig1 <- fig1 |>
    add_lines(y = ~bios,  name = "\u200A",  color = I('rgba(0,0,0,0)'),
              hoverinfo = "none")}
fig2 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_bars(y = ~`sz`,  hovertemplate="%{x|%Y} %{y:.2f}%", name = "Sprememebe zalog",  color = I(umar_cols()[3]))

  subplot(fig1,  fig2, nrows = 2, shareX = TRUE) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+100) |>
  layout(showlegend = TRUE,
         legend = list(tracegroupgap = 150),
         font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="Medletna rast, v %",
                                   font = list(size =12))),
         yaxis2 = list(title = list(text="Prispevki k medletni rasti BDP, v o.t",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0),
         annotations = list(
           x = 0.95, y = 1.05, text = "NaTJ", showarrow = FALSE,
           xref='paper', yref='paper', xanchor='right', yanchor='top',
           font=list(size=10, color = umar_cols()[3])
         ))|>
  config(modeBarButtonsToAdd = list(dl_button))
