#### rast povpr plače
# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[6]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(fiz_stan = value.x,
         fiz_nestan = value.y,
         prav_stan = value.x.x,
         prav_nesta = value.y.y) -> data


updated <- prep_l$updated

fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_bars(y = ~`fiz_stan`,  hovertemplate="%{x|%m-%Y} %{y:.0f}",
            name = "Fizi\u010dne osebe - stanovanjske stavbe", color = I(umar_cols()[1])) |>
  add_bars(y = ~`fiz_nestan`,  hovertemplate="%{x|%m-%Y} %{y:.0f}",
           name = "Fizi\u010dne osebe - nestanovanjske stavbe", color = I(umar_cols()[2])) |>
  add_bars(y = ~`prav_stan`,  hovertemplate="%{x|%m-%Y} %{y:.0f}",
           name = "Pravne osebe - stanovanjske stavbe", color = I(umar_cols()[4])) |>
  add_bars(y = ~`prav_nesta`,  hovertemplate="%{x|%m-%Y} %{y:.0f}",
           name = "Pravne osebe - nestanovanjske stavbe", color = I(umar_cols()[5]))

fig1 <- add_empty_lines(fig1, 8)

fig2 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`fiz_stan`,  hovertemplate="%{x|%m-%Y} %{y:.0f}",#showlegend = FALSE,
           name = "Fizi\u010dne osebe - stanovanjske stavbe", color = I(umar_cols()[1])) |>
  add_lines(y = ~`fiz_nestan`,  hovertemplate="%{x|%m-%Y} %{y:.0f}", #showlegend = FALSE,
           name = "Fizi\u010dne osebe - nestanovanjske stavbe", color = I(umar_cols()[2])) |>
  add_lines(y = ~`prav_stan`,  hovertemplate="%{x|%m-%Y} %{y:.0f}", #showlegend = FALSE,
           name = "Pravne osebe - stanovanjske stavbe", color = I(umar_cols()[4])) |>
  add_lines(y = ~`prav_nesta`,  hovertemplate="%{x|%m-%Y} %{y:.0f}", #showlegend = FALSE,
           name = "Pravne osebe - nestanovanjske stavbe", color = I(umar_cols()[5]))


subplot( fig1, fig2, nrows = 2, shareX = TRUE) |>
  rangeslider(as.Date("2015-01-01"), max(data$period)+10) |>
  umar_layout(slider_w, m,barmode = 'stack',
              yaxis = umar_yaxis("Povr\u0161ina, v m<sup>2</sup>"),
              yaxis2 = umar_yaxis("Povr\u0161ina, v m<sup>2</sup>"),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("JaKu"))|>
  config(modeBarButtonsToAdd = list(dl_button))
