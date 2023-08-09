# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
df <- df |> mutate(year_on_year = ifelse(year_on_year == "y", TRUE, year_on_year))
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[9]][1,], con)
prep_l2 <- prep_multi_line(spl[[9]][2,], con)


prep_l$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data

prep_l2$data_points[[1]] %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

updated <- max(prep_l$updated, prep_l2$updated)


fig1 <- data |>
  plot_ly(x = ~period, width = 1000, height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}%",
            name = "Delovno aktivno prebivalstvo v gradbeni\u0161tvu", color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m, annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                                                     prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)                                      \n",
                                                                     "Posodobljeno:",prep_l2$updated,
                                                                     prep_l2$transf_txt, "(Vir: SURS & prera\u010dun UMAR)               "),
                            font = list(size = 12)))

fig1 |>
  add_lines(data = data2, y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}", yaxis = "y2",
            name = "Pri\u010dakovano zaposlovanje v gradbeni\u0161tvu (desna os)", color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis("Medletna sprememba, v %", range = list(-25, 20), fixedrange = FALSE),
              yaxis2 = umar_yaxis("Ravnote\u017dje, v o.t.", range = list(-50, 40), fixedrange = FALSE,
                                  overlaying = "y",
                                  side = "right"),
              xaxis = umar_xaxis("Q"),
              title = umar_subtitle(updated),
              annotations = initials("JaKu")) |>
  rangeslider(as.Date("2013-01-01"), max(data$period) + 100)|>
  config(modeBarButtonsToAdd = list(dl_button))

