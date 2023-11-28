####
# get data
df <- read.csv2(here::here("data/088.csv"), encoding = "UTF-8")
df <- df |> mutate(year_on_year = ifelse(year_on_year == "y", TRUE, year_on_year))
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
data <- prep_l$data_points[[1]] |>
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()

prep_l2 <- prep_multi_line(spl[[2]], con)
purrr::reduce(prep_l2$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data2

updated <- max(prep_l$updated, prep_l2$updated)

fig1 <- data |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Izvoz blaga *", color = I(umar_cols()[1])) |>
  layout(shapes = emph_line(100, data$period),
         yaxis = list( range = list(50, 180)))

fig1 <- add_empty_lines(fig1, 11)


fig2 <- plot_ly(data2, x = ~period, width = 1000,
                height = 600) |>
  add_lines_m(y = ~value.x,  name = "Izvozna naročila",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.y,  name = "Pričakovan izvoz ",  color = I(umar_cols()[3]))

subplot(fig1, fig2,  nrows = 2, shareX = TRUE) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (povprečje 2010)'),
              yaxis2 = umar_yaxis('Ravnote\u017dje, v o.t.'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l2$transf_txt),
              annotations = initials("MaHr")) |>
  my_panel_note('* Popravljeno za oceno oplemenitenja blaga v povezavi z menjavo farmacije s Švico.') |>
  rangeslider(as.Date("2012-01-01"), max(data$period))

#
# fig1 |>
#   add_lines(data = data2, y = ~`value.x`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
#             name = "Izvozna naročila(desna os)", color = I(umar_cols()[2])) |>
#   add_lines(data = data2, y = ~`value.y`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
#             name = "Pričakovan izvoz (desna os)", color = I(umar_cols()[3])) |>
#   umar_layout(slider_w, m,
#               yaxis = umar_yaxis("Indeks", fixedrange = FALSE,
#                                  range = list(-80, 180)),
#               yaxis2 = umar_yaxis("Ravnote\u017dje, v o.t.",
#                                   overlaying = "y",
#                                   side = "right",
#                                   fixedrange = FALSE,
#                                   range = list(-80, 180)),
#               annotations = list(x = 0 , y = 1, showarrow = F,
#                                  xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
#                                                                           prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR) \n",
#                                                                           "Posodobljeno:",prep_l2$updated,
#                                                                           prep_l$transf_txt, "(Vir: SURS)                                      "
#                                  ),
#                                  font = list(size = 12)),
#               xaxis = umar_xaxis("M"),
#
#               annotations = initials("MaHr"))|>
#   my_panel_note('* Popravljeno za oceno oplemenitenja blaga v povezavi z menjavo farmacije s Švico.') |>
#   rangeslider(as.Date("2013-01-01"), max(data$period) + 100)|>
#   config(modeBarButtonsToAdd = list(dl_button))
#
#
