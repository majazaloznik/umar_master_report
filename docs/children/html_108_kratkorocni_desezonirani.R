# get data
df <- read.csv2(here::here("data/089.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

# rebase index to 2010 for all but export
prep_l$data_points <- purrr::map(prep_l$data_points, ~ dplyr::rename(., date = period))
prep_l$data_points <- purrr::map(prep_l$data_points, ~ dplyr::ungroup(.))
prep_l$data_points <- purrr::map(prep_l$data_points, ~ transform_index(., 2010)[[1]])

# add rolling average
prep_l$data_points <- purrr::map(prep_l$data_points, ~ transform_rolling(., periods = 3, align = "r"))
prep_l$data_points <- purrr::map(prep_l$data_points, ~ dplyr::rename(., period = date))

updated <- max(prep_l$updated)


purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data

plot_ly(data, x = ~period, width = 1000,
        height = 600) |>
  add_lines_m(y = ~value,  name = "Izvoz blaga *",  color = I(umar_cols()[1])) |>
  add_lines_m(y = ~value.x.x,  name = "Ind. proiz. predelovalnih dej.",  color = I(umar_cols()[2])) |>
  add_lines_m(y = ~value.y,  name = "Vred. opr. del v gradbeništvu",  color = I(umar_cols()[3])) |>
  add_lines_m(y = ~value.x,  name = "Prih. v trg. na drobno brez motornih goriv",  color = I(umar_cols()[4])) |>
  add_lines_m(y = ~value.y.y,  name = "Storitveni prihodek",  color = I(umar_cols()[5])) |>

  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Indeks (povprečje 2010)'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: 3-m drseča sredina"),
              annotations = initials("MoKo"),
              shapes = emph_line(100, data$period)) |>
  my_panel_note('* Popravljeno za oceno oplemenitenja blaga v povezavi z menjavo farmacije s Švico.') |>
  rangeslider(as.Date("2012-01-01"), max(data$period))
