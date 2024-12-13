# get data
df <- read.csv2(here::here("data/094.csv"), encoding = "UTF-8")
df <- df |>  dplyr::arrange(chart_no)
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

updated <- max(prep_l$updated)


purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  dplyr::select(-period_id) |>
  as_tibble()  -> data

# manually interpolate data for june 2020 for potovalne agencije N79
# and recalculate the year on year change
data <-   data  |>
  mutate(
    raw.x.x = case_when(
      period == "2020-06-01" ~ (lag(raw.x.x, default = NA) + lead(raw.x.x, default = NA)) / 2,
      TRUE ~ raw.x.x)) |>
  mutate(value.x.x = raw.x.x/lag(raw.x.x, 12)*100 - 100)


plot_ly(data, x = ~period, width = 1000) |>
  add_lines_mp(y = ~value.x,  name = "Druge poslovne dejavnosti (N)",  color = I(umar_cols()[3]),
               fill = "tozeroy") |>
  add_lines_mp(y = ~value.y,  name = "Zaposlovalne storitve (N 78)",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~value.x.x,  name = "Potovalne agencije (N 79) *",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~value.y.y,  name = "Dejavnost oskrbe stavb (N 81)",  color = I(umar_cols()[4])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Medletna sprememba, v %'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("AnVi")) |>
  my_panel_note('* Za 2020M06 je vzeto povprečje 2020M05 in 2020M07 (SURS ni objavil podatka).') |>
  rangeslider(as.Date("2011-01-01"), max(data$period))
