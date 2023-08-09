#### rast povpr plače
# get data
df <- read.csv2(here::here("data/013.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)

# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  rename(javni = value.x,
         zasebni = value.y,
         skupaj = value) |>
  dplyr::arrange(period) |>
  dplyr::mutate_if(is.numeric, function(x) x/dplyr::lag(x, n = 12)*100 - 100) |>
  dplyr::mutate_if(is.numeric,
                   function(x) zoo::rollmean(x, k = 3,fill= NA,align = "c"))-> data


updated <- prep_l$updated

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_mp(y = ~`javni`,
            name = "Javni sektor", color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~`zasebni`,
            name = "Zasebni sektor", color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~`skupaj`,
            name = "Skupaj", color = I(umar_cols()[4])) |>
  rangeslider(as.Date("2015-01-01"), max(data$period)+10) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis("3-m drsea\u010da sredina medletne sprememba, v %"),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle(updated),
    annotations = initials("DeRo"))|>
  config(modeBarButtonsToAdd = list(dl_button))


