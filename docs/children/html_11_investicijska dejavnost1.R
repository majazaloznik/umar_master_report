#### Investicijska dejavnost - option 1
# get data
df <- read.csv2(here::here("data/008.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l2 <- prep_multi_line(spl[[2]], con)


purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() -> data
colnames(data)[2:3] <- c("Oprema in stroji", "Zgradbe in objekti")

data <- data |> left_join(prep_l2$data_points[[1]]) |>
  select(-period_id) |>
  rename("Spremembe zalog (desna os)" = value)

updated <- prep_l$updated

fig <- data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~`Oprema in stroji`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "Oprema in stroji", color = I(umar_cols()[1])) |>
  add_lines(y = ~`Zgradbe in objekti`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "Zgradbe in objekti", color = I(umar_cols()[2])) |>
  layout(yaxis = list(range = c(-40, 40)))

ay <- list(
  overlaying = "y",
  side = "right",
  title = list(text="Prispevek h rasti BDP, v o.t.",
               font = list(size =12)),
  range = c(-4, 4))

fig <- fig %>% add_bars(data, x = data$period,  y = data$`Spremembe zalog (desna os)`,
                         name = "Spremembe zalog (desna os)", yaxis = "y2", opacity = 0.7,
                        color = I(umar_cols()[3]), hovertemplate="%{x|Q%q-%Y} %{y:.2f}")

fig <- fig %>% layout(
  title = "Double Y Axis Example", yaxis2 = ay,
  xaxis = list(title="xaxis title "),
  yaxis = list(title="Medletne spremembe, v %")
) |>
  layout(font=list(family = "Myriad Pro"),
         autosize = F, margin = m,
         yaxis = list(title = list(text="Medletna sprememba, v %",
                                   font = list(size =12))),
         xaxis = list(title = "",
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "%m.%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period)+100)|>
  config(modeBarButtonsToAdd = list(dl_button))
fig
