#### Investicijska dejavnost - option 2
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

fig1 <- data |>
  plot_ly(x = ~period, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", width = 1000,
          height = 600) |>
  add_lines(y = ~`Oprema in stroji`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "Oprema in stroji", color = I(umar_cols()[1])) |>
  add_lines(y = ~`Zgradbe in objekti`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "Zgradbe in objekti", color = I(umar_cols()[2])) |>
  layout(font=list(family = "Myriad Pro"))

fig2 <-  data |>
  plot_ly(x = ~period,hovertemplate="%{x|Q%q-%Y} %{y:.2f}", width = 1000,
          height = 600) |>
  add_bars( x = data$period,  y = data$`Spremembe zalog (desna os)`,
            name = "Spremembe zalog (desna os)",
            color = I(umar_cols()[3])) |>
  layout(font=list(family = "Myriad Pro"))

subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin =  m,
         font=list(family = "Myriad Pro"),
         yaxis = list(title = 'Medletna sprememba, v %', font = list(size =12)),
         yaxis2 = list(title = 'Prispevek h rasti BDP, v o.t.', font = list(size =12)),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.1),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y"))),
         title = list(text = paste("Posodobljeno:", prep_l$updated,
                                   prep_l$transf_txt, "(Vir: SURS)"),
                      font = list(size = 12),
                      x = 0)) |>
  rangeslider(as.Date("2018-01-01"), max(data$period) + 100)

