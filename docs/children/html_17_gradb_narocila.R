####
# get data
df <- read.csv2(here::here("data/016.csv"), encoding = "UTF-8")
df <- df |> mutate(year_on_year = ifelse(year_on_year == "y", TRUE, year_on_year))
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[5]][1,], con)
data <- prep_l$data_points[[1]] |>
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()

prep_l2 <- prep_multi_line(spl[[5]][2,], con)
data2 <- prep_l2$data_points[[1]] |>
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble()


fig1 <- data |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Gradbeni\u0161tvo skupaj", color = I(umar_cols()[1])) |>
  layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l$updated,
                                        prep_l$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                           font = list(size = 12)))

fig2 <- data2 |>
  plot_ly(x = ~period, width = 1000,
          height = 600) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|%b-%Y} %{y:.2f}",
            name = "Skupna naro\u010dila", color = I(umar_cols()[4])) |>
  layout(annotations = list(x = 0 , y = 1, showarrow = F,
                            xref='paper', yref='paper', text = paste("Posodobljeno:",prep_l2$updated,
                                                                     prep_l2$transf_txt, "(Vir: SURS & prera\u010dun UMAR)"),
                            font = list(size = 12)))


subplot(fig1,  fig2,  nrows = 2, shareX = TRUE) |>
  layout(showlegend = TRUE,
         autosize = F, margin =  m,
         font=list(family = "Myriad Pro"),
         yaxis = list(
           title = list(text="Medletna sprememba, v %",
                        font = list(size =12)),
           fixedrange = FALSE),
         yaxis2 = list(
           title = list(text="Ravnote\u017dje, v o.t.",
                        font = list(size =12)), fixedrange = FALSE),
         xaxis = list(title = "",
                      rangeslider = list(thickness = 0.1),
                      tickformatstops = list(
                        list(dtickrange = list("M1", "M6"),
                             value = "Q%q-%Y"),
                        list(dtickrange = list("M6", NULL),
                             value = "%Y")))) |>
  rangeslider(as.Date("2013-01-01"), max(data$period) + 100)


