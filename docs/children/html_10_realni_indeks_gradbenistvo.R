#### Prispevki agregatov potrošnje k rasti BDP
# get data
df <- read.csv2(here::here("data/011.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)
prep_l$data_points[[1]] |>
  select(-period_id) |>
  relocate(period) -> data

updated <- prep_l$updated

data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines(y = ~`value`,  hovertemplate="%{x|Q%q-%Y} %{y:.2f}%",
            name = "Realni indeksi vrednosti opravljenih \ngradbenih del v gradbeništv (3-mesečna drseča sredina)", color = I(umar_cols()[1])) |>
  rangeslider(as.Date("2012-01-01"), max(data$period)+10) |>
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
  config(modeBarButtonsToAdd = list(dl_button))
