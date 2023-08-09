con2 <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "davcne",
                      host = "192.168.38.21",
                      port = 5432,
                      user = "majaz",
                      password = Sys.getenv("PG_MZ_PSW"),
                      client_encoding = "utf8")
x <- DBI::dbExecute(con, "set search_path to platform")
library(dplyr)


x <- tbl(con2, "davcni_racuni") |>
  group_by(leto, teden_us) |>
  summarise(start = min(datum),
            end = max(datum),
            znesek = sum(znesek_davka),
            .groups = 'drop') |>  # drop groups after summarise
  collect() |>
  bind_rows(data.frame(leto = 2023, teden_us = "00", start= as.Date("01-01-2023"),
                       end = as.Date("01-01-2023"), znesek = NA) )|>
  arrange(leto, teden_us) |>
  mutate(medletna = (znesek/lag(znesek, 53)-1)*100) |>
  mutate(drseca =  zoo::rollmean(medletna, k = 4,fill= NA,align = "r"),
         period = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01"),
         dr2 = zoo::rollmean(znesek, k = 4,fill= NA,align = "r") ,
         mdl2 = (dr2/lag(dr2, 53)-1)*100) |>
  filter(!is.na(medletna))
updated <- Sys.Date()
prep_l <- list()
prep_l$transf_txt <- "drseca sredina medletne rasti in obratno"

plot_ly(x, x = ~period, width = 1000,
        height = 600) |>
  add_lines_mp(y = ~medletna,  name = "Medletna rast zneska DDV",  color = I(umar_cols()[3])) |>
  add_lines_mp(data = x, y = ~drseca,  name = "4-tedenska drseča sredina rasti",  color = I(umar_cols()[1])) |>
  add_lines_mp(data = x, y = ~mdl2,  name = "Medletna rast drseče sredine",  color = I(umar_cols()[2])) |>
  umar_layout(slider_w, m,
    yaxis = umar_yaxis('Medletna rast, v %'),
    xaxis = umar_xaxis("M"),
    title = umar_subtitle("FURS"),
    annotations = initials("MoKo")) |>
  rangeslider(as.Date("2020-01-01"), max(x$period))
