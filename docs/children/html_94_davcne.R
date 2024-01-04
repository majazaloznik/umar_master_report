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
  group_by(datum) |>
  filter(filter == "1") |>
  summarise(znesek = sum(znesek),
            .groups = 'drop') |>
  collect() |>
  arrange(datum) %>%
  mutate(
    znesek = ifelse((month(datum) == 2) & (day(datum) == 28) & (lubridate::leap_year(datum)),
                    (znesek + lead(znesek, 1)) / 2,
                    znesek)) |>
  filter(!(month(datum) == 2 & day(datum) == 29)) |> # remove 29th after averaging with 28th
  mutate(datum_lani = lag(datum, 365),
         znesek_lani = lag(znesek, 365)) |>
  filter(!is.na(datum_lani)) |>
  group_by(week_end = ceiling_date(datum, unit = "week", week_start = 7)-1)  |>
  mutate(n = n()) |>
  filter(n == 7) |>
  summarise(znesek = sum(znesek, na.rm = TRUE),
            znesek_lani = sum(znesek_lani, na.rm = TRUE),
            .groups = "drop") |>
  mutate(yoy = (znesek - znesek_lani) / znesek_lani * 100) |>
  filter(!is.infinite(yoy)) |>
  mutate( drseca =  zoo::rollmean(yoy, k = 4,fill= NA,align = "r"))



updated <- Sys.Date()
transf_txt <- "Transf.: drseča sredina medletne spremembe"

plot_ly(x, x = ~week_end, width = 1000) |>
  add_lines_dp(y = ~yoy,  name = "Medletna sprememba",  color = I(umar_cols()[3])) |>
  add_lines_dp(data = x, y = ~drseca,  name = "4-tedenska drseča sredina",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Medletna sprememba, v %'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, add = NULL,transf_txt, surs = FALSE, alt = "FURS"),
              annotations = initials("MoKo")) |>
  my_panel_note('* Brez SKD 35, 36, 52, 61, 64..') |>

  rangeslider(as.Date("2020-01-01"), max(x$week_end) + 20)


