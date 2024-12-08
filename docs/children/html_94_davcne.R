library(dplyr)
con2 <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "davcne",
                       host = "192.168.38.21",
                       port = 5432,
                       user = "majaz",
                       password = Sys.getenv("PG_MZ_PSW"),
                       client_encoding = "utf8")


tmp <- tbl(con2, "davcni_racuni") |>
  group_by(datum) |>
  filter(filter == "1") |>
  summarise(znesek = sum(znesek),
            .groups = 'drop') |>
  collect() |>
  arrange(datum)

df <- tmp  |>
  mutate(lanski_datum = datum - lubridate::years(1)) |>
  # full join pomeni, da v prestopnem manjka en lanski datum
  # in v poprestopnem letu manjka en letošnji
  full_join(tmp, by = c("lanski_datum" = "datum" )) |>
  rename(znesek = znesek.x, lanski_znesek = znesek.y) |>
  # zaradi full join-a je treba zbrisati vrstice kjer so lanski datumi v prihodnosti
  filter(is.na(lanski_datum)| lanski_datum < (max(tmp$datum) - lubridate::days(365))) |>
  # zaradi full join-a je treba zbrisati tudi prvo leto, ki nima lanskih datumov
  filter(!(is.na(lanski_znesek) & if_all(-lanski_znesek, ~!is.na(.)))) |>
  mutate(letos_prestopno = if_else(lubridate::leap_year(datum), TRUE, FALSE),
         lani_prestopno = if_else(lubridate::leap_year(lanski_datum), TRUE, FALSE),
         letos_prestopno = if_else(lani_prestopno & !is.na(lani_prestopno), FALSE,letos_prestopno ),
         lani_prestopno = if_else(letos_prestopno & !is.na(letos_prestopno), FALSE, lani_prestopno),
         week_start = floor_date(datum, unit = "week", week_start = 1)) |>
  arrange(lanski_datum) |>
  tidyr::fill(week_start, .direction = "up") |>
  group_by(week_start)

adjusted <- df |>
  # za predprestopno leto rabim znesek prvega dneva po tem tednu
  left_join(
    df |>
      group_by(week_start) |>
      summarise(next_group_first = first(lanski_znesek)) |>
      mutate(week_start = week_start - lubridate::weeks(1)),
    by = "week_start"
  ) |>
  # in znesek zadnjega dneva pred tem tednom
  left_join(
    df |>
      group_by(week_start) |>
      summarise(
        prev_group_last = last(lanski_znesek)
      ) |>
      mutate(week_start = week_start + lubridate::weeks(1)),
    by = "week_start"
  ) |>
  # in oba zneska povprečim tam, kjer manjka lanski znesek. kar je samo
  # v prestopnem 29.2.
  mutate(lanski_znesek = if_else(is.na(lanski_znesek),
                                 (next_group_first + prev_group_last) * 0.5,
                                 lanski_znesek)) |>
  arrange(datum) |>
  # filter(datum > as.Date("2020-02-21") & datum < as.Date("2020-03-14")) |>
  # filter(datum > as.Date("2024-02-21") & datum < as.Date("2024-03-15")) |>
  ungroup() |> # because lag doesn't work outside the group
  mutate(lanski_datum = if_else(is.na(lanski_datum),
                                lag(lanski_datum),
                                lanski_datum)) |>
  # filter(lanski_datum > as.Date("2020-02-19") & lanski_datum < as.Date("2020-03-10")) |>
  # filter(lanski_datum > as.Date("2024-02-19") & lanski_datum < as.Date("2024-03-10")) |>
  arrange(lanski_datum) |>
  select(-next_group_first, -prev_group_last) |>
  mutate(lanski_znesek = if_else(!is.na(week_start) & lani_prestopno &
                                   month(week_start) == 3 & day(week_start) == 1 &
                                   month(datum) == 3 & day(datum)  == 7,
                                 (lanski_znesek + lag(lanski_znesek, 7)) * 0.5,
                                 lanski_znesek)) |>
  group_by(week_start) |>
  filter(!is.na(lanski_znesek)) |>
  # če je 29. na sredini lanskega tedna, daš na zadnji dan povprečje prvega in osmega
  mutate(lanski_znesek = if_else(!is.na(datum) & lani_prestopno &
                                   month(week_start) == 2 & day(week_start) %in% 23:28 &
                                   datum == last(datum),
                                 (lanski_znesek + first(lanski_znesek)) * 0.5,
                                 lanski_znesek)) |>
  # potem pa vrednost 29.ga na prvi dan tedna
  mutate(lanski_znesek = if_else(!is.na(datum) & lani_prestopno &
                                   month(week_start) == 2 & day(week_start) %in% 23:28 &
                                   datum == first(datum),
                                 first(lanski_znesek[day(lanski_datum) == 29]),
                                 lanski_znesek)) |>
  filter(!is.na(datum))

final <- adjusted |>
  summarise(count = n(),
            znesek = sum(znesek, na.rm = TRUE),
            lanski_znesek = sum(lanski_znesek, na.rm = TRUE),
            .groups = "drop") |>
  mutate(yoy = (znesek - lanski_znesek) / lanski_znesek * 100,
         week_end = week_start + 6) |>
  mutate( drseca =  zoo::rollmean(yoy, k = 4,fill= NA,align = "r"))


# graf
updated <- Sys.Date()
transf_txt <- "Transf.: drseča sredina medletne spremembe"

plot_ly(final, x = ~week_end, width = 1000) |>
  add_lines_dp(y = ~yoy,  name = "Medletna sprememba",  color = I(umar_cols()[3])) |>
  add_lines_dp( y = ~drseca,  name = "4-tedenska drseča sredina",  color = I(umar_cols()[1])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Medletna sprememba, v %'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, add = NULL,transf_txt, surs = FALSE, alt = "FURS"),
              annotations = initials("MoKo")) |>
  my_panel_note('* Brez SKD 35, 36, 52, 61, 64..') |>

  rangeslider(as.Date("2020-01-01"), max(final$week_end) + 20)

