# get data
df <- read.csv(here::here("data/115.csv"), encoding = "UTF-8")
spl <- split(df, df$chart_no)
# prepare data
prep_l <- prep_multi_line(spl[[1]], con)

purrr::reduce(prep_l$data_points, dplyr::full_join, by = c("period_id", "period"))  |>
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  arrange(period) -> data

updated <- max(prep_l$updated)

con2 <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "davcne",
                       host = "192.168.38.21",
                       port = 5432,
                       user = "majaz",
                       password = Sys.getenv("PG_MZ_PSW"),
                       client_encoding = "utf8")

daily <- tbl(con2, "davcni_racuni") |>
  filter(filter == "1") |>
  group_by(datum) |>
  summarise(znesek = sum(znesek),
            .groups = 'drop') |>
  collect()

monthly <- daily |>
  group_by(month = floor_date(datum, "month")) |>
  filter(n() == days_in_month(month)) |>  # remove incomplete month
  ungroup() |>
  group_by(year = year(datum), month= month(datum))  |>
  summarise(znesek = sum(znesek),
            .groups = 'drop') |>
  mutate(yoy = (znesek - lag(znesek, 12))/lag(znesek, 12) * 100,
         period = as.Date(paste0(year, "-", month, "-01")))

data <- data |>
  full_join(monthly)
# hardcoded y-lims, just so you know..
data |>
  plot_ly(x = ~period, width = 1000) |>
  add_lines_mp(y = ~`value.x`, name = "Gostinski prihodek",  color = I(umar_cols()[1])) |>
  add_lines_mp(y = ~`value.y`, name = "Prihodek v trg. z živili",  color = I(umar_cols()[2])) |>
  add_lines_mp(y = ~`value.x.x`, name = "Prihodek v trg. z neživili",  color = I(umar_cols()[3])) |>
  add_lines_mp(y = ~`value.y.y`, name = "Prihodek v trg. z mot. vozili",  color = I(umar_cols()[4])) |>
  add_lines_mp(y = ~`value.x.x.x`, name = "Prenočitve domačih turistov",  color = I(umar_cols()[5])) |>
  add_lines_mp(y = ~`value.y.y.y`, name = "Prodaja novih avtov fizičnim osebam",  color = I(umar_cols()[6])) |>
  add_lines_mp(y = ~`yoy`, name = "Davčne blagajne",  color = I(umar_cols()[8])) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Medletna rast, v %', range = list(-50, 40)),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", prep_l$transf_txt),
              annotations = initials("MoKo")) |>
  rangeslider(as.Date("2023-01-01"), max(data$period) + 50)
