con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "platform",
                      host = "192.168.38.21",
                      port = 5432,
                      user = "majaz",
                      password = Sys.getenv("PG_MZ_PSW"),
                      client_encoding = "utf8")
# set schema search path
x <- DBI::dbExecute(con, "set search_path to platform")

slider_w <- 0.05

m <- list(
  l = 50,
  r = 300,
  b = 30,
  t = 30,
  pad = 4)
