#' this is a complicated one, with Q and M data, with the latter needing to be quarterised
#' depending on the data type and further yoy growth rates calculated on the quarters,
#' both complete or incomplete...
#'

# get data
df <- read.csv(here::here("data/116.csv"), encoding = "UTF-8")
spl <- split(df, df$sub_chart)
# prepare data
prep_l1<- prep_multi_line(spl[[1]], con) # Q
prep_l2<- prep_multi_line(spl[[2]], con) # M

data1 <-  prep_l1$data_points[[1]]

purrr::reduce(prep_l2$data_points, dplyr::full_join, by = c("period_id", "period"))  |>
  dplyr::relocate( period) |>
  select(-period_id) |>
  as_tibble() |>
  arrange(period) -> data2


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
  mutate(period = as.Date(paste0(year, "-", month, "-01")))

# quarterly indices
library(purrr)
library(tidyr)


# Main function
calculate_quarterly_yoy <- function(df, date_col, sum_cols = NULL, mean_cols = NULL) {
  all_results <- list()

  if (!is.null(sum_cols)) {
    for (col in sum_cols) {
      all_results[[col]] <- process_single_column(df, date_col, col, "sum")
    }
  }

  if (!is.null(mean_cols)) {
    for (col in mean_cols) {
      all_results[[col]] <- process_single_column(df, date_col, col, "mean")
    }
  }

  combined_results <- reduce(all_results, full_join, by = c("year", "quarter", "period"))
  return(combined_results)
}

# Process a single column
process_single_column <- function(df, date_col, value_col, agg_type) {
  # Prepare data
  working_df <- df %>%
    select(!!sym(date_col), !!sym(value_col)) %>%
    drop_na() %>%
    mutate(
      date = as.Date(!!sym(date_col)),
      year = year(date),
      month = month(date),
      quarter = quarter(date)
    )

  # Get quarterly aggregations
  quarterly_values <- aggregate_to_quarterly(working_df, value_col, agg_type)

  # Add year-over-year comparisons
  with_yoy <- calculate_yoy_growth(working_df, quarterly_values, value_col, agg_type)

  # Add period labels and clean up
  final_result <- with_yoy %>%
    mutate(
      label = case_when(
        num_months == 3 ~ paste0("Q", quarter, " ", year),
        TRUE ~ paste0("Partial Q", quarter, " ", year, " (", num_months, " months)")
      )
    ) %>%
    select(-months) # Remove the months list column

  return(final_result)
}

# Aggregate data to quarterly
aggregate_to_quarterly <- function(df, value_col, agg_type) {
  df %>%
    group_by(year, quarter) %>%
    summarise(
      num_months = n(),
      period = first(period),
      months = list(sort(month)),
      !!paste0(value_col) := if(agg_type == "sum") {
        sum(!!sym(value_col), na.rm = TRUE)
      } else {
        mean(!!sym(value_col), na.rm = TRUE)
      },
      .groups = "drop"
    )
}

# Calculate year-over-year growth
calculate_yoy_growth <- function(raw_df, quarterly_df, value_col, agg_type) {
  # Get previous year value for a specific quarter
  get_prev_year_value <- function(curr_year, curr_quarter, curr_months) {
    prev_year_data <- raw_df %>%
      filter(
        year == curr_year - 1,
        quarter == curr_quarter,
        month %in% unlist(curr_months)
      )

    if (nrow(prev_year_data) == 0) {
      return(NA_real_)
    }

    if (agg_type == "sum") {
      sum(prev_year_data[[value_col]], na.rm = TRUE)
    } else {
      mean(prev_year_data[[value_col]], na.rm = TRUE)
    }
  }

  # Apply the calculation row by row
  quarterly_df %>%
    arrange(year, quarter) %>%
    rowwise() %>%
    mutate(
      !!paste0(value_col, "_prev_year") := get_prev_year_value(year, quarter, months)
    ) %>%
    ungroup() %>%
    mutate(
      !!paste0(value_col, "_yoy_growth") := (!!sym(value_col) / !!sym(paste0(value_col, "_prev_year")) - 1) * 100
    )
}

data2 <- data2 |>
  full_join(monthly)

data2_summarised <- calculate_quarterly_yoy(data2, "period",
                                    mean_cols = c("value.x", "value.y", "value.x.x", "value.y.y"),
                                    sum_cols  = c("value.x.x.x", "value.y.y.y", "znesek"))




data22 <- data2_summarised |>
  select(period, value.x_yoy_growth, value.y_yoy_growth, value.x.x_yoy_growth,
         value.y.y_yoy_growth, value.x.x.x_yoy_growth, value.y.y.y_yoy_growth, znesek_yoy_growth)


full <-  data1 |>
  full_join(data22)

 updated <- max(prep_l2$updated)

# hardcoded y-lims, just so you know..
 full |>
  plot_ly(x = ~period, width = 1000) |>
   add_bars_qp(y = ~`value`, name = "Zasebna potrošnja",  color = I(umar_cols()[7])) |>
  add_lines_qp(y = ~`value.x_yoy_growth`, name = "Gostinski prihodek",  color = I(umar_cols()[1])) |>
  add_lines_qp(y = ~`value.y_yoy_growth`, name = "Prihodek v trg. z živili",  color = I(umar_cols()[2])) |>
  add_lines_qp(y = ~`value.x.x_yoy_growth`, name = "Prihodek v trg. z neživili",  color = I(umar_cols()[3])) |>
  add_lines_qp(y = ~`value.y.y_yoy_growth`, name = "Prihodek v trg. z mot. vozili",  color = I(umar_cols()[4])) |>
  add_lines_qp(y = ~`value.x.x.x_yoy_growth`, name = "Prenočitve domačih turistov",  color = I(umar_cols()[5])) |>
  add_lines_qp(y = ~`value.y.y.y_yoy_growth`, name = "Prodaja novih avtov fizičnim osebam",  color = I(umar_cols()[6])) |>
   add_lines_qp(y = ~`znesek_yoy_growth`, name = "Davčne blagajne",  color = I(umar_cols()[8])) |>
   umar_layout(slider_w, m,
               yaxis = umar_yaxis('Medletna rast, v %', range = list(-50, 40)),
               xaxis = umar_xaxis("Q"),
               title = umar_subtitle(updated, "UMAR", "Mesečni podatki agregirani v četrtletne in iz njih izračunane medletne rasti*"),
               annotations = initials("MoKo")) |>
   rangeslider(as.Date("2023-01-01"), max(data2$period) + 10) |>
   add_annotations(text='*V primeru, da za posamezno serijo niso  <br>na voljo podatki za celoten zadnji kvartal,  <br>je medletna rast izračunana iz ustreznega meseca  <br>oz. dveh v prejšnjem letu. ',
                  align='left',
                  showarrow=FALSE,
                  xref='paper',
                  yref='paper',
                  x=1.4,
                  y=0.2)
