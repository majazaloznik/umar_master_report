rolled_stopnja <- stopnja  |>
  ungroup() |>
  mutate(across(starts_with("ratio_"),
                ~ zoo::rollmean(., k = 3, align  = "r", fill = NA),
                .names = "roll_{.col}"))

plot_ly(rolled_stopnja, x = ~period, width = 1000) |>
  add_lines_q(y = ~roll_ratio_value.y.prosta,  name = "C predelovalne dejavnosti",  color = I(umar_cols()[1])) |>
  add_lines_q(y = ~roll_ratio_value.x.x.x.prosta,  name = "F gradbeništvo",  color = I(umar_cols()[2])) |>
  add_lines_q(y = ~roll_ratio_value.y.y.y.prosta,  name = "G trgovina, vzdrževanje in popravila motornih vozil",  color = I(umar_cols()[3])) |>
  add_lines_q(y = ~roll_ratio_value.x.x.x.x.prosta,  name = "H promet in skladiščenje",  color = I(umar_cols()[4])) |>
  add_lines_q(y = ~roll_ratio_value.y.y.y.y.prosta,  name = "I gostinstvo",  color = I(umar_cols()[5])) |>
  add_lines_q(y = ~roll_ratio_value.x.x.x.x.x.x.x.x.x.prosta,  name = "R kulturne, razvedrilne in rekreacijske dej.",  color = I(umar_cols()[6])) |>
  add_lines_q(y = ~roll_ratio_ostalo.prosta,  name = "Ostalo",  color = I(umar_cols()[7])) |>
  add_lines_q(y = ~roll_ratio_value.prosta,  name = "Skupaj",  color = I("black")) |>
  umar_layout(slider_w, m,
              yaxis = umar_yaxis('Stopnja prostih delovnih mest'),
              xaxis = umar_xaxis("M"),
              title = umar_subtitle(updated, "UMAR", "Transf: 3-m drseča sredina (centrirana)"),
              annotations = initials("MiPe")) |>
  rangeslider(as.Date("2011-01-01"), max(data_zasedena$period))











