df <- data.frame(x = 1:10, y = rnorm(10))
plot(df$x, df$y, main = "čšć")
plot(df$x, df$y, main = "\u010d")
replacements <- c("č", "š", "ž")
constructive::construct(replacements)


plot(1, main = "\u010d")
plot(1, main = "\\u010d")
