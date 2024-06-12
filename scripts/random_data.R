# Heights ----
n = 30
raten = data.frame(method = "Raten | Guess",
                   wert = rnorm(n, 10, 4),
                   baum = sample(c("B1", "B2", "B3"), n, replace = T),
                   zeit = sample(1700:2300, n) / 100)
laser = data.frame(method = "Laser",
                   wert = rnorm(n, 10, 1),
                   baum = sample(c("B2", "B3"), n, replace = T),
                   zeit = sample(1700:2300, n) / 100)
app = data.frame(method = "App",
                 wert = rnorm(n, 10, 2),
                 baum = sample(c("B2", "B3"), n, replace = T),
                 zeit = sample(1700:2300, n) / 100)

heights = rbind(raten, laser, app)
heights

write.csv(heights, "data/height_data.csv",
          row.names = F)