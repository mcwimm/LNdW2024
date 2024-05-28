
# Heights ----
raten = data.frame(method = "Raten",
                   wert = rnorm(10, 10, 4),
                   baum = sample(c("B1", "B2", "B3"), 10, replace = T))
laser = data.frame(method = "Laser",
                   wert = rnorm(15, 10, 1),
                   baum = sample(c("B1", "B2", "B3"), 15, replace = T))
app = data.frame(method = "App",
                 wert = rnorm(20, 10, 2),
                 baum = sample(c("B1", "B2", "B3"), 20, replace = T))

heights = rbind(raten, laser, app)
heights

write.csv(heights, "height_data.csv",
          row.names = F)

heights %>% 
    # filter(baum == "B1") %>% 
    group_by(method) %>% 
    sample_frac(., 1) %>% 
    ggplot(., aes(x = method, y = wert)) +
    geom_boxplot() +
    geom_jitter() +
    facet_wrap(~baum) +
    labs(x = "Methode zur Höhenschätzung",
         y = "Höhe (m)")


# DBH ----

raten = data.frame(method = "Raten",
                   wert = rnorm(12, 55, 3),
                   baum = sample(c("B1", "B2", "B3"), 12, replace = T))
laser = data.frame(method = "Massband",
                   wert = rnorm(18, 55, 15),
                   baum = sample(c("B1", "B2", "B3"), 18, replace = T))
app = data.frame(method = "Kluppe",
                 wert = rnorm(14, 55, 1),
                 baum = sample(c("B1", "B2", "B3"), 14, replace = T))

dbh = rbind(raten, laser, app)
dbh

write.csv(dbh, "dbh_data.csv",
          row.names = F)

dbh %>% 
    filter(method == "Raten") %>% 
    View()

df = bind_rows(dbh, heights) %>% 
    filter(method == "Raten")
View(df)
