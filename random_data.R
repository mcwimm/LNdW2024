raten = data.frame(method = "Raten",
                   wert = rnorm(10, 10, 4),
                   baum = sample(c("B1", "B2"), 10, replace = T))
laser = data.frame(method = "Laser",
                   wert = rnorm(15, 10, 1),
                   baum = sample(c("B1", "B2"), 15, replace = T))
app = data.frame(method = "App",
                 wert = rnorm(20, 10, 2),
                 baum = sample(c("B1", "B2"), 20, replace = T))

dd = rbind(raten, laser, app)
dd

write.csv(dd, "height_data.csv",
          row.names = F)

dd %>% 
    filter(baum == "B1") %>% 
    group_by(method) %>% 
    sample_frac(., 1) %>% 
    ggplot(., aes(x = method, y = wert)) +
    geom_boxplot() +
    geom_jitter() +
    labs(x = "Methode zur Höhenschätzung",
         y = "Höhe (m)")
