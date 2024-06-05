# Empty data frames

df <- read.table(text = "",
                 colClasses = c("character", "numeric", ""),
                 col.names = c("method", "wert", "baum"))
write.csv(df, "data/dbh_data.csv",
          row.names = F)
write.csv(df, "data/height_data.csv",
          row.names = F)


# Uhrzeit als Dezimalzahl

get.sys.time = function(ct){
    tt = format(Sys.time(), "%H %M %S")
    tt = strsplit(tt, " ")[[1]]
    return(round(as.numeric(tt[1]) + as.numeric(tt[2])/60 + as.numeric(tt[2])/60/60, 7))
}

get.sys.time(Sys.time())

# Heights ----
n = 20
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

heights %>% 
    ggplot(., aes(x = zeit, y = wert, col = method)) +
    geom_point() +
    geom_line() +
    # geom_smooth(method = "lm") + 
    facet_wrap(~baum)
# DBH ----

raten = data.frame(method = "Raten | Guess",
                   wert = rnorm(42, 55, 3),
                   baum = sample(c("B2", "B3"), 42, replace = T))
laser = data.frame(method = "Massband | Tapeline",
                   wert = rnorm(18, 55, 15),
                   baum = sample(c("B2", "B3"), 18, replace = T))
app = data.frame(method = "Kluppe | Caliper",
                 wert = rnorm(14, 55, 1),
                 baum = sample(c("B2", "B3"), 14, replace = T))

dbh = rbind(raten, laser, app)
dbh

write.csv(dbh, "data/dbh_data.csv",
          row.names = F)

# Raten ----
dbh_r = dbh %>% 
    filter(method == "Raten") 
dbh_r = dbh_r %>% 
    group_by(baum) %>% 
    mutate(i = sample(n())) %>% 
    rename("BHD" = "wert")

heights_r = heights %>% 
    filter(method == "Raten") 
heights_r = heights_r %>% 
    group_by(baum) %>% 
    mutate(i = sample(n())) %>% 
    rename("Höhe" = "wert")

df = left_join(dbh_r, heights_r %>% select(-method), by = c("i", "baum"), copy = T) 
View(df)

df %>% 
    ggplot(., aes(x = BHD, y = Höhe, col = baum)) +
    geom_point() +
    geom_smooth(method = "lm")
    stat_ellipse()
