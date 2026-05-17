# ============================================================
# 11. ora adatábrázolás: time series
# Adat: Szegedi időjárás, 2006-2016 (napi bontás)
# ============================================================

library(tidyverse)
library(lubridate) # idősoros adatok
library(slider)    # gördülő átlag
library(ggrepel)

szeged = read_tsv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R2/szeged.tsv")
magyar = read_tsv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/hun_birth_death_41_25.tsv")

# Gyors ellenőrzés
glimpse(szeged)

# mettől meddig?
szeged |>
  summarise(
    n = n(),
    elso = min(ymd),
    utolso = max(ymd)
  )

# ============================================================
# 1: Havi átlaghőmérséklet, 2006-2016
# ============================================================

# 1

ggplot(szeged, aes(x = ymd, y = temperature)) +
  geom_line()

# 2

szeged |>
  mutate(ev = factor(year(ymd))) |>
  ggplot(aes(x = yday(ymd), y = temperature, colour = ev)) +
  geom_line(alpha = 0.7)

# 3

honap <- szeged |>
  mutate(honap = floor_date(ymd, "month")) |> # kidobjuk a napokat
  group_by(honap) |>
  summarise(
    homerseklet = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  )

# 4

ggplot(honap, aes(x = honap, y = homerseklet)) +
  geom_line() +
  geom_point()

# 5

theme_clean <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey40"),
      plot.caption = element_text(colour = "grey50"),
      plot.margin = margin(10, 20, 10, 10)
    )
}

ggplot(honap, aes(x = honap, y = homerseklet)) +
  geom_line(colour = "steelblue", linewidth = 0.7) +
  geom_point(colour = "steelblue", size = 1.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    title = "Monthly mean temperature in Szeged",
    subtitle = "2006–2016"
  ) +
  theme_clean()

# 6

szelsok <- honap |>
  filter(homerseklet == max(homerseklet) |
           homerseklet == min(homerseklet))

p1_final <- ggplot(honap, aes(x = honap, y = homerseklet)) +
  geom_line(colour = "steelblue", linewidth = 0.7) +
  geom_point(colour = "steelblue", size = 1.5) +
  geom_point(data = szelsok, colour = "#c0392b", size = 3) +
  geom_text_repel(
    data = szelsok,
    aes(label = format(honap, "%b %Y")),
    size = 3.2,
    colour = "grey30"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    title = "Monthly mean temperature in Szeged",
    subtitle = "2006–2016"
  ) +
  theme_clean()

p1_final

# ============================================================
# 2: Szél és hőmérséklet -- kétváltozós probléma
# ============================================================

# 1

ggplot(szeged, aes(x = ymd)) +
  geom_line(aes(y = temperature), colour = "steelblue") +
  geom_line(aes(y = wind_speed), colour = "darkorange")

# 2

szeged_z <- szeged |>
  mutate(
    temp_z = scale(temperature)[,1],
    wind_z = scale(wind_speed)[,1]
  )

ggplot(szeged_z, aes(x = ymd)) +
  geom_line(aes(y = temp_z), colour = "steelblue", alpha = 0.6) +
  geom_line(aes(y = wind_z), colour = "darkorange", alpha = 0.6)

# 3

szeged_long <- szeged |>
  select(ymd, temperature, wind_speed) |>
  pivot_longer(
    cols = c(temperature, wind_speed),
    names_to = "valtozo",
    values_to = "ertek"
  ) |>
  mutate(valtozo = recode(valtozo,
                          "temperature" = "Temperature (°C)",
                          "wind_speed"  = "Wind speed (km/h)"
  ))

ggplot(szeged_long, aes(x = ymd, y = ertek)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ valtozo, ncol = 1, scales = "free_y")

# 4

szeged_long <- szeged_long |>
  group_by(valtozo) |>
  arrange(ymd) |>
  mutate(gordulo = slide_dbl(ertek, mean, .before = 3, .after = 3)) |>
  ungroup()

ggplot(szeged_long, aes(x = ymd)) +
  geom_line(aes(y = ertek), alpha = 0.15, linewidth = 0.3) +
  geom_line(aes(y = gordulo), linewidth = 0.8) +
  facet_wrap(~ valtozo, ncol = 1, scales = "free_y")

# 5

szeged_long <- szeged_long |>
  group_by(valtozo) |>
  arrange(ymd) |>
  mutate(gordulo30 = slide_dbl(ertek, mean, .before = 15, .after = 15)) |>
  ungroup()

szinek <- c("Temperature (°C)" = "steelblue",
            "Wind speed (km/h)" = "darkorange")

p2_final <- ggplot(szeged_long, aes(x = ymd, colour = valtozo)) +
  geom_line(aes(y = ertek), alpha = 0.12, linewidth = 0.3,
            show.legend = FALSE) +
  geom_line(aes(y = gordulo30), linewidth = 0.9,
            show.legend = FALSE) +
  scale_colour_manual(values = szinek) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~ valtozo, ncol = 1, scales = "free_y") +
  labs(
    x = NULL,
    y = NULL,
    title = "Temperature and wind speed in Szeged",
    subtitle = "Daily values (faded) with 30-day rolling mean",
    caption = "Source: Kaggle / Dark Sky"
  ) +
  theme_clean() +
  theme(strip.text = element_text(face = "bold"))

p2_final

# ============================================================
# hajrá csapat 1: Páratartalom szezonalitása
# ============================================================
# Ugyanaz a szerkezet mint 1, de páratartalom!



# ============================================================
# hajrá csapat 2: Éves hőmérsékleti anomália
# ============================================================
# Kiszámítjuk az év-nap szintű 10 éves átlagot (referencia),
# majd rávetítünk egy kiválasztott évet.
# geom_ribbon: a historikus tartomány (min-max sáv)
# geom_line: a kiválasztott év


# ============================================================
# hajrá csapat 3: Magyar népességfogyás:
# számold ki a népességfogyást minden sorra 
# számolj belőle gördülő átlagot a 01-25 intervallumra
# ábrázold, de a dátum legyen rendesen (1990 és 2000 között több hely legyen, mint 2000 és 2001 között)
# ============================================================
