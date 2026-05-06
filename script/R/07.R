library(tidyr)
library(readr)
library(dplyr)

# ------------------------------------------------------------------------------

# adattáblák

mem = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/memory.csv")
simon = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/simon.csv")
mood = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mood_sampling.csv")
stroop = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/stroop.csv")
anxiety = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/anxiety.csv")
wm = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/wm.csv")
sleep = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/sleep.csv")

# ------------------------------------------------------------------------------

# hosszú adatformátumból széles(ebb)et

# KÖZÖS

# MEMÓRIAFELADAT: feladattípus és ingertípus
# először rakjuk a feladattípusokat külön oszlopokba

mem |> pivot_wider(
  names_from = task,
  values_from = score
)

# utána a a feladattípusokat is és az ingertípusokat is! legyen résztvevőnként 1 sor és 4 oszlop:
# Recall_Words, Recognition_Words, Recall_Images, Recognition_Images

mem |> pivot_wider(
  names_from = c(task, stimulus_type),
  values_from = score
)

# ÖNÁLLÓ

# SIMON FELADAT: kongruens és inkongruens próbák reakcióidő és válaszhelyesség
# legyen résztvevőként egy sor!
# HINT: ha a neveket egy kicsit szebbre szeretnénk:
# -- még a pivot_longer() használata előtt rename(): át lehet nevezni az oszlopokat
# -- pivot_wider() names_sep argumentuma: mi legyen az elválasztó karakter a név összetevői között?

# ÁTGONDOLNI: HOGY NÉZ KI AZ ADATTÁBLA, ÉS MIT SZERETNÉNK, HOGY NÉZZEN KI?

simon |>
  rename(
    'RT' = reaction_time,
    'ACC' = accuracy
  ) |>
  pivot_wider(
    names_from = c(block, condition),
    values_from = c(RT, ACC),
    names_sep = " "
  )

# HANGULAT EXPERIENCE SAMPLING: mikor milyen a hangulata a résztvevőknek?
# -- legyen résztvevőnként egy sor
# -- legyenek az időpontok sorba rendezve, "Time 1" etc. névvel
# -- extra feladat: ahol hiányzik érték, ott az összes érték átlagával helyettesítsük
# HINT: pivot_wider() dokumentáció vagy Copilot/ChatGPT/Gemini etc.

mood |>
  pivot_wider(
    names_from = timepoint,
    values_from = mood_rating,
    names_prefix = "Time ",
    names_sort = TRUE,
    values_fill = mood$mood_rating |> mean(na.rm = TRUE)
  )

# ------------------------------------------------------------------------------

# szélesből hosszú adatformátum

# KÖZÖS

# STROOP: résztvevőnként külön oszlopban a feltételek reakcióidői
# legyen két oszlop, egy a kondíciónak, egy a reakcióidőnek

stroop |>
  pivot_longer(
    cols = -participant
  )

# adjunk új nevet is az oszlopoknak!

stroop |>
  pivot_longer(
    cols = -participant,
    names_to = "condition",
    values_to = "RT"
  )

# ÖNÁLLÓ

# SZORONGÁS: nehéz és könnyű feladat előtti és utáni szorongásszintet mérték fel
# impliciten két változó van kódolva: feladatnehézség, időpont
# legyen három oszlop: difficulty, time, anxiety
# HINT:
# -- names_to argumentumban lehet több elemet (vektorban) megadni
# -- pivot_longer() dokumentáció vagy Copilot/ChatGPT/Gemini etc.

# HOGY NÉZ KI, ÉS MIT SZERETNÉNK, HOGY NÉZZEN KI AZ ADATTÁBLA?

anxiety |>
  pivot_longer(
    cols = -participant,
    names_to = c("difficulty", "time"),
    names_sep = "_",
    values_to = "anxiety"
  )

# munkamemória: 2, 4, 6 és 8 elem megjegyzésénél milyen teljesítményt nyújtanak
# legyen két oszlop: "load" és "score"
# mindkettő legyen számformátumú

wm |>
  pivot_longer(
    cols = c("2", "4", "6", "8"),
    names_to = "load",
    names_transform = list(load = as.integer),
    values_to = "score"
  )

# ------------------------------------------------------------------------------

# ALVÁS HATÁSA A VÉGREHAJTÓ FUNKCIÓKRA: reggeli és esti teljesítmény
# legyen három oszlop: "time", "wm", "stroop"
# HINT: pivot_longer()-t és pivot_wider()-t kombinálni kell

sleep |>
  pivot_longer(
    cols = -participant,
    names_to = c("time", "task"),
    values_to = "performance",
    names_sep = "_"
  ) |>
  pivot_wider(
    names_from = task,
    values_from = performance
  )
