library(readr)
library(dplyr)
library(tidyr)

ment_lang_dem = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mentalization_language_demography.csv")
diagnoses = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mentalization_language_diagnoses.csv")
ef = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mentalization_language_EF.csv")
language = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mentalization_language_language.csv")
language_trials = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mentalization_language_language_trials.csv")
mentalization = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/mentalization_language_mentalization.csv")
pers_dem = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/personality_demography.csv")
big5 = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/personality_big5.csv")
schizotypy = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/personality_schizotypy.csv")
anxiety = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/personality_anxiety.csv")
prediction = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/personality_rt.csv")

# 1. nyelv és mentalizáció

left_join(ment_lang_dem, diagnoses, join_by(initials == child))
right_join(ment_lang_dem, diagnoses, join_by(initials == child))

left_join(ef, language, by = "participant")
right_join(ef, language, by = "participant")
inner_join(ef, language, by = "participant")
full_join(ef, language, by = "participant")

ment_lang_dem |>
  left_join(diagnoses, join_by(initials == child)) |>
  right_join(language, by = "participant")

# ML1
# legyen egy olyan adattábla, amiben csak azokat az adatokat tartjuk meg minden feladatból, ahova tartozik életkor is
# (benne van a demográfiai táblázatban)
# az aggregált nyelvi változókból
# monogramokat a végén vegyük ki belőle
# legyen rendben minden változónév, tudjuk, hogy melyik változók milyen feladathoz tartoznak

ML1 = ment_lang_dem |>
  left_join(diagnoses, join_by(initials == child)) |>
  left_join(language, by = "participant") |>
  left_join(ef, by = "participant") |>
  left_join(mentalization, join_by(initials == participant)) |>
  select(-initials) |>
  rename(
    "language ACC complex" = complex_accuracy,
    "language ACC simple" = simple_accuracy,
    "language RT complex" = complex_RT,
    "language RT simple" = simple_RT,
    "EF" = `composite score`,
    "verbal mentalization" = verbal,
    "nonverbal mentalization" = nonverbal
  )

# ML2
# legyen egy olyan adattábla, ahol minden nyelvi vagy mentalizációs adatot megtartunk
# legyenek rendben a változónevek

ML2 = ment_lang_dem |>
  left_join(diagnoses, join_by(initials == child)) |>
  right_join(language, by = "participant") |>
  full_join(mentalization, join_by(initials == participant)) |>
  left_join(ef, by = "participant") |>
  rename(
    "language ACC complex" = complex_accuracy,
    "language ACC simple" = simple_accuracy,
    "language RT complex" = complex_RT,
    "language RT simple" = simple_RT,
    "EF" = `composite score`,
    "verbal mentalization" = verbal,
    "nonverbal mentalization" = nonverbal
  )

# legyen egy olyan adattábla, ahol a trialenkénti nyelvi adatok közül minden megvan, és hozzá csatoljuk a többit
# ezekből csak azokat, ahol van a nyelvi feladatból adat

ML3 = ment_lang_dem |>
  full_join(diagnoses, join_by(initials == child)) |>
  right_join(language_trials, by = "participant") |>
  left_join(mentalization, join_by(initials == participant)) |>
  left_join(ef, by = "participant") |>
  select(-initials) |>
  rename(
    "language condition" = condition,
    "language ACC" = accuracy,
    "language RT" = rt,
    "EF" = `composite score`,
    "verbal mentalization" = verbal,
    "nonverbal mentalization" = nonverbal
  )

# 2. személyiség

# P1
# legyen egy olyan adattábla, ahol csak azok az esetek maradnak meg, ahol megvannak a big5 és a szkizotípia adatok is
# hozzá a csatoljuk a demográfiát és a szorongást
# sorrend: demográfia, big5, szkizotípia, szorongás

P1 = pers_dem |>
  right_join(big5, join_by(nickname == participant)) |>
  inner_join(schizotypy, join_by(participant == part)) |>
  left_join(anxiety, join_by(participant == part_num))

# P2
# módosítsuk még úgy a fenti adattáblát, hogy rakjuk hozzá a predikciós feladat aggregált RT-it a két kondícióból

prediction_aggregated = prediction |>
  group_by(ID, condition) |>
  summarize(rt = median(rt)) |>
  ungroup() |>
  pivot_wider(names_from = condition, values_from = rt, names_prefix = "rt_")

P2 = pers_dem |>
  right_join(big5, join_by(nickname == participant)) |>
  inner_join(schizotypy, join_by(participant == part)) |>
  left_join(anxiety, join_by(participant == part_num)) |>
  left_join(prediction_aggregated, join_by(participant == ID))

# P3
# legyen egy olyan adattábla, ahol a predikciós feladat trial szintű adataihoz rakjuk hozzá a többi változó adatait

P3 = pers_dem |>
  full_join(big5, join_by(nickname == participant)) |>
  full_join(schizotypy, join_by(participant == part)) |>
  full_join(anxiety, join_by(participant == part_num)) |>
  right_join(prediction, join_by(participant == ID))

# mentsük le ezeket a dolgokat .csv-ként és .xlsx-ként