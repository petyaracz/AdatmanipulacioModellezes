# Ezen az órán az adatok előkészítését és a felfedező adatelemzést fogjuk
# gyakorolni. A kutatás az életkor hatását vizsgálja a kognitív képességekre.

# A vizsgált képességek és az őket vizsgáló feladatok:
# - Munkamemória (digit span)
# - Kognitív kontroll (Stroop)
# - Statisztikai tanulás (verbal segmentation, visual segmentation)

# Az órán azt fogod megvizsgálni, hogy az egyes kognitív funkciók az életkorral,
# illetve egymással milyen kapcsolatban állnak.

# ==============================================================================

# Könyvtárak betöltése
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# 1. Hozz létre egy nagy adattáblát az összes feladat adataiból!

# 1.1. Adattáblák

# 1.1.1. Demográfiai adattábla
demography = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/demography.csv")
# A benne szereplő információk:
# - Azonosító (ID)
# - Tesztfelvétel dátuma (session_date, ÉÉÉÉ.HH.NN)
# - Nem (sex)
# - Születési dátum (birth_date, ÉÉÉÉ.HH.NN)
# - Képzettség (education, oktatásban eltöltött évek száma és megszerzett
#   legmagasabb iskolai képzettség)
# Feladat: adattábla rendbetétele, és az életkor (év) kiszámolása
# Segítség: életkor kiszámítása:
# as.numeric(difftime(as.Date(session_date, format = "%Y.%m.%d"), as.Date(birth_date, format = "%Y.%m.%d"), units = "days")) / 365.25
# Nézd át a kódot, miért van így!
# Segítség: nézd meg, hogy vannak-e fura életkorok, és ha igen, szűrd ki őket!

# 1.1.2. Számterjedelem teszt
digit_span = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/digit_span.csv")
# A benne szereplő információk:
# - Azonosító (ID)
# - Feladattípus (task_type: forward / backward)
# - Leghosszabb visszamondott számsor (span)
# Feladat: minden résztvevő adatai csak egy sorban legyenek!

# 1.1.3. Stroop feladat
stroop = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/stroop_long.csv")
# A benne szereplő információk:
# - Azonosító (ID)
# - Próbánkénti információk (blokk, próba, kondíció, megjelenített szöveg, betű
#   szín, adott válasz, helyes válasz)
# - Kondíció (condition: baseline / congruent / incongruent)
# - Válaszhelyesség (ACC)
# - Reakcióidő (RT)
# Feladat: reakcióidő mutató kiszámolása személyenként
# Segítség: a helyesen megválaszolt próbákat vesszük figyelembe. Személyenként
# összesítjük a kongruens és az inkongruens próbák medián reakcióidejét, és a
# medián inkongruens reakcióidőkből kivonjuk a medián kongruens reakcióidőket.

# 1.1.4. Nyelvi statisztikai tanulás
segmentation_verbal = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/segmentation_verbal.csv")
# A benne szereplő információk:
# - Azonosító (ID)
# - Online blokkok medián reakcióidője (medRT_TRN1—medRT_TRN5)
# - Offline teszt teljesítménye
# Feladat: online tanulási mutatók kiszámolása
# Segítség: a feladatban egy strukturált ingerfolyamot kell hallgatniuk a
# résztvevőknek. Egy "online" tanulási blokkbban tanulnak, ahol egy célingerre
# adott reakcióidőjét is figyeljük a résztvevőknek, ebből lesznek a reakcióidő
# adatok. A tanítás 5 blokkból áll, amelyikből a 4. szabálytalan. Úgy számoljuk
# ki a tanulást, hogy (1) egy mutatóba kiszámoljuk az 1. és 3. blokk
# különbségét, (2) egy mutatóba pedig a 3. és 4., és a 4. és 5. blokkok
# különbségének az átlagát. Utána egy "offline" utótesztben felmérjük a
# tudásukat, ebből lesz az "offline" mutató, ami már ki van számolva.
# Vizuális segítség: https://github.com/petyaracz/AdatmanipulacioModellezes/blob/main/segmentation_paradigm.png

# 1.2. Adattáblák egyesítése

# Készíts egyetlen adattáblát, amiben benne van az összes feladat minden adata!
# Csak azokra a résztvevőkre vagyunk kíváncsiak, akiknek van életkor adatuk.
# Segítség: sokszor nem transzparensek a változónevek az adatfájlokban, így
# érdemes majd felülírni őket!

# 2. Felfedező adatelemzés

# 2.1. Szűrd le az adattáblát a teljes kitöltésekre! Nem kell elmentened, csak
# nézd meg, hány teljes kitöltés van.

# 2.2. Készíts ábrákat a verbális statisztikai tanulás "RT_difference" és
# "offline" mutatójának kapcsolatáról! Nézd meg, mi a helyzet az "RT_training"
# és az "offline" között.

# 2.3. Hasonlítsd össze egy ábrán a férfiak és nők fordított számterjedelem
# teljesítményét!

# 2.4. Hogyan alakul a számterjedelem (sima) és a Stroop feladat kapcsolata
# a középiskolát (secondary) és az egyetemet (MA_MSc) végzett résztvevők
# esetében?
# Segítség: végzettség megadása a color aes() argumentumaként

# 2.5. Hogyan alakulnak a különböző kognitív képességek az életkor függvényében?
# Minden feladatból válassz egy tetszőleges mutatót, ábrázold pontdiagramon az
# életkorral, és simíts rájuk egy görbét. Hasonlítsd össze a különböző
# képességeket az életkor függvényében! Mit látsz? Mindenhol van életkori
# hatás? Ha igen, mindenhol ugyanolyan?
# Segítség: a geom_smooth() esetén a method = "loess" argumentumot használd!
# Látsz valami furát? Ha igen, mi okozhatja?
