# beolvassuk a szükséges könyvtárakat
library(readr) # adattáblák beolvasásához
library(tidyr) # adattáblák csinosításához
library(dplyr) # adatmanipulációhoz
library(ggplot2) # adatvizualizációhoz
library(stringr) # karakterláncok kezeléséhez
library(writexl) # adattáblák exportálásához

# adattábla betöltése
# én abszolút elérési utat használok
df = read_csv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R/IMDB_untidy.csv")

#####

# mit látunk? minden oké?
df |> head()
df |> glimpse()

#####

# 1. rendetlen adatok: több érték egy cellában

# 1.1. szétszedjük a `Rating|Votes|Metascore` oszlopot három külön oszlopra
### EGYÜTT ###
df = df |>
  # külön változókként (oszloponként) mentjük a 'rating_numbers' és 'rating_scores' értékeket
  separate_wider_delim(
    # cols: melyik oszlopo(ka)t szedje szét
    cols = `Rating|Votes|Metascore`,
    # delim: mi az elválasztó az értékek között
    delim = "|",
    # names: mi legyen az új változók neve
    names = c('Rating', 'Votes', 'Metascore')
  )

# még amúgy ezek is vannak
# separate_wider_position()
# separate_wider_regex()

# ha elégedettek vagyunk, módosítsuk úgy a kódot, hogy felül is írjuk a dataframe-et!

# szuper, megvan, nézzünk rá az új adatokra
df$Votes |> mean(na.rm = FALSE)

# van, hogy konvertálni kell az adattípust ezután
# a 'rating_numbers' és a 'rating_scores' karakterváltozó maradt, ezeket javítsuk!
df$Rating = as.numeric(df$Rating)
df$Votes = as.numeric(df$Votes)
df$Metascore = as.numeric(df$Metascore)

# még mit kell szétszedni?
# 1.2. szedjük szét a címet és az évszámot!
### EGYÉNI ###
# ez nem fog működni
df |>
  separate_wider_delim(
    # mi más itt, mint a 'ratings' esetében? mi történik, ha nem adunk meg names értéket?
    cols = `Title & Year`,
    delim = " (",
    names = c("Title", "Year")
  )

### EGYÜTT ###
# a substr segítségével szétszedhetjük a címet és az évszámot
df$Year = substr(df$`Title & Year`, nchar(df$`Title & Year`) - 4, nchar(df$`Title & Year`) - 1)
df$Title = substr(df$`Title & Year`, 1, nchar(df$`Title & Year`) - 7)
# regex segítségével is szétszedhetjük a címet és az évszámot
# \: hagyja ki a zárójelet, \d{4} pedig a négy számjegyet
df$Year = str_extract(df$`Title & Year`, "\\d{4}")
df$Title = str_remove(df$`Title & Year`, " \\(\\d{4}\\)")

df$Title
df$Year

df$Year = as.numeric(df$Year)

### EGYÜTT ###
# 1.3. mi történik a változókkal, amikre nincs szükségünk?

df |> select(c(-`Title & Year`))

# ha elégedettek vagyunk, módosítsuk úgy a kódot, hogy felül is írjuk a dataframe-et

df = df |> select(c(-`Title & Year`))

#####

# 2. rendetlen adatok: egy változó adatai több oszlopban

### EGYÜTT ###
# 2.1. fura, hogy a rendezők nevei több oszlopban vannak
# egyrészt, lehet, hogy rosszul parse-olta a neveket, másrészt a vezetéknév nem azonosítja a rendezőt
df |>
  unite(
    # col: az új változó neve
    col = "Director",
    # oszlopok, amiket egyesíteni szeretnénk (nincs külön neve az argumentumnak)
    c(`Director FirstN`, `Director SecondN`),
    # sep: elválasztó karakter(ek)
    sep = " "
  )

# ha elégedettek vagyunk, módosítsuk úgy a kódot, hogy felül is írjuk a dataframe-et

df = df |> unite(
  col = "Director",
  c(`Director FirstN`, `Director SecondN`),
  sep = " "
)

### EGYÉNI ###
# 2.2. fűzzük össze a 'Genre1'-től a 'Genre3'-ig az oszlopokat
df |> unite(
  col = "Genre",
  # hogyan tudjuk egyszerűbben megadni az összefűzendő oszlopneveket?
  Genre1:Genre3,
  sep = "|",
  # na.rm: ha NA érték van, azt figyelmen kívül hagyja?
  na.rm = TRUE
)

# ha rendben van, mentsük el!

df = df |> unite(
  col = "Genre",
  Genre1:Genre3,
  sep = "|",
  na.rm = TRUE
)

### EGYÉNI MUNKA AZ ADATTÁBLA VÉGSŐ RENDBETÉTELÉRE ###

# 3.1. szedjük szét, amit szét kell szedni még, és rakjuk össze, amit össze kell rakni
# Actors, Rank|Metascore
df = df |> unite(
  col = "Actors",
  Actor1:Actor4,
  sep = "|",
)

df = df |> separate_wider_delim(
  cols = `Rank|Metascore`,
  delim = " ",
  names = c("Rank", "Metascore_drop")
)

df$Metascore_drop = substr(df$Metascore_drop, 11, nchar(df$Metascore_drop))
df$Metascore_drop = as.numeric(df$Metascore_drop)
df$Metascore_drop == df$Metascore

df$Rank = substr(df$Rank, 6, nchar(df$Rank))
df$Rank = as.numeric(df$Rank)

# rendezzük szép átlátható sorba az oszlopokat
# HINT: a select() sorba is rendez

df = df |> select(
  Rank,
  Title,
  Year,
  Director,
  Genre,
  Actors,
  `Runtime (Minutes)`,
  `Revenue (Millions)`,
  Rating,
  Votes,
  Metascore
)

# kiírás
# HINT:
# write_csv(): .csv-be mentés, na = ""
# write_xlsx(): .xlsx-be mentés

# ha elégedettek vagyunk, mentsük el .csv-ben az adattáblát
write_csv(df, "IMDB_tidy.csv", na = "")

# és .xlsx-ben is
write_xlsx(df, "IMDB_tidy.xlsx")

# felfedező adatelemzés
# HINT:
# str_detect(): megnézi, hogy egy szubsztring van-e egy karakterláncban
# pull(): egy oszlop értékeit adja vissza vektor formájában
# min(), max(), mean(), median(), sd(), etc: ha na.rm = TRUE, akkor figyelmen kívül hagyja az NA értékeket

# milyen kapcsolatban van a népszerűség a bevétellel?

df |> ggplot(aes(x = `Revenue (Millions)`, y = Rating)) +
  geom_point() +
  geom_smooth(method = "lm")

# milyen kapcsolatban van a hossz a népszerűséggel?

df |> ggplot(aes(x = `Runtime (Minutes)`, y = Rating)) +
  geom_point() +
  geom_smooth(method = "lm")

# hogyan néz ki a népszerűség és a metascore eloszlása?

df |> ggplot(aes(x = Rating)) +
  geom_histogram()

df |> ggplot(aes(x = Metascore)) +
  geom_histogram()

# ezek alapján melyik mutató a megbízhatóbb?

# népszerűbbek az átlagnál azok a filmek, amelyekben szerepel Scarlett Johansson?

df |> filter(str_detect(Actors, "Scarlett Johansson")) |> pull(Rating) |> mean(na.rm = TRUE)
df |> pull(Rating) |> mean(na.rm = TRUE)

# melyik a top 10 rendező, aki a legtöbb filmet rendezett a top 1000-ből?

df |> count(Director) |> arrange(desc(n)) |> head(10)