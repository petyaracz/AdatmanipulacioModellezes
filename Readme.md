R órák
================
Rácz, Péter; Lukics, Kriszti
2026-06-30

# Sillabusz: Kezdő R

1.  Bevezetés az adatkezelésbe

- select
- filter
- arrange
- mutate

2.  Bevezetés az adatvizualizációba

- ggplot
- aes
- geom

3.  Leíró statisztikák készítése

- group by
- mutate
- summarise

4.  Adatvizualizáció alapok

- faceting
- smooth
- legend
- axis

5.  Takaros adatok
6.  Széles és hosszú adatformátumok
7.  További adatformátumok
8.  Projektmunka
9.  Haladó adatvizualizáció
10. Adatkommunikáció: RMarkdown

# Sillabusz: Haladó R

1.  lineáris regresszió

- elmélet
  - jel és zaj, inferencia és jóslás
  - ki négyzet próba, t próba, anova, akármi: helyett hierarchikus
    általánosított lineáris modellek
  - mi lesz: lineáris modell, többértékű ~, általánosított ~,
    hierarchikus ~, modellválasztás, modellkritika, jóslás, ábrázolás,
    munkafolyamatok
  - mi nem lesz: matek, p értékek, más módszerek, alap programozás
  - együtthatók
  - maradékok
  - metszéspont és meredekség
  - faktorok
- gyakorlat:
  - `read_delim`
  - `ggplot`
  - `geom_smooth(method = 'lm')`
  - `lm()`
- házi
  - Datacamp::Introduction_to_Regression::Chapter_1

2.  jóslás

- elmélet:
  - predikció és extrapoláció
- gyakorlat
  - `summary()`
  - `glance()`
  - `predict()`
- házi
  - Datacamp::Introduction_to_Regression::Chapter_2

3.  diagnosztika

- elmélet:
  - r^2 és RSE
  - AIC és BIC
  - linearitás, homoszkedaszticitás, invariancia, a hibák függetlensége
  - Ascombe’s quartet
- gyakorlat:
  - `performance::model_performance()`
- házi:
  - Datacamp::Introduction_to_Regression::Chapter_3

4.  logisztikus regresszió

- elmélet:
  - általánosított lineáris modellek
  - véletlen komponens, kapcsolati függvény
  - p, oddszok, log oddsz
- gyakorlat:
  - `glm()`
  - `qlogis(), plogis(), exp()`
- házi:
  - Datacamp::Introduction_to_Regression::Chapter_4

5.  többszörös lineáris regresszió: numer + faktor

- elmélet:
  - alul- és túlillesztés
    - szabadságfok\
  - több metszéspont
- gyakorlat:
  - `1 + a`, `0 + a`
  - `performance::compare_performance()`
  - `anova()`
- házi:
  - Datacamp::Intermediate_Regression::Chapter_1

6.  többszörös lineáris regresszió: numer \* faktor

- elmélet:
  - több meredekség
  - Simpson paradoxona
  - interakciók
- gyakorlat:
  - `a*b`, `a:b`
- házi:
  - Datacamp::Intermediate_Regression::Chapter_2

7.  többszörös lineáris regresszió: numer \* numer

- elmélet:
  - többfokú meredekség
- gyakorlat:
- `geom_density`, `facet_wrap`
- házi:
  - Datacamp::Intermediate_Regression::Chapter_3

8.  többszörös logisztikus regresszió

- elmélet:
  - interakciók, kumulatív oddszok
- gyakorlat:
  - `sec.axis`
- házi:
  - Datacamp::Intermediate_Regression::Chapter_4

9.  hierarchikus általánosított lineáris modellek: random metszéspontok

- elmélet:
- gyakorlat:
- résztvevőszintű adatok és megfigyelésszintű adatok
- glmmTMB()
- broom.mixed
- residual r^2, conditional r^2
- házi:
  - Datacamp::Hierarchical_Regression::Chapter_1

10. hierarchikus általánosított lineáris modellek: random meredekségek

- elmélet:
  - random meredekségek
- gyakorlat:
  - `(1|a)`, `(1 + b|a)`
- házi:
  - Datacamp::Hierarchical_Regression::Chapter_2

11. hierarchikus általánosított lineáris modellek: ismételt mérések

- elmélet:
  - anova, manova, paired t teszt
- gyakorlat:
  - `(1|item)`, `(1|participant)`
- házi:
  - Datacamp::Hierarchical_Regression::Chapter_4

# Hasznos linkek

[itt.](https://peterracz.wordpress.com/teaching/intro-r-bevezetes-az-r-programozasba/)

# Acknowledgement

This class is supported by DataCamp, the most intuitive learning
platform for data science and analytics. Learn any time, anywhere and
become an expert in R, Python, SQL, and more. DataCamp’s learn-by-doing
methodology combines short expert videos and hands-on-the-keyboard
exercises to help learners retain knowledge. DataCamp offers 350+
courses by expert instructors on topics such as importing data, data
visualization, and machine learning. They’re constantly expanding their
curriculum to keep up with the latest technology trends and to provide
the best learning experience for all skill levels. Join over 6 million
learners around the world and close your skills gap.

# Kisokos

Az órán tanultak, egy helyen.

## 0. Milyen package-ekre (library-kre) lesz szükségünk?

``` r
library(tidyverse) # tidyverse csomagok, select/filter/arrange/mutate/summarise, ggplot
library(broom.mixed) # tidy, glance, augment
library(performance) # modellösszehasonlítgatás
library(sjPlot) # modell becsléseinek ábrázolása
library(glmmTMB) # hierarchikus / kevert lineáris modellek
```

## 1. Ábrázolás, feltérképezés (exploratory data analysis)

Az R beépített adathalmazai közül egyet, az iris-t fogjuk használni. Az
iris méréseket tartalmaz virágfajtákról, amelyeknek három alfaja van, és
négy dimenziójukat mérjük: a virág szirmának hosszát és szélességét,
valamint a csészelevelek hosszát és szélességét. Az iris adathalmazt az
idők hajnala óta használják arra, hogy gépi tanulást meg kategorizációt
tanítsanak rá, mivel a fajták könnyen megkülönböztethetőek a méreteik
alapján. Mi most lineáris modelleket fogunk rá építeni, mert erre a
célra is tökéletesen megfelel.

``` r
str(iris)
```

    ## 'data.frame':    150 obs. of  5 variables:
    ##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

Nézzük meg, hogyan viszonyulnak egymáshoz a különféle méretek.

``` r
# sepal length x width x species
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Readme_files/figure-gfm/iris%20plots-1.png)<!-- -->

``` r
# petal length x width x species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Readme_files/figure-gfm/iris%20plots-2.png)<!-- -->

``` r
# petal length x sepal length x species
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Readme_files/figure-gfm/iris%20plots-3.png)<!-- -->

``` r
# petal width x sepal width x species
ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Readme_files/figure-gfm/iris%20plots-4.png)<!-- -->

A szirom (sepal) hossza összefügg a szélességével, a fellevél (petal)
hossza is összefügg a szélességével, és a szirom és a fellevél hossza
illetve szélessége is összefügg. De a szirom és a fellevél is nagyon más
alakú attól függően, hogy milyen fajról beszélünk, ezért enélkül az
információ nélkül nem tudjuk jól modellezni őket.

------------------------------------------------------------------------

Több tidyverse [itt](https://www.tidyverse.org/). Több ggplot2
[itt](https://ggplot2.tidyverse.org/).

------------------------------------------------------------------------

## 2. Modellépítés

### 2.1. Lineáris modell

Most főleg a sepal width-et fogjuk jósolgatni a petal width-ből.

Szintaxis:

``` r
# sima lineáris modell: y = a + b * x
lm1 <- lm(Sepal.Width ~ Petal.Width, data = iris)
# többváltozós lineáris modell: faktor és numerikus prediktor: y = a1 / a2 / a3 + b * x
lm2 <- lm(Sepal.Width ~ Species + Petal.Width, data = iris)
# interakció: faktor és numerikus prediktor: y = a1 + b1 * x / a2 + b2 * x / a3 + b3 * x
lm3 <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
# interakció: két numerikus prediktor:
lm4 <- lm(Sepal.Width ~ Sepal.Length * Petal.Length, data = iris)
# persze egy lineáris modellben lehet akárhány prediktor:
lm5 <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Sepal.Length + Species, data = iris)
```

### 2.2. Hierarchikus lineáris modell

Szintaxis:

``` r
# grouping faktor / random intercept:
lmm1 <- glmmTMB(Sepal.Width ~ Petal.Width + (1 | Species), data = iris)
# grouping faktor és saját slope / random intercept és random slope:
lmm2 <- glmmTMB(Sepal.Width ~ Petal.Width + (1 + Petal.Width | Species), data = iris)
```

### 2.3. Generalizált lineáris modell

Szintaxis:

``` r
# bináris válaszváltozó: binominalis eloszlás, logit linkfüggvény
iris$setosa = ifelse(iris$Species == "setosa", 1, 0)

glm1 <- glm(setosa ~ Sepal.Length, data = iris, family = binomial(link = "logit"))

# persze ez is lehet hierarchikus modell

glmm1 <- glmmTMB(setosa ~ Sepal.Length + (1 | Species), data = iris, family = binomial(link = "logit"))
```

    ## Warning in finalizeTMB(TMBStruc, obj, fit, h, data.tmb.old): Model convergence
    ## problem; false convergence (8). See vignette('troubleshooting'),
    ## help('diagnose')

------------------------------------------------------------------------

Több glmmTMB [itt](https://glmmtmb.github.io/glmmTMB/).

------------------------------------------------------------------------

## 3. Modellkritika

Három modell a komoly jelölt: az lm1, az lmm1 és az lmm2. Az lm2-t és az
lm3-at csak azért nézzük meg, hogy lássuk, mi történik kollineáris
prediktorokkal. Emlékeztetőül:

``` r
formula(lm1)
```

    ## Sepal.Width ~ Petal.Width

``` r
formula(lm2)
```

    ## Sepal.Width ~ Species + Petal.Width

``` r
formula(lm3)
```

    ## Sepal.Width ~ Species * Petal.Width

``` r
formula(lmm1)
```

    ## Sepal.Width ~ Petal.Width + (1 | Species)

``` r
formula(lmm2)
```

    ## Sepal.Width ~ Petal.Width + (1 + Petal.Width | Species)

Megfeleltek a modellek a regresszió alapvetéseinek?

#### lm1

``` r
check_model(lm1)
```

![](Readme_files/figure-gfm/lm_assumptions1-1.png)<!-- -->

Az első modellban maradékok eloszlása nagyjából normális, de a variancia
nem konstans, szemmel láthatóan van még valami struktúra az adatokban,
amit ez a modell nem talál meg.

#### lm2

``` r
check_model(lm2)
```

![](Readme_files/figure-gfm/lm_assumptions2-1.png)<!-- -->

Az lm2-ben megengedtük, hogy a lineáris modellnek a három fajra
különböző interceptje legyen, de azt nem, hogy a slope is különböző
legyen. A maradékok eloszlása és a variancia is javult, de nem
tökéletes. A prediktorok közötti kollinearitás viszont elég súlyos: a
három virágfajta méretei drasztikusan különböznek, ezért a species elég
jól megragadja a petal width-et, azaz aggályos őket ugyanabban a
modellban használni.

Vesd össze:

``` r
clm = lm(Sepal.Width ~ Species, data = iris)
tidy(clm) |> 
  knitr::kable('simple', digits = 2)
```

| term              | estimate | std.error | statistic | p.value |
|:------------------|---------:|----------:|----------:|--------:|
| (Intercept)       |     3.43 |      0.05 |     71.36 |       0 |
| Speciesversicolor |    -0.66 |      0.07 |     -9.69 |       0 |
| Speciesvirginica  |    -0.45 |      0.07 |     -6.68 |       0 |

#### lm3

``` r
check_model(lm3)
```

![](Readme_files/figure-gfm/lm_assumptions3-1.png)<!-- -->

Az lm3-ban külön intercept és slope is van a három fajra. Ez megoldja a
maradékok eloszlásának a problémáit, viszont itt már gigantikus bajunk
lesz azzal, hogy a két prediktor nagyon durván kollineáris (az
interakció két olyan dolgot “szoroz össze”, amik már eleve ugyanazt
mérik, tehát gáz van).

#### lmm1

``` r
check_model(lmm1)
```

    ## `check_outliers()` does not yet support models of class `glmmTMB`.

![](Readme_files/figure-gfm/lm_assumptions4-1.png)<!-- -->

Az az alapvető baj, hogy minket a species hatása igazából most nem
érdekel. Minket az érdekel, hogy ebben a virágtípusban a szirom hossza
megjósolja-e a szirom szélességét. Viszont a species fontos eleme az
adatok hierarchiájának: a különböző fajták eleve eltérő méretűek. Ezért
az lmm1-ben a species-t grouping faktornak tesszük be. A maradékok az
lm2-höz hasonlóan néznek ki, viszont kollinearitással most nincsen
córesz, mert a species nem prediktor, hanem grouping factor.

#### lmm2

``` r
check_model(lmm2)
```

    ## `check_outliers()` does not yet support models of class `glmmTMB`.

![](Readme_files/figure-gfm/lm_assumptions5-1.png)<!-- -->

Az lmm2-ben a slope is csoportonként különböző lehet. A maradékok
szépek, de az intercept-only lmm1-hez képest nincs nagy javulás.

**Melyik modell a jobb?**

Az lm1 maradékai csúnyák, az lm2 és az lm3 pedig a kollinearitás miatt
vállalhatatlan: az estimate-ek valószínűleg értelmezhetetlenek. Marad a
három jelölt, hasonlítsuk össze az lm1-lmm1-lmm2-t.

``` r
compare_performance(lm1, lmm1, lmm2) |> 
  knitr::kable('simple', digits = 2)
```

| Name | Model | AIC | AIC_wt | AICc | AICc_wt | BIC | BIC_wt | RMSE | Sigma | R2_conditional | R2_marginal | ICC | R2 | R2_adjusted |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| lm1 | lm | 159.96 | 0.00 | 160.13 | 0.0 | 169.00 | 0.00 | 0.4 | 0.41 | NA | NA | NA | 0.13 | 0.13 |
| lmm1 | glmmTMB | 89.75 | 0.88 | 90.02 | 0.9 | 101.79 | 0.99 | 0.3 | 0.30 | 0.91 | 0.32 | 0.87 | NA | NA |
| lmm2 | glmmTMB | 93.75 | 0.12 | 94.33 | 0.1 | 111.81 | 0.01 | 0.3 | 0.30 | 0.91 | 0.32 | 0.87 | NA | NA |

``` r
plot(compare_performance(lm1, lmm1, lmm2))
```

![](Readme_files/figure-gfm/lm_compare-1.png)<!-- -->

Úgy tűnik, hogy az lmm1 a legjobb modell. Őt nem csak ez az ábra
indokolja, hanem az is, hogy tudjuk, hogy az lm1-ben hülyén néznek ki a
maradékok, és azt is tudjuk, hogy valószínűleg azért, mert a
megfigyeléseinket nagyban meghatározza, hogy a mérések melyik
virágfajtához tartoznak, és az lm1 erről nem vesz tudomást. Nagyon
hasonló módon érdemes rögtön hierarchikus / kevert modellel kezdeni, ha
az adatok hierarchikussága egyértemű: pl egy kísérletben több válasz
tartozik egy-egy emberhez, vagy egy felmérésben több ember pontszáma
egy-egy iskolához, stb.

Biztonság kedvéért hasonlítsuk össze csupán a két hierarchikus modellt
egymással.

``` r
compare_performance(lmm1, lmm2) |> 
  knitr::kable('simple', digits = 2)
```

    ## Some of the nested models seem to be identical and probably only vary in
    ##   their random effects.

| Name | Model | AIC | AIC_wt | AICc | AICc_wt | BIC | BIC_wt | R2_conditional | R2_marginal | ICC | RMSE | Sigma |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| lmm1 | glmmTMB | 89.75 | 0.88 | 90.02 | 0.9 | 101.79 | 0.99 | 0.91 | 0.32 | 0.87 | 0.3 | 0.3 |
| lmm2 | glmmTMB | 93.75 | 0.12 | 94.33 | 0.1 | 111.81 | 0.01 | 0.91 | 0.32 | 0.87 | 0.3 | 0.3 |

``` r
plot(compare_performance(lmm1, lmm2))
```

    ## Some of the nested models seem to be identical and probably only vary in
    ##   their random effects.

![](Readme_files/figure-gfm/lm_compare2-1.png)<!-- -->

A két hierarchikus (kevert) modell közül is az lmm1 tűnik jobbnak, főleg
azoknak a mérőszámoknak az alapján, amelyke a komplexitást büntetik
(AIC, BIC). Csináljunk egy likelihood ratio tesztet, ami megmondja, hogy
melyik modell illeszkedik jobban az adatokra:

``` r
test_likelihoodratio(lmm1,lmm2) |> 
  knitr::kable('simple', digits = 2)
```

    ## Some of the nested models seem to be identical and probably only vary in
    ##   their random effects.

|      | Name | Model   |  df | df_diff | Criterion | Chi2 |   p |
|------|:-----|:--------|----:|--------:|----------:|-----:|----:|
| lmm1 | lmm1 | glmmTMB |   4 |      NA |     81.75 |   NA |  NA |
| lmm2 | lmm2 | glmmTMB |   6 |       2 |     81.75 |    0 |   1 |

A p-value 0.05 fölött van, tehát a lmm2 nem illeszkedik szignifikánsan
jobban az adatokra, mint a lmm1. A BIC és az AIC is kisebb az lmm1
esetén, tehát ez elég egyértelmű.

Vesd össze: ugyanez a teszt azt mondja, hogy az lmm1 jobb, mint az lm1,
hiába sokkal bonyibb.

``` r
test_likelihoodratio(lm1,lmm1) |> 
  knitr::kable('simple', digits = 2)
```

|      | Name | Model   |  df | df_diff | Criterion |  Chi2 |   p |
|------|:-----|:--------|----:|--------:|----------:|------:|----:|
| lm1  | lm1  | lm      |   3 |      NA |    153.96 |    NA |  NA |
| lmm1 | lmm1 | glmmTMB |   4 |       1 |     81.75 | 72.21 |   0 |

------------------------------------------------------------------------

Több modellkritika [itt](https://easystats.github.io/performance/).

------------------------------------------------------------------------

## 4. Modell értelmezése, vizualizáció

``` r
summary(lmm1)
```

    ##  Family: gaussian  ( identity )
    ## Formula:          Sepal.Width ~ Petal.Width + (1 | Species)
    ## Data: iris
    ## 
    ##       AIC       BIC    logLik -2*log(L)  df.resid 
    ##      89.7     101.8     -40.9      81.7       146 
    ## 
    ## Random effects:
    ## 
    ## Conditional model:
    ##  Groups   Name        Variance Std.Dev.
    ##  Species  (Intercept) 0.59433  0.7709  
    ##  Residual             0.08991  0.2998  
    ## Number of obs: 150, groups:  Species, 3
    ## 
    ## Dispersion estimate for gaussian family (sigma^2): 0.0899 
    ## 
    ## Conditional model:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   2.1691     0.4696   4.619 3.85e-06 ***
    ## Petal.Width   0.7406     0.1231   6.015 1.80e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

A summary függvény kiirogatja az R konzolba a modell fő paramétereit. Ez
jó, csak nehéz berakni mondjuk egy táblázatba.

A tidy függvény kevesebb információt közöl, de takarosabb.

``` r
tidy(lmm1, conf.int = T) |> 
  knitr::kable('simple', digits = 2)
```

| effect | component | group | term | estimate | std.error | statistic | p.value | conf.low | conf.high |
|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| fixed | cond | NA | (Intercept) | 2.17 | 0.47 | 4.62 | 0 | 1.25 | 3.09 |
| fixed | cond | NA | Petal.Width | 0.74 | 0.12 | 6.02 | 0 | 0.50 | 0.98 |
| ran_pars | cond | Species | sd\_\_(Intercept) | 0.77 | NA | NA | NA | 0.27 | 0.34 |
| ran_pars | cond | Residual | sd\_\_Observation | 0.30 | NA | NA | NA | NA | NA |

Ábrázoljuk a modell becsléseit.

``` r
plot_model(lmm1, 'est')
```

![](Readme_files/figure-gfm/sjplot1-1.png)<!-- -->

Hát itt egy “b” van, a petal width, szóval ez nem túl érdekes. Nézzünk
meg egy modellt, amiben sok van.

``` r
plot_model(lm5, 'est')
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## ℹ The deprecated feature was likely used in the sjPlot package.
    ##   Please report the issue at <https://github.com/strengejacke/sjPlot/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Readme_files/figure-gfm/sjplot1b-1.png)<!-- -->

Vigyázat: ha a különféle prediktorok nem ugyanazon a skálán vannak,
akkor ez jó hülyén fog kinézni. Itt ez egyszerűen eleve adott (mindegyik
valami méret), illetve a scale() függvény segítségével is átalakíthatunk
prediktorokat (`pred = scale(pred)`).

Ábrázoljuk a modell predikcióit.

``` r
plot_model(lmm1, 'pred')
```

![](Readme_files/figure-gfm/sjplot2-1.png)<!-- -->

Predikciók ábrázolása több prediktor esetén. A `plot_model` mindig a
kimeneti változóra tett jóslatot teszi az y tengelyre, az első megadott
term-et (prediktort) teszi az x tengelyre, és a többi megadott term
szerint bont. Ha a második (harmadik) term egy folyamatos változó, akkor
csinál belőle három bödönt, és ábrázolja azokat. (Itt valszeg *mind a
két példának használt modell teljesen értelmetlen jóslatokat tesz*, de
arra jók, hogy gyakoroljuk ezeket a függvényeket.)

``` r
plot_model(lm3, 'pred', terms = c('Petal.Width','Species'))
```

![](Readme_files/figure-gfm/sjplot2b-1.png)<!-- -->

``` r
plot_model(lm3, 'pred', terms = c('Species','Petal.Width'))
```

    ## Ignoring unknown labels:
    ## • linetype : "Petal.Width"
    ## • shape : "Petal.Width"

![](Readme_files/figure-gfm/sjplot2b-2.png)<!-- -->

``` r
plot_model(lm4, 'pred', terms = c('Petal.Length','Sepal.Length'))
```

![](Readme_files/figure-gfm/sjplot2b-3.png)<!-- -->

``` r
plot_model(lm4, 'pred', terms = c('Sepal.Length','Petal.Length'))
```

![](Readme_files/figure-gfm/sjplot2b-4.png)<!-- -->

Hierarchikus / kevert modellek esetén ábrázolhatjuk a random
intercepeket és slope-okat is.

``` r
plot_model(lmm1, 're')
```

![](Readme_files/figure-gfm/sjplot3-1.png)<!-- -->

``` r
plot_model(lmm2, 're')
```

![](Readme_files/figure-gfm/sjplot3-2.png)<!-- -->

------------------------------------------------------------------------

Több sjplot [itt](https://strengejacke.github.io/).

------------------------------------------------------------------------
