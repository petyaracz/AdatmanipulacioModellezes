# ============================================================
# 10. ora Custom ggplot2 Themes
# ============================================================

library(tidyverse)
library(gapminder)
library(ggrepel)   # szetdobalja a feliratokat

# ============================================================
# GDP vs life expectancy
# ============================================================

gap_2007 <- gapminder |>
  filter(year == 2007)

# 1

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# 2

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point()

# 3

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point() +
  scale_x_log10()

# 4

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    colour = NULL
  )

# 5

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp,
                     colour = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    colour = NULL
  )

# 6

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp,
                     colour = continent, size = pop)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 14), guide = "none") +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    colour = NULL
  )

# 7

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp,
                     colour = continent, size = pop)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 14), guide = "none") +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    colour = NULL
  ) +
  theme_minimal()

# 8

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp,
                     colour = continent, size = pop)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 14), guide = "none") +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    colour = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )

# 9

outliers <- gap_2007 |>
  filter(country %in% c("Botswana", "Kuwait", "Singapore", "Haiti"))

ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp,
                     colour = continent, size = pop)) +
  geom_point(alpha = 0.6) +
  geom_text(data = outliers,
            aes(label = country), size = 3) +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 14), guide = "none") +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    colour = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )

# 10

p1_final <- ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp,
                                 colour = continent, size = pop)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(data = outliers,
                  aes(label = country),
                  size = 3.5,
                  colour = "grey30",
                  show.legend = FALSE) +
  scale_x_log10(labels = scales::comma) +
  scale_size_continuous(range = c(1, 14), guide = "none") +
  labs(
    x = "GDP per capita (log scale, USD)",
    y = "Life expectancy (years)",
    title = "Wealth and health across the world, 2007",
    caption = "Source: Gapminder",
    colour = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey50"),
    plot.margin = margin(10, 20, 10, 10)
  )

p1_final

# ============================================================
# life expectancy change 1952-2007
# ============================================================

gap_americas <- gapminder |>
  filter(continent == "Americas",
         year %in% c(1952, 2007))

# 1

gapminder |>
  filter(continent == "Americas") |>
  ggplot(aes(x = year, y = lifeExp, group = country)) +
  geom_line()

# 2

ggplot(gap_americas, aes(x = year, y = lifeExp, group = country)) +
  geom_point() +
  geom_line()

# 3

ggplot(gap_americas, aes(x = year, y = lifeExp, group = country)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ country)

# cf.:

ggplot(gap_americas, aes(x = lifeExp, y = country, group = country)) +
  geom_point() +
  geom_line()

# 4

gap_americas <- gap_americas |>
  mutate(country = fct_reorder(country, lifeExp, max))

ggplot(gap_americas, aes(x = lifeExp, y = country, group = country)) +
  geom_point() +
  geom_line()

# 5

ggplot(gap_americas,
       aes(x = lifeExp, y = country,
           group = country, colour = factor(year))) +
  geom_point(size = 2) +
  geom_line(colour = "grey70") +
  scale_colour_manual(values = c("1952" = "#d7816a", "2007" = "#4a7c59"),
                      name = NULL)

# 6

gap_americas_wide <- gapminder |>
  filter(continent == "Americas", year %in% c(1952, 2007)) |>
  select(country, year, lifeExp) |>
  pivot_wider(names_from = year, values_from = lifeExp,
              names_prefix = "y") |>
  mutate(country = fct_reorder(country, y2007))

ggplot(gap_americas_wide) +
  geom_segment(aes(x = y1952, xend = y2007,
                   y = country, yend = country),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "grey50") +
  geom_point(aes(x = y1952, y = country), colour = "#d7816a", size = 2) +
  geom_point(aes(x = y2007, y = country), colour = "#4a7c59", size = 2)

# 7

ggplot(gap_americas_wide) +
  geom_segment(aes(x = y1952, xend = y2007,
                   y = country, yend = country),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "grey50") +
  geom_point(aes(x = y1952, y = country), colour = "#d7816a", size = 2) +
  geom_point(aes(x = y2007, y = country), colour = "#4a7c59", size = 2) +
  geom_text(aes(x = y2007, y = country, label = round(y2007, 1)), size = 2.5)

# 8

ggplot(gap_americas_wide) +
  geom_segment(aes(x = y1952, xend = y2007,
                   y = country, yend = country),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "grey60") +
  geom_point(aes(x = y1952, y = country), colour = "#d7816a", size = 2) +
  geom_point(aes(x = y2007, y = country), colour = "#4a7c59", size = 2) +
  geom_text(aes(x = y2007, y = country, label = round(y2007, 1)),
            nudge_x = 1, size = 2.8, hjust = 0) +
  scale_x_continuous(limits = c(37, 85)) +
  labs(
    x = "Life expectancy (years)",
    y = NULL,
    title = "Life expectancy in the Americas",
    subtitle = "Change from 1952 (orange) to 2007 (green)",
    caption = "Source: Gapminder"
  )

# 9

theme_clean <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey40"),
      plot.caption = element_text(colour = "grey50"),
      plot.margin = margin(10, 20, 10, 10)
    )
}

p2_final <- ggplot(gap_americas_wide) +
  geom_segment(aes(x = y1952, xend = y2007,
                   y = country, yend = country),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "grey60") +
  geom_point(aes(x = y1952, y = country), colour = "#d7816a", size = 2) +
  geom_point(aes(x = y2007, y = country), colour = "#4a7c59", size = 2) +
  geom_text(aes(x = y2007, y = country, label = round(y2007, 1)),
            nudge_x = 0.8, size = 2.8, hjust = 0) +
  scale_x_continuous(limits = c(37, 85)) +
  labs(
    x = "Life expectancy (years)",
    y = NULL,LASDKn qw/lkhqhoru
    title = "Life expectancy in the Americas",
    subtitle = "Change from 1952 (orange) to 2007 (green)",
    caption = "Source: Gapminder"
  ) +
  theme_clean()

p2_final

# ============================================================
# hajrá csapat: life exp x gdp
# ============================================================
# - válasszatok évet
# - x = lifeExp, y = gdpPercap
# - keressetek más outliereket









# ============================================================
# hajrá csapat 2: Asia, GDP per capita change
# ============================================================
# - csak ázsia
# - GDP per capita change (nem life expectancy)
# - legyen a theme_clean()













