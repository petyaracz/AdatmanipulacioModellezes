set.seed(1337)

library(tidyverse)
library(glue)
library(ggthemes)

generate_correlated_base = function(n, rho, noise_sd = 1) {
  x = rnorm(n)
  
  # Signal component (correlated with x)
  signal = rho * x
  
  # Noise component (independent of x)
  noise = sqrt(1 - rho^2) * rnorm(n, sd = noise_sd)
  
  y = signal + noise
  
  return(data.frame(x = x, y = y))
}

d = generate_correlated_base(n = 1000, rho = 0.7, noise_sd = 20)

summary(lm(y ~ x, data = d))

my_d = d |> 
  mutate(
    x = scales::rescale(x),
    y = scales::rescale(y),
    # x = 1-x,
    s_id = sample(20, 1000, replace = T)
  )

p1 = my_d |> 
  ggplot(aes(x,y)) +
  geom_point(alpha = .25) +
  theme_void()

built = ggplot_build(p1)
x_range = built$layout$panel_params[[1]]$x.range
y_range = built$layout$panel_params[[1]]$y.range

p2 = my_d |> 
  ggplot(aes(x,y)) +
  geom_point(alpha = .25) +
  theme_void() +
  geom_smooth(method = 'lm', se = F)

# Define 20 colours
my_palette = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', 
               '#ffff33', '#a65628', '#f781bf', '#999999', '#66c2a5',
               '#fc8d62', '#8da0cb', '#e78ac3', '#a6d854', '#ffd92f',
               '#e5c494', '#b3b3b3', '#8dd3c7', '#ffffb3', '#bebada')

# Create directory if it doesn't exist
dir.create("~/Downloads/konfint", showWarnings = FALSE)

# Save first two plots
ggsave("~/Downloads/konfint/p1.pdf",  bg = "white", p1, width = 4, height = 4)
ggsave("~/Downloads/konfint/p2.pdf",  bg = "white", p2, width = 4, height = 4)

# Counter for filenames
plot_num = 3

# First loop - highlight each subject
for(i in 1:20) {
  p = my_d |> 
    mutate(my_colour = s_id == i) |> 
    ggplot(aes(x, y, colour = my_colour)) +
    geom_point(alpha = .5) +
    guides(colour = 'none') +
    theme_void() +
    scale_colour_manual(values = c('lightgrey', my_palette[i]))
  
  ggsave(plot = p, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
  plot_num = plot_num + 1
}

# Second loop - individual subject regressions
for(i in 1:20) {
  p = my_d |> 
    filter(s_id == i) |> 
    ggplot(aes(x, y)) +
    geom_point(colour = my_palette[i], alpha = .5) +
    geom_smooth(method = 'lm', se = F, colour = my_palette[i]) +
    theme_void() +
    coord_cartesian(xlim = x_range, ylim = y_range)
  
  ggsave(plot = p, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
  plot_num = plot_num + 1
}

p3 = my_d |> 
  ggplot(aes(x, y, colour = as.factor(s_id))) +
  geom_point(alpha = .5) +
  geom_smooth(method = 'lm', se = F) +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range)

ggsave(plot = p3, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p4 = my_d |> 
  ggplot(aes(x, y, colour = as.factor(s_id))) +
  geom_smooth(method = 'lm', se = F) +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range)

ggsave(plot = p4, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p5 = my_d |> 
  filter(s_id == 2) |> 
  ggplot(aes(x, y)) +
  geom_smooth(method = 'lm') +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range)

ggsave(plot = p5, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p6 = my_d |> 
  filter(s_id == 2) |> 
  ggplot(aes(x, y)) +
  geom_smooth(method = 'lm', level = .999) +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range)

ggsave(plot = p6, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p7 = my_d |> 
  filter(s_id == 2) |> 
  ggplot(aes(x, y)) +
  geom_smooth(method = 'lm', level = .5) +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range)

ggsave(plot = p7, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p8 = my_d |> 
  filter(s_id == 2) |> 
  ggplot(aes(x, y)) +
  geom_smooth(method = 'lm', level = .5) +
  geom_smooth(method = 'lm', level = .95) +
  geom_smooth(method = 'lm', level = .999) +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range)

ggsave(plot = p8, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p9 = p5 +
  geom_smooth(data = my_d, aes(x, y), se = FALSE, color = "red")

ggsave(plot = p9, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
plot_num = plot_num + 1

p10 = my_d |> 
  filter(s_id == 6) |> 
  ggplot(aes(x, y)) +
  geom_smooth(method = 'lm') +
  theme_void() +
  guides(colour = 'none') +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  geom_smooth(data = my_d, aes(x, y), se = FALSE, color = "red")

ggsave(plot = p10, filename = glue('~/Downloads/konfint/p{plot_num}.pdf'),  bg = "white", width = 4, height = 4)
