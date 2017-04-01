
library(ggvis)
library(dplyr)

xs <- runif(n = 1000)

data.frame(x = xs) %>% ggvis(~x) %>% layer_densities()

data.frame(x = xs) %>% ggvis(~x) %>% layer_bars()
data.frame(x = cut(xs, breaks = 4)) %>% ggvis(~x) %>% layer_bars()

circs <- data.frame(x = c(1, 2, 3), y = c(1, 2, 2), rad = c(1, 8, 2))

circs %>% ggvis(~x, ~y) %>% layer_points()
circs %>% mutate(rad = 400*rad) %>% ggvis(~x, ~y, shape := "circle", size := ~rad) %>% layer_points()

cross <- data.frame(x = c(1, 2, 3, 3), y = c(1, 2, 2, 3))

cross %>%
  ggvis(~x, ~y) %>%
  layer_points(shape := "cross", size := 1000, fill := "red", stroke := "black", strokeWidth := 3)

cross$sw <- c(1, 3, 4, 2)
cross %>%
  ggvis(~x, ~y) %>%
  layer_points(shape := "cross", size := 1000, fill := "red", stroke := "black", strokeWidth := ~sw)

cross$op <- c(1, 1/3, 1/4, 30/100)
cross %>%
  ggvis(~x, ~y) %>%
  layer_points(shape := "cross", size := 1000, fill := "red",
               stroke := "black", strokeWidth := ~sw, fillOpacity := ~op)

tp <- data.frame(x = seq(1, 10), y = c(0, 0, 1, 1, 2, 2, 5.5, 5.5, 11, 12))
tp %>% ggvis(~x, ~y) %>% layer_bars()
tp %>% ggvis(~x) %>% layer_bars()

data("faithful")
faithful %>% ggvis(~waiting) %>% layer_bars()

data("cocaine")
cocaine %>% ggvis(~state) %>% layer_bars()
cocaine %>% ggvis(~month) %>% xxx()
?cocaine
cocaine %>% compute_count(~state)
View(layer_bars)


df <- structure(list(colour = structure(c(1L, 2L, 1L, 2L), .Label = c("Black",
                                                                      "White"), class = "factor"), variable = c("A", "A", "B", "B"),
                     value = c(1, 2, 0.74, 0.85)), row.names = c(NA, -4L), .Names = c("colour",
                                                                                      "variable", "value"), class = "data.frame")
df <- data.frame(colour = c("Black", "White", "Black", "White"),
                 variable = c("A", "A", "B", "B"),
                 value = c(1, 2, 0.74, 0.85))

df %>%
  ggvis(x=~variable, y=~value, fill=~colour) %>%
  group_by(colour) %>%
  layer_bars()

df %>%
  group_by(colour) %>%
  ggvis(x=~variable, y=~value, fill=~colour) %>%
  layer_bars()

df %>%
  ggvis(x=~variable, y=~value, fill=~colour) %>%
  layer_bars()

df %>%
  ggvis(x=~variable, y=~value) %>%
  group_by(colour) %>%
  layer_bars()

# generate some data to group_by with
xs <- data.frame(x = c(rnorm(n = 10), rnorm(n = 10, mean = 3)), class = factor(c(rep(1, 10), rep(2, 10))))
xs %>% ggvis(~x) %>% layer_densities(adjust = input_slider(0.1, 1))
xs %>% group_by(class) %>% ggvis(~x, fill = ~as.factor(class)) %>% layer_densities(adjust = input_slider(0.1, 1))

# different places to put group_by
xs %>%  ggvis(~x, fill = ~as.factor(class)) %>% group_by(class) %>% layer_densities()
xs %>% group_by(class) %>%  ggvis(~x, fill = ~class) %>% layer_densities()
xs %>% group_by(class) %>%  ggvis(~x, fill = ~as.character(class)) %>% layer_densities()
xs %>% group_by(class) %>% mutate(cc = as.factor(class)) %>% do({print(.); print(levels(.$class))})

mtcars %>% group_by(am) %>%
  ggvis(~mpg, ~hp, stroke = ~factor(am)) %>%
  layer_smooths() %>%
  layer_points(fill = ~factor(am))

mtcars %>% group_by(am) %>%
  ggvis(~mpg, ~hp, stroke = ~factor(am)) %>%
  layer_smooths()  %>%
  layer_points(fill = ~factor(am))

mtcars %>% group_by(am) %>%
  ggvis(~mpg, ~hp, stroke = ~factor(am)) %>%
  layer_smooths() %>%
  layer_points(fill = ~factor(am))

# mutate data after setting which to use, but before using
mtcars %>%
  ggvis(x = ~mpg, y = ~disp) %>%
  layer_points() %>%
  mutate(disp = disp / 2) %>%
  layer_points(fill := "red")

# key interaction
keys_s <- left_right(10, 1000, step = 50)
mtcars %>% ggvis(~wt, ~mpg, size := keys_s, opacity := 0.5) %>% layer_points()

# tooltip
mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_points() %>%
  add_tooltip(function(df) df$wt)
