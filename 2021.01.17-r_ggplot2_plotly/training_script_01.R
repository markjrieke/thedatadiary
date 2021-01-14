# install packages ----
install.packages("ggplot2")
library(ggplot2)

# iris dataset ----
data(iris)
head(iris)
tail(iris)
summary(iris)

# exploring data with plot ----
plot(iris)
plot(iris$Petal.Length, iris$Sepal.Width)

# exploring data with ggplot ---- 
ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) + 
  geom_point()

?geom_point

ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()

ggplot(iris, aes(x = Petal.Length, 
                 y = Sepal.Width, 
                 color = Species,
                 size = Petal.Width)) + 
  geom_point()

ggplot(iris, aes(x = Petal.Length, 
                 y = Sepal.Width, 
                 color = Species,
                 size = Petal.Width,
                 shape = Species)) + 
  geom_point()

ggplot(iris, aes(x = Petal.Length, 
                 y = Sepal.Width, 
                 color = Species,
                 size = Petal.Width,
                 shape = Species,
                 alpha = Sepal.Length)) + 
  geom_point()

# bar & box plot geoms
ggplot(iris, aes(Species)) + 
  geom_bar()

ggplot(iris, aes(Sepal.Width)) +
  geom_bar()

ggplot(iris, aes(Species, Sepal.Length)) +
  geom_bar(stat = "summary")

ggplot(iris, aes(Species, Sepal.Length,
                 color = Species)) +
  geom_bar(stat = "summary")

ggplot(iris, aes(Species, Sepal.Length,
                 fill = Species)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue")

ggplot(iris, aes(Species, Sepal.Length,
                 fill = Species)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue") + 
  geom_point()

ggplot(iris, aes(Species, Sepal.Length,
                 fill = Species)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue") + 
  geom_point(position = position_jitter(0.2), size = 3, shape = 21)

myplot <- ggplot(iris, aes(Species, Sepal.Length,
                           fill = Species)) +
            geom_bar(stat = "summary", fun.y = "mean", fill = "blue") + 
            geom_point(position = position_jitter(0.2), size = 3, shape = 21)

myplot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = "white"),
               axis.line.y = element_line(color = "black", size = 0.2),
               axis.line.x = element_line(color = "black", size = 0.2))

myplot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(color = "black", fill = NA, size = 0.2))

?theme

myplot + theme_bw()
myplot + theme_classic()
myplot + theme_linedraw()
myplot + theme_light()
myplot 