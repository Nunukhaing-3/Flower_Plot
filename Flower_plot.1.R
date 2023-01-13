#####################################
#       Drawing Flowers with R      #
#####################################

# Load the ggplot2 package
library(ggplot2)

# Set plot images to a nice size
options(repr.plot.width = 4, repr.plot.height = 4)

# Create circle data to plot
t <- seq(0, 2*pi, length.out = 50)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point()

# Define the number of points
points <- 500

# Define the Golden Angle
angle <- pi*(3 - sqrt(5))

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(color="red")

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(color="red") + 
  theme(panel.background = element_rect(fill="white"),
        panel.grid=element_blank(), axis.ticks=element_blank(),
        axis.title=element_blank(), axis.text=element_blank())

p + geom_point(size=8, alpha=0.5, color="orangered") + 
  theme(panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())

p +  geom_point(aes(size=t), alpha=0.5, shape=8) + 
  theme(legend.position="none",
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(aes(size=t), alpha=0.5, color="orangered",shape=8) + 
  theme(legend.position="none",
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())

p +  geom_point(aes(size=t), alpha=0.5, shape=17, color="yellow")+
  theme(legend.position="none",
        panel.background = element_rect(fill="darkmagenta"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())


p +  geom_point(aes(size=t), alpha=0.5, shape=17, color="salmon")+
  theme(legend.position="none",
        panel.background = element_rect(fill="transparent"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())

#############################################

angle <- 2.0
points <- 1000

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(aes(size=t), alpha=0.5, shape=17, color="yellow")+
  theme(legend.position="none",
        panel.background = element_rect(fill="darkmagenta"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank())

ggplot(df, aes(x * t, y * t)) + 
  geom_point(size=8,color="orangered") +
  coord_fixed() +
  theme_void()

###########################################################

#References:: https://roweyerboat.github.io/drawing_flowers_with_r_and_ggplot2














