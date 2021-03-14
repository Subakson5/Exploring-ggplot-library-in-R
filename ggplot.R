library(ggplot2)
head(iris)
ggplot(data = iris)
p=ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length))
summary(p)

#violin plot
p+
  geom_violin()

p+
  geom_violin(fill="red")

p+
  geom_violin(aes(fill=Species))

png("vp.png")
p+
  geom_violin(aes(fill=Species), alpha=0.25)
dev.off()

head(mpg)
?mpg

ggplot(data = mpg, mapping = aes(x=trans, y=hwy))+
  geom_violin()

ggplot(data = mpg, mapping = aes(x=trans, y=hwy, fill=as.factor(cyl)))+
  geom_violin()

ggplot(data = diamonds, mapping = aes(x=cut, y=carat))+
  geom_violin()

ggplot(data = diamonds, mapping = aes(x=cut, y=carat), fill=cut)+
  geom_violin()

p+
  geom_density()

p+
  geom_density(fill="red")

p+
  geom_density(aes(fill=Species))

ggplot(data = diamonds, mapping = aes(x=carat))+
  geom_density()

ggplot(data = diamonds, mapping = aes(x=carat))+
  geom_density( fill="red")

ggplot(data = iris, mapping = aes(Sepal.Length))+
  geom_histogram()

p=ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length))+
  geom_boxplot()
summary(p)
ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length))+
  geom_boxplot(fill="red")
p=ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length))+
  geom_boxplot(aes(fill=Species))
png("bp.png")
p
dev.off()

ggplot(data = diamonds, mapping = aes(x=cut, y=carat, fill=cut))+
  geom_boxplot(alpha=0.75)+
  facet_wrap(cut~.)

ggplot(data = diamonds, mapping = aes(x=cut, y=carat, fill=cut))+
  geom_boxplot(alpha=0.75, outlier.color = "green")+
  geom_jitter(alpha=0.005, color="red")+
  facet_wrap(cut~.)+
  coord_flip()

ggplot(data = diamonds, mapping = aes(x=cut, y=carat, fill=cut))+
  geom_boxplot(alpha=0.75, outlier.color = "green")+
  geom_jitter(alpha=0.005, color="red")+
  facet_wrap(cut~.)+
  coord_flip()+
  theme_bw()

library(ggridges)
p=ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length))+
geom_density_ridges(fill="red")
p

p=ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Species))+
  geom_density_ridges(aes(fill=Species), alpha=0.25)
p

#scatter plot
ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length))+
  geom_point(color="red")
ggplot(data = iris, mapping = aes(y=Sepal.Length, x=Petal.Length))+
  geom_density_ridges(aes(color=Species))

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Sepal.Length))+
  geom_density_ridges(aes(color=Species), size=Petal.Width)

ggplot(data = iris, mapping = aes(x=Petal.Length, y=Sepal.Length, shape=Species))+
  geom_point()+
  geom_smooth(method = "lm", se=0)+
  theme_classic()

head(iris)
data=as.matrix(iris[-5])
heatmap(data)

heatmap(data, scale = "column", Colv = NA) #column dendogram are absent
heatmap(data, scale = "column", Rowv = NA) #row dendogram are absent
heatmap(data, scale = "column", Colv = NA, Rowv= NA) #both are absent

heatmap(data, scale = "column", Colv = NA, Rowv= NA, cexCol = 0.75)

heatmap(data, scale = "column", col=terrain.colors(256))
heatmap(data, scale = "column", col=cm.colors(256))

library(RColorBrewer)
custcol<- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, scale= "column", col=custcol)

#correlogram
plot(iris, col="blue")
#install.packages("corrgram")
library(corrgram)
corrgram(iris)

#ellipse with regression line
corrgram(iris, panel=panel.ellipse)
corrgram(iris, lower.panel=panel.shade, upper.panel = NULL)
corrgram(iris.order=TRUE)

library(GGally)
ggpairs(iris)
ggpairs(iris, ggplot2::aes(color=Species))
ggcorr(iris)
ggcorr(mtcars)
ggcorr(diamonds)
ggcorr(mpg)

library(ellipse)
corr_iris= cor(iris[1:4])
plotcorr(corr_iris)

library(car)
scatterplotMatrix(~Sepal.Width+Sepal.Length+Petal.Length+Petal.Width|Species, data=iris)

#bubble plot
p=ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, size=Petal.Width, color=Species))+
  geom_point()+
  geom_smooth(method="lm")
p

#connected scatter plot
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species))+
  geom_point(size=5)+
  geom_line()+
  theme_bw()

#density 2D plot
ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_point()
ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_bin2d()
ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_bin2d(bins=70)

ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_bin2d(bins=70)+
  scale_fill_continuous(type="viridis")+
  theme_classic()

library(hexbin)
ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_hex()

ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_hex(bins=70)+
  scale_fill_continuous(type="viridis")+
  theme_bw()

ggplot(data=diamonds, aes(x=depth, y=price))+
  geom_density2d()

ggplot(data=diamonds, aes(x=depth, y=price))+
  stat_density2d(aes(fill=..density..), geom = "raster", contour = FALSE)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

ggplot(data=diamonds, aes(x=depth, y=price))+
  stat_density2d(aes(fill=..density..), geom = "raster", contour = FALSE)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_distiller(palette = 12, direction=-1)
