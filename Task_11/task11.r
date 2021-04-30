setwd('~/Desktop/Evolution/tasks/Task_11')
x<- rnorm(100, mean=5, sd=2)
x
y<-(x*5)+2+(rnorm(100, 0:0.1))
y
plot(x, y)
abline(lm(y~x), col='red')
coef(lm(y~x))
z<- c()
x<- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	z[i]<- runif(1)
	y<- (x*z[i])+2+(rnorm(100,0:0.1))
	l<- coef(lm(z[1:100]~y))
}
l
plot(z[1:100], y)
abline(lm(y~z[1:100]))
plot(c(z, -0.029))
install.packages('meme')
library('meme')
B<- 'https://www.google.com/imgres?imgurl=https%3A%2F%2Fwww.sunset.com%2Fwp-content%2Fuploads%2FPLANT-MEME-BEAN-0120-1280x720.jpg&imgrefurl=https%3A%2F%2Fwww.sunset.com%2Fhome-garden%2Fplants%2Fplant-memes-instagram&tbnid=wJHde2f0jx70AM&vet=12ahUKEwjztYqflqXwAhVBbK0KHSzRCv4QMygKegUIARCkAQ..i&docid=Ed-N89qFDZdKPM&w=1280&h=720&q=memes&ved=2ahUKEwjztYqflqXwAhVBbK0KHSzRCv4QMygKegUIARCkAQ'
Blake_meme<- meme(B, lower= "One does not simply grow plants if one has not botany", color='black', size='1.5')
