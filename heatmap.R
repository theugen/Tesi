library(ggplot2)
data(iris)
risultati <- read.csv('~/Tesi/results.txt', sep='', stringsAsFactors = F, header = F)
colnames(risultati) <- c('x', 'y', 'ux', 'uy','uz')
risultati$ux <- risultati$ux*(10^10)

g1 <- ggplot(data=risultati, aes(x, y, fill=ux)) + geom_tile()
g1<- g1 + labs(title='x_displacement')
g1


g2 <- ggplot(data=risultati, aes(x, y, fill=uy)) + geom_tile()
g2<- g2 + labs(title='y_displacement')
g2

g3 <- ggplot(data=risultati, aes(x, y, fill=uz)) + geom_tile()
g3<- g3 + labs(title='z_displacement')
g3
