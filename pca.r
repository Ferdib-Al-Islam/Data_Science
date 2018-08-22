# Generating artificial data with two correlated variables

set.seed(12345)
x1<-rnorm(n=1000,mean=0,sd=1)
x2<-x1+rnorm(1000,mean=0,sd=1)

# Examining the correlation
cor(x1,x2)

dat<-data.frame(x1,x2)

#dat<-data.frame(x1,x2,pred)

head(dat)

library(ggplot2)

#pdf("original_data.pdf")
pcplot <- ggplot(dat)
pcplot <- pcplot+geom_point(aes(x=x1,y=x2),col="black",size=1)+stat_ellipse(aes(x=x1,y=x2),size=1)+theme_bw()
pcplot <- pcplot+geom_text(x=-1,y=-5,label="Pearson Correlation Coefficient (r) = 0.72",hjust=0)
pcplot
#dev.off()

# Performing PCA
p<-prcomp(cbind(x1,x2))
pred<-predict(p)

summary(p)

p$rotation

dat<-data.frame(x1,x2,pred)

head(dat)

#pdf("pac_data.pdf")
pcplot <- ggplot(dat)
pcplot <- pcplot+geom_point(aes(x=PC1,y=PC2),col="red",size=1)+stat_ellipse(aes(x=PC1,y=PC2),size=1,col="red")+theme_bw()
pcplot <- pcplot+geom_text(x=-3,y=2,label="Pearson Correlation Coefficient (r)  <0.0001",hjust=0)
pcplot 
#dev.off()

#pdf("original_and_pca.pdf")
pcplot <- ggplot(dat)
pcplot <- pcplot+geom_point(aes(x=x1,y=x2),col="black",size=1)+stat_ellipse(aes(x=x1,y=x2),size=1)
pcplot <- pcplot+geom_point(aes(x=PC1,y=PC2),col="red",size=1)+stat_ellipse(aes(x=PC1,y=PC2),size=1,col="red")
pcplot <- pcplot+geom_segment(x=-4,xend=4,y=0,yend=0,col="red",size=1)
pcplot <- pcplot+geom_segment(x=0,xend=0,y=-4,yend=4,col="red",size=1)
pcplot <- pcplot+geom_text(x=4.5,y=0,label="PC1",col="red")+geom_text(x=0,y=-4.5,label="PC2",col="red")+theme_bw()
pcplot
#dev.off()

# Performing PCA on Image to reduce dimension and then revoer the image from pc

# update the location of the image in your computer otherwise it will not work

setwd("/data")
library(jpeg)
img1 <- readJPEG('Image1.jpg') # The file name of the image is "image1.jpg"

# Number of rows and columms
nrow(img1)
ncol(img1)

# Dimension of img1
dim(img1)

# Extracting each color as a separate matrix to perform PCA
r <- img1[,,1]
g <- img1[,,2]
b <- img1[,,3]

img1.r.pca <- prcomp(r, center = FALSE)
img1.g.pca <- prcomp(g, center = FALSE)
img1.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(img1.r.pca, img1.g.pca, img1.b.pca)


redChannel <- img1.r.pca$x[,1:100] %*% t(img1.r.pca$rotation[,1:100])
writeJPEG(redChannel, "redChannel.jpg")

for (i in c(5, 10, 15, 25, 50, 100, 250, 500)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])}, 
    simplify = 'array')
  writeJPEG(pca.img, paste('First_', paste(i), '_components.jpg', sep = ''))
}
