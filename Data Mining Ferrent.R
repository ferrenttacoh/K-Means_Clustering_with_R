library(ggplot2)
library(plyr)
library(ROCR)

#Data Preparation
vaksin <- read.csv("C:/DataMining/vaksin.csv",sep=",", header=TRUE)
colnames(vaksin) <- c('Country', 'Doses.administered.per.100.people', 'Total.doses.administered', 'percentage.of.population.vaccinated', 
                     'percentage.of.population.fully.vaccinated')

#Menghapus kolom yang tidak ingin diolah
vaksin$Country <- NULL
View(vaksin)


#Melihat struktur vaksin
str(vaksin)
#Rangkuman statistik vaksin
summary(vaksin)

#Install Package
install.packages("ggplot2")
install.packages("factoextra")
library(factoextra)
library(ggplot2)


vaksin

head(vaksin)

vaksin1 <- na.omit(vaksin)
summary(vaksin1)

vaksinfix <- scale(vaksin1) #standarisasi data

#Mencari K Optimal Klaster
fviz_nbclust(vaksinfix, kmeans, method = "wss") # metode elbow

fviz_nbclust(vaksinfix, kmeans, method = "silhouette") # metode silhouette

set.seed(123)
gap_stat <- clusGap(vaksinfix, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50) # metode gap statistic
fviz_gap_stat(gap_stat)

#eksekusi k-means
final <- kmeans(vaksinfix, 4, nstart = 25)
print(final)

fviz_cluster(final, data = vaksinfix)

