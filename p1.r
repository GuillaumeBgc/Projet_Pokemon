#Projet

#Packages
library(dplyr)
library(cluster)
library(tidyr)
library(factoextra)
library(NbClust)
library(ggplot2)

#Import des données
don <- read.csv("pokedex.csv",header=TRUE, sep=",",encoding="UTF-8")

#Enleve la premiere colonne qui correspond aux numéros de lignes
don <- don[-1]

#Enlève les doublons sur des méta pokémon pour avoir un identifiant unique pour chaque pokémon
don <- don %>% distinct(pokedex_number,.keep_all = TRUE)


#Choix des données
don_base <- don %>% select(hp, attack, defense, sp_attack, sp_defense, 
                           speed, catch_rate, base_friendship, base_experience, 
                           height_m, weight_kg)

don_base2 <- don %>% select(status, hp, attack, defense, sp_attack, sp_defense, 
                           speed, catch_rate, base_friendship, base_experience, 
                           height_m, weight_kg)
#don_base <- don %>% select(hp, attack, defense, sp_attack, sp_defense, speed)

#don_base <- don %>% select(starts_with("against"))



#Cherche les NA
which(is.na(don_base))
#on supprime les NA
don_base <- don_base %>% drop_na()

don_base2 <- don_base2 %>% drop_na()


# CAH sur les caractéristiques quantitatives sans les faiblesses.
d = dist(don_base)
cah.ward = hclust(d,method="ward.D")
#Dendrogramme
plot(cah.ward,hang=-1)
#Courbe de perte d'inertie
plot(rev(cah.ward$height)[1:20],type="b")
#Je prendrai 3 classes
k=3

#Classe les individus en 3 classes
groupes.cah = cutree(cah.ward,k=k)

#Classes avec le dendrogramme
plot(cah.ward,hang=-1)
rect.hclust(cah.ward, k, border ="blue")

#Réalisation d'une acp
acp <- princomp(don_base.cr,cor=T,scores=T)

#positionnement des groupes dans le plan factoriel 
plot(acp$scores[,1],acp$scores[,2],type="p")

text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black")[groupes.cah],cex=0.65)





#Methode 2 avec centrage réduction (mieux je pense pour le premier don_base car pas les mêmes échelles)
don_base.cr <- scale(don_base,center=T,scale=T)

d <- dist(don_base.cr)

cah.ward = hclust(d,method="ward.D")
don_base <- na.omit(don_base)
NbClust(don_base,min.nc = 2,max.nc = 15,method="ward.D",index="all")
plot(cah.ward,hang=-1)

#Courbe de perte d'inertie
plot(rev(cah.ward$height)[1:20],type="b")
#Je prendrai 2 ou 4 classes
k=2

plot(cah.ward,hang=-1)
rect.hclust(cah.ward, k, border ="blue")
#4 classes me parait bien

#découpage en 4 groupes
groupes.cah <- cutree(cah.ward,k=k)
#liste des groupes
print(sort(groupes.cah))


#Réalisation d'une acp
acp <- princomp(don_base.cr,cor=T,scores=T)

#positionnement des groupes dans le plan factoriel 
ggplot(df_score)+aes(x=Comp.1,y=Comp.2, color=status)+geom_point()
df_score=data.frame(acp$scores)
df_score$status=don_base2$status
text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black")[groupes.cah],cex=1)

#Ce n'est pas très très clair, on a l'impression d'avoir un groupe poubelle en noir à droite

plot(acp$scores[,1],acp$scores[,2],type="p")

text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black")[groupes.cah], 
     labels=don$status,cex=1)
#On remarque que le groupe qui est beaucoup plus vaste correspond à la plupart des pokémons légendaires




#K-means

#Inertie intra
I.intra = sapply(1:20,FUN=function(k) kmeans(don_base.cr,centers=k,nstart=50)$tot.withinss)

plot(I.intra,type="b",xlab="nb groupes",ylab="inertie intra")
#Difficile mais j'aurai pris 3 groupes
k=3

groupes.kmeans = kmeans(don_base.cr,k)
#clusplot(don_base,don_base.kmeans$cluster,labels=4,col.p=as.numeric(iris$Species))

#correspondance avec les groupes de la CAH
print(table(groupes.cah,groupes.kmeans$cluster))

#Visualisation
fviz_cluster(groupes.kmeans, don_base.cr, ellipse.type = "norm") 
#Le groupe à droite paraît être celui des légendaires



