# Premiers résultats ---------------------------------------------------------------

# Libraries ------------------------------------------------------------------------
library(dplyr)
library(cluster)
library(tidyr)
library(factoextra)
library(ade4)
library(FactoMineR)

# Donnees --------------------------------------------------------------------------
data <- read.csv("pokedex.csv",header=TRUE,row.names=1, sep=",",encoding="UTF-8", stringsAsFactors = TRUE)
summary(data)
data <- data  %>% 
  distinct(pokedex_number,.keep_all = TRUE) %>% 
  drop_na() 

# data type 1 to binaries
data.quali.type1 <- data %>% 
  select(type_1) %>% 
  droplevels()

data.disj.type1 <- acm.disjonctif(data.quali.type1)
names(data.disj.type1) <- sub("type_1.", "", names(data.disj.type1))
data.disj.type1$pokedex_number <- data$pokedex_number

# data type 2 to binaries
data.quali.type2 <-data %>% 
  select(type_2) %>%
  droplevels()

data.disj.type2 <- acm.disjonctif(data.quali.type2) 
names(data.disj.type2) <- sub("type_2.", "", names(data.disj.type2)) 
data.disj.type2$pokedex_number <- data$pokedex_number
data.disj.type2 <- data.disj.type2 %>% select(-1)
# data merge type 1 and type 2
data.disj_type = bind_rows(data.disj.type1  , 
                         data.disj.type2 ) %>% 
  group_by(pokedex_number) %>%
  summarise_all(sum) %>% select(-19)

# disjonctive table with all other qualitative variables
data.quali_wo_type = data %>% 
  select(status, growth_rate, egg_type_1) %>% 
  droplevels()

data.disj_wo_type <- acm.disjonctif(data.quali_wo_type) 
data.disj_wo_type$pokedex_number <- data$pokedex_number

data.disj <- merge(data.disj_wo_type, data.disj_type, by = "pokedex_number") %>% select(-1)
data.disj <- as.matrix(data.disj)

# classic distance

njl = apply(data.disj,2,sum)
distk2 = sapply(1:nrow(data.disj),FUN=function(j){
  apply(sapply(1:nrow(data.disj),FUN=function(i) {
    abs(data.disj[j,]-data.disj[i,])},simplify = TRUE)/njl,2,sum)
})
distk2 = as.dist(distk2*nrow(data.disj)/ncol(data.disj))

data.ward = hclust(distk2,method="ward.D")

plot(data.ward,hang=-1)
plot(rev(data.ward$height)[1:15],type="b")

# difficile de choisir un k , k=11?
K=11
gpe.ward = cutree(data.ward,k=K)
plot(data.ward,hang=-1)
rect.hclust(data.ward, K, border ="blue")


View(data)


# quali bio part ------------------------------------------------------------------

# disjonctive table with all other qualitative variables
data.quali_wo_type = data %>% 
  select(growth_rate) %>% 
  droplevels()

data.disj_wo_type <- acm.disjonctif(data.quali_wo_type) 
data.disj_wo_type$pokedex_number <- data$pokedex_number

data.disj <- merge(data.disj_wo_type, data.disj_type, by = "pokedex_number") %>% select(-1)
data.disj <- as.matrix(data.disj)

njl = apply(data.disj,2,sum)
distk2 = sapply(1:nrow(data.disj),FUN=function(j){
  apply(sapply(1:nrow(data.disj),FUN=function(i) {
    abs(data.disj[j,]-data.disj[i,])},simplify = TRUE)/njl,2,sum)
})
distk2 = as.dist(distk2*nrow(data.disj)/ncol(data.disj))


#### quanti bio
don_bio <- data %>% 
  select(height_m, weight_kg, growth_rate, percentage_male, egg_cycles) %>% 
  drop_na()

don_bio_quanti <- don_bio %>% select(-growth_rate)

don_bio_quanti.cr <- scale(don_bio_quanti,center=T,scale=T)

d <- dist(don_bio_quanti.cr)

d_final = d+distk2
cah.ward = hclust(d_final,method="ward.D")
plot(cah.ward,hang=-1)

plot(rev(cah.ward$height)[1:20],type="b")

data.disj$id <- data$pokedex_number
quanti$id <- data$pokedex_number
data_final <- merge(data.disj, quanti)
data_final <-data_final %>%  select(-1)

gpe.ward2 = cutree(cah.ward,k=5)
gpe.ward1 = cutree(cah.ward,k=3)
data_final$groupe = as.factor(gpe.ward1)
des = catdes(data_final,num.var=28)
plot(des,barplot=TRUE)
NbClust(don_base,min.nc = 2,max.nc = 15,method="ward.D",index="all")