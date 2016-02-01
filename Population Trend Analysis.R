library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(robR)


source("Population Trend Analysis_data.r")

## To Do
# K nearest neighbor cluster on overall, 90s, set seed, etc.  Chart growth

## Ranking Plots

p1=pop_anngr%>%
  select(FIPS, ann_gr_90_00)%>%
  filter(FIPS!=14)%>%
  inner_join(countynames)%>%
  ggplot(aes(x=reorder(county,ann_gr_90_00), y=ann_gr_90_00, group=county))+
  geom_point(color=rgb(31,74,126,max=255))+
  geom_hline(yintercept=2.778303, color="red", size=1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_text(size=rel(.8)))+
  labs(x="County", y="Compound Annual Growth Rate, 1990 to 1999")
p1

ggsave("cagr_9099.png", p1, h=150, w=250, unit="mm")

p2=pop_anngr%>%
  select(FIPS, ann_gr_00_10)%>%
  filter(FIPS!=14)%>%
  inner_join(countynames)%>%
  ggplot(aes(x=reorder(county,ann_gr_00_10), y=ann_gr_00_10, group=county))+
  geom_point(color=rgb(31,74,126,max=255))+
  geom_hline(yintercept=1.53612, color="red", size=1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_text(size=rel(.8)))+
  labs(x="County", y="Compound Annual Growth Rate, 2000 to 2009")
p2

ggsave("cagr_0009.png", p2, h=150, w=250, unit="mm")


p3=pop_anngr%>%
  select(FIPS, ann_gr_10_14)%>%
  filter(FIPS!=14)%>%
  inner_join(countynames)%>%
  ggplot(aes(x=reorder(county,ann_gr_10_14), y=ann_gr_10_14, group=county))+
  geom_point(color=rgb(31,74,126,max=255))+
  geom_hline(yintercept=1.468165, color="red", size=1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_text(size=rel(.8)))+
  labs(x="County", y="Compound Annual Growth Rate, 2010 to 2014")
p3

ggsave("cagr_1014.png", p3, h=150, w=250, unit="mm")

p4=non_metro_anngr%>%
  select(FIPS, ann_gr_90_00)%>%
  filter(FIPS!=14)%>%
  inner_join(countynames)%>%
  ggplot(aes(x=reorder(county,ann_gr_90_00), y=ann_gr_90_00, group=county))+
  geom_point(color=rgb(31,74,126,max=255))+
  geom_hline(yintercept=2.881307495, color="red", size=1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_text(size=rel(.8)))+
  labs(x="County", y="Compound Annual Growth Rate, 1990 to 1999")
p4

ggsave("nonmetro_cagr_9099.png", p4, h=150, w=250, unit="mm")

p5=non_metro_anngr%>%
  select(FIPS, ann_gr_00_10)%>%
  filter(FIPS!=14)%>%
  inner_join(countynames)%>%
  ggplot(aes(x=reorder(county,ann_gr_00_10), y=ann_gr_00_10, group=county))+
  geom_point(color=rgb(31,74,126,max=255))+
  geom_hline(yintercept=0.833523432, color="red", size=1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_text(size=rel(.8)))+
  labs(x="County", y="Compound Annual Growth Rate, 2000 to 2009")
p5

ggsave("nonmetro_cagr_0009.png", p5, h=150, w=250, unit="mm")


p6=non_metro_anngr%>%
  select(FIPS, ann_gr_10_14)%>%
  filter(FIPS!=14)%>%
  inner_join(countynames)%>%
  ggplot(aes(x=reorder(county,ann_gr_10_14), y=ann_gr_10_14, group=county))+
  geom_point(color=rgb(31,74,126,max=255))+
  geom_hline(yintercept=0.055536551, color="red", size=1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_text(size=rel(.8)))+
  labs(x="County", y="Compound Annual Growth Rate, 2010 to 2014")
p6

ggsave("nonmetro_cagr_1014.png", p6, h=150, w=250, unit="mm")

### Clustering

## Hierarchical Clustering 

## Full Data
all64_clust=all64%>%
  select(FIPS, ann_gr_90_00, ann_gr_90_00_2534, ann_gr_90_00_2544, ann_gr_90_00_gsr, ann_gr_90_00_epr)


### Population Growth
#data prep


d=all64_clust%>%
  select(FIPS, ann_gr_90_00)%>%
  inner_join(countynames)%>%
  na.omit()%>%
  select(county, ann_gr_90_00)
row.names(d)=d$county

dnm=all64_clust%>%
  filter(FIPS %!in% metro_fips)%>%
  select(FIPS, ann_gr_90_00)%>%
  inner_join(countynames)%>%
  na.omit()%>%
  select(county, ann_gr_90_00)
row.names(dnm)=dnm$county
## clusters

# All
dd=dist(d,method="euclidian")

fit=hclust(dd, method="ward.D")
g_fit=cutree(fit,k=5)

plot(fit)

#Non Metro

dnmd=dist(dnm,method="euclidian")

fitnm=hclust(dnmd, method="ward.D")
g_fitnm=cutree(fitnm,k=5)


plot(fitnm)

#### Population and 25 to 34 ####

d2=all64_clust%>%
  select(FIPS, ann_gr_90_00, ann_gr_90_00_2544)%>%
  inner_join(countynames)%>%
  na.omit()%>%
  select(county, ann_gr_90_00, ann_gr_90_00_2544)
row.names(d2)=d2$county

dnm2=all64_clust%>%
  filter(FIPS %!in% metro_fips)%>%
  select(FIPS, ann_gr_90_00, ann_gr_90_00_2544)%>%
  inner_join(countynames)%>%
  na.omit()%>%
  select(county, ann_gr_90_00, ann_gr_90_00_2544)
row.names(dnm2)=dnm2$county

## clusters

# All
dd2=dist(d,method="euclidian")

fit2=hclust(dd, method="ward.D")
g_fit2=cutree(fit2,k=5)

plot(fit2)
#Non Metro

dnmd2=dist(dnm2,method="euclidian")

fitnm2=hclust(dnmd, method="ward.D")
g_fitnm2=cutree(fitnm2,k=5)


plot(fitnm2)

dnm2_c=dnm2%>%
  mutate(c_pop_age=g_fitnm2)%>%
  select(FIPS, c_pop_age)
