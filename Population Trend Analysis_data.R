library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(robR)

pop=read_excel("County Compare Data.xlsx", sheet= "Population 90_14")[,-1]
names(pop[,27])="ann_gr_90_14"
countynames=codemog::county_est%>%
  filter(year==2014)%>%
  select(FIPS=countyfips, county)

pop9099=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==1990 | year==1999)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_90_99=ann.gr(lag(as.numeric(population)), as.numeric(population), 9))%>%
  filter(year==1999)%>%
  select(FIPS, ann_gr_90_99)

pop0006=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2006)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_06=ann.gr(lag(as.numeric(population)), as.numeric(population), 6))%>%
  filter(year==2006)%>%
  select(FIPS, ann_gr_00_06)

pop0009=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2009)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_09=ann.gr(lag(as.numeric(population)), as.numeric(population), 9))%>%
  filter(year==2009)%>%
  select(FIPS, ann_gr_00_09)

pop0710=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2007 | year==2010)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_07_10=ann.gr(lag(as.numeric(population)), as.numeric(population), 3))%>%
  filter(year==2010)%>%
  select(FIPS, ann_gr_07_10)

pop1114=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2011 | year==2014)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_11_14=ann.gr(lag(as.numeric(population)), as.numeric(population), 3))%>%
  filter(year==2014)%>%
  select(FIPS, ann_gr_11_14)

pop1014=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2010 | year==2014)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_10_14=ann.gr(lag(as.numeric(population)), as.numeric(population), 4))%>%
  filter(year==2014)%>%
  select(FIPS, ann_gr_10_14)

pop_anngr=pop%>%
  inner_join(pop9099)%>%
  inner_join(pop0009)%>%
  inner_join(pop1014)%>%
  inner_join(pop0006)%>%
  inner_join(pop0710)%>%
  inner_join(pop1114)

state=filter(pop, FIPS==0)
state_anngr=filter(pop_anngr, FIPS==0)

## Non-Metro Data

"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
metro_fips=c(0,1,5,13,14,19,31,35,39,41,47,59,69,77,93,119,123)
non_metro=filter(pop, FIPS %!in% metro_fips)


non_metro9099=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==1990 | year==1999)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_90_99=ann.gr(lag(as.numeric(population)), as.numeric(population), 9))%>%
  filter(year==1999)%>%
  select(FIPS, ann_gr_90_99)

non_metro0006=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2006)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_06=ann.gr(lag(as.numeric(population)), as.numeric(population), 6))%>%
  filter(year==2006)%>%
  select(FIPS, ann_gr_00_06)

non_metro0009=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2009)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_09=ann.gr(lag(as.numeric(population)), as.numeric(population), 9))%>%
  filter(year==2009)%>%
  select(FIPS, ann_gr_00_09)

non_metro0710=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2007 | year==2010)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_07_10=ann.gr(lag(as.numeric(population)), as.numeric(population), 3))%>%
  filter(year==2010)%>%
  select(FIPS, ann_gr_07_10)

non_metro1114=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2011 | year==2014)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_11_14=ann.gr(lag(as.numeric(population)), as.numeric(population), 3))%>%
  filter(year==2014)%>%
  select(FIPS, ann_gr_11_14)

non_metro1014=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2010 | year==2014)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_10_14=ann.gr(lag(as.numeric(population)), as.numeric(population), 4))%>%
  filter(year==2014)%>%
  select(FIPS, ann_gr_10_14)

non_metro_anngr=non_metro%>%
  inner_join(non_metro9099)%>%
  inner_join(non_metro0009)%>%
  inner_join(non_metro1014)%>%
  inner_join(non_metro0006)%>%
  inner_join(non_metro0710)%>%
  inner_join(non_metro1114)
