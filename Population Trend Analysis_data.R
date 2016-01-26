library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(robR)


#### All County Total Population Data ####
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

###### Non-Metro Total Population Data ####

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

#### Population by Age Data ####

pop2534=codemog::county_forecast%>%
  filter(year<=2014, age>=25, age<=34)%>%
  group_by(countyfips, county, year)%>%
  summarize(pop2534=sum(totalPopulation))%>%
  ungroup()%>%
  mutate(year=paste0("a", year))%>%
  spread(year, pop2534)%>%
  mutate(ann_gr_90_99_2534=ann.gr(a1990, a1999, 9),
         ann_gr_00_09_2534=ann.gr(a2000, a2009, 9),
         ann_gr_10_14_2534=ann.gr(a2010, a2014, 4))%>%
  rename(FIPS=countyfips)%>%
  select(-county)

pop2544=codemog::county_forecast%>%
  filter(year<=2014, age>=25, age<=44)%>%
  group_by(countyfips, county, year)%>%
  summarize(pop2544=sum(totalPopulation))%>%
  ungroup()%>%
  mutate(year=paste0("a", year))%>%
  spread(year, pop2544)%>%
  mutate(ann_gr_90_99_2544=ann.gr(a1990, a1999, 9),
         ann_gr_00_09_2544=ann.gr(a2000, a2009, 9),
         ann_gr_10_14_2544=ann.gr(a2010, a2014, 4))%>%
  rename(FIPS=countyfips)%>%
  select(-county)

#### Goods and Services Ratio - Chris ####

goods_service_ratio=read_excel("County Compare Data.xlsx", sheet= "Goods_ Service Ratio")[,-1]%>%
  rename(FIPS=FIPS_NUM)%>%
  filter(FIPS!=0)%>%
  gather(year, goods_service, -FIPS)%>%
  mutate(year=paste0("a", year))%>%
  spread(year, goods_service)%>%
  mutate(ann_gr_90_99_gsr=ann.gr(a1990, a1999, 9),
         ann_gr_00_09_gsr=ann.gr(a2000, a2009, 9),
         ann_gr_10_14_gsr=ann.gr(a2010, a2014, 4))


#### Employment to Population Ratio - Chris ####
emp_pop_ratio=read_excel("County Compare Data.xlsx", sheet= "Emp_Pop Ratio")[,-1]%>% # Added the first two column names by hand
  rename(FIPS=FIPS_Num)%>%
  filter(FIPS!=0)%>%
  gather(year, emp_pop, -FIPS)%>%
    mutate(year=paste0("a", year))%>%
    spread(year, emp_pop)%>%
    mutate(ann_gr_90_99_epr=ann.gr(a1990, a1999, 9),
           ann_gr_00_09_epr=ann.gr(a2000, a2009, 9),
           ann_gr_10_14_epr=ann.gr(a2010, a2014, 4))
  
  
#### Cluster Base Data ####

all64=countynames%>%
    inner_join(select(pop_anngr, FIPS,ann_gr_90_99:ann_gr_10_14))%>%
    inner_join(select(pop2534, FIPS,ann_gr_90_99_2534:ann_gr_10_14_2534))%>%
    inner_join(select(pop2544, FIPS,ann_gr_90_99_2544:ann_gr_10_14_2544))%>%
    inner_join(select(goods_service_ratio, FIPS,ann_gr_90_99_gsr:ann_gr_10_14_gsr))%>%
    inner_join(select(emp_pop_ratio, FIPS,ann_gr_90_99_epr:ann_gr_10_14_epr))



