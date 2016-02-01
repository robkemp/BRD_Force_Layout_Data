library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(robR)
library(codemog)


#### All County Total Population Data ####
pop=read_excel("County Compare Data.xlsx", sheet= "Population 90_14")[,-1]
names(pop[,27])="ann_gr_90_14"
countynames=read.csv("countynames.csv")

pop9000=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==1990 | year==2000)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_90_00=ann.gr(lag(as.numeric(population)), as.numeric(population), 10))%>%
  filter(year==2000)%>%
  select(FIPS, ann_gr_90_00)

pop0006=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2006)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_06=ann.gr(lag(as.numeric(population)), as.numeric(population), 6))%>%
  filter(year==2006)%>%
  select(FIPS, ann_gr_00_06)

pop0010=pop%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2010)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_10=ann.gr(lag(as.numeric(population)), as.numeric(population), 10))%>%
  filter(year==2010)%>%
  select(FIPS, ann_gr_00_10)

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
  inner_join(pop9000)%>%
  inner_join(pop0010)%>%
  inner_join(pop1014)%>%
  inner_join(pop0006)%>%
  inner_join(pop0710)%>%
  inner_join(pop1114)%>%
  mutate(FIPS=as.numeric(FIPS))

pop_gr_avg=pop[,-27:-28]%>%
  gather(year, population, -FIPS)%>%
  mutate(group=car::recode(year, "1990:2000='90_00'; 2000:2010='00_10'; 2010:2014='10_14'"))%>%
  group_by(FIPS)%>%
  mutate(gr=((population-lag(population))/lag(population))*100)%>%
  ungroup()%>%
  group_by(FIPS, group)%>%
  summarize(avg_gr=mean(gr, na.rm=TRUE),
            sd_gr=sd(gr, na.rm=TRUE))%>%
  gather(variable, value, avg_gr:sd_gr)%>%
  mutate(variable=paste(variable, group, sep="_"))%>%
  ungroup()%>%
  select(-group)%>%
  spread(variable, value)%>%
  select(FIPS, avg_gr_90_00,avg_gr_00_10, avg_gr_10_14,sd_gr_90_00,sd_gr_00_10, sd_gr_10_14 )

pop_gr_all=pop[,-27:-28]%>%
  gather(year, population, -FIPS)%>%
  group_by(FIPS)%>%
  mutate(gr=((population-lag(population))/lag(population))*100)%>%
  summarize(avg_gr_90_14=mean(gr, na.rm=TRUE),
            sd_gr_90_14=sd(gr, na.rm=TRUE))%>%
  select(FIPS, avg_gr_90_14,sd_gr_90_14)

pop_gr_avg=pop_gr_avg%>%
  inner_join(pop_gr_all)%>%
  select(FIPS, avg_gr_90_00,avg_gr_00_10, avg_gr_10_14,avg_gr_90_14,sd_gr_90_00,sd_gr_00_10, sd_gr_10_14,sd_gr_90_14 )

pop_gr=pop[,-27:-28]%>%
  gather(year, population, -FIPS)%>%
  group_by(FIPS)%>%
  mutate(gr=((population-lag(population))/lag(population))*100)

  
state=filter(pop, FIPS==0)
state_anngr=filter(pop_anngr, FIPS==0)

###### Non-Metro Total Population Data ####

"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
metro_fips=c(0,1,5,13,14,19,31,35,39,41,47,59,69,77,93,119,123)
non_metro=filter(pop, FIPS %!in% metro_fips)


non_metro9000=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==1990 | year==2000)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_90_00=ann.gr(lag(as.numeric(population)), as.numeric(population), 9))%>%
  filter(year==2000)%>%
  select(FIPS, ann_gr_90_00)

non_metro0006=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2006)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_06=ann.gr(lag(as.numeric(population)), as.numeric(population), 6))%>%
  filter(year==2006)%>%
  select(FIPS, ann_gr_00_06)

non_metro0010=non_metro%>%
  gather(year, population, -FIPS)%>%
  filter(year==2000 | year==2010)%>%
  group_by(FIPS)%>%
  arrange(year)%>%
  mutate(ann_gr_00_10=ann.gr(lag(as.numeric(population)), as.numeric(population), 9))%>%
  filter(year==2010)%>%
  select(FIPS, ann_gr_00_10)

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
  inner_join(non_metro9000)%>%
  inner_join(non_metro0010)%>%
  inner_join(non_metro1014)%>%
  inner_join(non_metro0006)%>%
  inner_join(non_metro0710)%>%
  inner_join(non_metro1114)

#### Population by Age Data ####

pop2534=read.csv("county_forecast.csv")%>%
  filter(year<=2014, age>=25, age<=34)%>%
  group_by(countyfips, county, year)%>%
  summarize(pop2534=sum(totalPopulation))%>%
  ungroup()%>%
  mutate(year=paste0("a", year))%>%
  spread(year, pop2534)%>%
  mutate(ann_gr_90_00_2534=ann.gr(a1990, a2000, 9),
         ann_gr_00_10_2534=ann.gr(a2000, a2010, 9),
         ann_gr_10_14_2534=ann.gr(a2010, a2014, 4))%>%
  rename(FIPS=countyfips)%>%
  select(-county)

pop2544=read.csv("county_forecast.csv")%>%
  filter(year<=2014, age>=25, age<=44)%>%
  group_by(countyfips, county, year)%>%
  summarize(pop2544=sum(totalPopulation))%>%
  ungroup()%>%
  mutate(year=paste0("a", year))%>%
  spread(year, pop2544)%>%
  mutate(ann_gr_90_00_2544=ann.gr(a1990, a2000, 9),
         ann_gr_00_10_2544=ann.gr(a2000, a2010, 9),
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
  mutate(ann_gr_90_00_gsr=ann.gr(a1990, a2000, 9),
         ann_gr_00_10_gsr=ann.gr(a2000, a2010, 9),
         ann_gr_10_14_gsr=ann.gr(a2010, a2014, 4))


#### Employment to Population Ratio - Chris ####
emp_pop_ratio=read_excel("County Compare Data.xlsx", sheet= "Emp_Pop Ratio")[,-1]%>% # Added the first two column names by hand
  rename(FIPS=FIPS_Num)%>%
  filter(FIPS!=0)%>%
  gather(year, emp_pop, -FIPS)%>%
    mutate(year=paste0("a", year))%>%
    spread(year, emp_pop)%>%
    mutate(ann_gr_90_00_epr=ann.gr(a1990, a2000, 9),
           ann_gr_00_10_epr=ann.gr(a2000, a2010, 9),
           ann_gr_10_14_epr=ann.gr(a2010, a2014, 4))

#### Brian Data - Jan 26th ####

brian=read_excel("Resiliency_012616.xlsx", sheet= "import")
  
  
#### Cluster Base Data ####

all64=countynames%>%
    inner_join(select(pop_anngr, FIPS,ann_gr_90_00:ann_gr_10_14))%>%
    inner_join(select(pop2534, FIPS,ann_gr_90_00_2534:ann_gr_10_14_2534))%>%
    inner_join(select(pop2544, FIPS,ann_gr_90_00_2544:ann_gr_10_14_2544))%>%
    inner_join(select(goods_service_ratio, FIPS,ann_gr_90_00_gsr:ann_gr_10_14_gsr))%>%
    inner_join(select(emp_pop_ratio, FIPS,ann_gr_90_00_epr:ann_gr_10_14_epr))%>%
    inner_join(select(brian, FIPS, lq_goods_90:emp_sd_gr_90_14))%>%
    inner_join(pop_gr_avg)



