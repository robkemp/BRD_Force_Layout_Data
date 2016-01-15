library(dplyr)
library(tidyr)
library(readxl)
library(stringr)


# Read in each sheet seperately 
qcw=read_excel("County Compare Data.xlsx", sheet= "All counties QCEW")
pop=read_excel("County Compare Data.xlsx", sheet= "Population 90_14")[,-1]
emp=read_excel("County Compare Data.xlsx", sheet= "Total Emp") # Added the first two column names by hand
emp_pop_ratio=read_excel("County Compare Data.xlsx", sheet= "Emp_Pop Ratio") # Added the first two column names by hand
goods=read_excel("County Compare Data.xlsx", sheet= "Goods Producing") # Put Ownership and Service Column names in by hand
service=read_excel("County Compare Data.xlsx", sheet= "Service Providing")# Put Ownership and Service Column names in by hand
goods_service_ratio=read_excel("County Compare Data.xlsx", sheet= "Goods_ Service Ratio")


qcw_z=qcw%>%
  filter(FIPS!=0)%>%
  gather(key, ann_avg, -c(FIPS:Industry))%>%
  mutate(year=str_sub(key, 1, 4))%>%
  filter(FIPS!="Unknown Or Undefined, Colorado")%>%
  select(-key)%>%
  group_by(year, Ownership, Industry)%>%
  mutate(ann_avg_qcw_z=(ann_avg-mean(ann_avg))/sd(ann_avg))%>%
  ungroup()%>%
  mutate(own=ifelse(Ownership=="Total Covered", "total", "private"),
         ind=ifelse(Industry==  "Total, all industries", "total", ifelse(Industry=="Goods-producing" , "goods", "service")),
         key=paste(own, ind, "qcw", sep="_"))%>%
  select(FIPS, year, ann_avg_qcw_z, key)%>%
  filter(!is.na(FIPS))%>%
  spread(key, ann_avg_qcw_z)

pop_z=pop[,-27]%>% #drops last column since it has spaces.  Also, I dropped the first column in the read_excel call now.
  filter(FIPS!=0)%>%
  gather(year, population, -FIPS)%>%
  group_by(year)%>%
  mutate(population=as.numeric(ifelse(population=="N/A", 0, population)),
         population_z=(population-mean(population))/sd(population))%>%
  select(FIPS, year, population_z)



emp_z=emp[,-1]%>%
  select(-CAGR)%>%
  filter(FIPS!="Unknown Or Undefined, Colorado",
         FIPS!=0)%>%
  gather(year, employment, -FIPS)%>%
  group_by(year)%>%
  mutate(employment=ifelse(is.na(employment), 0, employment),
         total_employment_z=(employment-mean(employment)/sd(employment)))%>%
  select(FIPS, year, total_employment_z)

emp_pop_z=emp_pop_ratio[,-1]%>%
  rename(FIPS=FIPS_Num)%>%
  filter(FIPS!=0)%>%
  gather(year, emp_pop, -FIPS)%>%
  group_by(year)%>%
  mutate(emp_pop=ifelse(is.na(emp_pop), 0, emp_pop),
         emp_pop_z=(emp_pop-mean(emp_pop))/sd(emp_pop))%>%
  select(FIPS, year, emp_pop_z)

goods_z=goods[,-1]%>%
  rename(FIPS=FIPS_Num)%>%
  filter(FIPS!=0)%>%
  select(-Ownership, -Industry)%>%
  gather(year, goods,-FIPS)%>%
  group_by(year)%>%
  mutate(goods=ifelse(is.na(goods), 0, goods),
         goods_z=(goods-mean(goods))/sd(goods))%>%
  select(FIPS, year, goods_z)

service_z=service[,-1]%>%
  rename(FIPS=FIPS_NUM)%>%
  filter(FIPS!=0)%>%
  select(-Ownership, -Industry)%>%
  gather(year, service, -FIPS)%>%
  group_by(year)%>%
  mutate(service=ifelse(is.na(service), 0, service),
         service_z=(service-mean(service)/sd(service)))

goods_service_z=goods_service_ratio[,-1]%>%
  rename(FIPS=FIPS_NUM)%>%
  filter(FIPS!=0)%>%
  gather(year, goods_service, -FIPS)%>%
  group_by(year)%>%
  mutate(goods_service=ifelse(is.na(goods_service), 0, goods_service),
         goods_service_z=(goods_service-mean(goods_service)/sd(goods_service)))


## JSON Conversion



json_out=function(data, year, filename){
  require(RJSONIO, quietly=TRUE)
  d=filter(data, year==year)
  jd=toJSON(split(d, 1:nrow(d)))
  write(jd, paste0(filename, ".JSON"))
}

json_out(pop_z, 1990, "cocountiesPop1990")
json_out(emp_z, 1990, "cocountiesEmp1990")
json_out(qcw_z, 1990, "cocountiesQCW1990")
json_out(emp_pop_z, 1990, "cocountiesEmpPop1990")
json_out(goods_z, 1990, "cocountiesGoods1990")
json_out(service_z, 1990, "cocountiesService1990")
json_out(goods_service_z, 1990, "cocountiesGoodsService1990")











