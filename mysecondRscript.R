#DOWNLOAD INCIDENCE DATABASE:
library(tidyverse)
library(DBI)
library(odbc)
dbConn<-dbConnect(odbc(),
                  driver="SQL Server",
                  server="rea-inf-dsql08",
                  database="cancerstatsr",
                  trusted_connection=TRUE          )

incidence<-dbGetQuery(dbConn, 
                      "select *         
                      from [national].incidence i         
                      where baseyearkey=2016
                      and incidenceyearkey>=2000")

#LOOK AT ALL OF THE TABLES IN THE DATABASE
dbListTables(dbConn)

#LOOK AT THE FIRST SIX TABLES IN THE DATABASE
sample(dbListTables(dbConn),6)

#USE THE METADATA TO TALK TO THE DATABASE
dbGetQuery(dbConn,"SELECT*
           FROM information_schema.tables")

#LOOKING JUST AT THE CANCERS SITE TABLE
cancersite<-dbGetQuery(dbConn,"SELECT*
                       FROM dim.cancersite")
View(cancersite)

#SET BASE YEAR IF INTEREST AS NEW OBJECT
baseyear<-2016

#SELECT TOP 100 RECORDS FROM NATIONAL INCIDENCE TABLE
sqltorun<-sqlInterpolate(dbConn,"SELECT TOP 100*
               FROM [national].incidence 
               WHERE baseyearkey = ?baseyear",
               baseyear=baseyear)

#CREATE NEW DATASET BASED ON THE ABOVE QUERY
newincidences<-dbGetQuery(dbConn,sqltorun)

  #The above methodology is useful for dashboards and template work

#USING PIPES TO TIDY UP CODING FOR STE-BY-STEP PROCESSES
library(tidyverse)
incidence %>% 
  filter(IncidenceYearKey==2000) %>% 
  View()

#without the pipe you could: filter(incidence,IncidenceYearKey==2000)

#BUILDING A ROUGH MODEL FOR THE INCIDENCE DATA
incidence %>% 
  lm(IncidenceCount ~ IncidenceYearKey, data= .)
  #the . indicates to use the information from the LHS

#PLOTTING DATA
incidence %>% 
  filter(IncidenceYearKey==2000,
         GenderKey=="M") %>%
  select_if(is.numeric) %>% 
  pairs()

#PLOTTING DATA USING THE DATASAURUS DATA
library(datasauRus)
library(ggplot2)
datasauRus::datasaurus_dozen %>% 
  ggplot(aes(x=x,y=y)) +
  geom_point() +
  facet_wrap(~dataset)

#USING THE PIPES PROCESS TO GET IRIS COLUMN NAMES TO UPPER CASE
iris %>% 
  colnames() %>% 
  toupper()

#USING PIPES TO TAKE THE LETTERS VARIABLE AND USE SUB TO REPLACE S WITH Z
LETTERS %>% 
  sub("S","Z", .) ->
  LETTERZ
View(LETTERZ)

#FILTERING: INCIDENCE YEAR KEY=2000 AND GENDER=M
incidence %>% 
  filter(IncidenceYearKey==2000 ,
         GenderKey=="M")

#FILTERING: INCIDENCE YEAR KEY=2000 OR GENDER=M
incidence %>% 
  filter(IncidenceYearKey==2000 |
           GenderKey=="M")

#SEARCHING FOR UNIQUE VALUES
unique(incidence$CountryKey)
  #will show all of the unique countries

#WHAT COLUMNS ARE THERE IN THE INCIDENCE DATASET
colnames(incidence)

#FILTERING IRIS WHERE SEPAL LENGTH OR SEPAL WIDTH ARE SMALLER THAN AVERAGE
iris %>%
  filter(Sepal.Length<mean(Sepal.Length) |
           Sepal.Length<mean(Sepal.Width)) %>% 
  View()

#FILTER THAT REMOVES SETOSA IRISES FROM THE DATASET
iris %>%
  filter(Species!="setosa") %>% 
  View()

  #filter(!(Species=="setosa")) would also be correct

#REMOVING COLUMNS: SELECTING COLUMNS WANT TO KEEP
iris %>%
  select(Sepal.Length,Sepal.Width) %>% 
  View()

#REMOVING COLUMNS: SELECTING COLUMNS NOT TO BE INCLUDED
iris %>% 
  select(-Sepal.Length) %>% 
  View()

#REMOVING COLUMNS: USING THE BEGINS WITH COMMAND TO FILTER DOWN
iris %>% 
  select(starts_with("Sepal")) %>% 
  View()

#REMOVING COLUMNS: USING THE ENDS WITH COMMAND TO FILTER DOWN
iris %>% 
  select(ends_with("Length")) %>% 
  View()

#REMOVING COLUMNS: USING SOMETHING INDEPEDENT OF LOCATION IN NAME TO FILTER DOWN
iris %>% 
  select(contains("Petal")) %>% 
  View()

#REMOVING COLUMNS: FILTERING DOWN BY A RANGE OF COLUMNS
iris %>% 
  select(Petal.Width:Species) %>% 
  View()

#USING THE SELECT COMMAND TO RE-ORDER COLUMNS ALPHABETICALLY
iris %>% 
  select(.,sort(colnames(.))) %>% 
  View()

#FILTERING COLUMNS FROM IRIS THAT START WITH "S"
iris %>% 
  select(starts_with("S")) %>% 
  View()

#SORTING IRIS SO THAT ALL COLUMNS BEGINNING WITH "P" ARE AT THE BEGINNING
iris %>% 
  select(starts_with("P"),everything()) %>% 
  View()

#USING SELECT TO RENAME COLUMNS
iris %>% 
  select(P=Petal.Width, everything()) %>% 
  View()

#USING MUTATE TO ADD COLUMNS
iris %>% 
  mutate(Sepal.Area=Sepal.Width*Sepal.Length,
         Avg.Sepal.Area=mean(Sepal.Area)) ->
  iris
  View(iris)

#CREATE AN IRIS IMPERIAL TABLE WITH NUMERIC MEASUREMENTS CONVERTED TO INCHES
iris %>% 
  mutate(Sepal.Length=Sepal.Length/2.5,
         Sepal.Width=Sepal.Width/2.5,
         Petal.Length=Petal.Length/2.5,
         Petal.Width=Petal.Width/2.5,
         Sepal.Area=Sepal.Width*Sepal.Length,
         Avg.Sepal.Area=mean(Sepal.Area),
         Species=toupper(Species)) %>% 
  View()
  #note still need to create new irisImperial variable

iris %>%
  mutate_if(is.numeric,~./2.5) %>% 
  mutate_if(is.character,toupper) %>% 
  View()
  #note that with the above, averages are incorrect so would need to add in a mutate for these

#USING SUMMARISE COMMAND
iris %>% 
  summarise(Total.Area=sum(Sepal.Area))

iris %>%
  summarise(Total.Area=sum(Sepal.Length*Sepal.Width))

iris %>% 
  as_tibble() %>% 
  summarise(Total.Area=sum(Sepal.Length*Sepal.Width))
  #as_tibble tidies up the output

#FIND MIN AND MAX VALUES FOR PETAL LENGTH
iris %>% 
  summarise(min(Petal.Length), 
            max(Petal.Length))

#GROUPING BY
iris %>%
  group_by(Species) %>% 
  summarise(TotalSA=sum(Sepal.Length*Sepal.Width))

iris %>% 
  group_by(FirstChar=substr(Species,1,1),
           Species) %>% 
  summarise(TotalSA=sum(Sepal.Length*Sepal.Width))

iris %>% 
  group_by(Species) %>% 
  mutate(Avg.Sepal.Length=mean(Sepal.Length)) %>% 
  View()

#GROUP IRIS BY SPECIES AND ADD A COLUMN OF THE AVERAGE PETAL WIDTH FOR EACH SPECIES
iris %>% 
  group_by(Species) %>% 
  mutate(Avg.Petal.Width=mean(Petal.Width)) %>% 
  View()

#ADDING FLAGS AND EXTRACTING MAXIMUM VALUES USING GROUP_BY_IF
iris %>%
  mutate(bAvg=Sepal.Length<mean(Sepal.Length)) %>% 
  group_by(Species,bAvg) %>% 
  summarise_all(max) %>% 
  View()

iris %>%
  mutate(bAvg=Sepal.Length<mean(Sepal.Length)) %>% 
  group_by(Species,bAvg) %>% 
  summarise_all(c("max","min")) %>% 
  View()

iris %>%
  mutate(bAvg=Sepal.Length<mean(Sepal.Length)) %>% 
  group_by_if(~!is.numeric(.)) %>% 
  summarise_all(max) %>% 
  View()

#RESHAPING DATA (PIVOTING)
incidence %>% 
  filter(IncidenceYearKey==2000) %>% 
  group_by(GenderKey, CancerSiteKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  spread(GenderKey,IncidenceCount) %>% 
  View()

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  spread(CountryKey,IncidenceCount) %>% 
  View()

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(CountryKey) %>% 
  mutate(Prop=IncidenceCount/sum(IncidenceCount)) %>% 
  select(-IncidenceCount) %>% 
  mutate(Prop=scales::percent(Prop)) %>% 
  spread(CountryKey,Prop) %>% 
  View()

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  group_by(AgeRangeKey, GenderKey, CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  tidyr::unite("GenderAge",c("GenderKey","AgeRangeKey")) %>%
  spread(CountryKey,IncidenceCount) ->
  genderAgeCountry

#RESHAPING DATA(UNPIVOTING)
genderAgeCountry %>% 
  gather(CountryKey,IncidenceCount,-`GenderAge`) %>% 
  separate(`GenderAge`,c("GenderKey","AgeRangeKey")) %>% 
  View()

#PIVOT DATA SO IS PIVOT BY POPULATION AND YEAR
View(population)
population %>% 
  group_by(country,year) %>% 
  summarise(population=sum(population)) %>% 
  spread(year,population) %>% 
  View()

#UNPIVOT DATA SO ALL COLUMNS ARE ROWS
View(who)
who %>% 
  gather(Measure,Value,-(country:year),
         na.rm=TRUE) %>% 
  separate(Measure,c("Type","GenderAge"),
           extra="merge") %>% 
  View()
  
  #when split by both gender and age you get a warning of too few values at various locations (so keeping as genderage combined for now)

#JOINING
incidence %>% 
  inner_join(cancersite, by=c("CancerSiteKey"="CancerSiteKey")) ->
  incidence_cancersite
  
  #use 'by' command where there are too many columns which the two being joined might match on (to specify) 

incidence %>% 
  distinct(CancerSiteKey) %>% 
  slice(1:5) ->
  exclusions

incidence %>% 
  anti_join(exclusions)

  #anti_join = a filter for what not to include

#DATA VISUALISATION
library(datasauRus)
library(ggplot2)

datasaurus_dozen %>% 
  ggplot(aes(x=x,y=y,colour=dataset)) +
  geom_point() +
  theme_minimal()

datasaurus_dozen %>% 
  filter(dataset=="dino") %>% 
  ggplot(aes(x=x,y=y)) +
  geom_point() +
  theme_minimal()

datasaurus_dozen %>% 
  filter(dataset=="dino") %>% 
  ggplot(aes(x=x,y=y)) +
  geom_path() +
  theme_minimal()

datasaurus_dozen %>% 
  filter(dataset=="dino") %>% 
  ggplot(aes(x=x,y=y)) +
  geom_col() +
  theme_minimal()

datasaurus_dozen %>% 
  filter(dataset=="dino") %>% 
  ggplot(aes(x=x,y=y)) +
  geom_col(position = "stack") +
  theme_minimal()

datasaurus_dozen %>% 
  ggplot(aes(x=x,y=y,colour=dataset)) +
  geom_point() +
  facet_wrap(~dataset) +
  theme_minimal() +
  theme(legend.position = "none")

datasaurus_dozen %>% 
  ggplot(aes(x=x,y=y,colour=dataset)) +
  geom_point() +
  geom_path() +
  facet_wrap(~dataset) +
  theme_minimal() +
  theme(legend.position = "none")

datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarise_all(mean) ->
  dd_summary

datasaurus_dozen %>% 
  ggplot(aes(x=x,y=y,colour=dataset)) +
  geom_point() +
  geom_point(data=dd_summary, colour="black") +
  facet_wrap(~dataset) +
  theme_minimal() +
  theme(legend.position = "none")

datasaurus_dozen %>% 
  ggplot(aes(x=x,y=y,colour=dataset)) +
  geom_point() +
  geom_path() +
  geom_point(data=dd_summary, colour="black") +
  facet_wrap(~dataset) +
  theme_minimal() +
  theme(legend.position = "none") ->
  dd_plot

dd_plot %+% dd_summary
dd_plot %+% sample_n(datasaurus_dozen,1000)
dd_plot %+% sample_n(datasaurus_dozen,800)
dd_plot %+% sample_n(datasaurus_dozen,400)
dd_plot %+% sample_n(datasaurus_dozen,100)

iris %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour=Species)) +
  geom_point(size=2,alpha=0.8) +
  labs(x="Sepal length (cm)",y="Sepal width (cm)",
       title="Size relationship",
       subtitle=paste("In cm,",nrow(iris),"observations"))

iris %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour=Species)) +
  geom_jitter(size=3) +
  labs(x="Sepal length (cm)",y="Sepal width (cm)",
       title="Size relationship",
       subtitle=paste("In cm,",nrow(iris),"observations, has been jittered"))


