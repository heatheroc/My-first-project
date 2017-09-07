#LOADING LIBRARY'S:
library(tidyverse)
library(DBI)
library(odbc)
library(plotly)
library(ggplot2)
library(modelr)
library(forcats)
library(glue)
library(pivottabler)

#rdrr.io will show you where a package is and show you the documentation, source code, and examples

#RECAP:
letters[letters < LETTERS]

mtcars %>%
  View()
#dataset has row names as car names, instead of a specific column; to make these a column:
mtcars %>%
  mutate(car = rownames(.)) %>%
  View()

who %>%
  gather(Measure, Value, -(country:year)) %>%
  group_by(country, Measure) %>%
  summarise(Avg = mean(Value, na.rm = TRUE)) %>%
  mutate(Avg = ifelse(is.nan(Avg), -99, Avg)) %>%
  View()
#to reduce the number of decimal places, you would use a round function

who %>%
  gather(Measure, Value, -(country:year), na.rm = TRUE)
#will produce many fewer values if remove na's at this stage

install.packages(("ggplot2movies"))
library(ggplot2movies)

movies %>%
  resample_partition(c(train = .7, test = .3)) ->
  movies_split

movies_split$train %>%
  as_data_frame() %>%
  #mutate(year=(year-min(year))/(2020-min(year))) %>%
  #mutate(year=year-min(year))
  lm(
    rating ~ year + length + Action + Animation + Comedy + Drama + Documentary + Romance + Short,
    data = .
  ) ->
  movies_ratings
#note: adding the scale to the year to take account of this value starting from ~1930, as opposed to starting from 0

movies_split$test %>%
  as_data_frame() %>%
  #mutate(year=year-min(as_data_frame(movies_split$train)$year)) %>%
  add_predictions(model = movies_ratings) %>%
  mutate(residuals = rating - pred) %>%
  ggplot(aes(x = residuals)) +
  geom_density()
#NOTE: commented out lines above are the scaling of the year, but are resulting in a non-normal distribution

#to check which obs are causing the strange / outlying residuals
movies_split$test %>%
  as_data_frame() %>%
  add_predictions(model = movies_ratings) %>%
  mutate(residuals = rating - pred) %>%
  filter(residuals < (-5)) %>%
  View()

movies %>%
  filter(length > 1000) %>%
  View()
#three films are outliers with a run-time of greater than 1000 minutes

#STRING MANIPULATION
library(stringr)

simple<-"This IS HOrribly typed! "
numbers<-c("02","11","10","1")
sort(numbers)

str_to_lower(simple)
str_to_upper(simple)

str_sort(numbers,numeric=TRUE)

str_length(numbers)

str_split(simple," ")
str_split(simple,"i|I")
str_split(simple,"[iI]")
str_split(simple,boundary("character"))
str_split(simple,boundary("word"))
  #better to use the word boundary in place of spaces, because of erroneous spaces / errors in string

#to detect if a pattern is present = str_detect(simple,"typed), e.g. email addresses, phone numbers etc
  #to extract that pattern = str_extract(simple,"typed") - NOTE: .=all values, *=wildcard/characters,+=one or more of something,
str_extract(simple,"r+") #+ indicates want all instances of "r"
str_extract(str_to_lower(simple),"is*") #* indicates want all variants of "is"
str_extract(str_to_lower(simple),"is.*") #.* indicates want everything past that point

str_count(simple, "is")
str_count(str_to_lower(simple), "is")

who %>% 
  mutate_if(is_character,str_to_upper)
  #don't use brackets as are telling the function to use the functions
  #NOTE: could use a mutate for each column to do this, but the above code allows you to do this for all at once
  #mutate(country=str_to_upper())

#package lettercase has additional str_ functions

"the quick brown fox jumps over the lazy dog" %>%
  str_to_upper() %>% 
  str_split(boundary("word")) %>% 
  .[[1]] %>% 
  str_length()

#QUICK FREQUENCY TABLE TO LOOK AT DISTRIBUTIONS
library(forcats)

myFactor<-as.factor(c("red","blue","yellow",NA,"red"))
fct_count(myFactor)
myFactor %>% 
  fct_count()

fct_explicit_na(myFactor) %>% 
  fct_count()
  #to tell NA's are missing values
  #can force it to allocate NA to another value, if wanted

fct_infreq(myFactor) %>% 
  fct_count()
  #tells how to handle the ordering of values in models etc. by their frequency  

fct_lump(myFactor,n=1) %>% 
  fct_count()
  #tells to keep the most frequent value and out everything else into other
    #good to do before handle NA's, as might want to keep NA's together as separate group for analysis
    #if do this after handling NA's then they will be put in with the 'other' category

fct_anon(myFactor) %>% 
  fct_count
  #to anonymise data


View(gss_cat)

fct_count(as.factor(gss_cat$year))
#alernative:
gss_cat %>% 
  count(year)

gss_cat %>% 
  mutate(marital=fct_lump(marital,n=2),
         race=fct_anon(race),
         partyid=fct_anon(partyid),
         relig=fct_anon(relig))


#STRING INTERPOLATION (FOR DYNAMIC STRING CONSTRUCTION):
library(glue)

#glue is to combine things, like paste (can specify seperater)
#glue_data

age<-40
gender<-"Male"
location<-"England"
cancerlocation<-"Thyroid"
glue("The most common cancer for {ifelse(gender=='Female','women','men')} in {location} aged {round(age,-1)} to {round(age,-1)+10} is {cancerlocation}.")

#alternatively:
glue("The most common cancer for {gender} in {location} aged {lbage} to {ubage} is {cancerlocation}.",gender=ifelse(gender=='Female','women','men'),lbage=round(age,-1),ubage=round(age,-1)+10)

cancersite %>% 
  glue_data("The code for {CancerSiteLevel6Desc} is {CancerSiteKey}.")
  
incidence %>%
  group_by(IncidenceYearKey,CountryKey,AgeRangeKey,GenderKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(IncidenceYearKey,CountryKey,GenderKey) %>% 
  mutate(prop=IncidenceCount/sum(IncidenceCount)) %>% 
  glue_data("In {IncidenceYearKey}, for {GenderKey} aged {AgeRangeKey} in {CountryKey} there were {format(IncidenceCount, trim=TRUE)} incidences of cancer. This is {scales::percent(prop)} of all cancers for {GenderKey} aged {AgeRangeKey} in {CountryKey} that year.")
  #would be able to modify this to change country definitions etc.

#PIVOT TABLER
install.packages(pivottabler)
library(pivottabler)

  #pivottabler is for the creation of outputable pivot tables (gives more sophisticated control over look and feel)

#creating pivot table:
pt<-PivotTable$new()
  #new tells R want to make a new, empty pivot table - note:there are lots of options of what can put after $
pt$addData(incidence)
pt$addRowDataGroups("AgeRangeKey")
pt$addColumnDataGroups("GenderKey")
pt$defineCalculation(calculationName = "Incidence",summariseExpression = "sum(IncidenceCount)")
pt$renderPivot()

#making quick, less complicated pivots:
  #note: can also use qlpvt, if using Latex code
qhpvt(dataFrame = incidence,rows=c("AgeRangeKey"),columns=c("CountryKey","GenderKey"),calculations = c("round"="round(sum(IncidenceCount),-3)", "raw"="sum(IncidenceCount)"))

#getting the Latex code for pivot table:
writeLines(pt$getLatex(),"pivot.tex")

#styling pivot tables:
pt$renderPivot()

pt$theme<-"largeplain"
pt$renderPivot()

pt$theme<-"compact"
pt$renderPivot()

locke_branded<-list(headerBackgroundColor="rgb(33,101,182)",headerColor="rgb(255,255,255)",cellBackgroundColor="rgb(255,255,255)",cellColor="rgb(77,77,77)")
locke_theme<-getSimpleColoredTheme(pt,"locke_branded",colors=locke_branded,fontName = "Roboto,Arial")
pt$theme<-locke_theme
pt$renderPivot()

cruk_branded<-list(headerBackgroundColor="#f24cae",headerColor="rgb(255,255,255)",cellBackgroundColor="rgb(255,255,255)",cellColor="#7f7f7f",totalBackgroundColor="rgb(255,255,255",totalColor="#7f7f7f")
cruk_theme<-getSimpleColoredTheme(pt,"cruk_branded",colors=cruk_branded,fontName = "Roboto,Arial")
pt$theme<-cruk_theme
pt$renderPivot()

#conditional formatting pivot tables:
s<-PivotStyle$new(pt,styleName="cellHighlight",declarations=list("color"="red"))
g<-pt$getCells(specifyCellsAsList = TRUE,columnNumbers = 3)
gc<-lapply(g,function(cell){cell$style<-s})
pt$renderPivot()
  #for conditional formatting, go to a website and select colour interested in, right-click and select inspect, choose the colur and copy the code associated with it
  #for conditional formatting, use the code in the vignettes for the pivottabler function and work with until happy with outputs

s<-PivotStyle$new(pt,styleName="cellBold",declarations=list("font-weight"="bold"))
g<-pt$findCells(minValue = 250000)
gc<-lapply(g,function(cell){cell$style<-s})
pt$renderPivot()

  #to undo this bolding:
s<-PivotStyle$new(pt,styleName="cellBold",declarations=list("font-weight"="1-rem"))
g<-pt$findCells(minValue = 0)
gc<-lapply(g,function(cell){cell$style<-s})
pt$renderPivot()










































#FORMAT ALL CODE TO TIDY
#ctr+A then ctrl+shift+A
#To get long lines to appear wrapped within the view box: Tools>Global options>Code>select Soft-wrap R source files 