LETTERS=="a"
 myFirstVariable<-1
 myFirstVariable
 mySecondVariable<-2
 mySecondVariable
 myThirdVariable<-1:10
 myThirdVariable
 
 ls()
 
 oneToFifty<-1:50
 testWords<-c("red", "blue")
 c(oneToFifty, testWords)
 
 largeABC<-data.frame(id=1:26, abc=letters)
 View(largeABC)
 
 exampleTable<-data.frame(oneToFifty, testWords)
 View(exampleTable)
 

list(numbers=1:50, words2=c("red", "blue"))
 
 list(
   firstList=list(myFirstVariable),
   secondList=list(myFirstVariable, largeABC)
 )
 
class(c(1,TRUE)) #returns data type
is.integer(c(1,TRUE)) #FALSE as is not an integer
as.integer(c(1,TRUE)) #forces to make an integer
is.integer(as.integer(c(1,TRUE))) #TRUE as have combined to force to be integer

paste(1:30, 1:3)
paste(1:30, 1:3, sep="-", collapse=";")
paste0(1:30, 1:3)

rnorm(100, mean=10, sd=1)

runif(50)

library(tidyverse)

library(dplyr)
vignette("dplyr")

a<-10:21
length(a)
a[1] #value of first variable
a[1:5] #value of first five variable
a[c(1,3,5)] #value of first, third and fifth variable
a[-1] #value of all variables except the first
a[-(1:5)] #value of all variables except the first five
a<=15 #generates a true or false for every row in vector that meets this criteria
a[a<=15] #gives us all of the variables which meet this criteria
a[a<=15 | a>19] #gives all of the variable which meet both of these criteria

df<-data.frame(a=letters, b=1:26)
df[] #will select everything
df[,]
df[1,] #will select first row only
df[,1] #will select first column only; note that this command converts this to a vector as it is the simplest form of presenting this data
df[1:5,1:2] #will return rows 1 to five for columns 1 and 2
df[1:10,"a"] #first 10 rows for column named a
df[1:10, c("a","b")] # first 10 rows for columns a and b
df[-1,] #everything but the first row
df[-1,-2] #everything but the first row and second column
df[c(TRUE,FALSE),] #every other row
df[,c(TRUE,FALSE)] #every other column
df[,2]<=5

df[df[,2]<=5,]
df[df$b<=5,] #same as the above code but written in a much more tidy way

colnames(df)
df[,colnames(df)<="a"]

letters<"x"
letters[c(letters<"x")] #returns all letters before x

View(iris)
iris[1,]
iris[1:5,]

head(iris)
iris[1:6,] #identical to above code

tail(iris)
iris[145:150,]

head(iris, n=5) #changing the number of obs reported by the head command

iris[,1:2]
iris[,"Sepal.Length"]
iris[,c("Sepal.Length", "Sepal.Width")]

letters>"g"
letters[c(letters>"g")] #returns all letters after g

iris$Sepal.Length>=5.8
iris[iris$Sepal.Length>=5.8,] #to get all those with sepal width of 5.8 and over
avgSepalWidth<-mean(iris$Sepal.Width)
avgSepalWidth
iris[iris$Sepal.Width<avgSepalWidth,] #to get all those with a sepal width greater than the average

colnames(iris)
iris[,1:4]
iris[,-5]
iris[, colnames(iris)!="Species"]
myIris<-iris[1:100,]
View(myIris)
myIris$Species<-"Unknown"
View(myIris)
myIris$Sepal.Length<=5.5
myIris<-myIris[myIris$Sepal.Length<=5.5,]
View(myIris)

write.csv(iris,"iris.csv", row.names=FALSE) #creates a csv in the myfirstproject folder

library(haven)

read_csv("iris.csv",col_names=letters[1:5], skip=1)

library(readxl)
cn<-"cancersurvivalEng2015.xls"
readxl::excel_sheets(cn)
cancerdata<-read_excel(cn,sheet="Table 5",skip=2,na=":")
View(cancerdata)

writexl::write_xlsx(cancerdata,"newACS.xlsx")

library(openxlsx)
openxlsx::write.xlsx(cancerdata, "newACS.xlsx")

openxlsx::write.xlsx(myIris, "myIris.xlsx", colWidths="auto")








