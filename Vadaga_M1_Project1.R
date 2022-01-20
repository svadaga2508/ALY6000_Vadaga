#version.string R version 4.1.2 (2021-11-01)
#Student Name    - Satyanarayana Vadaga
#Course title    - ALY6000, Introduction to Analytics
#Module          - 1
#===========Version===========
# Version Number - 1.1
# Description    - Develop a script in  R to install vcd packages
#                  Create a scatter plot using sales & temperature sample data
#                  Determine descriptive statistics
#                  Create sample vector, data frame
#                  Import data from a delimited text file
#=============================
install.packages("vcd")
library("vcd")

#Creating a vector of sales and assigning to a sales variable
sales <- c(7,11,15,20,19,11,18,10,6,22)

#Creating a vector of temperatures and assigning to a temperature variable
temperature <- c(69,81,77,84,80,97,87,70,65,90)
plot(sales, temperature)

#Calculating mean of temperature with function - mean
mean(temperature) # Output is : 80

#Calculating standard deviation of temperature with function - sd
sd(temperature) # 10.0

#Calculating correlation of temperature with function - sd
cor(sales, temperature) # 0.6

#Delete 3rd element from sales vector
sales <- sales[-3]
sales

#Insert 16 as 3rd element in sales vector
sales <- append(sales,16,after=2)
sales

#Create a vector<names> with elements Tom, Dick, Harry
names <- c("Tom", "Dick", "Harry")
names

#Create a 5 row and 2 column matrix of 10 integers
myfirstmatrix1 <- matrix(1:10,nrow=5,ncol=2,byrow=TRUE)
myfirstmatrix2 <- matrix(1:10,nrow=5,ncol=2)

myfirstmatrix1
myfirstmatrix2

#Create a data frame<icSales> with sales & temp attributes
icsales <- data.frame(sales,temperature) 

#Display the data frame structure icsales
icsales

#Display a summary of the icsales data frame
summary(icsales)

#Import the dataset Student.csv
student = read.csv(file.choose(),
                      header=TRUE, sep =",")

#Display only variable names of student.csv dataset
colnames(student,do.NULL = TRUE, prefix ="col")



