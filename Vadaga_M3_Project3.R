#version.string R version 3.6.3 (2020-02-06)
#Name            - Vadaga, Satyanarayana
#Course title    - ALY6000, Introduction to Analytics
#Module          - 3


install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")


library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("tidyr")
library("plyr")
library("tidyverse")


getwd()

bio = read.csv(file.choose(),
               header=TRUE, sep =",")

# Display head, tail & structure of bio
headtail(bio)
str(bio)
summary(bio)
# Display datatypes of bio
glimpse(bio)

# Create an object that counts & lists all species records

tmp <- count(bio, "species")

# Display 8 levels of the species
levels(bio$species)

# Create a subset of just species variable & display first 5 records
tmp2 = bio[1:5,3]

?table()

# Create a table w of species variable
w <- table(bio$species)
class(w)

# Convert above table - w to data frame 
t <- data.frame(w)
t

# Extract & display frequency values from table
t[2]

# Create a table cspec from species attribute table

cspec <- table(bio$species)
class(cspec)
class(bio)

# Create a table cspecpct to display species & percentage
cspecpct <- prop.table(cspec)
class(cspecpct)

# Convert cSpecPct to a dataframe u
u <- data.frame(cspecpct)
class(u)

# Create a barplot1

par(las=2)
par(mar = c(5.6, 5.1, 4.1,2.1))
par(cex.axis = 1, cex.lab =1)
barplot(cspec,
        main ="Fish Count",
        xlab = "COUNTS",
        col = "light green",
        horiz = TRUE,
        cex.names = 0.6,
        xlim = c(0,300))

# Create a barplot2

barplot(cspecpct,
        main ="Fish Relative Frequency",
        col = "light blue",
        cex.names = 0.6,
        ylim = c(0,0.5)
)

# Create a dataframe r by ordering dataframe u by frequency descening order

d <- u[order(-u$Freq),]

# Rename column names in d dataframe

colnames(d) <- c("Species","RelFreq")

# Add new variables cumfreq, counts, cumcounts

d['cumfreq'] <- NA
d['counts'] <- NA
d['cumcounts'] <- NA

tmp

d$cumfreq <- round(cumsum(d$RelFreq)*100,0)
d$counts <- (676*d$RelFreq)
d$cumcounts <- cumsum(d$counts)

# Create a barplot pc

par(las=2)
par(mar = c(7, 4.1, 4.1,2.8))
par(cex.axis = 1, cex.lab =1)
pc <- barplot(d$counts,
              width = 2,
              space = 0.15,
              ylim = c(0,800),
              #yaxt = "n",
              axes = FALSE,
              names.arg = d$Species,
              cex.names = 0.8,
              ylab = "Cummulative Counts",
              main = "Species Pareto - VADAGA"
              )
lines(pc, d$cumcounts, type ="b", pch =19, col = "cyan4")
box(col = "grey62")
axis(2, at=d$cumcounts,col = "grey62",col.axis = "grey62",cex.axis = 0.80, col.ticks = "grey62", las =1)
axis(4, at=d$cumcounts,col = "cyan4",col.axis = "cyan4",labels = (paste(d$cumfreq,"%",sep ="")),cex.axis = 0.80, col.ticks = "cyan4")

# Create a scatter plot to visualize relations between weight & length

plot(x=bio$tl, 
     y=bio$w,
     xlim = c(0,500),
     ylim = c(0,1200),
     xlab = "Length",
     ylab = "Weight",
     col.axis = "Cyan4",
     pch = 21,
     main = "Length vs Weight",
     las =2)