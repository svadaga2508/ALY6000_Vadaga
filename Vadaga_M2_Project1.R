#version.string R version 3.6.3 (2020-02-29)
#Name            - Plotting Basics :  Vadaga
#Course title    - ALY6000, Introduction to Analytics
#Module          - 2


install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")


library("magrittr")
library("dplyr")
library("plotrix")
library("ggplot2")
library("ggplot2")
library("moments")
library("FSA")
library("FSAdata")



class(BullTroutRML2) # data.frame
# Print first & last 3 records from data frame - BullTroutRML2
headtail(BullTroutRML2,3)

# Filter "Harrison" records from BullTroutRML2 and assign to a different data frame
fil_BullTroutRML2 <- filterD(BullTroutRML2, lake == 'Harrison')


# Finding frequency distribution of age
# Step 1 : Calculate range from the given data set.
age_range <- range(fil_BullTroutRML2$age) # [1]  0 14
fl_range  <- range(fil_BullTroutRML2$fl)  # [1]  20 480

# Step 2 : Break the range in such a way that intervals are not overlapped
age_breaks <- seq(0,15,by=5) # [1]  0  5 10 15
fl_breaks <- seq(100,500,by=100) # [1]   0 100 200 300 400 500


# Step 3 : Classify the ages according to intervals
age_frequency <- table(cut(fil_BullTroutRML2$age, c(-1,age_breaks)))
fl_frequency <- table(cut(fil_BullTroutRML2$fl, c(0,fl_breaks)))


# Step 4: Calculate cumulative frequency
age_cum_frequency <- cumsum(age_frequency)
fl_cum_frequency <- cumsum(fl_frequency)

# Plot cumulative frequency ogive graph for age
plot(age_breaks, age_cum_frequency,
      main = "Plot 2: Cumulative Frequency Distribution",
      xlab = "Age (yrs)",
      ylab = "Cumulative Frequency",
      xlim =c(0,15), 
      ylim =c(0,70),
      cex = 1,
      col = "cadetblue",
      col.main = 'cadetblue')
lines(age_breaks, age_cum_frequency)
text(age_cum_frequency~age_breaks, label = age_cum_frequency, cex = 0.8, font =1, col =2, adj =c(0,0))

# Plot cumulative frequency ogive graph for fl
plot(fl_breaks, fl_cum_frequency,
     main = "Plot 4: Cumulative Frequency Distribution",
     xlab = "Fork Length (mm)",
     ylab = "Cumulative Frequency",
     xlim =c(0,500), 
     ylim =c(0,70),
     cex = 1,
     col = "cadetblue",
     col.main = 'cadetblue')
lines(fl_breaks, fl_cum_frequency)
text(fl_cum_frequency~fl_breaks, label = fl_cum_frequency, cex = 0.8, font =1, col =2, adj =c(0,0))


# Display first & last 5 records from filtered dataset
headtail(fil_BullTroutRML2,5)

# Display structure of filtered dataset.
str(fil_BullTroutRML2)

# Display summary of filtered dataset.
summary(fil_BullTroutRML2)

# Calculate standard deviation of age & fl

sd(fil_BullTroutRML2$age) # 3.334945
sd(fil_BullTroutRML2$fl) # 128.6174

# Calculate variance of age & fl

var(fil_BullTroutRML2$age) #11.12186
var(fil_BullTroutRML2$fl)  #16542.43

# Calculate Skewness & kurtosis

skewness(fil_BullTroutRML2$age) # 0.1677228
skewness(fil_BullTroutRML2$fl)  # -0.7353215

kurtosis(fil_BullTroutRML2$age) # 2.357344
kurtosis(fil_BullTroutRML2$fl) # 2.345694



# Load ggplot package
library("ggplot2")

plot(fil_BullTroutRML2$fl, 
     fil_BullTroutRML2$age,
     type ="p", 
     xlim =c(0,500), 
     ylim =c(0,15),
     main = "Plot 5: Harrison Lake Trout",
     xlab = "Fork Length (mm)",
     ylab = "Age (yrs)",
     pch = 1,
     cex = 0.5
)

# Plot an "Age" histogram

hist(fil_BullTroutRML2$age, 
     main = "Plot 1: Harrison Fish Age Distribution",
     xlim = c(0,15),
     ylim = c(0,15),
     xlab ="Age (yrs)",
     ylab ="Frequency",
     col = "cadetblue",
     col.main = 'cadetblue'
     )

# Plot an "FL" histogram

hist(fil_BullTroutRML2$fl, 
     main = "Plot 5: Harrison Fish length(fl) Distribution",
     xlim = c(0,500),
     ylim = c(0,25),
     xlab ="Fork Length (mm)",
     ylab ="Frequency",
     col = "cadetblue",
     col.main = 'cadetblue'
)


# Create an overdense plot.

ggplot(fil_BullTroutRML2, aes(fl,age,color =era))+
    geom_point(size =1)+
    labs(title ="Plot 6: Harrison Density Shaded by Era",
         x="Fork Length (mm)",y="Age (yrs)")+
    theme_bw()+
    scale_x_continuous(breaks = seq(0, 500, 100))+
    scale_y_continuous(breaks = seq(0, 15, 5))+
    coord_cartesian(ylim = c(0, 20))

# Create a new object tmp and include first & last 3 records from dataset

tmp <- headtail(BullTroutRML2,3)


# Display era column in new tmp object
tmp$era

# Create a pchs vector with arguments + and x
pchs <- c("+","x")
pchs

# Create a cols vector with two elements "red" and "gray60"
cols <- c("red","gray60")
cols

# Convert tmp era values to numeric
tmp$era <- as.numeric(tmp$era)
tmp$era

#Initialize cols vector with tmp
cols <-tmp$era
cols

plot(tmp$fl,
     tmp$age,
     main="Plot 7: Symbol & Color by Era",
     xlim=c(0,500), 
     ylim=c(0,15),
     ylab="Fork Length (mm)", 
     xlab="Age (yrs)", 
     pch=pchs,
     col= cols)

# Plot regression line overlay graph

plot(tmp$fl,
     tmp$age,
     main="Plot 8: Regression Overlay",
     xlim=c(0,500), 
     ylim=c(0,15),
     ylab="Fork Length (mm)", 
     xlab="Age (yrs)", 
     pch=pchs,
     col=cols,
     abline(lm(tmp$age~tmp$fl),col = 'blue'))

# Plot legend overlay

plot(tmp$fl,
     tmp$age,
     main="Plot 9: :Legend Overlay",
     xlim=c(0,500), 
     ylim=c(0,15),
     ylab="Fork Length (mm)", 
     xlab="Age (yrs)", 
     pch=pchs,
     col= cols)
     abline(lm(tmp$age~tmp$fl),col = 'blue')
     legend("bottomleft", 
             legend=c("GREY = 1977-80","RED = 1997-01"),
             bg ='lightblue',
             col=c(1,2),
             cex =0.5)