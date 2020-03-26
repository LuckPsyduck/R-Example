
#第二章 2010.03.05#
a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
a[3]
a[c(1, 3, 5)]
a[2:6]

y <- matrix(1:20, nrow = 5, ncol = 4)

cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(rnames, cnames))

mymatrix <- matrix(cells, nrow = 2, ncol = 2, byrow = FALSE, dimnames = list(rnames, cnames))

x <- matrix(1:10,, nrow = 2)
x[ ,2]
x[2, ]
x[1, 4]

dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata[1:2]
patientdata[c("diabetes", "status")]
patientdata$age
table(patientdata$diabetes, patientdata$status)

summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)

attach(mtcars)
	summary(mpg)
	plot(mpg, disp)
	plot(mpg, wt)
detach(mtcars)

with(mtcars, {
	summary(mpg, disp, wt)
	plot(mpg, disp)
	plot(mpg, wt)
	}
)

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order = TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c("one", "two", "three")
mylist <- list(title = g, ages = h, j, k)

# 第三章 2020.03.04#
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight")
detach(mtcars)

pdf("mygraph.pdf")
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight")
detach(mtcars)
dev.off()

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type = "b")

opar <- par(no.readonly = TRUE)
par(lty = 2, pch = 17) #虚线、实心三角
plot(dose, drugA, type = "b")
#plot(dose, drugA, type = "b", lty = 2, pch = 17)
par(opar)

n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels = mycolors, col = mycolors)
mygrays <- gray(0:n / n)
pie(rep(1, n), labels = mygrays, col = mygrays)

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
par(pin = c(2, 3))
par(lwd = 2, cex = 1.5)
par(cex.axis = 0.75, font.axis = 3)
plot(dose, drugA, type = "b", pch = 19, lty = 2, col = "red")
plot(dose, drugB, type = "b", pch = 23, lty = 6, col = "blue", bg = "green")
par(opar)

plot(dose, drugA, type = "b", col = "red", lty = 2, pch = 2, lwd = 2,
	main = "Clinical Trials for Drug A", 
	sub = "This is hypothetical data",
	xlab = "Dosage", ylab = "Drug Response",
	xlim = c(0, 60), ylim = c(0, 70)
	)

x <- c(1: 10)
y <- x
z <- 10 / x
opar <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 8) + 0.1)
plot(x, y, type= "b", pch = 21, col = "red", yaxt = "n", lty = 3, ann = FALSE)
lines(x, z, type = "b", pch = 22, col = "blue", lty = 2)
axis(2, at = x, labels = x, col.axis = "red", las = 2)
axis(4, at = z, labels = round(z, digits = 2), col.axis = "blue", las = 2, cex.axis = 0.7, tck = -0.01)
mtext("y = 1 / x", side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")
title("An Example of Creative Axes", xlab = "X values", ylab = "Y = X")
par(opar)

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
par(lwd = 2, cex = 1.5, font.lab = 2)
plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red", ylim = c(0, 60),
	main = "Drug A vs Drug B", xlab = "Drug Dosage", ylab = "Drug Response"
	)
lines(dose, drugB, type = "b", pch = 17, lty = 2, col = "blue")
abline(h = c(30), lwd = 1.5, lty = 2, col = "gray")
library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
legend("topleft", inset = 0.05, title = "Drug Type", c("A", "B"), lty = c(1, 2), pch = c(15, 17), col = c("red", "blue"))
par(opar)

attach(mtcars)
plot(wt, mpg, 
	main = "Mileage vs Car Weight", xlab = "Weight", ylab = "Mileage", pch = 18, col = "blue")
text(wt, mpg, row.names(mtcars), cex = 0.6, pos = 4, col = "red")
detach(mtcars)

attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(wt, mpg, main = "Scatterplot of wt vs mpg")
plot(wt, disp, main = "Scatterplot of wt vs disp")
hist(wt, main = "Histogram of wt")
boxplot(wt, main = "Boxplot of wt")
par(opar)
detach(mtcars)

attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 1))
hist(wt)
hist(mpg)
hist(disp)
par(opar)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), widths = c(3, 1), heights = c(1, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

opar <- par(no.readonly = TRUE)
par(fig = c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg, xlab = "Miles Per Gallon", ylab = "Car Weight")
par(fig = c(0, 0.8, 0.55, 1), new = TRUE)
boxplot(mtcars$wt, horizontal = TRUE, axes = FALSE)
par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
boxplot(mtcars$mpg, axes = FALSE)
mtext("Enhanced Scatterplot", side = 3, outer = TRUE, line = -3)
par(opar)

#第四章 2020.03.05#
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 2, NA, 2)
q5 <- c(5, 5, 2, NA, 1)

leadership <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors = FALSE)

mydata <- data.frame(x1 = c(2, 2, 6, 4), x2 = c(3, 4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2) / 2

attach(mydata)
	mydata$sumx <- x1 + x2
	mydata$meanx <- (x1 + x2) / 2
detach(mydata)

mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2) / 2)

leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 & leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"

leadership <- within(leadership, {
	agecat <- NA
	agecat[age > 75] <- "Elder"
	agecat[age >= 55 & age <= 75] <- "Middle Aged"
	agecat[age < 55] <- "Young"
	}) 

y <- c(1, 2, 3, NA)
is.na(y)

is.na(leadership[, 6:10])

x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4]
z <- sum(x)

x <- c(1, 2, NA, 3)
y <- sum(x, na.rm = TRUE)

newdata <- na.omit(leadership)

today <- Sys.Date()
dob <- as.Date("1956-10-12")
difftime(today, dob, units = "weeks")

a <- c(1, 2, 3)
is.numeric(a)
is.vector(a)

a <- as.character(a)
is.numeric(a)
is.vector(a)
is.character(a)

attach(leadership)
newdata <- leadership[order(gender, age), ]
detach(leadership)

attach(leadership)
newdata <- leadership[order(gender, -age), ]
detach(leadership)

newdata <- leadership[1:3, ]
newdata <- leadership[which(leadership$gender == "M" & leadership$age > 30), ]
attach(leadership)
newdata <- leadership[which(gender == "M" & age > 30), ]
detach(leadership)

newdata <- subset(leadership, age >= 35 | age < 24, select = c(q1, q2, q3, q4))
newdata <- subset(leadership, gender = "M" & age > 25, select = gender : q4)

#第五章 2020.03.06
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
mean(x)
sd(x)
n <- length(x)
meanx <- sum(x) / n
css <- sum((x - meanx) ^ 2)
sdx <- sqrt(css / (n - 1))

runif(5)
runif(5)
set.seed(1234)
runif(5)
set.seed(1234)
runif(5)

library(MASS)
options(digits = 3)
set.seed(1234)
mean <- c(230.7, 146.7, 3.6)
sigma <- matrix(c(15360.8, 6721.2, -47.1,
	6721.2, 4700.9, -16.5, 
	-47.1, -16.5, 0.3
	), nrow = 3, ncol = 3)
mydata <- mvrnorm(500, mean, sigma)
mydata <- as.data.frame(mydata)
names(mydata) <- c("y", "x1", "x2")
dim(mydata)
head(mydata, n = 10)

sub("\\s", ".", "Hello There")

y <- strsplit("abc", "")

paste("x", 1:3, sep = "")
paste("x", 1:3, sep = "M")
paste("Today is", date())

toupper("abc")
tolower("ABC")

x <- c(2, 3 ,5, 6)
length(x)

indices <- seq(1, 10, 2)

y <- rep(1:3, 2)

a <- 5
sqrt(a)

b <- c(1.243, 5.654, 2.99)
round(b)

c <- matrix(runif(12), nrow = 3)
log(c)
mean(c)

mydata <- matrix(rnorm(30), nrow = 6)
apply(mydata, 1, mean)
apply(mydata, 2, mean)
apply(mydata, 2, mean, trim = 0.2)

options(digits = 2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
	"David Jones", "Janice Markhammer", "Cheryl Cushing", 
	"Reuven Ytzrhak", "Grep Knox", "Joel England", "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English, stringsAsFactors = FALSE)
z <- scale(roster[, 2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)
y <- quantile(score, c(0.8, 0.6, 0.4, 0.2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"
name <- strsplit((roster$Student), " ")
lastname <- sapply(name, "[", 2)
firstname <- sapply(name, "[", 1)
roster <- cbind(firstname, lastname, roster[, -1])
roster <- roster[order(lastname, firstname), ]
roster

feelings <- c("sad", "afraid")
for(i in feelings)
	print(
		switch(i, 
		happy = "I am glad you are happy",
		afraid = "There is nothing to fear",
		sad = "Cheer up",
		angry = "Calm down now"
			)
		)

mystats <- function(x, parametric = TRUE, print = FALSE)
{
	if(parametric)
	{
		center <- mean(x); spread <- sd(x)
	}
	else
	{
		center <- median(x); spread <- mad(x)
	}
	if(print & parametric)
	{
		cat("Mean = ", center, "\n", "SD = ", spread, "\n")
	}
	else if(print & !parametric)
	{
		cat("Median = ", center, "\n", "MAD = ", spread, "\n")
	}
	result <- list(center = center, spread = spread)
	return(result)
}
set.seed(1234)
x <- rnorm(500)
y <- mystats(x)
y <- mystats(x, parametric = FALSE, print = TRUE)

mydata <- function(type = "long")
{
	switch(type, 
		long = format(Sys.time(), "%A %B %d %Y"),
		short = format(Sys.time(), "%m-%d-%y"),
		cat(type, "is not a recognized type\n")
		)
}
mydata("long")
mydata("short")
mydata("medium")

cars <- mtcars[1:5, 1:4]
cars
t(cars)

options(digits = 3)
attach(mtcars)
aggdata <- aggregate(mtcars, by = list(cyl, gear), FUN = mean, na.rm = TRUE)
aggdata

#第六章 2020.03.07#
library(vcd)
counts <- table(Arthritis$Improved)
counts
barplot(counts, main = "Simple Bar Plot", 
	xlab = "Improvement", ylab = "Frequency")
barplot(counts, main = "horizontal Bar Plot",
	xlab = "Frequency", ylab = "Improvement", horiz = TRUE)

library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts
barplot(counts, main = "Stacked Bar Plot", 
	xlab = "Treatment", ylab = "Frequency",
	col = c("red", "yellow", "green"), legend = rownames(counts))
barplot(counts, main = "Grouped Bar plot", xlab = "Treatment", ylab = "Frequency",
	col = c("red", "yellow", "green"), legend = rownames(counts), beside = TRUE)

states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by = list(state.region), FUN = mean)
means

means <- means[order(means$x), ]
means
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

par(mar = c(5, 8, 4, 2))
par(las = 2)
counts <- table(Arthritis$Improved)
barplot(counts, main = "Treatment Outcome", horiz = TRUE, 
	cex.name = 0.8, names.arg = c("No Improvement", "Some Improvement", "Marked Improvement"))

library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)

par(mtrow = c(2, 2))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main = "Simple Pie Chart")
pct <- round(slices / sum(slices) * 100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col = rainbow(length(lbls2)), main = "Pie Chart with Percentages")
library(plotrix)
pie3D(slices, labels = lbls, explode = 0.1, main = "3D Pie Chart")
mytable <- table(state.region)
lbls3 <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls3, main = "Pie Chart from a Table \n(with sample sizes)")

par(mfrow = c(2, 2))
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 12, col = "red", 
	xlab = "Miles Per Gallon", main = "colored Histogram with 12 bins")
hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red", xlab = "Miles Per Gallon", 
	main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = "blue", lwd = 2)

par(mfrow = c(2, 1))
d <- density(mtcars$mpg)
plot(d)
d <- density(mtcars$mpg)
plot(d, main = "Kernel Density of Miles Per Gallon")
polygon(d, col = "red", border = "blue")
rug(mtcars$mpg, col = "brown")

par(lwd = 2)
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels = c(4, 6, 8), labels = c("4 cylinder", "6 cylinder", "8 cylinder"))
sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")
title(main = "MPG Distribution by Car Cylinders")
colfill <- c(2, (1 + length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill = colfill)
detach(mtcars)

boxplot(mpg ~ cyl, data = mtcars, main = "Car Mileage Data",
 xlab = "Number of Cylinders", vlab = "Miles Per Gallon")

mtcars$cyl.f <- factor(mtcars$cyl, levels = c(4, 6, 8), labels = c("4", "6", "8"))
mtcars$am.f <- factor(mtcars$am, levels = c(0, 1), labels = c("auto", "standard"))
boxplot(mpg ~ am.f * cyl.f, data = mtcars, varwidth = TRUE, col = c("gold", "darkgreen"), 
	main = "MPG Distribution by Auto Type", xlab = "Auto Type")

library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x1, x2, x3, names = c("4 cy1", "6 cy1", "8 cy1"), col = "gold")
title("violin Plots of Miles Per Gallon")

x <- mtcars[order(mtcars$mpg), ]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, labels = row.names(x),
	cex = 0.7, groups = x$cy1, gcolor = "black", color =x$color, pch = 19, 
	main = "Gas Mileage for Car Models\ngrouped by cylinder", xlab = "Miles Per Gallon")

#第七章2020.03.08#
vars <- c("mpg", "hp", "wt")
head(mtcars[vars])
summary(mtcars[vars])

mystats <- function(x, na.omit = FALSE)
{
	if(na.omit)
		x <- x[!is.na(x)]
	m <- mean(x)
	n <- length(x)
	s <- sd(x)
	skew <- sum((x - m) ^ 3 / s ^ 3)
	kurt <- sum((x - m) ^ 4 / s ^ 4) / n - 3
	return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}
sapply(mtcars[vars], mystats)

library(Hmisc)
describe(mtcars[vars])

library(psych)
describe(mtcars[vars])

aggregate(mtcars[vars], by = list(am = mtcars$am), mean)
aggregate(mtcars[vars], by = list(am = mtcars$am), mean)

dstats <- function(x)
{
	c(mean = mean(x), sd = sd(x))
}
by(mtcars[vars], mtcars$am, dstats)

library(doBy)
summaryBy(mpg + hp + wt ~ am, data = mtcars, FUN = mystats)

library(psych)
describe.by(mtcars[vars], mtcars$am)

library(reshape)
dstats <- function(x)
{
	c(n = length(x), mean = mean(x), sd = sd(x))
}
dfm <- melt(mtcars, measure.vars = c("mpg", "hp", "wt"), id.vars = c("am", "cyl"))
cast(dfm, am + cyl + variable ~ ., dstats)

library(vcd)
head(Arthritis)

mytable <- with(Arthritis, table(Improved))
mytable
prop.table(mytable)

mytable <- xtabs(~ Treatment + Improved, data = Arthritis)
mytable
margin.table(mytable, 1)
prop.table(mytable, 1)
margin.table(mytable, 2)
prop.table(mytable, 2)
prop.table(mytable)
addmargins(mytable)
addmargins(prop.table(mytable))

library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

mytable <- xtabs(~ Treatment + Sex + Improved, data = Arthritis)
mytable

library(vcd)
mytable <- xtabs(~ Treatment + Improved, data = Arthritis)
chisq.test(mytable)
fisher.test(mytable)
mytable <- xtabs(~ Improved + Sex, data = Arthritis)
chisq.test(mytable)
fisher.test(mytable)

states <- state.x77[, 1:6]
cov(states)
cor(states)
cor(states, method = "spearman")
x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cor(x, y)

#第八章 2020.03.09#
fit <- lm(weight ~ height, data = women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight, xlab = "Height", ylab = "Weight")
abline(fit)

fit2 <- lm(weight ~ height + I(height ^ 2), data = women)
summary(fit2)
plot(women$height, women$weight, xlab = "Height", ylab = "Weight")
lines(women$height, fitted(fit2))

library(car)
scatterplot(weight ~ height, data = women, spread = FALSE, lty.smooth = 2,
	pch = 19, main = "Women Age 30-39", xlab = "Height", ylab = "Weight")

states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, spread = FALSE, lty.smooth = 2, main = "Scatter Plot Matrix")

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fit)

fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)
library(effects)
plot(effect("hp:wt", fit, list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
confint(fit)

fit <- lm(weight ~ height, data = women)
par(mfrow = c(2, 2))
plot(fit)

fit2 <- lm(weight ~ height + I(height ^ 2) ,data = women)
par(mfrow = c(2, 2))
plot(fit2)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
par(mfrow = c(2, 2))
plot(fit)

library(car)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit, labels = row.names(states), id.method = "identify", simulate = TRUE, main = "Q-Q plot")
states["Nevada"]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]

residplot <- function(fit, nbreaks = 10)
{
	z <- rstudent(fit)
	hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residual", main = "Distribution of Errors")
	rug(jitter(z), col = "brown")
	curve(dnorm(x, mean = mean(x), sd = sd(z)), add = TRUE, col = "blue", lwd = 2)
	lines(density(z)$x, density(z)$y, col = "red", lwd = 2, lty = 2)
	legend("topright", lengend = c("Normal Curve", "Kernel Density Curve"), lty = 1:2, 
		col = c("blue", "red"), cex = 0.7)
}
residplot(fit)

fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
anova(fit2, fit1)
AIC(fit1, fit2)

library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
stepAIC(fit1, direction = "backward")

#第九章2020.03.09#
library(multcomp)
attach(cholesterol)
table(trt)
aggregate(response, by = list(trt), FUN = mean)
aggregate(response, by = list(trt), FUN = sd)
fit <- aov(response ~ trt)
summary(fit)
library(gplots)
plotmeans(response ~ trt, xlab = "Treatment", ylab = "Response", main = "Mean Plot\twith 95% CI")
detach(cholesterol)

library(car)
qqPlot(lm(response ~ trt, data = cholesterol), simulate = TRUE, main = "Q-Q Plot", labels = FALSE)
bartlett.test(response ~ trt, data = cholesterol)

library(car)
outlierTest(fit)

data(litter, package = "multcomp")
attach(litter)
table(dose)
aggregate(weight, by = list(dose), FUN = mean)
fit <- aov(weight ~ gesttime + dose)
summary(fit)

library(effects)
effect("dose", fit)

library(multcomp)
contrast <- rbind("no drug vs . drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct = mcp(dose = contrast)))

library(multcomp)
fit2 <- aov(weight ~ gesttime * dose, data = litter)
summary(fit2)

library(HH)
ancova(weight ~ gesttime + dose, data = litter)

attach(ToothGrowth)
table(supp, dose)
aggregate(len, by = list(supp, dose), FUN = mean)
aggregate(len, by = list(supp, dose), FUN = sd)
fit <- aov(len ~ supp * dose)
summary(fit)
interaction.plot(dose, supp, len, type = "b", col = c("red", "blue"), pch = c(16, 18),
	main = "interaction between Dose and Supplement Type")

w1b1 <- subset(CO2, Treatment == "chilled")
fit <- aov(uptake ~ conc * Type + Error(Plant / (conc)), w1b1)
summary(fit)

par(las = 2)
par(mar = c(10, 4, 4 ,2))
with(w1b1, interaction.plot(conc, Type, uptake, type = "b", col = c("red", "blue"), pch = c(16, 18), 
	main = "interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type * conc, data = w1b1, col = c("gold", "green"), 
	main = "Chilled Quebec and Mississippi Plants", ylab = "Carbon dioxide uptake rate")

library(MASS)
attach(UScereal)
y <- cbind(calories, fat, sugars)
aggregate(y, by = list(shelf), FUN = mean)
cov(y)
fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit)

library(multcomp)
levels(cholesterol$trt)
fit.aov <- aov(response ~ trt, data = cholesterol)
summary(fit.aov)

fit.lm <- lm(response ~ trt, data = cholesterol)
summary(fit.lm)

#第十章2020.03.10#
library(pwr)
pwr.t.test(d = 0.8, sig.level = 0.05, power = 0.9, type = "two.sample", alternative = "two.sided")

pwr.t.test(n = 20, d = 0.5, sig.level = 0.01, type = "two.sample", alternative = "two.sided")

pwr.anova.test(k = 5, f = 0.25, sig.level = 0.05, power = 0.8)

pwr.r.test(r = 0.25, sig.level = 0.05, power = 0.90, alternative = "greater")

pwr.f2.test(u = 3, f2 = 0.0769, sig.level = 0.05, power = 0.90)

pwr.2p.test(h = ES.h(0.65, 0.6), sig.level = 0.05, power = 0.9, alternative = "greater")

library(pwr)
r <- seq(0.1, 0.5, 0.01)
nr <- length(r)
p <- seq(0.4, 0.9, 0.1)
np <- length(p)
samsize <- array(numeric(nr * np), dim = c(nr, np))
for(i in 1:np)
	for(j in 1:nr)
	{
		result <- pwr.r.test(n = NULL, r = r[j], sig.level = 0.05, power = p[i],
			alternative = "two.sided")
		samsize[j, i] = ceiling(result$n)
	}
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type = "n", xlab = "Correlation Coefficient (r)", ylab = "Sample Size (n)")
for(i in 1:np)
{
	lines(r, samsize[, i], type = "l", lwd = 2, col = colors[i])
}
abline(v = 0, h = seq(0, yrange[2], 50), lty = 2, col = "grey89")
abline(h = 0, v = seq(xrange[1], xrange[2], 0.02), lty = 2, col = "gray89")
title("Sample Size Estimation for Correlation Studies\n Sig = 0.05 (Two - tailed)")
legend("topright", title = "Power", as.character(p), fill = colors)

#第十一章2020.03.10#
attach(mtcars)
plot(wt, mpg, main = "Basic Scatter plot of MPG vs . Weight", 
	xlab = "Car Weight (1bs / 1000)", ylab = "Miles Per Gallon", pch = 19)
abline(lm(mpg ~ wt), col = "red", lwd = 2, lty = 1)
lines(lowess(wt, mpg), col = "blue", lwd = 2, lty = 2)

library(car)
scatterplot(mpg ~ wt | cyl, data = mtcars, lwd = 2, main = "Scatter Plot of MPG vs . Weight by #Cylinders",
	xlab = "Weight of Car (1bs/1000)",
	ylab = "Miles Per Gallon", legend.plot = TRUE, id.method = "identify", labels = row.names(mtcars), 
	boxplot = "xy")
pairs(~ mpg + disp + drat + wt, data = mtcars, main = "Basic Scatter Plot Matrix")

library(car)
scatterplotMatrix(~ mpg + disp + drat + wt, data = mtcars, spread = FALSE, 
	lty.smooth = 2, main = "Scatter Plot Matrix via car Package")

library(car)
scatterplotMatrix(~ mpg + disp + drat + wt | cyl, data = mtcars, spread = FALSE, diagonal = "histogram",
	main = "Scatter Plot Matrix via car Package")
cor(mtcars[c("mpg", "wt", "disp", "drat")])

library(gclus)
mydata <- mtcars[c(1, 3, 5, 6)]
mydata.corr <- abs(cor(mydata))
mycolors <- dmat.color(mydata.corr)
myorder <- order.single(mydata.corr)
cpairs(mydata, myorder, panel.colors = mycolors, gap = 0.5, 
	main = "Variables Ordered and Colored by Correlation")

set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean = 0, sd = 0.5), ncol = 2)
c2 <- matrix(rnorm(n, mean = 3, sd = 2), ncol = 2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
with(mydata, 
	plot(x, y, pch = 19, main = "Scatter Plot with 10000 Observations")
	)
with(mydata, smoothScatter(x, y, 
	main = "Scatterplot Colored by Smoothed Densities"))

library(hexbin)
with(mydata, {
	bin <- hexbin(x, y, xbins = 50)
	plot(bin, main = "Hexagonal Binning with 10000 Observations")
		}
	)

library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, main = "Basic 3D Scatter Plot")

library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, pch = 16, highlight.3d = TRUE, type = "h", main = 
	"3D Scatter Plot with Vertical Lines")

library(scatterplot3d)
attach(mtcars)
s3d <- scatterplot3d(wt, disp, mpg, pch = 16, highlight.3d = TRUE, type = "h", main = 
	"3D Scatter Plot with Vertical Lines")
fit <- lm(mpg ~ wt + disp)
s3d$plane3d(fit)

attach(mtcars)
r <- sqrt(disp / pi)
symbols(wt, mpg, circle = r, inches = 0.30, 
	fg = "white", bg = "lightblue", main = 
	"Bubble Plot with point size proportional to displacement", ylab = "Miles Per Gallon", 
	xlab = "Weight of Car ")
text(wt, mpg, rownames(mtcars), cex = 0.6)
detach(mtcars)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
t1 <- subset(Orange, Tree == 1)
plot(t1$age, t1$circumference, xlab = "Age (days)", ylab = "circumference (mm)",
	main = "Orange Tree 1 Growth")
plot(t1$age, t1$circumference, xlab = "Age (days)", ylab = "circumference (mm)",
	main = "Orange Tree 1 Growth", type = "b")
par(opar)

options(digits = 2)
cor(mtcars)
library(corrgram)
corrgram(mtcars, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, 
	text.panel = panel.txt, main = "Correlogram of mtcars intercorrelations")

ftable(Titanic)
library(vcd)
mosaic(Titanic, shade = TRUE, legend = TRUE)
mosaic(~ Class + Sex + Age + Survived, data = Titanic, shade = TRUE, legend = TRUE)

#第十二章2020.03.11#
library(coin)
score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
treatment <- factor(c(rep("A", 5), rep("B", 5)))
mydata <- data.frame(treatment, score)
t.test(score ~ treatment, data = mydata, var.equal = TRUE)
oneway_test(score ~ treatment, data = mydata, distribution = "exact")

library(MASS)
UScrime <- transform(UScrime, So = factor(So))
wilcox_test(Prob ~ So, data = UScrime, distribution = "exact")

library(multcomp)
set.seed(1234)
oneway_test(response ~ trt, data = cholesterol, distribution = approximate(B = 9999))

library(coin)
library(vcd)
Arthritis <- transform(Arthritis, Improved = as.factor(as.numeric(Improved)))
set.seed(1234)
chisq_test(Treatment ~ Improved, data = Arthritis, distribution = approximate(B = 9999))

states <- as.data.frame(state.x77)
spearman_test(Illiteracy ~ Murder, data = states, distribution = approximate(B = 9999))

library(coin)
library(MASS)
wilcoxsign_test(U1 ~ U2, data = UScrime, distribution = "exact")

library(lmPerm)
set.seed(1234)
fit <- lmp(weight ~ height, data = women, perm = "Prob")
summary(fit)

library(lmPerm)
set.seed(1234)
fit <- lmp(weight ~ height + I(height ^ 2), data = women, perm = "Prob")
summary(fit)

library(lmPerm)
set.seed(1234)
states <- as.data.frame(state.x77)
fit <- lmp(Murder ~ Population + Illiteracy + Income + Frost, data = states, perm = "Prob")
summary(fit)

library(lmPerm)
library(multcomp)
set.seed(1234)
fit <- aovp(response ~ trt, data = cholesterol, perm = "Prob")
summary(fit)

library(lmPerm)
set.seed(1234)
fit <- aovp(weight ~ gesttime + dose, data = litter, perm = "Prob")
summary(fit)

library(lmPerm)
set.seed(1234)
fit <- aovp(len ~ supp * dose, data = ToothGrowth, perm = "Prob")
summary(fit)


rsq <- function(formula, data, indices){
	d <- data[indices, ]
	fit <- lm(formula, data = d)
	return(summary(fit)$r.square)
}
library(boot)
set.seed(1234)
results <- boot(data = mtcars, statistic = rsq, R = 1000, formula = mpg ~ wt + disp)
print(results)
plot(results)
boot.ci(results, type = c("perc", "bca"))

bs <- function(formula, data, indices){
	d <- data[indices, ]
	fit <- lm(formula, data = d)
	return(coef(fit))
}
library(boot)
set.seed(1234)
results <- boot(data = mtcars, statistic = bs, R = 1000, formula = mpg ~ wt + disp)
print(results)
plot(results, index = 2)
boot.ci(results, type = "bca", index = 2)
boot.ci(results, type = "bca", index = 3)
