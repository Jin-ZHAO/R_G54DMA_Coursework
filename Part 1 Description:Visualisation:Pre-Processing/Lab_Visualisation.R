######################################################################
###### LAB 5: (SELECTED) SOLUTIONS BY DR MERCEDES TORRES TORRES ######
######################################################################

# Contact: mercedes.torrestorres@nottingham.ac.uk

######################################################################
#DISCLAIMER:                                                         #
# This document presents a set of possible solutions.                #  
# HOWEVER, these solutions are not unique, and you might have found  #
#a different one.                                                    #  
######################################################################

library("ggplot2")
library("ggrepel")
library("grid")
library("dplyr")

########################################
######A. BASIC VISUALISATION IN R ######
########################################

ir=iris

#3. Create a dotplot that groups samples into their species and shows how the petal lengths of all instances vary. Each group should have a different colour of your choosing.
dotplot_ir = function(ir){
  x = ir[order(ir$Petal.Length),] # sort by Length
  x$cyl = factor(x$Species) # it must be a factor
  x$color[x$cyl=='setosa'] = "red"
  x$color[x$cyl=='virginica'] = "blue"
  x$color[x$cyl=='versicolor'] = "darkgreen"	
  dotchart(x$Petal.Length,labels=row.names(x),cex=.7,groups= x$cyl,
           main="Petal Length \ngrouped by Plant type",
           xlab="cm", gcolor="black", color=x$color)
}


print(dotplot_ir)


########################################
#####B.ADVANCED VISUALISATION IN R #####
########################################

dat = read.csv("team.csv")
head(dat) #Some attributes have weird names
names(dat)[starts_with(match = "Height", vars=names(dat))]="Height"
names(dat)[starts_with(match = "Weig", vars=names(dat))]="Weight"
names(dat)[starts_with(match = "Spe", vars=names(dat))]="Speed"
head(dat) #nicer to look at now

#1.Create a histogram of that shows the age distribution from all players at intervals of 1 year.
ggplot(dat, aes(x=Age)) + scale_x_continuous(breaks = round(seq(min(dat$Age), max(dat$Age), by = 1),1)) +geom_histogram(binwidth=1)+ggtitle("Age distribution of all players") +ylab("Frequency")

#2. Create a histogram that shows the salary distribution of players that are Defenders. Choose a sensible interval
ggplot(subset(dat, Position %in% c("Defender")), aes(x=Salary)) + geom_histogram(bins=35)+ scale_x_continuous(breaks = seq(min(dat$Salary), max(dat$Salary), by = 5000))+ggtitle("Distribution of salary for Defenders") +ylab("Frequency")

#3. Repeat the previous histogram, but grouping the data according to Gender.
ggplot(subset(dat, Position %in% c("Defender" )), aes(x=Salary, fill=Gender)) + geom_histogram(bins=35)+ scale_x_continuous(breaks = seq(min(dat$Salary), max(dat$Salary), by = 5000))+ggtitle("Distribution of salary for Defenders")+ylab("Frequency")

#4. Create a histogram that shows the age distribution of Forward players grouped into teams. Choose a sensible age interval.
ggplot(subset(dat, Position %in% c("Forward")), aes(x=Age, fill=Team)) + geom_histogram(binwidth=1) +ggtitle("Distribution of Age by Teams") +ylab("Frequency") 

#5. Create a scatterplot that shows the relationship between speed and height of players from Bim, Dragon Island and Calormen, grouped by Gender
ggplot(subset(dat, Team %in% c("Bism", "Dragon Island", "Calormen")), aes(x=Speed, y=Height, color=Gender)) + geom_point(size = 1, stroke = 1.5) + geom_smooth(method = "lm", se=TRUE) + ggtitle("Height vs Speed according to Gender") 

#6. Create a graph with separate scatterplots that show Weight vs Height of players grouped into their teams. Separate each scatterplot according to Gender.
ggplot(dat, aes(x=Weight, y=Height)) + geom_point (aes(color = Team), shape = 2,stroke = 1.5) + facet_grid(. ~ Gender, labeller = label_both) +ggtitle("NARNIA'S NATIONAL TEAM INFORMATION SEPARATED ACCORDING TO GENDER")

#7. Apply the functions and exercises done previously to replicate the graph shown in Figure 2
#I've divided this into different steps
pc2 = ggplot(dat, aes(x=Weight, y=Height, color=Team, shape = Gender)) + geom_point(size = 4, stroke = 1.5) + scale_shape_manual(values = c(23,24))
pc3 = pc2 + geom_text_repel(aes(label=Speed), color = "gray20", data = subset(dat, Position %in% c("Forward")), force=5)
pc4 = pc3 + ggtitle("NARNIA'S NATIONAL TEAM INFORMATION") + scale_x_continuous(name = "Weight of all players", limits=c(40,85), breaks=seq(40, 85, by=5)) + scale_y_continuous(name = "Height of all players", limits=c(1.49,2.01), breaks=seq(1.49, 2.01, by=0.1))
pc4


########################################
###C.CHOSING THE RIGHT VISUALISATION ###
########################################

movs=read.csv(file="ratings_final.csv", header=TRUE, stringsAsFactors = FALSE)

#Some tidying up - this is not necessary and hasn't been asked in the exercise, I just don't like that one genre has parenthesis and the others don't
movs[movs$movieGenre=="(no genres listed)",]$movieGenre="Unknown"

head(movs)

#3. Use visualisation to answer the following questions:

#a) What is the distribution of ratings for all movie genres? 

#A histogram, or a trend line would be great if we had fewer genres. Something like this:
ggplot(movs, aes(x=rating, color=movieGenre))+geom_density() 
#However, this becomes too busy when we have multiple populations. You can use facets as alternatives:
ggplot(movs) + geom_histogram(bins=25,aes(x=rating, fill=movieGenre)) + facet_wrap(~movieGenre) + ggtitle("Distribution by Genre")+ylab("Frequency")

#d) Do a monthly comparison of the number of reviews from Horror movies and Animations uploaded
movs = read.csv("ratings_final.csv")

#Checks on the Date field
class(movs$date) #Not the class that I need
movs$date=as.Date(movs$date, "%d/%m/%y")
class(movs$date) #Now it is a date

#Reformat date 
movs$date=format(movs$date, "%Y/%m") 

#Get the genres that I want
movs=movs[movs$movieGenre=="Horror" |movs$movieGenre=="Animation",]

#Calculate how many reviews per movieGenre and per date. 
s=summarize(group_by(movs, movieGenre,date), tot=n())

#Order according to date
s= s[order(s$date),]

#This has waaaay too many points, but it was good for practice:
ggplot(s, aes(x=date, y=tot, group=movieGenre))+geom_line(aes(color=movieGenre))+ggtitle("Monthly frequency of reviews for Animation and Horror Movies") +ylab("Total Reviews")

#More visualisations!
#We can choose smaller chunks of time to see how data behaves:

#Plotting the 2000s
s1=s[s$date>"2000/01" & s$date<"2010/01",]
ggplot(s1, aes(x=date, y=tot, group=movieGenre))+geom_line(aes(color=movieGenre))+ggtitle("Monthly frequency of reviews for Animation and Horror Movies in the 00s") +ylab("Total Reviews") + theme(axis.text.x=element_text(angle=60, hjust=1))

#Plotting the 1990s
s1=s[s$date>"1990/01" & s$date<"1999/01",]
ggplot(s1, aes(x=date, y=tot, group=movieGenre))+geom_line(aes(color=movieGenre))+ggtitle("Monthly frequency of reviews for Animation and Horror Movies in the 90s") +ylab("Total Reviews") + theme(axis.text.x=element_text(angle=60, hjust=1))