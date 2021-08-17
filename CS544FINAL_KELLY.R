

#install plotly
#install plotrix
#install rgl
#install ggplot2
#install plot3D
#install ggthemes
#install cowplot
#install rmarkdown

marathon <- read.csv("/Users/barbarakelly/Downloads/Dataset-Boston-2019.csv")

marathon_sum <- summary(marathon) 
marathon_min <- as.numeric( strsplit(marathon_sum[1, 6], " :")[[1]][2] )
marathon_max <- as.numeric( strsplit(marathon_sum[6, 6], " :")[[1]][2] )
marathon_mean <- as.numeric( strsplit(marathon_sum[4, 6], " :")[[1]][2] )
marathon_median <- as.numeric( strsplit(marathon_sum[3, 6], " :")[[1]][2] )




head(marathon)
tail(marathon)
length(marathon)

view(marathon)

#Age Distribution of runners
find_age_group <- function( x_i, Range ) {
  res = 0
  for (i in 1:(length(Range)-1) ) {
    if ( x_i >= Range[i] & x_i < Range[i+1] ) {
      res <- age_str[i]
    }
  }
  return(res)
}

get_age_col <- function( X, Range ){
  X$Group=0           # initialize group column
  for (i in 1:dim(X)[1] ) {
    X[i, ]$Group = find_age_group( X[i, ]$Age, Range )
  }
  return(X)
}

Interval <- 10
marathon_age <- marathon['Age']
age_min <- min( marathon_age )
age_max <- max( marathon_age )
age_range <- seq( age_min, age_max, Interval) 
age_range <- c(age_range, 100)
age_str <- c('18-27', '28-37', '38-47', '48-57', '58-67', '68-77', '78-83')

#new column to dataframe
marathon <- get_age_col( marathon, age_range )


#plot age distribution
count_instances <- function( marathon, age_str, gender ) {
  list_i = numeric( length(age_str) )
  for (i in 1:length(age_str)) {
    list_i[i] = sum( marathon[marathon[, 'Gender'] == gender, ]$Group == age_str[i] )
  }
  return(list_i)
}

F_count = count_instances( marathon, age_str, gender='F' )
F_tot   = sum(F_count)
Freq_F  = round(F_count/F_tot*100, 2)

M_count = count_instances( marathon, age_str, gender='M' )
M_tot   = sum(M_count)
Freq_M  = round(M_count/M_tot*100, 2)

options(repr.plot.width=13, repr.plot.height=5)
marathon_count = data.frame(Age=age_str, Male=M_count, Female=F_count, Freq_M=Freq_M, Freq_F=Freq_F)

par(mfrow = c(1,2))
options(repr.plot.width=12, repr.plot.height=5)

b1 <- pie(marathon_count$Male, labels=marathon_count$Age, main='Males', explode = 0.15, start=0.5, theta=0.7, labelcex=2, cex.main=2)

b2 <- pie(marathon_count$Female, labels=marathon_count$Age, main='Females', explode = 0.15, start=-.4, theta=0.7, labelcex=2, cex.main=2)


#numerical data of age distribution

median(age_range)

table(age_range)
range(age_range)
fivenum(age_range)
summary(age_range)

#graphical representation of numerical age data

library(plotly)

M_gen_median <- median( marathon[marathon[, 'Gender'] == 'M',][['Age']] )
F_gen_median <- median( marathon[marathon[, 'Gender'] == 'F',][['Age']] )

print( paste('Median Male Runners :', M_gen_median, '[years]') )
print( paste('Median Female Runners :', F_gen_median, '[years]') )




barplot(fivenum(age_range), 
        names.arg=c("18","33","53","73","100"), las=2,
        col = "blue", ylim=c(18,100), 
        xlab = "Five Number Summary of Ages", ylab = "Age Range")




#Total Count of Countries vs. USA

options(repr.plot.width=18, repr.plot.height=8)
df_country <- table(marathon$Country)

df_country

print( paste('Total number of countries :' , length(df_country)) )


#Top 10 countries other than USA

df_no_USA <- marathon[!(marathon$Country_code=="USA"),]
t_count <- table(df_no_USA$Country_code)
t_above <- t_count[t_count > 50]
library(RColorBrewer)
coul <- brewer.pal( 8, "Set2") 
barplot( t_above, main='Top 25 Countries', xlab='Country', ylab='Count', col=rainbow(length(t_above)), 
         font.main=4, font.lab=4, cex.main=2, cex.axis=2, cex.names=1.15, cex.lab=1.75)
grid(nx=NA, ny=10)



#One variable - gender distribution

options(repr.plot.width=12, repr.plot.height=7)
df_gen = table(marathon$Gender)

gen <- pie(df_gen, labels=c(paste('Female     \n', '(11981)     '), paste('     Male\n', '     (14670)')), 
             main='Gender distribution', explode = 0.12, start=2, theta=0.9, labelcex=2.0, cex.main=2, radius=0.9 )


print(paste('Male / Female (ratio) =', df_gen[[2]], '/', df_gen[[1]], ' = ', round(as.numeric(df_gen[[2]]/df_gen[[1]]), 5) ) )


#Central Limit Theorem

#updated histo

sec_to_period <- function( x ){
  td <- seconds_to_period( round(x) )
  Str <- sprintf('%02d:%02d:%02d', td@hour, minute(td), second(td) )
  return(Str)
}

breaks = c(7200, 10800, 14400, 18000, 21600)
labels = c('2:00:00', '3:00:00', '4:00:00', '5:00:00', '6:00:00')
options(repr.plot.width=14, repr.plot.height=7)

X_res_hist<-ggplot(data = marathon, mapping = aes(x = Result_sec )) + ylab("Frequency") +
  geom_histogram(bins=30, fill = "blue", color = "black", size = 1.3, alpha = .5) +
  ggtitle("Results Histogram") + scale_x_continuous(name='Results [hr:mn:sc]', breaks=breaks, labels=labels)

X_res_hist

mean(age_range)
sd(age_range)

# Added for more samples
samples <- 10000
sample.size <- 5

Age <- (age_range)

for (i in 1:samples) {
  Age[i] <- mean(rexp(sample.size, 
                       rate = 2))
}

hist(Age, prob = TRUE, 
     breaks = 15, xlim=c(0,2), 
     ylim = c(0, 2), ylab = " Age Range",
     main = "Sample Size = 5")

mean(age_range)
sd(age_range)

par(mfrow = c(2,3))

for (size in c(15, 20, 25, 30)) {
  for (i in 1:samples) {
    Age[i] <- mean(rexp(sample.size, 
                        rate = 2))
  }
  
  hist(Age, prob = TRUE, 
       breaks = 15, xlim=c(0,2.5), col = "black",
       ylim = c(0.5, 2), ylab="Density",
       main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(age_range),
      " SD = ", sd(age_range), "\n")
}





#simple random sampling

names(marathon)

head(marathon[c(1,2,3,4,7)])
#nothing much shown as far as gender here, but could be useful
#for future if other genders are allowable. 

table(marathon$Age)
#shows total runners per age 18-100.

table(marathon$Age<35)

#19649 runners under 35
#7002 runners over 35

table(marathon$Gender == F)
#26651 runners are male


tail(marathon[c(4)])
#the last runners are all from USA

tail(marathon[c(2, 3, 5)])
#age, gender, result hour
#all F, various ages finished above 6 hours


head(marathon$Result_hr)

#top 5 results
marathon$Result_hr[1:5] #top 5
marathon$Result_hr[26647:26651] #last 5 finishers's results
#compare to top 5 hour

#stratified sampling
set.seed(123)
# Stratified, equal sized strata

Country <- rep(LETTERS[1:4], each = 25)

section.scores <- round(runif(100, 60, 80))

data <- data.frame(
  Section = Country_code, 
  Score = Result_hr)

head(data)

table(data$Section)

freq <- table(data$Section)
freq

st.sizes <- 10 * freq / sum(freq)
st.sizes

#
set.seed(123)
# Stratified, unequal sized strata

section.ids <- rep(LETTERS[1:4], c(10, 20, 30, 40))
section.genders <- 
  rep(rep(c("F", "M"), 4), 
      c(10, 0, 5, 15, 20, 10, 15, 25))
section.scores <- round(runif(100, 60, 80))

data <- data.frame(
  Section = Country_code, 
  Gender = Gender,
  Score = Result_hr)

head(data)

data <- data[order(data$Section, data$Gender), ]

freq <- table(data$Section, data$Gender)
freq

st.sizes <- 20 * freq / sum(freq)
st.sizes







#Age vs Result

M_res_median <- median( marathon[marathon[, 'Gender'] == 'M',][['Result_sec']] )
F_res_median <- median( marathon[marathon[, 'Gender'] == 'F',][['Result_sec']] )


library(ggplot2)
library(ggthemes)

options(repr.plot.width=18, repr.plot.height=9)

ggplot(marathon, aes(x=Result_sec, y=Age, color=Gender)) + geom_point(shape = 15, size = 3, alpha = 0.4) + 
  ggtitle("Age vs. Result") + ylab("") + xlab("") + scale_x_continuous(labels=labels) 




#extra feature
library(cowplot)

options(repr.plot.width=16, repr.plot.height=7)

# ------------------------ Gender analysis ------------------------ #
df_age <- ggplot(data = marathon, mapping = aes(x = Result_sec)) + ylab("") +
  geom_density(mapping = aes(fill = Group), color = "black", size = 1.5, alpha = .3)  +
  ggtitle("Results distribution") + scale_x_continuous (name='', labels=labels)

plot_grid(df_age)



