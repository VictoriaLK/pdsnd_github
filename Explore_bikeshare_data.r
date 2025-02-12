# Exploring Bike Rentals in Chicago, NYC and Washington DC.

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')
library(ggplot2)

# Question 1
summary(chi$Trip.Duration)
summary(ny$Trip.Duration)
summary(wash$Trip.Duration)

chi_mean <- summary(chi$Trip.Duration)['Mean']
ny_mean <- summary(ny$Trip.Duration)['Mean']
wash_mean <- summary(wash$Trip.Duration)['Mean']
mean_data <- data.frame(City=c('chicago', 'new york', 'washington'), Mean=c(chi_mean, ny_mean, wash_mean))
mean_data

add_boxplot <- function(name_str, city_data, color_str){
    return(geom_boxplot(aes(x=name_str, y=Trip.Duration), data=city_data, color=color_str))
}

p <- ggplot() + coord_cartesian(ylim = c(0,3000)) 
p <- p + add_boxplot('chicago', chi, 'red') + add_boxplot('new york', ny, 'green') + add_boxplot('washington', wash, 'blue') 
p <- p + labs(title='Trip Durations across Chicago, New York and Washington', x='City', y='Trip Duration (s)')
p + geom_point(aes(x=City, y=Mean), data=mean_data, color=c('red', 'green', 'blue'), size=3, shape=18)

# Question 2
nrow(ny)
nrow(chi)

summary(ny$Gender)
summary(ny$Birth.Year)
summary(chi$Gender)
summary(chi$Birth.Year)

filter_df <- function(datset, end_row){
    new_df = subset(dataset, !is.na(dataset$Birth.Year))
    new_df <- new_df[order(new_df$Birth.Year),]
    new_df <- new_df[-(1:end_row),]
    new_df <- subset(new_df, !new_df$Gender=='')
    return(new_df)
}
ny_end_row = 16 #removes the rows with Birth.Year 1885-1901 as https://en.wikipedia.org/wiki/List_of_the_verified_oldest_people shows that no such male was alive in 2017
chi_end_row = 5 #no men who were born in 1899-1901 were alive in 2017 - removing these outliers

p <- ggplot(aes(x=Birth.Year), data=filter_df(ny, ny_end_row))
p <- p + geom_histogram(stat='count', color='blue') + facet_wrap(~Gender)
p + labs(y='Number of users born per year', x='Birth years', title='Female and male users by birth year in NYC')

p <- ggplot(aes(x=Birth.Year), data=filter_df(chi, chi_end_row))
p <- p + geom_histogram(stat='count', color='orange') + facet_wrap(~Gender)
p + labs(y='Number of users born per year', x='Birth years', title='Female and male users by birth year in Chicago')

p <- ggplot(aes(x=Birth.Year, color='Chicago'), data=subset(new_chi, !new_chi$Gender==''))
p <- p + geom_line(stat='count') + facet_wrap(~Gender)
p <- p + geom_line(aes(x=Birth.Year, color='NYC'), data=subset(new_ny, !new_ny$Gender==''), stat='count')
p <- p + labs(y='Number of users born per year', x='Birth years', title='Female and male users by birth year in NYC vs Chicago')
p <- p + scale_color_manual(name='City', breaks=c('Chicago', 'NYC'), values=c('Chicago'='orange', 'NYC'='blue'))
p + theme(legend.position='top', legend.title=element_blank())

# Question 3

pophours_graph <- function(dataset, color_str){
    hrs = substr(dataset$Start.Time, 12, 13) #selects the hour value
    p <- ggplot() + geom_bar(aes(x=hrs), fill=color_str) + theme_minimal() + labs(y='Number of bike rentals', x='Hours of the day')
    return(p)
}

pophours_graph(chi, 'darkorange2') + ggtitle('Most popular times of the day for bike rentals in Chicago')

pophours_graph(ny, 'aquamarine4') + ggtitle('Most popular times of the day for bike rentals in New York')

pophours_graph(wash, 'cornflowerblue') + ggtitle('Most popular times of the day for bike rentals in Washington')

hrs_count <- function(dataset){
    hrs_count_df = as.data.frame(table(factor(substr(dataset$Start.Time, 12, 13))))
    colnames(hrs_count_df)[1] <- 'Hour'
    return(hrs_count_df)
}

p <- ggplot() + geom_line(aes(x=Hour, y=Freq, group=1, color='Chicago'), data=hrs_count(chi)) 
p <- p + geom_line(aes(x=Hour, y=Freq, group=1, color='NYC'), data=hrs_count(ny))
p <- p + geom_line(aes(x=Hour, y=Freq, group=1, color='Washington'), data=hrs_count(wash))
p <- p + labs(y='Number of bike rentals', x='Hours of the day', title='Most popular times of the day for bike rentals')
p <- p + scale_color_manual(breaks=c('Chicago', 'NYC', 'Washington'), values=c('Chicago'='darkorange2', 'NYC'='aquamarine4', 'Washington'='cornflowerblue'))
p + theme(legend.position='top', legend.title=element_blank())

length(chi_hrs)
length(ny_hrs)
length(wash_hrs)

chi_hrs_norm = chi_hrs_count
chi_hrs_norm$Freq <- chi_hrs_count$Freq * 10.3 # 89051/8630 = ~10.3
ny_hrs_norm = ny_hrs_count
ny_hrs_norm$Freq <- ny_hrs_count$Freq * 1.6 # 89051/54770 = ~1.6

p <- ggplot() + geom_line(aes(x=Hour, y=Freq, group=1, color='Chicago'), data=chi_hrs_norm)
p <- p + geom_line(aes(x=Hour, y=Freq, group=1, color='NYC'), data=ny_hrs_norm)
p <- p + geom_line(aes(x=Hour, y=Freq, group=1, color='Washington'), data=wash_hrs_count)
p <- p + labs(y='Number of bike rentals', x='Hours of the day', title='Most popular times of the day for bike rentals - normalised')
p <- p + scale_color_manual(breaks=c('Chicago', 'NYC', 'Washington'), values=c('Chicago'='darkorange2', 'NYC'='aquamarine4', 'Washington'='cornflowerblue'))
p + theme(legend.position='top', legend.title=element_blank())

system('python -m nbconvert Explore_bikeshare_data.ipynb')
