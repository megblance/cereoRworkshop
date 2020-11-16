#Explore bike to data to see if relationship btw weather and ridership

library(tidyverse)

#Load the data to work with
df<- read_csv('data/daily_bike_data.csv')
df

#Fix data date error???
df$dteday <- as.Date(df$dteday)

#Time trend of ridership
p <- ggplot(data=df) + 
  geom_line(aes(x = as.Date(dteday), y = cnt))

# Relationship btw ridership and temp
#Can specify data in the main ggplot or down in the plot function to use multiple data sets
ggplot(data = df, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth()

#What is weathersit?
summary(df$weathersit)
unique(df$weathersit)

#dplyr verbs: (also don't need to add dplyr::, we add it here to be specific and emphasize we're using this package)
#mutate - adds new columns to data frame 
#transmute keeps only new columns you generate
#select - picks columns from the dataset, you can also drop variables using a minus sign
# filter - filters the rows 
# ctrl shift m is the shortcut for a pipe; converts the variable in weather sit from a numerical levels to words
df2 <- df %>%
  dplyr::mutate(
    weather_fac = factor(weathersit,
                         levels = c(1,2,3,4),
                         labels = c('clear', 'cloudy', 'rainy', 'heavy rain'))
  )

df2 %>%  dplyr::select(dteday, weathersit, weather_fac)

df2 %>%
  dplyr::filter(weather_fac == 'rainy') %>% 
  ggplot(aes(x = temp, y = cnt)) +
    geom_point() +
    geom_smooth()

#drop a variable
df3 <- df2 %>% 
  dplyr::select(-weathersit)

#can also use character lists to change variable quantities
keep_vars <- c('dteday', 'weather_fac', 'temp', 'cnt')
df4 <- df2 %>%  select(all_of(keep_vars))

#other ways of filtering
weather_factors_we_like <- c('rainy', 'cloudy')
df2 %>%  dplyr::filter(weather_fac == 'rainy' | weather_fac == 'cloudy')
df2 %>%  dplyr::filter(weather_fac %in% weather_factors_we_like)
df2 %>%  dplyr::filter(weather_fac != 'rainy')
df2 %>%  dplyr::filter(!(weather_fac %in% weather_factors_we_like))

## dplyr::summarize
df2 %>% 
  dplyr::group_by(weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )

df2 %>% 
  dplyr::group_by(season, weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )

## Transforming data format from long to wide or vice versa

#Transform into separate temp variables for each month
months<-c('January', 'February', 'March', 'April', 'May', 'June',
          'July', 'August', 'September', 'October', 'November', 'December')

df_wide <- df2 %>%
  dplyr::mutate(mnth = factor(mnth, levels = months, labels = months)) %>% 
  dplyr::rename(year = yr) %>% 
  dplyr::select(year, mnth, temp) %>% 
  dplyr::group_by(year, mnth) %>%
  dplyr::summarize(temp_mean = mean(temp)) %>% 
  tidyr::pivot_wider(names_prefix = 'temp_', names_from = mnth, values_from = temp_mean) %>% 
  dplyr::rename_with(tolower)

#Pivot longer
df_long <- df2 %>% 
  tidyr::pivot_longer(cols = c(temp, atemp, hum, windspeed), values_to = 'value', names_to = 'variable')

#Pivot wider
df_wide2 <- df_long %>% 
  tidyr::pivot_wider(names_prefix = 'v_', names_from = variable, values_from = value)

#or another wider example - temperature change by weekday
#If you run without the last line, see the same output but pivoted  the other direction
df %>% 
  group_by(weekday) %>% 
  summarize(mean_temp = mean(temp)) %>% 
  pivot_wider(names_from = weekday, values_from = mean_temp)

##Plotting with a longer data frame

#Facetting -   #free_y allows for diff scales, just remember that they're different! 
p <- ggplot(data = df2, aes(x=temp, y=cnt)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) +
  facet_wrap(~weather_fac) +
  labs(x = 'Temperature', y = 'Ridership Count') +
  ggtitle('Relationship between temperature and ridership') +
  theme_linedraw()
#save the plot as a file: ggsave(plot = p, filename = 'temp_count_scatter.png')

#New long plot
ggplot(data = df_long, aes(x = value, y = cnt, color = weather_fac)) +
  geom_point() +
  facet_wrap(~variable, scales ='free_y') +
  geom_smooth(se=FALSE)

