library(tidyverse)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl==8)
filter(diamond, carat > 3)

install.packages("nycflights13") 
library(nycflights13)
library(tidyverse)

nycflights13::flights

names(flights)

view(flights)

jan1 <- filter(flights, month == 1, day ==1)

(dec25 <- filter(flights, month == 12 , day == 25))

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

df <- tibble(x = c(5,2,NA))
arrange(df,x)
arrange(df, desc(x))
arrange(df, desc(is.na(x), x))
arrange(flights, desc(dep_delay))
select(flights, starts_with("dep"))


flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       speed = distance /air_time * 60)


summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))


by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, 
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
                   )

delay <- filter(delay, count >20, dest != "HNL")

ggplot(data = delay,  aes(x = dist, y = delay) ) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count>20, dest != "HNL")


not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x=n, y = delay)) +
  geom_point(alpha = 1/10)



install.packages("Lahman") 

batting <- as_tibble(Lahman::Batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>%
  filter(ab > 100) %>%
  ggplot(aes(x=ab , y=ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

batters %>%
  arrange(desc(ba))


x <- not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%

x2 <- not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(tailnum, wt = distance)


daily <- group_by(flights, year, month, day) #progressively peels off summarize function when grouped
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))


daily %>%
  ungroup() %>%
  summarize(flights = n())


flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)


#chapter 5


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.8)



diamonds %>%
  count(cut_width(carat, 0.5))


smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,20))


unsual <- diamonds %>%
  filter(y<3 | y>20) %>%
  arrange(y)

diamonds2 <- diamonds %>%
  filter(between(y, 3, 20))

diamonds3 <- diamonds %>%
  mutate(y = ifelse(y <3 | y >20 , NA, y))


ggplot(data = diamonds2, aes(x =x , y=y)) + 
  geom_point(na.rm = TRUE)

ggplot(data = mpg, mapping = aes(x=class, y=hwy)) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = mean),
      y = hwy
    )
  ) +
  coord_flip()



ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(cut, color)

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill=n))


ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y =price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_hex()

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x = cut, y = resid))



ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) +
  geom_freqpoly(binwidth = 0.25)



diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) +
  geom_tile()










