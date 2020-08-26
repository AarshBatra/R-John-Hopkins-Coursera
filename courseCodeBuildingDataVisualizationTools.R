# Building Data Visualization Tools

library(faraway)
library(dplyr)
data(nepali)

foo <- nepali %>%
  mutate(id = factor(id), sex = factor(sex, levels = c(1, 2), labels =c("Male", "Female")))

foo1 <- worldcup %>% group_by(Team) %>%
  summarise(mean_time = mean(Time)) %>%
  arrange(mean_time)

foo1 <- foo1 %>% mutate(Team = factor(Team, levels = Team))

  foo1 %>% ggplot() + geom_point(mapping = aes(x = mean_time, y = Team)) +
   xlab("Mean time per player (minutes)") +
  ylab("")


foo3 <- worldcup %>%
    select(Position, Time, Shots) %>%
    group_by(Position) %>%
    mutate(ave_shots = mean(Shots),
           most_shots = Shots == max(Shots))

# mapping

us_map  <- ggplot2::map_data("state")

us_map %>% as_tibble() %>% ggplot() +
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "blue", color = "black") + theme_void()

data("votes.repub")
head(votes.repub)
class(votes.repub)

votes.repub <- tbl_df(votes.repub)

# choroplethr

library(choroplethr)
library(choroplethrMaps)
data(df_pop_county)
df_pop_county %>% slice(1:3)

foo4 <- votes.repub %>%
  tbl_df() %>%
  mutate(state = rownames(votes.repub),
         state = tolower(state)) 

foo4 %>% right_join(us_map, c("state" = "region")) %>%
  ggplot() +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = `1976`), color = "black") +
  theme_void() +
  scale_fill_viridis(name = "Republican\nVotes (%)")

worldMap <- map_data("world")
world.cities.unique <- distinct(world.cities, country.etc, .keep_all = TRUE)
world.cities.summ <- world.cities %>% as_tibble() %>%
  group_by(country.etc) %>% summarise(popSum = sum(pop, na.rm = TRUE))
foo5 <- world.cities.summ  %>%
  right_join(worldMap, c("country.etc" = "region"))


foo5 %>% ggplot() + geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = popSum), color = "black") +
  theme_void() + scale_fill_viridis(name = "Population Heat Map")

 
# Grid Graphics

library(grid)

candy <- grid.circle()
stick <- segmentsGrob(x0 = 0.5, x1 = 1, y0 = 0.5, y1 = 1) 

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5, 
                      width = 0.5, height = 0.5,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

