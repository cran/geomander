## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libs, warning = FALSE, message = FALSE-----------------------------------
library(geomander)

library(ggplot2)
library(dplyr)
library(stringr)

library(redist)
library(sf)
library(tigris)

## ---- results = 'hide', eval = FALSE, message = FALSE-------------------------
#  blockRockland <- create_block_table(state = 'NY', county = 'Rockland')
#  blockOrange <- create_block_table(state = 'NY', county = 'Orange')
#  
#  block <- bind_rows(blockRockland, blockOrange)

## -----------------------------------------------------------------------------
data("orange")
data("rockland")
block <- bind_rows(rockland, orange)

## ---- results = 'hide', eval = FALSE, message = FALSE-------------------------
#  school <- school_districts(state = 'NY') %>% filter(str_detect(NAME, 'North Rockland'))

## -----------------------------------------------------------------------------
data("nrcsd")
school <- nrcsd

## -----------------------------------------------------------------------------
block %>% ggplot() + 
  geom_sf() +
  geom_sf(data = school, fill = NA, color = 'red') +
  theme_void()

## -----------------------------------------------------------------------------
block <- block %>% geo_filter(to = school)

## -----------------------------------------------------------------------------
block %>% mutate(id = row_number()) %>% 
  ggplot() + geom_sf() +
  geom_sf(data = school, fill = NA, color = 'red') 

## -----------------------------------------------------------------------------
block$trim <- block %>% geo_trim(to = school, bool = TRUE)

block %>% ggplot() + geom_sf(aes(fill = trim)) + 
  geom_sf(data = school, fill = NA, lwd = 1.5)

## -----------------------------------------------------------------------------
block <- block %>% filter(trim)

## ----data---------------------------------------------------------------------
data("towns")

block %>% ggplot() +
  geom_sf() +
  theme_void() +
  geom_sf(data = towns, aes(fill = as.character(ID)))

## -----------------------------------------------------------------------------
matched <- geo_match(from = block, to = towns, method = 'centroid')

## -----------------------------------------------------------------------------
block %>% 
  ggplot() +
  geom_sf(aes(fill = as.character(matched))) +
  theme_void() +
  labs(fill = 'Match')

## -----------------------------------------------------------------------------
block <- block %>% mutate(TownID = matched) %>% 
  mutate(TownID = ifelse(county != '087', 8, TownID)) 

## ---- message = FALSE---------------------------------------------------------
adj <- redist.adjacency(shp = block)

comp <- check_contiguity(adjacency = adj, group = block$TownID)

which(comp$component > 1)

## -----------------------------------------------------------------------------
block$TownID[409] <- 7
block$TownID[586] <- 2
block$TownID[591] <- 4

## -----------------------------------------------------------------------------
comp <- check_contiguity(adjacency = adj, group = block$TownID)

which(comp$component > 1)

## -----------------------------------------------------------------------------
map <- redist_map(block, pop_tol = 0.02, ndists = 7, adj = adj)

sims005 <- redist_smc(map, nsims = 50, counties = TownID, silent = TRUE)

plans <- get_plans_matrix(sims005) %>% unique(MARGIN  = 2)

par <- redist.parity(plans = plans, total_pop = block$pop)

comp <- redist.compactness(shp = block, plans = plans, adj = adj, measure = 'EdgesRemoved')

comp_m <- comp %>% group_by(draw) %>% summarize(mean = mean(EdgesRemoved))

pick <- tibble(parity = par) %>% bind_cols(comp_m) %>% slice_max(order_by = mean, n = 1) %>% pull(draw)

## -----------------------------------------------------------------------------
block %>% 
  mutate(district = plans[,pick]) %>% 
  group_by(district) %>% 
  summarize(across(starts_with('vap'), sum))

