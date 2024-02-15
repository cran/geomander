## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libs, warning = FALSE, message = FALSE-----------------------------------
library(geomander)
library(dplyr)
library(ggplot2)
library(tinytiger)
library(sf)

## -----------------------------------------------------------------------------
data(va18sub)
unique(va18sub$COUNTYFP)

## ----message = FALSE, results = 'hide', eval=FALSE----------------------------
#  block <- create_block_table(state = 'VA', county = '087', year = 2010)

## -----------------------------------------------------------------------------
data("va_blocks")
block <- va_blocks

## -----------------------------------------------------------------------------
matches <- geo_match(from = block, to = va18sub, method = 'centroid')

## -----------------------------------------------------------------------------
prec <- block2prec(block_table = block, matches = matches)

## ----eval = FALSE-------------------------------------------------------------
#  prec_by_c <- block2prec_by_county(block_table = block, precinct = va18sub, precinct_county_fips = 'COUNTYFP')

## -----------------------------------------------------------------------------
fulldata <- bind_cols(va18sub, prec)

## -----------------------------------------------------------------------------
data(va18sub)

## ----message = FALSE, eval = FALSE--------------------------------------------
#  block <- create_block_table(state = 'VA', county = '087')

## ----results='hide', eval = FALSE, message = FALSE----------------------------
#  vtd <- tt_voting_districts(state = 'VA', county = '087', year = 2010)

## -----------------------------------------------------------------------------
data("va_vtd")
vtd <- va_vtd

## -----------------------------------------------------------------------------
vtd %>% ggplot() +
  geom_sf() + 
  geom_sf(data = va18sub, color = 'red', fill = 'NA' )+
  theme_void()

## -----------------------------------------------------------------------------
matches_p <- geo_match(from = block, to = va18sub, method = 'centroid')

## -----------------------------------------------------------------------------
matches_v <- geo_match(from = block, to = vtd, method = 'centroid')

## -----------------------------------------------------------------------------
disagg_kaine <- estimate_down(wts = block$vap, value = va18sub$G18USSDKAI, group = matches_p)

## -----------------------------------------------------------------------------
vtd_kaine <- estimate_up(value = disagg_kaine, group = matches_v)

## -----------------------------------------------------------------------------
disagg_kaine_geo <- geo_estimate_down(from = va18sub, to = block, wts = block$vap, 
                                      value = va18sub$G18USSDKAI, method = 'centroid')

vtd_kaine_geo <- geo_estimate_up(from = block, to = vtd, value = disagg_kaine_geo, method = 
                                   'centroid')

## -----------------------------------------------------------------------------
all(disagg_kaine == disagg_kaine_geo) & all(vtd_kaine == vtd_kaine_geo)

