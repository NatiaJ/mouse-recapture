rm(list=ls())

## library('jagsUI')
library('rjags')
library('R2jags')
library('parallel')

source('misc.R')
source('analyses.R')
source('model-full.R')

## load data
load('recap.RData')

case <- 'DF_BriCon'
case <- 'DF_ChrHue'
case <- 'V_BriCon'
case <- 'DF_Hue'
case <- 'V_ChrHue'
case <- 'type'

if(case!='type')
  trait <- as.vector(scale(traits[,case]))
if(case=='type')
  trait <- as.numeric(as.factor(traits[,'type']))-1

## create data for JAGS
d <- list(X=recaptures,
          first=first,
          trait=trait,
          site=site,
          enc=paste(site, enc, sep=';'))

fn <- sprintf('out-%s.RData', case)

analyse(d=d, scale=10, file.name=fn)
