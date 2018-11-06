run.model <- function(d,
                      ni=1100,
                      nt=10,
                      nb=100,
                      nc=3) {

  model.jags <- function() {

    ## priors
    p.0            ~ dnorm(0,0.001)
    p.site         ~ dnorm(0,0.001)
    p.trait      ~ dnorm(0,0.001)
    p.site.trait ~ dnorm(0,0.001)

    ## priors
    phi.0            ~ dnorm(0,0.001)
    phi.site         ~ dnorm(0,0.001)
    phi.trait      ~ dnorm(0,0.001)
    phi.site.trait ~ dnorm(0,0.001)

    ## for each individual
    for(ind in 1:nind) {

      logit(phi[ind]) <-
        phi.0 +
        phi.trait*trait[ind] +
        phi.site*site[ind] +
        phi.site.trait*site[ind]*trait[ind]
      
      logit(p[ind]) <-
        p.0 +
        p.trait*trait[ind] +
        p.site*site[ind] +
        p.site.trait*site[ind]*trait[ind]

      ## set alive to 1 for first time individual was seen
      for(rep in 1:first[ind]) {
        alive[ind, rep] <- 1
      }

      ## for each rep after the first time an individual was observed
      for(rep in (first[ind]+1):nrep) {

        ## state equation
        alivep[ind,rep] <- phi[ind] * alive[ind,rep-1] 
        alive[ind,rep] ~ dbern(alivep[ind,rep])

        ## observation equation
        sightp[ind,rep] <- p[ind] * alive[ind, rep] 
        X[ind,rep] ~ dbern(sightp[ind,rep])

      }
    }

    logit(phi.dm.ls) <-
      phi.0 +
      phi.trait*min(trait) +
      phi.site*1 +
      phi.site.trait*1*min(trait)
    logit(phi.dm.ds) <-
      phi.0 +
      phi.trait*min(trait) +
      phi.site*0 +
      phi.site.trait*0*min(trait)

    logit(phi.lm.ls) <-
      phi.0 +
      phi.trait*max(trait) +
      phi.site*1 +
      phi.site.trait*1*max(trait)
    logit(phi.lm.ds) <-
      phi.0 +
      phi.trait*max(trait) +
      phi.site*0 +
      phi.site.trait*0*max(trait)

    logit(p.dm.ls) <-
      p.0 +
      p.trait*min(trait) +
      p.site*1 +
      p.site.trait*1*min(trait)
    logit(p.dm.ds) <-
      p.0 +
      p.trait*min(trait) +
      p.site*0 +
      p.site.trait*0*min(trait)

    logit(p.lm.ls) <-
      p.0 +
      p.trait*max(trait) +
      p.site*1 +
      p.site.trait*1*max(trait)
    logit(p.lm.ds) <-
      p.0 +
      p.trait*max(trait) +
      p.site*0 +
      p.site.trait*0*max(trait)

    p.mean <- mean(p)
    phi.mean <- mean(phi)
  }

  jags(d$data,
       d$inits,
       d$params,
       model.file=model.jags,
       n.chains=nc,
       n.thin=nt,
       n.iter=ni,
       n.burnin=nb,
       working.directory=NULL)
}

## specify the parameters to be monitored
get.params <- function()
  c('p.mean',
    'p',
    'p.0',
    'p.trait', 
    'p.site',
    'p.site.trait',
    'phi.mean',
    'phi',
    'phi.0',
    'phi.trait', 
    'phi.site',
    'phi.site.trait',
    'phi.lm.ls',
    'phi.lm.ds',
    'phi.dm.ls',
    'phi.dm.ds')
