analyse <- function(d, scale, file.name, vcov=FALSE) {
  
  ## create data for JAGS
  if(vcov)
    my.data <- list(X=d$X,
                    nind=nrow(d$X),
                    nrep=ncol(d$X),
                    trait=d$trait,
                    site=as.integer(as.factor(d$site))-1,
                    VCOV=d$VCOV,
                    ID=d$ID,
                    first=d$first,
                    alive=known.state.cjs(d$X))
  if(!vcov)
    my.data <- list(X=d$X,
                    nind=nrow(d$X),
                    nrep=ncol(d$X),
                    trait=d$trait,
                    site=as.integer(as.factor(d$site))-1,
                    first=d$first,
                    alive=known.state.cjs(d$X))
  
  my.inits <- function() {
    list(alive=cjs.init.z(d$X, my.data$first))
  }
  
  ## specify the parameters to be monitored
  my.params <- get.params()
  
  ## data
  dd <- list(data=my.data,
             inits=my.inits,
             params=my.params)
  model.out <- run.model(dd,
                         ni=(1e3+1e1)*scale,
                         nt=1e1*scale,
                         nb=1e1*scale)
  res <- list(data=my.data, bugs=model.out)
  model.summary <- model.out$BUGSoutput$summary
  save(my.data,
       model.out,
       model.summary,
       file=file.name)
  
  NULL
}
