
# Simulations using multiple methods

library(dplyr)

df.data<-read.csv("simulations/recapture_data.csv")

estpop <- function(df.data, M) { 
  
  df.cmr <- data.frame(df.data %>% 
                         dplyr::group_by(check) %>% 
                         dplyr::summarise(date=unique(check), n=sum(present), m=sum(tagged), M=M))
  df.cmr$date <- NULL
  mrclObj <- with(df.cmr, FSA::mrClosed(n=n, m=m, M=M, method="Schnabel"))
  cint <- stats::confint(mrclObj)
  c(summary(mrclObj), cint[1], cint[2])
}

allchecks <- unique(df.data$check)
alltraps <- unique(df.data$trap)

# ------ Using plyr::adply -----

results <- expand.grid(traps=seq(from=2, to=50, by=2), checks=5:20, repeats=0:40)
results$repeats <- NULL

starttime <- Sys.time()
results1 <- plyr::adply(results, .margins=1, .fun=function(slice) {
  use_traps <- sample(alltraps, slice$traps)
  use_checks <- sample(allchecks, slice$checks)
  subset <- data.frame(df.data%>%dplyr::filter((trap %in% use_traps) & (check %in% use_checks)))
  estpop(subset, 400)
})
names(results1) <- c("traps", "checks", "pop.est", "pop.low", "pop.high")
totaltime1 <- difftime(Sys.time(), starttime, units="secs")
message("Method plyr::adply: ", totaltime1)
#Method plyr::adply: 77.7962670326233


# ---- Using a standard for loop ----

results <- expand.grid(traps=seq(from=2, to=50, by=2), checks=5:20, repeats=0:40)
results$repeats <- NULL

results$pop.est<-NA
results$pop.high<-NA
results$pop.low<-NA

starttime <- Sys.time()
for(i in 1:nrow(results)) {
  use_traps <- sample(alltraps, results$traps[i])
  use_checks<-sample(allchecks, results$checks[i])
  subset <- data.frame(df.data%>%dplyr::filter((trap %in% use_traps) & (check %in% use_checks)))
  results[i,3:5] <- estpop(subset, 400)
}
totaltime2 <- difftime(Sys.time(), starttime, units="secs")
message("Method standard for loop: ", totaltime2)
#Method standard for loop: 85.5260829925537

# ---- Using {foreach} and {doParallel} ----

library(foreach)
library(doParallel)

results <- expand.grid(traps=seq(from=2, to=50, by=2), checks=5:20, repeats=0:40)
results$repeats <- NULL

registerDoParallel()
starttime <- Sys.time()
results2 <- foreach(i=1:nrow(results), .inorder = FALSE, .combine = rbind) %dopar% {
  ntraps <- results$traps[i]
  nchecks <- results$checks[i]
  use_traps <- sample(alltraps, ntraps)
  use_checks<-sample(allchecks, nchecks)
  subset <- data.frame(df.data%>%dplyr::filter((trap %in% use_traps) & (check %in% use_checks)))
  c(ntraps, nchecks, estpop(subset, 400))
}
colnames(results2) <- c("traps", "checks", "pop.est", "pop.low", "pop.high")
totaltime3 <- difftime(Sys.time(), starttime, units="secs")
message("Method doParallel; foreach: ", totaltime3)
#Method doParallel; foreach: 27.2572259902954

# ---- Using {plyr} with a {doParalell} backend ----

library(doParallel)

results <- expand.grid(traps=seq(from=2, to=50, by=2), checks=5:20, repeats=0:40)
results$repeats <- NULL

registerDoParallel()
starttime <- Sys.time()
results3 <- plyr::adply(results, .margins=1, .parallel=TRUE, .fun=function(slice) {
  use_traps <- sample(alltraps, slice$traps)
  use_checks <- sample(allchecks, slice$checks)
  subset <- data.frame(df.data%>%dplyr::filter((trap %in% use_traps) & (check %in% use_checks)))
  estpop(subset, 400)
})
totaltime4 <- difftime(Sys.time(), starttime, units="secs")
message("Method doParallel; plyr::adply: ", totaltime4)
#Method doParallel; plyr::adply: 29.3261539936066
#using .paropts=list(.inorder=FALSE) doesn't make a difference



