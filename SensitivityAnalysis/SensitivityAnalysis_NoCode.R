# Ecological studies often suffer from limited resource availability, including time, money, and people power. Oversampling not only taxes these resources needlessly, but may also negatively influence the system being studied; for example, through lethal collections of organisms or unnecessary handling of non-target species. Projects relying on data collection over multiple field seasons may benefit from resampling algorithms that can identify efficient collection protocols if oversampling is known or thought to have occurred.
# We'll be using a mock data set of tagged and non-tagged eels captured in a small lake to estimate population size, then build a resampling algorithm to explore the sensitivity of using varying numbers of traps and sampling events.
# - 400 American eels were captured from a small lake, marked with an external tag, and released back into the lake
# - 50 traps were checked 20 times over a season
# - number of tagged and non-tagged eels in each trap was recorded

# ----Load Required Packages ----
library(dplyr)
library(FSA)
library(ggplot2)
library(digest)
library(lubridate)

# ---- Import Data ----
# You can find the dataset at https://github.com/DanielleQuinn/RLessons/blob/master/SensitivityAnalysis/recapture_data.csv
df.data<-read.csv("recapture_data.csv")

# ---- Inspect Data ----
# Look at the data frame `df.data`
head(df.data)

# ---- Baseline Population Size Estimate ----
# Before we begin our sensitivity analysis, we need to establish a baseline that our results will be compared to. In this case our baseline will be the population size estimated using all of the avilable data. We'll be using the `mrClosed()` function from the `FSA` package to estimate the population size using the Schnabel method. Without getting into too much detail, the Schnbabel method is used when we have multiple sampling events and assumes a closed population. The function requires specific input, including:
# - `n`: the number of captured animals
# - `m`: the number of recaptured marked animals
# - `M`: the number of extant marked animals prior to the sample

## Step 1: Build data frame for mrClosed() called df.cmr
# Eventually we're going to be repeating this process over a number of iterations, so we need it to be as efficient as possible to save us computing time. In this case, we'll use `dplyr` to set up our data frame. Each row needs to be a check (i.e. sampling event).
df.cmr<-data.frame(df.data%>%
  group_by(check)%>%
  summarise(n=sum(present), m=sum(tagged), M=400))
head(df.cmr)

## Step 2: Estimate population size and confidence limits
pop.est<-summary(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))
pop.low<-stats::confint(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))[1]
pop.high<-stats::confint(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))[2]
baseline<-data.frame(pop.est, pop.low, pop.high)
baseline

# ---- Resampling Traps: Sample 10 Random Traps ----
# We'll use the `sample()` function to randomly select 10 of the 50 possible traps.
all_traps<-unique(df.data$trap)
use_traps<-sample(all_traps, 10)
use_traps

# Subset the data to include only those traps (using `dplyr`), then, just like before, create `df.cmr` and use `mrClosed()` to estimate population size.
my_subset<-data.frame(df.data%>%filter(trap %in% use_traps))
head(my_subset)

## Step 1: Build data frame for mrClosed() called df.cmr
# Eventually we're going to be repeating this process over a number of iterations, so we need it to be as efficient as possible to save us computing time. In this case, we'll use `dplyr` to set up our data frame. Each row needs to be a check (i.e. sampling event).
df.cmr<-data.frame(my_subset%>%
                     group_by(check)%>%
                     summarise(n=sum(present), m=sum(tagged), M=400))
head(df.cmr)

## Step 2: Estimate population size and confidence limits
pop.est<-summary(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))
pop.low<-stats::confint(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))[1]
pop.high<-stats::confint(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))[2]
results<-data.frame(pop.est, pop.low, pop.high)
results

baseline
results

# ---- Resampling Traps: Sample X Random Traps ----
# Repeat this analysis while choosing varying numbers of random traps and see how the results compare to our baseline population estimates. Resample from 1 to 50 random traps and apply the analysis for each subset.
# Since we're going to be applying an algorithm to multiple subsets of data, we're going to build a function to make this as efficient as possible.

## Step 1: Build a function
# This function needs to take a subset of data, produce the `df.cmr` data frame, and use it to estimate population size and confidence limits.
## Step 1: Build data frame for mrClosed() called df.cmr
# Eventually we're going to be repeating this process over a number of iterations, so we need it to be as efficient as possible to save us computing time. In this case, we'll use `dplyr` to set up our data frame. Each row needs to be a check (i.e. sampling event).
my_cmr<-function(my_subset)
{
  df.cmr<-data.frame(my_subset%>%
                       group_by(check)%>%
                       summarise(n=sum(present), m=sum(tagged), M=400))
  head(df.cmr)
  
  ## Step 2: Estimate population size and confidence limits
  pop.est<<-summary(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))
  pop.low<<-stats::confint(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))[1]
  pop.high<<-stats::confint(with(df.cmr,mrClosed(n=n, m=m, M=M,method="Schnabel")))[2]
}

## Step 2: Set up a data frame to handle the output of the analyses
# We want a data frame that has information about how many traps were randomly selected, and the results of our `my_cmr()` function. In this case, we're going to have 50 sets of output (1 per random number of traps we're selecting).
results<-data.frame(traps=c(1:50), pop.est=NA, pop.low=NA, pop.high=NA)
head(results)

## Step 3: Resample the data and fill in the results data frame
for(i in 1:nrow(results))
{
  use_traps<-sample(all_traps, results$traps[i])
  my_subset<-data.frame(df.data%>%filter(trap %in% use_traps))
  my_cmr(my_subset)
  results$pop.est[i]<-pop.est
  results$pop.low[i]<-pop.low
  results$pop.high[i]<-pop.high
}
head(results)

## Step 4: Visualize results
my_plot1<-ggplot(results)+
  geom_line(aes(x=traps, y=pop.est))+
  geom_line(aes(x=traps, y=pop.low), linetype="dashed")+
  geom_line(aes(x=traps, y=pop.high), linetype="dashed")+
  theme_bw(20)
my_plot1
  
# Add baseline
my_plot2<-my_plot1+geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')
my_plot2

# ---- Resampling Traps: Iterations ----
# This time instead of varying the number of traps to be 1 to 50, we'll limit those options to be from 2 to 50 by 2 (i.e. 2, 4, 6, ...50).
# Then we'll repeat the whole algorithm 30 times.

## Step 1: Build a function - already done!

## Step 2: Set up a data frame to handle the output of the analyses
results<-data.frame(traps=rep(seq(from=2, to=50, by=2),30),
                    pop.est=NA, pop.low=NA, pop.high=NA)

## Step 3: Resample and fill in the results data frame
# In addition, we'll set up a means of keeping track of how long the resampling takes, and how long each iteration takes to complete. Being able to estimate how long your algorithm will take to complete will help you make decisions about how many iterations you can or should realistically set up.
starttime=Sys.time()
for(i in 1:nrow(results))
{
  use_traps<-sample(all_traps, results$traps[i])
  my_subset<-data.frame(df.data%>%filter(trap %in% use_traps))
  my_cmr(my_subset)
  results$pop.est[i]<-pop.est
  results$pop.low[i]<-pop.low
  results$pop.high[i]<-pop.high
}
totaltime<-difftime(Sys.time(), starttime, units="secs")
totaltime
per.it<-round(as.numeric(totaltime)/nrow(results),3)
paste(per.it, "seconds per iteration")

## Step 4: Visualize results
my_plot3<-ggplot(results)+
  geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')+
  geom_line(aes(x=traps, y=pop.est))+
  geom_line(aes(x=traps, y=pop.low), linetype="dashed")+
  geom_line(aes(x=traps, y=pop.high), linetype="dashed")+
  theme_bw(20)
my_plot3

# Option 1: Don't use a line to represent the resampled data.
my_plot4<-ggplot(results)+
  geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')+
  geom_jitter(aes(x=traps, y=pop.est), alpha=0.5)+
  geom_jitter(aes(x=traps, y=pop.low), col="purple", alpha=0.5)+
  geom_jitter(aes(x=traps, y=pop.high), col="red", alpha=0.5)+
  theme_bw(20)
my_plot4

# Option 2: Summarize the data before plotting.
results.sum<-data.frame(results%>%
                          group_by(traps)%>%
                          summarise(mean.pop=mean(pop.est),
                                    mean.low=mean(pop.low),
                                    mean.high=mean(pop.high)))

my_plot5<-ggplot(results.sum)+
  geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')+
  geom_line(aes(x=traps, y=mean.pop))+
  geom_line(aes(x=traps, y=mean.low), linetype="dashed")+
  geom_line(aes(x=traps, y=mean.high), linetype="dashed")+
  theme_bw(20)
my_plot5

# Option 3: Combine these methods.

# ---- Resampling Traps and Checks: Iterations ----
# Recall that each trap was checked 20 times. Imagine all of the time and effort that would be saved if we could sample traps fewer times and still be confident in our population estimates! In addition to exploring the sensitivity of the number of traps, let's add another level to this analysis and include a varying number of checks. We'll say that we want to see what would happen if 5 to 20 checks were done.
all_checks<-unique(df.data$check)
all_checks

## Step 1: Build a function - already done!

## Step 2: Set up a data frame to handle the output of the analyses
results<-expand.grid(traps=seq(from=2, to=50, by=2),
            checks=5:20,
            repeats=0:40)
nrow(results)*per.it
results<-results[,-3]
results$pop.est<-NA
results$pop.low<-NA
results$pop.high<-NA

## Step 3: Resample and fill in the results data frame
starttime=Sys.time()
for(i in 1:nrow(results))
{
  use_traps<-sample(all_traps, results$traps[i])
  use_checks<-sample(all_checks, results$checks[i])
  my_subset<-data.frame(df.data%>%
                          filter(trap %in% use_traps)%>%
                          filter(check %in% use_checks))
  my_cmr(my_subset)
  results$pop.est[i]<-pop.est
  results$pop.low[i]<-pop.low
  results$pop.high[i]<-pop.high
}
totaltime<-difftime(Sys.time(), starttime, units="secs")
totaltime
per.it<-round(as.numeric(totaltime)/nrow(results),3)
paste(per.it, "seconds per iteration")

## Step 4: Visualize results

# Option 1: Don't use a line to represent the resampled data.
# Use facet_wrap to look at grouped by number of checks.
my_plot7<-ggplot(results)+
  geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')+
  geom_jitter(aes(x=traps, y=pop.est), alpha=0.5)+
  geom_jitter(aes(x=traps, y=pop.low), col="purple", alpha=0.5)+
  geom_jitter(aes(x=traps, y=pop.high), col="red", alpha=0.5)+
  theme_bw(20)+facet_wrap(~checks)
my_plot7
# Use facet_wrap to look at grouped by number of traps.
my_plot8<-ggplot(results)+
  geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')+
  geom_jitter(aes(x=checks, y=pop.est), alpha=0.5)+
  geom_jitter(aes(x=checks, y=pop.low), col="purple", alpha=0.5)+
  geom_jitter(aes(x=checks, y=pop.high), col="red", alpha=0.5)+
  theme_bw(20)+facet_wrap(~traps)
my_plot8

# Option 2: Summarize the data before plotting.
results.sum<-data.frame(results%>%
                          group_by(traps, checks)%>%
                          summarise(mean.pop=mean(pop.est),
                                    mean.low=mean(pop.low),
                                    mean.high=mean(pop.high)))

my_plot9<-ggplot(results.sum)+
  geom_hline(yint=baseline$N, col='green4')+
  geom_hline(yint=baseline$pop.low, col='green4',linetype='dashed')+
  geom_hline(yint=baseline$pop.high, col='green4',linetype='dashed')+
  geom_line(aes(x=traps, y=mean.pop))+
  geom_line(aes(x=traps, y=mean.low), linetype="dashed")+
  geom_line(aes(x=traps, y=mean.high), linetype="dashed")+
  theme_bw(20)+facet_wrap(~checks)
my_plot9