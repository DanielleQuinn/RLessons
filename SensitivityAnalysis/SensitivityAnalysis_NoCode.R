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

# ---- Baseline Population Size Estimate ----
# Before we begin our sensitivity analysis, we need to establish a baseline that our results will be compared to. In this case our baseline will be the population size estimated using all of the avilable data. We'll be using the `mrClosed()` function from the `FSA` package to estimate the population size using the Schnabel method. Without getting into too much detail, the Schnbabel method is used when we have multiple sampling events and assumes a closed population. The function requires specific input, including:
# - `n`: the number of captured animals
# - `m`: the number of recaptured marked animals
# - `M`: the number of extant marked animals prior to the sample

## Step 1: Build data frame for mrClosed() called df.cmr
# Eventually we're going to be repeating this process over a number of iterations, so we need it to be as efficient as possible to save us computing time. In this case, we'll use `dplyr` to set up our data frame. Each row needs to be a check (i.e. sampling event).

## Step 2: Estimate population size and confidence limits

# ---- Resampling Traps: Sample 10 Random Traps ----
# We'll use the `sample()` function to randomly select 10 of the 50 possible traps.

# Subset the data to include only those traps (using `dplyr`), then, just like before, create `df.cmr` and use `mrClosed()` to estimate population size.

# ---- Resampling Traps: Sample X Random Traps ----
# Repeat this analysis while choosing varying numbers of random traps and see how the results compare to our baseline population estimates. Resample from 1 to 50 random traps and apply the analysis for each subset.
# Since we're going to be applying an algorithm to multiple subsets of data, we're going to build a function to make this as efficient as possible.

## Step 1: Build a function
# This function needs to take a subset of data, produce the `df.cmr` data frame, and use it to estimate population size and confidence limits.

## Step 2: Set up a data frame to handle the output of the analyses
# We want a data frame that has information about how many traps were randomly selected, and the results of our `my_cmr()` function. In this case, we're going to have 50 sets of output (1 per random number of traps we're selecting).

## Step 3: Resample the data and fill in the results data frame

## Step 4: Visualize results

# Add baseline

# ---- Resampling Traps: Iterations ----
# This time instead of varying the number of traps to be 1 to 50, we'll limit those options to be from 2 to 50 by 2 (i.e. 2, 4, 6, ...50).
# Then we'll repeat the whole algorithm 30 times.

## Step 1: Build a function - already done!

## Step 2: Set up a data frame to handle the output of the analyses

## Step 3: Resample and fill in the results data frame
# In addition, we'll set up a means of keeping track of how long the resampling takes, and how long each iteration takes to complete. Being able to estimate how long your algorithm will take to complete will help you make decisions about how many iterations you can or should realistically set up.

## Step 4: Visualize results

# Option 1: Don't use a line to represent the resampled data.

# Option 2: Summarize the data before plotting.

# Option 3: Combine these methods.

# ---- Resampling Traps and Checks: Iterations ----
# Recall that each trap was checked 20 times. Imagine all of the time and effort that would be saved if we could sample traps fewer times and still be confident in our population estimates! In addition to exploring the sensitivity of the number of traps, let's add another level to this analysis and include a varying number of checks. We'll say that we want to see what would happen if 5 to 20 checks were done.

## Step 1: Build a function - already done!

## Step 2: Set up a data frame to handle the output of the analyses

## Step 3: Resample and fill in the results data frame

## Step 4: Visualize results

# Option 1: Don't use a line to represent the resampled data.
# Use facet_wrap to look at grouped by number of checks.

# Use facet_wrap to look at grouped by number of traps.

# Option 2: Summarize the data before plotting.
