library(conflicted)
library(haven)
library(devtools)
library("SCtools")
library(skimr)
library("tidyverse")
library("microsynth")
library("Synth")
library("doParallel")
library("data.table")
library("statar")
library("dplyr")

texas <- read_dta("C:/Users/guima/Downloads/texas13 (2).dta")

texas <- as.data.frame(texas)
texas[is.na(texas)] <- 0

texas$statefip <- as.numeric(factor(texas$state, levels = unique(texas$state)))
skim_without_charts(texas)



#data prep
dataprep.out <-
  dataprep(
    foo = texas
    ,predictors= c("income", 'ur', 'poverty'
    )
    ,predictors.op = c("mean")
    ,dependent     = c("bmprison")
    ,unit.variable = c("statefip")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("aidscapita", seq(from = 1985, to = 2000, by = 1), "mean"),
      list('perc1519', seq(from = 1985, to = 2000, by = 1), "mean"),
      list("alcohol", seq(from = 1985, to = 2000, by = 1), "mean"),
      list('bmpop', seq(from = 1985, to = 2000, by = 1), "mean"),
      list('bmprate', seq(from = 1985, to = 1992, by = 1), "mean")
    )
    ,treatment.identifier  = 44
    ,controls.identifier   = c(1:43,45:51)
    ,time.predictors.prior = seq(from = 1985, to = 2000, by = 1)
    ,time.optimize.ssr     = c(1985:1990)
    ,unit.names.variable   = c("state")
    ,time.plot            = c(1985:2000) 
    
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out)

# Get result tables
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

# results tables:
syn <- as.data.frame(synth.tables[["tab.w"]])

syn %>%
  group_by(unit.names) %>%
  summarise(weight = w.weights*100) %>%
  arrange(desc(weight)) %>%
  head(5)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("bmprate"),
          Xlab = c("year"), 
          Legend = c("Texas","Synthetic Texas"),
          tr.intake = c(1993)
) 



gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out, 
          Ylab = c("gap in bmprate "),
          Xlab = c("year"), 
          tr.intake = c(1993)
)

tdf <- generate.placebos(
  dataprep.out,
  synth.out,
  Sigf.ipop = 3,
  strategy = "sequential"
)

#Compute the post/pre MSPE ratio and and a measure of p-value
ratio <- mspe.test(tdf,discard.extreme = TRUE) #
ratio$p.val

#Plot Post/pre ratio of MSPE
mspe.plot(tdf, discard.extreme = TRUE)

#Plot the placebos discarding MSPE 10 times larger than that of south korea in the pre-treatment period.
plot_placebos(tdf,discard.extreme=TRUE, mspe.limit=10, xlab='Year', )

multi <- multiple.synth(foo = texas,
                        predictors = c("income", 'ur', 'poverty'),
                        predictors.op = "mean",
                        dependent = "bmprison",
                        unit.variable = "statefip",
                        time.variable = "year",
                        treatment.time = 1992,
                        list(
                          list("aidscapita", seq(from = 1985, to = 2000, by = 1), "mean"),
                          list('perc1519', seq(from = 1985, to = 2000, by = 1), "mean"),
                          list("alcohol", seq(from = 1985, to = 2000, by = 1), "mean"),
                          list('bmpop', seq(from = 1985, to = 2000, by = 1), "mean"),
                          list('bmprate', seq(from = 1985, to = 1992, by = 1), "mean")
                        ),
                        treated.units = c(44),
                        control.units = c(1:43,45:51),
                        time.predictors.prior = seq(from = 1985, to = 2000, by = 1),
                        time.optimize.ssr = c(1985:1990),
                        unit.names.variable = "state",
                        time.plot = 1985:2000, 
                        gen.placebos = TRUE,
                        Sigf.ipop = 3
)


## Plot with the average path of the treated units and the average of their
## respective synthetic controls:
multi$p
## Bootstrap the placebo units to get a distribution of placebo average
## treatment effects, and plot the distribution with a vertical line
## indicating the actual ATT:
plac.dist(multi,  nboots = 2000)


