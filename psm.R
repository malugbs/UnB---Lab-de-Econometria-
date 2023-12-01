library(Matching) 
library(foreign)
library(stargazer)
library(texreg)
library(sandwich)
library(lmtest)
library(mfx)
library(miceadds)
library(tidyverse)
library(AER)
library(car)
library(survival)
library(MatchIt)
library(MatchItSE)
install.packages("MatchItSE")
#Import Dataset based on BRUNE ET AL 2006)

#path & file name

path<-"C:/Users/guima/Documents/R/Laboratório de Econometria/replic_data.dta"
mydata <- read.dta(path, convert.factors=FALSE,convert.dates=TRUE, convert.underscore=TRUE)

#gen dummy of treatment arm
mydata$tr<- ifelse(mydata$treatment>=1 & mydata$treatment<=6 ,1,0)	

# divide by 10,000 for easy of display
listvar<-c("bl.totsavcash.099", "bl.transNet.199", "bl.inpexp.099", "bl.salecrop.099")
mydata[,listvar]<-lapply(mydata[,listvar],  function(i) i/10000)

#Baseline characteristics list

xlist<- c("bl.d.female",	"bl.d.married",	"bl.age.099",	"bl.eduyrs.099",	
          "bl.hhnum.099",	"bl.asset.pca.199",	"bl.livest.pca.199", 
          "bl.acre.adj.099",	"bl.d.anyformalacc",	"bl.totsavcash.099",	"bl.d.hyper",
          "bl.d.hypo",	"bl.inpexp.099",	"bl.salecrop.099", 	"bl.transNet.199"
)


ylist<- c("el.landcult.099", "el.input.val.099", "el.maizetobasale.099", 
          "el.mainoutput.099", "el.farmprofit.199", "el.exp30.099"
)

#Setting seed

set.seed(1234567)
mydata[,"aleatorio"] <- runif(nrow(mydata))
mydata <- mydata[order(mydata$aleatorio, method = "auto"),]
rownames(mydata) <- NULL

model.1<- (tr~bl.d.female+bl.d.married+bl.age.099+bl.eduyrs.099+bl.hhnum.099+bl.asset.pca.199+
             bl.livest.pca.199+bl.acre.adj.099+bl.d.anyformalacc+bl.totsavcash.099+bl.d.hyper+
             bl.d.hypo+bl.inpexp.099+bl.salecrop.099+bl.transNet.199)
model.2<- (has.anyActive.o22~bl.d.female+bl.d.married+bl.age.099+bl.eduyrs.099+bl.hhnum.099+bl.asset.pca.199+
             bl.livest.pca.199+bl.acre.adj.099+bl.d.anyformalacc+bl.totsavcash.099+bl.d.hyper+
             bl.d.hypo+bl.inpexp.099+bl.salecrop.099+bl.transNet.199)    
model.3<- (trans.d.any~bl.d.female+bl.d.married+bl.age.099+bl.eduyrs.099+bl.hhnum.099+bl.asset.pca.199+
             bl.livest.pca.199+bl.acre.adj.099+bl.d.anyformalacc+bl.totsavcash.099+bl.d.hyper+
             bl.d.hypo+bl.inpexp.099+bl.salecrop.099+bl.transNet.199)    


mydata.filtered <-mydata[,c("tr","bl.d.female",	"bl.d.married",	"bl.age.099",	"bl.eduyrs.099",	
                            "bl.hhnum.099",	"bl.asset.pca.199",	"bl.livest.pca.199", 
                            "bl.acre.adj.099",	"bl.d.anyformalacc",	"bl.totsavcash.099",	"bl.d.hyper",
                            "bl.d.hypo",	"bl.inpexp.099",	"bl.salecrop.099", 	"bl.transNet.199", "el.landcult.099", "el.input.val.099", "el.maizetobasale.099", 
                            "el.mainoutput.099", "el.farmprofit.199", "el.exp30.099", "has.anyActive.o22", "trans.d.any")]

mydata.filtered$id<-row.names(mydata.filtered)
mydata.filtered <- subset(mydata.filtered, complete.cases(mydata.filtered))
mydata.filtered$id<-as.numeric(as.character(mydata.filtered$id))

m.out.2 <- matchit(model.2,data = mydata.filtered[,c("has.anyActive.o22","bl.d.female",	"bl.d.married",	"bl.age.099",	"bl.eduyrs.099",	
                                                     "bl.hhnum.099",	"bl.asset.pca.199",	"bl.livest.pca.199", 
                                                     "bl.acre.adj.099",	"bl.d.anyformalacc",	"bl.totsavcash.099",	"bl.d.hyper",
                                                     "bl.d.hypo",	"bl.inpexp.099",	"bl.salecrop.099", 	"bl.transNet.199"
)],distance="glm", link="probit", method="nearest", m.order="largest", discard="both", replace=TRUE, ratio=1)

summary(m.out.2)

m.data.2 <- match.data(m.out.2)

m.data.2$noweights<- 1

#unmatched
#assigned to treatment and mantain an account
t.table(data=mydata.filtered,xlist,"has.anyActive.o22")

#matched
wtd.t.table(data=m.data.2,xlist,"has.anyActive.o22",weightvar=c("weights"))

for (i in ylist) {
  cat(i ,
      "ATT=", att(obj = m.out.2, Y = mydata.filtered[,i]),
      "AI Standard Errors=", abadie_imbens_se(m.out.2, mydata.filtered[,i]),
      "t-statistic = ", att(obj = m.out.2, Y = mydata.filtered[,i])/abadie_imbens_se(m.out.2, mydata.filtered[,i]),
      "p-value = ", p.value = 2*pt(-abs(att(obj = m.out.2, Y =mydata.filtered[,i])/abadie_imbens_se(m.out.2, mydata.filtered[,i])), df=length(m.out.2)-1), sep="\n")
}
par(mar = c(2,2,2,2))
plot(m.out.2, discrete.cutoff = 5, type = "hist", numdraws = 5000, interactive = TRUE, which.xs = NULL)

