
# File  : run_analysis longitudinal.R ------------------

# Check prevalence and longitudinal studies for association with exposure
# variables, by age group and condition. Written by Georgia Salanti.



# Perform pre-during analysis for longitudinal studies -------------------------

#create pairwise data from data.longi
dat<-data%>% 
  filter(days_after_first < 365) %>% 
  filter(main_analysis==T) %>% 
  select(record_id,
         timepoint,
         is_prepandemic,
         condition,
         diagnosed,
         sample_size,
         population,
         authoryear,
         days_after_first,
         threshold,scale,
         country,
         pop0_percent_female,
         pop0_central_age,
         gdp_per_capita_2019,
         gini_2019) %>%
  arrange(record_id, condition, timepoint) %>%
  group_by(record_id, condition) %>%
  mutate(timeID = row_number())

data.preduring<-NULL
for(i in names(sort(table(dat$condition),T))){
  #print(i)
  pairwisedata<-pairwise(data=dat %>% filter(condition==i),treat=timeID,studlab=record_id, n=sample_size, event=diagnosed,sm="OR",keep.all.comparisons=T)
  pairwisedata$seTE.adj<-netmeta(pairwisedata)$seTE.adj
  data.preduring<-bind_rows(data.preduring,pairwisedata)
  }

data.preduring<-data.preduring %>% filter(days_after_first1==0)#select comparisons pre-during only

#meta-analysis of pre vs during ORs using dichotomous outcomes
meta.preduringBin <- meta::metabin(data = data.preduring,
                                event.e=event2,
                                n.e=n2,
                                event.c=event1,
                                n.c=n1,
                                sm="OR",
                              studlab = record_id,
                              subgroup = condition,
                              fixed = FALSE,
                              prediction = TRUE,
                              title="Meta-analysis of pre- versus during the pandemic OR",
                              prediction.subgroup = TRUE,
                              tau.common = FALSE)
meta.preduringIV <- meta::metagen(data = data.preduring,
                                 TE=-TE,
                                   seTE=seTE.adj,
                                   sm="OR",
                                   studlab = record_id,
                                   subgroup = condition,
                                   fixed = FALSE,
                                   prediction = TRUE,
                                   title="Meta-analysis of pre- versus during the pandemic OR",
                                   prediction.subgroup = TRUE,
                                   tau.common = FALSE)


forest(meta.preduringBin,
       xlab="OR for of people above the threshold",
       smlab="OR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=F,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear","country","population", "event1","n1", "event2","n2","days_after_first2"),
       leftlabs=c("Study","country","population", "Cases pre","N pre", "Cases during","N during", "Days since first case"),
       subgroup.name="",
       test.subgroup =F,
       sortvar =days_after_first2)

forest(meta.preduringIV,
       xlab="OR for of people above the threshold",
       smlab="OR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=F,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear","country","population", "event1","n1", "event2","n2","days_after_first2"),
       leftlabs=c("Study","country","population", "Cases pre","N pre", "Cases during","N during", "Days since first case"),
       subgroup.name="",
       test.subgroup =F,
       sortvar =days_after_first2)



# Subgroups by scale --------
# The following analyses are only for the three most common conditions: 
data.preduringBinD<-data.preduring %>% filter(condition=="Depression" )
data.preduringBinA<-data.preduring %>% filter(condition=="Anxiety" )
data.preduringBinPD<-data.preduring %>% filter(condition=="Psychological Distress" )

### Subgrouping by scale Depression------

meta.preduringBinD <- meta::metabin(data = data.preduringBinD,
                                         event.e=event2,
                                         n.e=n2,
                                         event.c=event1,
                                         n.c=n1,
                                         sm="OR",
                                         studlab = record_id,
                                         subgroup = scale,
                                         fixed = FALSE,
                                         prediction = TRUE,
                                         title="Meta-analysis of pre- versus during the pandemic OR (Depression)",
                                         prediction.subgroup = TRUE,
                                         tau.common = T)

forest(meta.preduringBinD,
       xlab="OR for of people above the threshold (Depression)",
       smlab="OR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=T,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear", "threshold"),
       leftlabs=c("Study", "threshold"),
       subgroup.name="",
       sortvar=threshold,
       test.subgroup =T)

### Subgrouping by scale Anxiety------

meta.preduringBinA <- meta::metabin(data = data.preduringBinA,
                                    event.e=event2,
                                    n.e=n2,
                                    event.c=event1,
                                    n.c=n1,
                                    sm="OR",
                                    studlab = record_id,
                                    subgroup = scale,
                                    fixed = FALSE,
                                    prediction = TRUE,
                                    title="Meta-analysis of pre- versus during the pandemic OR (Anxiety)",
                                    prediction.subgroup = TRUE,
                                    tau.common = T)

forest(meta.preduringBinA,
       xlab="OR for of people above the threshold (Anxiety)",
       smlab="OR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=T,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear", "threshold"),
       leftlabs=c("Study", "threshold"),
       subgroup.name="",
       sortvar=threshold,
       test.subgroup =T)



### Subgrouping by scale Psychological Distress ------

meta.preduringBinPD <- meta::metabin(data = data.preduringBinPD,
                                    event.e=event2,
                                    n.e=n2,
                                    event.c=event1,
                                    n.c=n1,
                                    sm="OR",
                                    studlab = record_id,
                                    subgroup = scale,
                                    fixed = FALSE,
                                    prediction = TRUE,
                                    title="Meta-analysis of pre- versus during the pandemic OR (Psychological Distress)",
                                    prediction.subgroup = TRUE,
                                    tau.common = T)

forest(meta.preduringBinPD,
       xlab="OR for of people above the threshold (Psychological Distress)",
       smlab="OR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=T,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear", "threshold"),
       leftlabs=c("Study", "threshold"),
       subgroup.name="",
       sortvar=threshold,
       test.subgroup =T)

## Adults subgroup ---------------------------------------------------------

meta.preduringBinAdults <- meta::metabin(data = data.preduring[data.preduring$population=="Adults",],
                                         event.e=event2,
                                         n.e=n2,
                                         event.c=event1,
                                         n.c=n1,
                                         sm="OR",
                                         studlab = record_id,
                                         subgroup = condition,
                                         fixed = FALSE,
                                         prediction = TRUE,
                                         title="Meta-analysis of pre- versus during the pandemic OR",
                                         prediction.subgroup = TRUE,
                                         tau.common = FALSE
)

forest(meta.preduringBinAdults,
       xlab="OR for of people above the threshold",
       smlab="OR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=F,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear"),
       leftlabs=c("Study"),
       subgroup.name="",
       sortvar=threshold,
       test.subgroup =F)



# Meta-regressions for the three main conditions ------ 


data.preduringBinDAPD<-data.preduring %>% filter(condition=="Depression" |condition=="Anxiety"|condition=="Psychological Distress" )
data.preduringBinDAPD$pop0_central_age<-data.preduringBinDAPD$pop0_central_age-30
data.preduringBinDAPD$gini_2019<-data.preduringBinDAPD$gini_2019-min(data.preduringBinDAPD$gini_2019,na.rm=T)
data.preduringBinDAPD$gdp_per_capita_2019<-(data.preduringBinDAPD$gdp_per_capita_2019-min(data.preduringBinDAPD$gdp_per_capita_2019,na.rm=T))/1000


meta.preduringBinDAPD <- meta::metabin(data = data.preduringBinDAPD,
                                     event.e=event2,
                                     n.e=n2,
                                     event.c=event1,
                                     n.c=n1,
                                     sm="OR",
                                     studlab = record_id,
                                     fixed = FALSE,
                                     prediction = TRUE
                                     )
meta.preduringBinDAPDunadjusted <- metareg(meta.preduringBinDAPD,condition,intercept=F)

#age regression
meta.preduringBinDAPDage<-metareg(meta.preduringBinDAPD, pop0_central_age+condition, intercept=F)
meta.preduringBinDAPDfemales<-metareg(meta.preduringBinDAPD, pop0_percent_female+condition, intercept=F)
meta.preduringBinDAPDgdp<-metareg(meta.preduringBinDAPD, gdp_per_capita_2019+condition, intercept=F)
meta.preduringBinDAPDgini<-metareg(meta.preduringBinDAPD, gini_2019+condition, intercept=F)

#Create table with meta-regression results
metaregResults<-rbind.data.frame(
  cbind(meta.preduringBinDAPDunadjusted$k,
        meta.preduringBinDAPDunadjusted$b,
        meta.preduringBinDAPDunadjusted$ci.lb,
        meta.preduringBinDAPDunadjusted$ci.ub,
        meta.preduringBinDAPDunadjusted$tau2),
cbind(meta.preduringBinDAPDage$k,
      meta.preduringBinDAPDage$b,
      meta.preduringBinDAPDage$ci.lb,
      meta.preduringBinDAPDage$ci.ub,
     meta.preduringBinDAPDage$tau2),
cbind(meta.preduringBinDAPDfemales$k,
      meta.preduringBinDAPDfemales$b,
      meta.preduringBinDAPDfemales$ci.lb,
      meta.preduringBinDAPDfemales$ci.ub,
      meta.preduringBinDAPDfemales$tau2),
cbind(meta.preduringBinDAPDgdp$k,
      meta.preduringBinDAPDgdp$b,
      meta.preduringBinDAPDgdp$ci.lb,
      meta.preduringBinDAPDgdp$ci.ub,
      meta.preduringBinDAPDgdp$tau2),
cbind(meta.preduringBinDAPDgini$k,
      meta.preduringBinDAPDgini$b,
      meta.preduringBinDAPDgini$ci.lb,
      meta.preduringBinDAPDgini$ci.ub,
      meta.preduringBinDAPDgini$tau2))
metaregResults[,2:4]<-exp(metaregResults[,2:4])

colnames(metaregResults)<-c("Number of studies","OR","CIlow","CIhigh", "tau2")
rownames(metaregResults)<-NULL
dapd<-c("Anxiety","Depression","Psychological Distress")
metaregResults<-cbind.data.frame(parameter=c(dapd,"Mean age in years",dapd,"Percentage of females",dapd,"GDP per capita (in 1000$)",dapd,"GINI index",dapd),metaregResults)


# Sensitivity analysis using RR ----

data.preduringRR<-NULL
for(i in names(sort(table(dat$condition),T))){
  pairwisedata<-pairwise(data=dat %>% filter(condition==i),treat=timeID,studlab=record_id, n=sample_size, event=diagnosed,sm="RR",keep.all.comparisons=T)
  pairwisedata$seTE.adj<-netmeta(pairwisedata)$seTE.adj
  data.preduringRR<-bind_rows(data.preduringRR,pairwisedata)
}

data.preduringRR<-data.preduringRR %>% filter(days_after_first1==0)#select comparisons pre-during only

meta.preduringIVRR <- meta::metagen(data = data.preduringRR,
                                  TE=-TE,
                                  seTE=seTE.adj,
                                  sm="RR",
                                  studlab = record_id,
                                  subgroup = condition,
                                  fixed = FALSE,
                                  prediction = TRUE,
                                  title="Meta-analysis of pre- versus during the pandemic OR",
                                  prediction.subgroup = TRUE,
                                  tau.common = FALSE)

forest(meta.preduringIVRR,
       xlab="RR for of people above the threshold",
       smlab="RR",
       print.I2=F,
       lwd=1,
       fs.test.effect.subgroup=0,
       print.pval.Q=F,
       lower.equi=0,
       upper.equi=1,
       just="right",
       addrow=F,
       overall=F,
       overall.hetstat =F,
       col.square="black",
       col.by="black",
       hetlab="",
       fill.equi="aliceblue",
       calcwidth.subgroup=T,
       prediction=T,
       text.random = "Random Effects",
       text.predict = "Prediction interval",
       calcwidth.pooled = F,
       calcwidth.predict = F,
       leftcols = c("authoryear","country","population", "event1","n1", "event2","n2","days_after_first2"),
       leftlabs=c("Study","country","population", "Cases pre","N pre", "Cases during","N during", "Days since first case"),
       subgroup.name="",
       test.subgroup =F,
       sortvar =days_after_first2)

# Perform dose-response meta-analysis for longitudinal studies ------------------------------------------------------------------------
## 
data.longi<-data
doseres_D<-dosresconx.fun(data.longi %>% filter(condition=="Depression"),condition.name="Depression",myylim=c(0.6,7))
doseres_A<-dosresconx.fun(data.longi %>% filter(condition=="Anxiety"),condition.name="Anxiety",myylim=c(0.6,7))
doseres_PD<-dosresconx.fun(data.longi %>% filter(condition=="Psychological Distress")  ,condition.name="Psychological distress",myylim=c(0.6,7))

# Create figure 3 -------
# par(mfrow=c(3,4))
# doseres_D<-dosresconx.fun(data.longi %>% filter(condition=="Depression"),condition.name="Depression",myylim=c(0.6,7),save.plot=F)
# doseres_A<-dosresconx.fun(data.longi %>% filter(condition=="Anxiety"),condition.name="Anxiety",myylim=c(0.6,7),save.plot=F)
# doseres_PD<-dosresconx.fun(data.longi %>% filter(condition=="Psychological Distress")  ,condition.name="Psychological distress",myylim=c(0.6,7),save.plot=F)
# mtext('Depression', side=1, line=-67, outer=TRUE)
# mtext('Anxiety', side=1, line=-45, outer=TRUE)
# mtext('Psychological Distress', side=1, line=-22, outer=TRUE)
######

# Sensitivity analysis in dose-response meta-analysis for longitudinal studies ------------------------------------------------------------------------

## Days since first case: exclude outlier; in Depression and anxiety there is an observation at 142 and then 252 ---- 

par(mfrow=c(2,1))
### Depression ------
out=data.longi %>% filter(condition=="Depression") %>% filter(days_after_first>200)
data.longi.condition=data.longi %>% filter(condition=="Depression") %>% filter(record_id!=out$record_id)

dosedata=createdatasetdoseresponse.fun(data.longi.condition,diagnosed,sample_size,record_id,days_after_first,nameoflogOR="logOR",nameofselogOR="selogOR")
dosedata$type<-"ir"   
knots = quantile(dosedata$days_after_first,c(0.20,0.5,0.8))
doseresDAYS = dosresmeta(logOR ~ rcs(days_after_first,knots), record_id, data = dosedata,
                         cases = diagnosed, n = sample_size,
                         type = type, se = selogOR, proc = "1stage")


newdata = data.frame(days_after_first = seq(0,max(data.longi.condition$days_after_first,na.rm=T)))
xref = 0
with(predict(doseresDAYS, newdata, xref, exp = TRUE), {
  plot(get("rcs(days_after_first, knots)days_after_first"), pred, log = "y", type = "l",lwd=2, 
       xlim = c(0, max(data.longi.condition$days_after_first,na.rm=T)), ylim = c(0.6,7),
       xlab = expression("Days since"~'the first case'), ylab = expression("OR for depression (up to 150 days)"))
  matlines(get("rcs(days_after_first, knots)days_after_first"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed")
})
with(dosedata, rug(days_after_first, quiet = TRUE))
abline(h=1,col=4,lty=3)

### Anxiety ------
out=data.longi %>% filter(condition=="Anxiety") %>% filter(days_after_first>200)
data.longi.condition=data.longi %>% filter(condition=="Anxiety") %>% filter(record_id!=out$record_id)

dosedata=createdatasetdoseresponse.fun(data.longi.condition,diagnosed,sample_size,record_id,days_after_first,nameoflogOR="logOR",nameofselogOR="selogOR")
dosedata$type<-"ir"   
knots = quantile(dosedata$days_after_first,c(0.20,0.5,0.8))
doseresDAYS = dosresmeta(logOR ~ rcs(days_after_first,knots), record_id, data = dosedata,
                         cases = diagnosed, n = sample_size,
                         type = type, se = selogOR, proc = "1stage")


newdata = data.frame(days_after_first = seq(0,max(data.longi.condition$days_after_first,na.rm=T)))
xref = 0
with(predict(doseresDAYS, newdata, xref, exp = TRUE), {
  plot(get("rcs(days_after_first, knots)days_after_first"), pred, log = "y", type = "l",lwd=2, 
       xlim = c(0, max(data.longi.condition$days_after_first,na.rm=T)), ylim = c(0.6,7),
       xlab = expression("Days since"~'the first case'), ylab = expression("OR for anxiety (up to 150 days)"))
  matlines(get("rcs(days_after_first, knots)days_after_first"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed")
})
with(dosedata, rug(days_after_first, quiet = TRUE))
abline(h=1,col=4,lty=3)


## Changing knot location at 0.3,0.5 and 0.7----

doseres_DSenscloser<-dosresconx.fun(data.longi %>% filter(condition=="Depression"),condition.name="Depression",myylim=c(0.6,7),knotpercent=c(0.3,0.5,0.8),additional.file.name=" knots closer ")#does not work for location 0.7 because same as 0.5

doseres_ASenscloser<-dosresconx.fun(data.longi %>% filter(condition=="Anxiety") %>% filter(record_id!=162331),condition.name="Anxiety",myylim=c(0.4,6), knotpercent=c(0.3,0.5,0.7), additional.file.name=" knots closer ")

doseres_PDSenscloser<-dosresconx.fun(data.longi %>% filter(condition=="Psychological Distress") ,condition.name="Psychological distress",myylim=c(0.4,6),knotpercent=c(0.3,0.5,0.7), additional.file.name=" knots closer ")


## Changing knot location at 0.1,0.5 and 0.9---- 

doseres_DSensfurther<-dosresconx.fun(data.longi %>% filter(condition=="Depression"),condition.name="Depression",myylim=c(0.6,7),knotpercent=c(0.1,0.5,0.9),additional.file.name=" knots further ")

doseres_ASensfurther<-dosresconx.fun(data.longi %>% filter(condition=="Anxiety") %>% filter(record_id!=162331),condition.name="Anxiety",myylim=c(0.4,6), knotpercent=c(0.1,0.5,0.9), additional.file.name=" knots further ")

doseres_PDSensfurther<-dosresconx.fun(data.longi %>% filter(condition=="Psychological Distress") ,condition.name="Psychological distress",myylim=c(0.4,6),knotpercent=c(0.1,0.5,0.9), additional.file.name=" knots further ")


