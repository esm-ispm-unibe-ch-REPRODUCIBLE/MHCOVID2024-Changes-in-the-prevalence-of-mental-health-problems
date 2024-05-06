TableStudiesTime.fun<-function(dataset,mycaption="Number of studies and timepoints per condition"){ #creates tables of Number of studies and timepoints per condition
  a<-apply(table(dataset$out_pop,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$out_pop,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("Population", "Nr of Studies","Nr of timepoints") ))
  
  a<-apply(table(dataset$condition,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$condition,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("Condition", "Nr of Studies","Nr of timepoints")))
  
  
  a<-apply(table(dataset$country,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$country,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("Country", "Nr of Studies","Nr of timepoints")))
  
  a<-apply(table(dataset$study_design,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$study_design,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("Design", "Nr of Studies","Nr of timepoints")))
  
  
  a<-apply(table(dataset$rob_info_bias,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$rob_info_bias,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("Risk of information bias", "Nr of Studies","Nr of timepoints")))
  
  a<-apply(table(dataset$rob_is_target_pop,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$rob_is_target_pop,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("General population (risk of difference)", "Nr of Studies","Nr of timepoints")))
  
  a<-apply(table(dataset$rob_non_bias,dataset$record_id)>0,1,sum)
  print(kableExtra::kable(
    cbind.data.frame(
      condition=names(a),
      studies=as.numeric(a),
      timepoints=as.double(margin.table(table(dataset$rob_non_bias,dataset$record_id),1))), 
    format = "markdown",
    digits = 3,
    caption = mycaption,
    col.names = c("Risk of non-response bias", "Nr of Studies","Nr of timepoints")))
}

forcebind.fun = function(df1, df2, df3, df4,df5, df6,df7) {
  colnames(df2) = colnames(df1)=colnames(df3) = colnames(df4)=colnames(df5) = colnames(df6)=colnames(df7)
  rbind.data.frame(df1, df2, df3, df4,df5, df6,df7)
}




UnpairedAnalysis <- function(data, title, binary, sex) {
  # Check if we should do meta-analysis or subgroup analysis
  sex.subset <- sex  # we need to create a separate variable for filtering to work
  data <- data %>%
    filter(sex == sex.subset) %>%
    filter(!is.na(diagnosed))
  if (requiresMetaAnalysis(data, binary = binary, sex = sex.subset)) {
    metaorsub <- TRUE
    meta <- metaprop(event = diagnosed,
                    n = sample_size,
                    studlab = record_id,
                    data = data,
                    method = "GLMM",
                    prediction = TRUE,common = F,prediction.subgroup=T,overall=F,
                    method.tau.ci = "QP",
                    subset = (sex == sex.subset))
  } else {
    metaorsub <- FALSE
    meta <- metaprop(event = diagnosed,
                     n = sample_size,
                     studlab = record_id,
                     data = data,
                     method="GLMM",
                     prediction = TRUE,common = F,prediction.subgroup=T,overall=F,
                     method.tau.ci = "QP",
                     subset = (sex == sex.subset),
                     tau.common = F,
                     subgroup = out_pop)
  }
  return(meta)
}


PlotUnpairedAnalysis <- function(meta, condition) {
  # Draw a forest plot of the result
  if(min(meta$k.w)==1 & meta$k<10){
    meta::forest(meta,
                 xlab = paste("Proportion above the threshold for", condition),
                 smlab = "Prevalence",
                 plotwidth = "5cm",
                 print.I2 = FALSE,
                 fontsize = 9,
                 fs.test.effect.subgroup = 0,
                 fs.hetstat = 7,
                 lower.equi = 0,
                 upper.equi = 1,
                 xlim = c(0,1),
                 just = "left",
                 addrow = TRUE,
                 colgap = "0.1cm",
                 col.by = "black",
                 fill.equi = "aliceblue",
                 leftcols = c("authoryear","diagnosed","sample_size"),
                 leftlabs=c("Study", "Cases","N"),
                 subgroup.name="")
  }else
  {forest <- meta::forest(update(meta, tau.common = F),
                         study.results = FALSE,
                         resid.hetstat = F,
                         
                         prediction = TRUE,
                         comb.fixed = FALSE,
                         pooled.totals = TRUE,
                         pooled.events = TRUE,
                         xlab = paste("Proportion above the threshold for", condition),
                         smlab = "Prevalence",
                         plotwidth = "5cm",
                         print.I2 = FALSE,
                         fontsize = 9,
                         fs.test.effect.subgroup = 0,
                         fs.hetstat = 7,
                         lower.equi = 0,
                         upper.equi = 1,
                         xlim = c(0,1),
                         just = "left",
                         addrow = TRUE,
                         colgap = "0.1cm",
                         col.by = "black",
                         fill.equi = "aliceblue",
                         calcwidth.subgroup = TRUE,
                         text.random.w = sapply(meta$k.w, paste, "studies (RE model)"),
                         print.pval.Q = FALSE,
                         subgroup.name="",
                         prediction.subgroup=T)
  }
  # grid.text(title, .5, .9, gp=gpar(cex=1.3))
}


requiresMetaAnalysis <- function(dataset, binary = TRUE, sex) {
  # Determines if there are enough data to perform subgroup analyses, or only
  # meta-analysis.
  if (binary) {
    diagnosed.outcomes <- dataset %>% filter(!is.na(diagnosed) & sex == sex)
  } else {
    diagnosed.outcomes <- dataset %>% filter(!is.na(sey) & sex == sex)
  }
  lhs <- nrow(diagnosed.outcomes)
  rhs <- length(unique(diagnosed.outcomes$out_pop))
  if (lhs <= rhs | rhs < 2) {
    do.meta <- TRUE
  } else {
    do.meta <- FALSE
  }
  # do.meta is TRUE when there are not enough data for subgroup analyses
  return(do.meta)
}

format.bar <- function(n = 0, string = NA) {
  # Convenience function to print separating bars in output file
  if (!is.na(string)) {
    n <- nchar(string)
  }
  paste0(paste(rep("-", n), collapse = ""), "\n")
}

new.line <- function() {
  return("\n")
}

print.output <- function(output, cex = 0.7) {
  tmp <- capture.output(output)
  plot.new()
  text(0, 1, paste(tmp, collapse='\n'), adj = c(0,1), family = 'mono', cex = cex)
  box()
}



##########################################
#FUNCTIONS NEEDED in the dose-response analysis
###########################################

#a simple function that excludes single-arm studies
exludesinglearmsdata.fun<-function(dataset,studyid)
{
  #the dataset to correct dataset
  # studyid: defining the name of the studyid column
  studyid=eval(substitute(studyid), dataset)
  singlearmstudies=names(table(studyid))[table(studyid)<2]
  dataset2=dataset[is.na(match(studyid,singlearmstudies)),]
  dataset2
  #returns a dataset with the same columns after excluding multiarms
}

# A FUNCTION THAT TAKES A DATABASE AND CREATES LOGOR AND THE SE FOR A DOSE-RESPONSE ANALYSIS

createRRreference.fun=function(r,n)
{
  
  logOR=c(0)
  selogOR=c(NA)
  
  for(i in 2:c(length(n)))
  {
    calculate=metabin(r[i],n[i],r[1],n[1],sm="OR")
    logOR=c(logOR,calculate$TE)
    selogOR=c(selogOR,calculate$seTE)
    
  }
  return(cbind(logOR=logOR,selogOR=selogOR))
}

# A FUNCTION THAT CREATES THE DATA IN DOSE-RESPONSE FORMAT
createdatasetdoseresponse.fun<-function(dataset,r,n,studyid,dose, nameoflogOR="logOR",nameofselogOR="selogOR"){
  
  #--------------------------------------------
  #this function takes a dataset and a) orders the within-study data by dose b)checks and excludes single arm studies
  # c) calculates OR and selogOR to be used in the dose-response model
  
  #the dataset: the dataset, one arm per row
  # r,n,studyid,dose: defining the events, sample size, studyid and dose columns
  # nameoflogOR="logOR",nameofselogOR="selogOR": define the names of the output columns to be added to the dataset
  
  # Needs the functions:exludesinglearmsdata.fun, createRRreference.fun
  #--------------------------------------------
  
  originaldimension=dim(dataset)[2]
  r=eval(substitute(r), dataset)
  n=eval(substitute(n), dataset)
  dose=eval(substitute(dose), dataset)
  studyid=eval(substitute(studyid), dataset)
  
  #Order within study data from the lowest to the highest dose
  dataset=dataset[with(dataset,order(studyid,dose)),]
  
  #Exclude single arm studies
  dataset=exludesinglearmsdata.fun(dataset, studyid)
  
  #create the variables
  ID=unique(studyid)
  logOR=c()
  selogOR=c()
  RRmat=c()
  
  for(i in ID){
    r1=c(r[studyid==i])
    n1=c(n[studyid==i])
    a=createRRreference.fun(r1,n1)
    
    RRmat=rbind(RRmat,a)
  }
  RRmat=as.data.frame(RRmat)
  names(RRmat)=c(nameoflogOR,nameofselogOR)
  
  #create the final dataset by adding the logOR and selogOR
  dataset=cbind.data.frame(dataset,RRmat)
  #dataset$logOR=RRmat[,1]
  #dataset$selogOR=RRmat[,2]
  #names(dataset)[originaldimension+1]=nameoflogOR
  #names(dataset)[originaldimension+2]=nameofselogOR
  #return
  dataset
}



######################

#this function clears the data out of problems that may make the code in dose-reponse not running

cleandosresdata.fun=function(dataset,studyid,logOR,r,n,dose,nametoexclude="toexclude")
{ 
  # this function takes a database and exclude studies that will cause problemsin fiting the dosres model for a particular outcome
  # It return the same database but in the end it has a variable that tells you which studies to exclude
  # Give a name to that column in the nametoexclude variable
  originaldimension=dim(dataset)[2]
  r=eval(substitute(r), dataset)
  logOR=eval(substitute(logOR), dataset)
  dose=eval(substitute(dose), dataset)
  n=eval(substitute(n), dataset)
  studyid=eval(substitute(studyid), dataset)
  #exlude missing logOR
  out0=unique(studyid[is.na(logOR)])
  #exclude studies with zero events
  out1=unique(studyid[r==0 & !is.na(r)])
  out2=unique(studyid[r==n & !is.na(r)])
  #exclude those studies with the same dose in all arms
  out3=unique(studyid)[tapply(dose,studyid,max)==tapply(dose,studyid,min)]
  toexclude=as.data.frame(studyid%in%c(out0,out1,out2,out3))
  names(toexclude)=c(nametoexclude)
  #exclude single arm studies
  dataset=cbind.data.frame(dataset,toexclude)
  dataset
}

######################

#this function clears the data out of problems that may make the code in dose-reponse not running

cleandosresdata1.fun=function(dataset,studyid,logOR,r,n,dose,nametoexclude="toexclude")
{ #same as function before but does not exclude zero events
  # this function takes a database and exclude studies that will cause problemsin fiting the dosres model for a particular outcome
  # It return the same database but in the end it has a variable that tells you which studies to exclude
  # Give a name to that column in the nametoexclude variable
  originaldimension=dim(dataset)[2]
  r=eval(substitute(r), dataset)
  logOR=eval(substitute(logOR), dataset)
  dose=eval(substitute(dose), dataset)
  n=eval(substitute(n), dataset)
  studyid=eval(substitute(studyid), dataset)
  #exlude missing logOR
  out0=unique(studyid[is.na(logOR)])
  #exclude those studies with the same dose in all arms
  out3=unique(studyid)[tapply(dose,studyid,max)==tapply(dose,studyid,min)]
  toexclude=as.data.frame(studyid%in%c(out0,out3))
  names(toexclude)=c(nametoexclude)
  #exclude single arm studies
  dataset=cbind.data.frame(dataset,toexclude)
  dataset
}



dosresconx.fun<-function(data.longi.condition,condition.name,myylim, pdf.pic=F,knotpercent=c(0.20,0.5,0.8),additional.file.name=" ", save.plot=T)
{
  #set output directories
# data.longi.condition=data.longi %>% filter(condition=="Psychological Distress") %>% group_modify(~ mutate(.x,is_longi2=n_distinct(timepoint)>1)) ##
# condition.name="Psychological Distress"##
# 
# data.longi.condition=data.longi %>% filter(condition=="Depression") %>% group_modify(~ mutate(.x,is_longi2=n_distinct(timepoint)>1)) ##
# condition.name="Depression"##

out.file.jpeg <- paste0("out/", condition.name,additional.file.name,"dose-response results.jpeg")
out.file.pdf <- paste0("out/", condition.name, additional.file.name,"dose-response results.pdf")
 
if(save.plot){

  if(pdf.pic==T) {
    pdf(out.file.pdf , width = 15, height = 10)
  }else(jpeg(out.file.jpeg ))
  
  par(mfrow=c(2,2))}

#Days

dosedata=createdatasetdoseresponse.fun(data.longi.condition,diagnosed,sample_size,record_id,days_after_first,nameoflogOR="logOR",nameofselogOR="selogOR")
dosedata$type<-"ir"   
# dosedata %>% select(record_id,timepoint, days_after_first,diagnosed,sample_size,logOR,selogOR)
knots = quantile(dosedata$days_after_first,knotpercent)
doseresDAYS = dosresmeta(logOR ~ rcs(days_after_first,knots), record_id, data = dosedata,
                          cases = diagnosed, n = sample_size,
                          type = type, se = selogOR, proc = "1stage")



newdata = data.frame(days_after_first = seq(0,max(data.longi.condition$days_after_first,na.rm=T)))
xref = 0
with(predict(doseresDAYS, newdata, xref, exp = TRUE), {
  plot(get("rcs(days_after_first, knots)days_after_first"), pred, log = "y", type = "l",lwd=2, 
       xlim = c(0, 250), ylim = myylim,
       xlab = expression("Days since"~'the first case'), ylab = expression("OR"))
  matlines(get("rcs(days_after_first, knots)days_after_first"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed")
})
with(dosedata, rug(days_after_first, quiet = TRUE))
abline(h=1,col=4,lty=3)

#stringency

dosedata=createdatasetdoseresponse.fun(data.longi.condition[!is.na(data.longi.condition$stringency),],diagnosed,sample_size,record_id,stringency,nameoflogOR="logOR",nameofselogOR="selogOR")
dosedata$type<-"ir"   
knots = quantile(dosedata$stringency,knotpercent) 
doseresSTR = dosresmeta(logOR ~ rcs(stringency,knots), record_id, data = dosedata,
                         cases = diagnosed, n = sample_size,
                         type = type, se = selogOR, proc = "1stage")



newdata = data.frame(stringency = seq(0,100))
xref = 0
with(predict(doseresSTR, newdata, xref, exp = TRUE), {
  plot(get("rcs(stringency, knots)stringency"), pred, log = "y", type = "l",lwd=2, 
       xlim = c(0, 100), ylim = myylim,
       xlab = expression("Stringency"~'Index'), ylab = expression("OR"))
  matlines(get("rcs(stringency, knots)stringency"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed")
})
abline(h=1,col=4,lty=3)
with(dosedata, rug(stringency, quiet = TRUE))

#cases

dosedata=createdatasetdoseresponse.fun(data.longi.condition,diagnosed,sample_size,record_id,logconfirmed_cumulative100k,nameoflogOR="logOR",nameofselogOR="selogOR")
dosedata$type<-"ir"   
knots = quantile(dosedata$logconfirmed_cumulative100k,knotpercent)
doseresCASE = dosresmeta(logOR ~ rcs(logconfirmed_cumulative100k,knots), record_id, data = dosedata,
                          cases = diagnosed, n = sample_size,
                          type = type, se = selogOR, proc = "1stage")


newdata = data.frame(logconfirmed_cumulative100k = seq(0,max(data.longi.condition$logconfirmed_cumulative100k,na.rm=T),0.1))
xref = 0
with(predict(doseresCASE, newdata, xref, exp = TRUE), {
  plot(get("rcs(logconfirmed_cumulative100k, knots)logconfirmed_cumulative100k"), pred, log = "y", type = "l",lwd=2, 
       xlim = c(0, max(data.longi.condition$logconfirmed_cumulative100k,na.rm=T)),ylim = myylim,
       xlab = expression("log-cumulative cases per"~'100.000'),  ylab = expression("OR"))
  matlines(get("rcs(logconfirmed_cumulative100k, knots)logconfirmed_cumulative100k"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed")
})
with(dosedata, rug(logconfirmed_cumulative100k, quiet = TRUE))
abline(h=1,col=4,lty=3)

#deaths

dosedata=createdatasetdoseresponse.fun(data.longi.condition,diagnosed,sample_size,record_id,logdeaths_cumulative100k,nameoflogOR="logOR",nameofselogOR="selogOR")
dosedata$type<-"ir"   
knots = quantile(dosedata$logdeaths_cumulative100k,knotpercent)
doseresDEATH = dosresmeta(logOR ~ rcs(logdeaths_cumulative100k,knots), record_id, data = dosedata,
                           cases = diagnosed, n = sample_size,
                           type = type, se = selogOR, proc = "1stage")


newdata = data.frame(logdeaths_cumulative100k = seq(0,max(data.longi.condition$logdeaths_cumulative100k,na.rm=T),0.1))
xref = 0
with(predict(doseresDEATH, newdata, xref, exp = TRUE), {
  plot(get("rcs(logdeaths_cumulative100k, knots)logdeaths_cumulative100k"), pred, log = "y", type = "l",lwd=2, 
       xlim = c(0, max(data.longi.condition$logdeaths_cumulative100k,na.rm=T)),ylim=myylim,
       xlab = expression("log-cumulative deaths per"~'100.000'),  ylab = expression("OR"))
  matlines(get("rcs(logdeaths_cumulative100k, knots)logdeaths_cumulative100k"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed")
})
with(dosedata, rug(logdeaths_cumulative100k, quiet = TRUE))
abline(h=1,col=4,lty=3)

if(save.plot){  dev.off()}

doseres_all<-list(doseresDAYS=doseresDAYS,doseresSTR=doseresSTR,doseresCASE=doseresCASE,doseresDEATH=doseresDEATH)
return(doseres_all)
}


