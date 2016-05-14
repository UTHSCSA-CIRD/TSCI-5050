#' ---
#' title: "Homework Results"
#' author: "Alex F. Bokov"
#' date: "05/13/2016"
#' precious: TRUE
#' ---
#' 
#' # Yuanhang Liu
#' ## Homework 1
#' Score: outstanding
#' Perfect responses all around. 
#+ echo=FALSE, cache=TRUE
#+ echo=FALSE,cache=TRUE
rawdeid <- subset(read.delim('deid_bmi_temp.csv',head=T),BMI<90);
rawdeid$AGE <- rawdeid$AGE/365;
rawdeid$BMI <- as.numeric(as.character(rawdeid$BMI));
rawdeid$TEMPERATURE <- as.numeric(as.character(rawdeid$TEMPERATURE));
rawdeid$BMI <- as.numeric(as.character(rawdeid$BMI));
rawdeid$BMI_BIN <- factor(sign(scale(rawdeid$BMI,center = 28.59999)),labels=c('low','hi'));
levels(rawdeid$BMI_BIN) <- c('low','hi','hi');
levels(rawdeid$SEX)<-c('f','m','m');
levels(rawdeid$RACE)<-ifelse((xx<-levels(rawdeid$RACE))%in%c('other','black','white'),xx,'other')
rawdeid$Y[rawdeid$SEX=='f']<- with(subset(rawdeid,SEX=='f'),-1+BMI/2.5+0.01*TEMPERATURE+rnorm(length(SEX)));
rawdeid$Y[rawdeid$SEX!='f']<- with(subset(rawdeid,SEX!='f'),17+0.007*TEMPERATURE-.002*AGE^2+rnorm(length(SEX)));
rawdeid$Y[rawdeid$RACE=='black'&rawdeid$SEX=='f'] <- with(subset(rawdeid,RACE=='black'&SEX=='f'),Y+5-BMI/2-AGE^2*.001);
rawdeid.sam <- subset(rawdeid, PATIENT_NUM %in% unique(c(sample(PATIENT_NUM,100),sample(unique(PATIENT_NUM[SEX=='f'&RACE=='black']),15),sample(unique(PATIENT_NUM[SEX=='m'&RACE=='black']),15))));

#' Nice function, for the last question. I like the fact that you are 
#' taking advantage of the fact that you can pass functions as variables
#' and learning to use file-handlers.
my.write <- function(x, file, header, f = write.csv, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file
  if(!missing(header)) {
    writeLines(header,con=datafile, sep='\t')
    writeLines('', con=datafile)
  }
  # write the file using the defined function and required addition arguments  
  f(x, datafile,...)
}

#' Note, though, that it has the following problems:
#' 
#' * The `sep` argument is hardcoded to '\t' while write.csv uses ',' as its separator. Therefore, if you use the `headers` argument, all the headers will be smushed into the first row, first column.
#' * The column name is still off-by-one
#' 
#' The below version fixes both problems.
my.write <- function(x, file, header, f = write.csv, sepr=',',...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file
  if(!missing(header)) {
    writeLines(c('',header),con=datafile, sep=sepr)
    writeLines('', con=datafile)
  }
  # write the file using the defined function and required addition arguments  
  f(x, datafile,...)
}

#' However, even so, the following commands create files that are identical
#' to each other:
my.write(rawdeid.sam,'foo.csv')
write.csv(rawdeid.sam,'bar.csv')

#' The problem is really that usually useless column of row-names. Here is how
#' you override that behavior:
write.csv(rawdeid.sam,'baz.csv',row.names = F)

#' ## Homework 2
#' Score: excellent
#'
#' ## Homework 3
#' Score: outstanding
#' 
#' I had expected people to use the SQLite .db file they created during HW 02
#' but this is just as good, if not better. If R is more convenient for 
#' importing data into SQLite than a dedicated SQLite client, then by all 
#' means, do that. 
#' 
#' # Desiree Wilson
#' ## Homework 1
#' Score: good
#' 
#' ### Question 4
#' No function, but that's okay, don't worry about it-- given how much R you use
#' I'm sure you've written functions in R by now.
#' 
#' ## Homework 2
#' Score: good
#' 
#' ### Question 30 in part 1:
#' 
#' > select * from robots where substr(location, -2) like 'ny' 
#' 
#' Note: this works, but gives the same results as an exact match (`=`) and the
#' latter is faster on large datasets.
#' 
#' ### Part 3
#' 
#' > Sorry. I totally forgot that SQLite is a flat file-based database system. The TCGA data gives out information inf XML format. I plan to use my own data to answer the questions in HW02 and HW03. 
#' 
#' Actually, SQLite is a relational database system. What dictates how to best 
#' store data is not its original format but what you plan to do with it. If 
#' the tools you are using are designed to work with XML files in the first 
#' place or if there already exists some export tool for getting TCGA data into
#' a tabular format, you don't need to worry about it.
#' 
#' If in the future you ever do need get XML data into SQLite, the following 
#' post might be helpful: 
#' http://stackoverflow.com/questions/2085430/populating-data-from-xml-file-to-a-sqlite-database-using-python
#' 
#' ### 3.2
#' 
#' Not broken up by group. Here is how it should look:
#' 
#'     select patientoutcome
#'     , min(samplepurity) min
#'     , avg(samplepurity) avg
#'     ,max(samplepurity) max
#'     ,count(*) count 
#'     from 
#'     PhenotypeGEdata group by patientoutcome
#' 
#+ echo=FALSE,message=FALSE,results='asis'
library(RSQLite); con <- dbConnect(SQLite(), dbname='DesireeWilson/test');
dat <- dbGetQuery(con, 'select * from PhenotypeGEdata');
library(pander); panderOptions('table.split.table',Inf);
pander(dbGetQuery(con, 'select patientoutcome,min(samplepurity) min, avg(samplepurity) avg,max(samplepurity) max,count(*) count from PhenotypeGEdata group by patientoutcome'))
#' 
#' ## Homework 3
#' Score: outstanding
#' 
#' ### 3.
#' I suggest cleaning up the data in the `data.frame` rather than in the model
#' because otherwise the internal naming of the variables might get messed up.
dat$SamplePurity<-as.numeric(dat$SamplePurity)
#' Also, if you let `lm()` automagically convert the character variable 
#' `TissueType` to a factor, it will order the leves in alphabetical order by 
#' default and you will have `Adj` as the reference level to which `Normal` and
#' `Tumor` get compared. One would think that the natural reference level would
#' be `Normal`, and `Tumor` and `Adj` would get compared to that.
#' Here is how you make sure `Normal` is made the reference level (in the 
#' `levels` you have to put a vector of the exact names of all the levels)
dat$TissueType<-factor(dat$TissueType,levels = c('Normal','Adj','Tumor'))
dat.lm <- lm(as.numeric(SamplePurity) ~ TissueType, data=dat)
summary(dat.lm)
#' 
#' ### 4.
#' 
#' > I think I answered this in question 3 (the previous question). Am I missing something? I must be misunderstanding something. Please forgive me. :(
#' 
#' No, you're just fine. I don't strongly care where you put 
#' your answer as long as I can find it.
#'
#' > Based on the results, I think that the tumor samples are significantly different from the normal tissues. It is important to note here that the normal tissue in this case is actually prostate tissue that is KNOWN to not have any cancer in it (a pathologist checked AND we know from reading the patient records that the individual didn't have prostate cancer; they had bladder cancer). This being the case, it makes complete sense that the normal control prostate tissues are significantly different from the prostate tumor tissue samples because the normal control samples are 100% pure; all of them. I find the results EXTREMELY comforting because this means that the normal ADJACENT tissues are NOT significantly different from the tumor tissue. This tells me that the tissue purity is NOT confounded by tissue type (normal, normal adjacent and tumor).
#' 
#' To me it looks like `Normal` tissues are not significantly different 
#' very different than whatever the first level of that factor represents while 
#' the `Tumor` tissues have a _higher_ `SamplePurity` than that first level. 
#' Here is what the model predicts would be the means for the three groups:
predict(dat.lm,newdata = data.frame(TissueType=c('Normal','Adj','Tumor')))
#' (for a univariate model with a categoric predictor like this one, you can 
#' get the same result by adding 0 and each of the coefficients to the 
#' intercept term from the model summary above)
#' What hasn't changed is that your adjacent and normal tissues are more similar
#' to each other than to tumor tissues. 
#' 
#' # Mohammed Al Fayyadh
#' ## Homework 1
#' Score: okay
#' 
#' ### 1.1
#' missing interpretation of what code does
#' 
#' ### 1.2
#' missing interpretation of what code does
#' 
#' #### What does "function" do? 
#' 
#' > Equation  application
#' 
#' A clearer way of putting it might be "a key-word indicating that a the 
#' following lines will be defining a function"
#' 
#' ### Part 4
#' 
#' Aw, come on. :-) You have bigger problems than calculating the mean of 
#' a few numbers-- you later showed me some of them. 
#' 
#' ## Homework 2
#' 
#' Missing. 
#' 
#' ## Homework 3
#' 
#' Missing.
#'
#' # Meghna N Chinchankar
#' ## Homework 1
#' Score: very good
#' 
#' ### 1.1
#' 
#' > Answer: In the code, 'riddle' designates a 'function' such that 'object = nm, nm'. In curly brackets, 'nm' is defined as the 'names of the object' such that whenever 'nm' is entered in the function, it represents the names of object. l
#' 
#' Basically, the `names(object) <- nm` line is where the contents of `nm` are
#' assigned to be the new names of `object`. `nm` is supposed to be a vector of
#' valid names the same length as `object`. In R, there are some functions that
#' can be called like this: `foo(bar)` to return a value, and can be called like
#' this: `foo(bar) <- baz` to set a new value. `names()` is one such function.
#' 
#' ### 1.3 ... How about "[" and "]"? 
#' 
#' > [] defines the variables required for detailing this code.
#' 
#' You're right, but a more accurate way to phrase this might be...
#' 
#' [] _contains_ the variables required for _subscripting_ this _data structure_.
#' 
#' ### Part 4
#' Original function...
#+ eval = FALSE
censor <- function(notdead)
{ notdead = is.numeric(lost + bags + explode)
worm <- function(live)
  for i {live[i] = live[i-1] - notdead}
worm
}
#'* `notdead` is an argument passed to this function but it immediately gets overwritten with the result of `is.numeric(lost + bags + explode)`.
#'* `is.numeric(...)` returns either a single `TRUE` if its argument is numeric or a single `FALSE` otherwise. So whatever `notdead` is when you pass it to `censor()`, if it gets past this line it's value will be either TRUE or FALSE. If your goal is to _make_ something numeric, you should use `as.numeric()` but as will be explained below, if the argument of this function doesn't result in an error then it probably already is numeric in the first place. Have a look at `?is.numeric` and `?as.numeric` in RStudio
#'* `lost + bags + explode` will give a non-error result only if all these conditions are met...
#'      * `+` is a valid operator for the all three of these variables
#'      * each of `lost`, `bags`, and `explode` are variables that have previously been created-- usually either in the global environment (i.e. your R session), or passed as arguments to your function, or created within your function
#'* `worm <- function(live)` means that you are creating a new function within the `censor()` function, which will only be available while `censor()` is running. This function will be called `worm()` and it will require that one argument be passed to it. Inside the body of the `worm()` function, this argument will behave like a variable named `live`.
#'* The correct syntax for `for` is: `for(i in something) { ... }` where `something` is a vector and `i` takes on each of the values of the vector in turn, and the contents of `{...}` run each time, usually making use of `i` in some way. This is the line that was the proximal cause of the error.
#' 
#' Without reference to the actual data it's hard for me to say how this 
#' function should actually be written, but I will take a guess at a few 
#' scenarios:
#' 
#' You have a `data.frame` with columns for `lost`, `bags`, and `explode`. You
#' know how many worms you started with (for illustration purposes let's say 
#' 30), and you want the number still left at each follow-up point. So your 
#' data (let's say it's a `data.frame` named `worms`) starts out looking like 
#' this:
#+ echo=FALSE
worms <- data.frame(day=0:5,lost=c(0,1,0,4,1,3),bags=c(0,0,0,2,3,8),explode=c(0,0,1,0,2,3));
print(worms,row.names=F);
#' Let's assume that day 0 is a placeholder for before the start of follow-up, 
#' when nothing happened yet.
#' To find out how many worms are left at the start of each observation period
#' you first add together the `lost`, `bags`, and `explode` columns...
events <- worms$lost + worms$bags + worms$explode;
events;
#' A more concise way to do this is...
events <- with(worms, lost + bags + explode);
events;
#' Or this...
events <- rowSums(worms[,-1]);
events;
#' In any case, next you use the `cumsum()` function to calculate the 
#' cumulative of all the worms that for one reason or another are no longer
#' alive.
cumsum(events);
#' Now subtract this from the original population which we decided was 30...
30 - cumsum(events)
#' We can insert this value into a new column of the table as follows:
worms$alive <- 30 - cumsum(events);
print(worms,row.names=F);
#' 
#' Another scenario: you have a `data.frame` where each worm has a row, with a
#' column (`day`) indicating when it ceased being part of the sample, and 
#' three other columns indicating whether on that day it was lost, bagged, or 
#' exploded:
#+ echo=FALSE
expandcounts <- function(data,daycol,eventcols){
  output <- c();
  for(ii in eventcols){
    iioutput <- list(day=rep(data[[daycol]],times=data[[ii]]));
    for(jj in eventcols){
      if(ii == jj) iioutput[[jj]] <- 1 else iioutput[[jj]] <- 0;
    }
    output <- rbind(output,do.call(data.frame,iioutput))
  }
  output[order(output$day),]
}
tallworms <- rbind(expandcounts(worms,'day',c('lost','bags','explode')),c(6,1,0,0),c(6,1,0,0))
print(tallworms,row.names=F);
#' If this is your data format, you are in luck because it is directly usable 
#' for survival analysis:
library(survival);
summary(coxph(Surv(day,1-lost)~1,tallworms));
#' Though in real life you would likely have one or more columns of predictor 
#' variables and you would put those columns instead of `1` into the formula, 
#' like this: `Surv(day,lost)~X1+X2+X3` and then it would not be a `Null model`
#' 
#' Functions from the `survival` package can be adapted to extract the number
#' surviving worms at each time point (if for some reason you need that directly
#' instead of letting the `survival` package functions plot your survival 
#' curves etc. for you):
summary(survfit(Surv(day,1-lost)~1,tallworms));
summary(survfit(Surv(day,1-lost)~1,tallworms))[2:3];
#' Here I assume that `lost` is the variable that indicates censoring and so I 
#' subtract it from 1 because R uses 0 to indicate a censored event and 1 to 
#' indicate an ordinary death. If that's not the case, use the appropriate
#' variable in that spot. Censoring is anything that prevents the true
#' death-date of the worm from being observed _including_ death from artifactual
#' causes _and_ including surviving past the end of the observation period. You
#' should be very skeptical of any survival dataset of more than a tiny
#' sample-size if none of the events are reported as censored.
#' 
#' Here is a function for converting the first type of table, where there are 
#' total numbers for each type of occurrence reported each day to the second,
#' tall format where there is one row per worm, and which is ready for analysis
#' using `survival` functions such as `coxph()` and `survfit()`.
#+ echo=FALSE,message=FALSE,comment=""
cat('expandcounts <-',paste0(capture.output(expandcounts),collapse='\n'));
#' If you're working with survival data, it behooves you to read the help files
#' for the `survival` package, particularly `?coxph` and `?survfit`:
help(package='survival');
#' ## Homework 2
#' 
#' Score: 
#' 
#' ## Homework 3
#' 
#' Missing
#'
#' # Michael Nipper
#' ## Homework 1
#' Score: outstanding
#' 
#' ### 1.3
#' 
#' > I do not understand the distinction between 'class' and 'mode' in the element readout.
#' 
#' Good question! Try the following on a `data.frame` and on a vector of integers...
class(rawdeid.sam);
mode(rawdeid.sam);
class(rawdeid.sam$PATIENT_NUM);
mode(rawdeid.sam$PATIENT_NUM);
#' `class` and `mode` (a.k.a. storage mode) are not always the same. Class is 
#' a high-level attribute that you more often check and sometimes
#' set, because it determines what functions are allowed to operate on a given
#' object and what they do with it. I.e. it's used for R's equivalent of object
#' oriented programming and for [overloading functions](https://en.wikipedia.org/wiki/Function_overloading).
class(rawdeid.sam)<-'list';
summary(rawdeid.sam);
class(rawdeid.sam)<-'data.frame';
summary(rawdeid.sam);
#' On the other hand the mode is a low-level attribute and is less easily 
#' changed. Whatever class R permits you to set `rawdeid.sam` in the above 
#' examples, it will still retain certain fundamental properties of a list-like 
#' object. For example, you can use `$...` and `[[...]]` operators to extract 
#' individual sub-objects, you can use `with()` to pretend that all its 
#' sub-objects are just variables in your environment, you can pass it to 
#' functions like `sapply()` and `lapply()`, and if you use `[...]` to subset it 
#' like you would a vector you will get a plain list containing the sub-objects 
#' indicated by the indices between the brackets.
#' 
#' ## Homework 2
#' Score: excellent
#' 
#' ### Part 1
#' 
#' > (note: command functioned as expected even when "select" "from" and "where" were all lowercase, need to ask if caps is convention or has functional purpose)
#' 
#' It's convention, except nobody seems to quite agree on whether the keywords 
#' should be all-caps, or the table names, or the column names. Sometimes same
#' piece of code switches between different conventions. One thing that is 
#' rarely done, though, is mixing cases, though even that will run without 
#' error in SQLite.
#' 
#' ### Part 3, 3.1
#' 
#' > SELECT COUNT(*) FROM Results WHERE Treatment = 'Medicine'; and 
#' > SELECT COUNT(*) FROM Results WHERE Treatment = 'Placebo';
#' 
#' I was actually hoping to see a one-liner query that does this. Here is what
#' that would look like:
#' 
#'     SELECT Treatment,COUNT(*) FROM Results GROUP BY Treatment;
#'
#' ### 3.2
#' 
#' As above, can combine `AVG()`, `MAX()`, and `MIN()` into one `SELECT` query 
#' and group by `Treatment`
#' 
#' ### 3.3
#' 
#' > Each subject was measured once, before and after treatment.
#'
#' That counts as being measured twice. You would have all measurements, both
#' before and after in one column and then an additional column with a 
#' two-level factor indicating whether a particular row is for a 'before' or 
#' an 'after' measurement.
#' 
#' ## Homework 3
#' Score: 
#'
#' ### 3.
#' 
#+ echo=FALSE
con <- dbConnect(RSQLite::SQLite(), dbname="MichaelNipper/Hypothetical_research.db")
#' > 3. For using pre-treatment blood pressure values as the dependent variable:
dat <- dbGetQuery( con,'select age, sex, BMI, pre_BP_mmHg from results' );
#' > For using post-treatment blood pressure values as the dependent variable:
dat <- dbGetQuery( con,'select age, sex, BMI, treatment, post_BP_mmHg from results' );
#' Here you have a repeated-measures dataset (see comment at the end of HW02, 
#' above), but this was before we did stats crash courses, so you didn't yet 
#' know about mixed-effect models. Here is how you would set this up as a 
#' single dataset for use with `lme()`:
dat <- dbGetQuery( con,"select patientid
                   , age,sex,bmi,treatment
                   , pre_bp_mmhg mmhg
                   , 'pre' prepost 
                   from results 
                   union all 
                   select patientid
                   , age,sex,bmi,treatment
                   , post_bp_mmhg mmhg
                   , 'post' prepost 
                   from results" );
#' This gives identical results to the following:
dat <- rbind(
  dbGetQuery(con,"select patientid,age,sex,bmi,treatment
             , pre_bp_mmhg mmhg, 'pre' prepost from results")
  ,dbGetQuery(con,"select patientid,age,sex,bmi,treatment
              , post_bp_mmhg mmhg, 'post' prepost from results"))

#' ### 4.
#' 
#' > 4. Using pre-treatment blood pressure values
#+ eval=FALSE
dat.lm <- lm(pre_BP_mmHg~age+sex+BMI,data=dat);
#' etc...
#' 
#' Here is how one would do this using `lme()`, for repeated-measures:
#+ eval=FALSE
library(nlme)
dat.lme <- lme(mmhg~age+sex+BMI+treatment,data=dat,random=~1|PatientID);
# compare to an equivalent fixed-effect model
anova(gls(mmhg~age+sex+BMI+treatment,dat),dat.lme);
summary(dat.lme);
plot(dat.lme);
#' However, since in generating the data you did not add any individual 
#' variation (some random quantity that is dependent on `PatientID` just as you 
#' already have random qantities dependent on `sex` and `treatment`), there is 
#' no reason why `lme()` would improve the fit of this model.
#' 
#' > It showed that sex affected blood pressure by approximately 8 mmHg, 
#' significantly higher than the average change in blood pressure applied due to
#' sex in the construction of the data,
#' 
#' Be careful about the use of the word 'significant', it has a very specific 
#' meaning. You probably meant 'somewhat'.
#' 
#' > The rise near the right side suggests that the data is somewhat right 
#' skewed, which might call for some rewriting of the analysis, but in this case
#' (because the data was generated) we already know that the deviation is just 
#' sample variance
#' 
#' Actually, the way you generated this data will cause slight departures from 
#' normality. For the fixed-effect portion (i.e. everything except variation by 
#' `PatientID`) there should only be _one_ normally distributed error term and 
#' all the effects of all the predictor variables should be _deterministic_. If 
#' you're giving additional random variation only to certain individuals, then 
#' you are modeling heteroscedastic errors (i.e. errors that depend on the 
#' variables, which violates the assumptions of ordinary linear models, and in 
#' real life happens all the time though often weakly enough that you can get 
#' away with using `lm()` without any modifications). It's fine to model 
#' heteroscedasticity if that is your intention, just be aware that this is what
#' you are doing. Also, if you use anything other than or in addition to 
#' `rnorm(...)` for your error, there will perforce be departures from normality
#' whose magnitude will depend on the magnitude of the error.
#' 
#' Great job on the homeworks. I haven't had a chance to review the code you
#' sent me after our last class, but please remind me if you don't hear back
#' from me in a week or so.
#' 
#' # Robert Martinez 
#' 
#' ## Homework 1
#' 
#' Missing
#' 
#' ## Homework 2
#' 
#' Missing
#' 
#' ## Homework 3
#' 
#' Missing
#' 
#' # Rafael J Veraza
#' 
#' ## Homework 1 
#' 
#' Score: okay
#' 
#' ### 1.1
#' 
#' > This is intended to run, that every time that object is run a value of nm,nm will be displayed. 
#' 
#' More precise answer: a function that assigns the second argument to the names
#' of the first argument returning a named version of the object it was given.
#' 
#' ### 1.2
#' 
#' >  ?????? 
#' 
#' Answer: it calculates a sequence of numbers (that happens to be called the
#' Fibbonaci sequence) that is the same length as the argument that gets passed
#' to it. In Part 2 of this homework were additional readings that could help
#' answer this. In Part 3 it suggests copy-pasting the code into R and trying it
#' out to see if your initial guesses are right.
#' 
#' ### What do you think "<-" does? 
#' 
#' > I think it is used for comparisons of some type??? 
#'
#' Actually, "<-" is used for assignment-- it makes the value on the left side 
#' equal to the value on the right (and creates an object by that name if one
#' does not already exist)
#' 
#' ### What does "function" do? 
#' 
#' > Function is the event or the action that will initiate a response. 
#' 
#' Answer: `function` tells R that what comes after will describe a new function.
#' This function can be assigned to a new R object, and from that point forward it 
#' that object can be used as a function.
# foo doesn't yet exist
foo
foo()
# we give foo a value, now it exists, but it is not a function
foo <- 3
foo
foo()
# now we replace that value with date, and date happens to be a built-in
# function that returns the current date and time
foo <- date
# now foo by itself returns the body of the function
foo
# foo() with parentheses actually invokes this function. In this case, the 
# function has no arguments. If it had arguments, they would go inside the
# parentheses.
foo()
#' 
#' ### 1.3 What is the purpose of the parentheses immediately after the "function"? 
#' 
#' > Parentheses is intended to hold the command or block of coding. It seems that the parenthesis is used for (functions)
#' 
#' These parentheses contain the arguments to a function
#' 
#' ### Parts 2, 3, 4
#' 
#' Missing
#'   
#' ## Homework 2 
#' 
#' Missing
#' 
#' ## Homework 3 
#' 
#' Missing
#' 
#' # Name 
#' 
#' ## Homework 1 
#' Score:
#' 
#' ## Homework 2 
#' Score:
#' 
#' ## Homework 3 
#' Score:
#' 
