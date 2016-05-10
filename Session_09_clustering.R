#' ---
#' title: "Session 09: Clustering, Classification and Regression Trees"
#' author: "Alex F. Bokov"
#' date: "May 10, 2016"
#' output:
#'    html_document:
#'      toc: true
#' ---
#' 

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
rawdeid.sam <- subset(rawdeid, PATIENT_NUM %in% unique(c(sample(PATIENT_NUM,200),sample(unique(PATIENT_NUM[SEX=='f'&RACE=='black']),30),sample(unique(PATIENT_NUM[SEX=='m'&RACE=='black']),30))));

#'# Why?
#'
#' * When there are many variables to choose from, finding the ones that are most relevant to the outcome of interest.
#' * Better understanding at a high level which groups of observations are similar to each other.
#' 
#'# Clustering
#'Let's add some noise to our data, and see if we can separate out the relevant variables from the rest.
rawdeid.sam$VAR01<-rnorm(nrow(rawdeid.sam),mean = 5,sd=2.5)
rawdeid.sam$VAR02<-rlnorm(nrow(rawdeid.sam),mean = 2.5,sd=1.2)
rawdeid.sam$VAR03<-rchisq(nrow(rawdeid.sam), df=2, ncp = 3)

#'### Correlation Matrix
#'Which variables correlate with which other variables? Let's create a correlation matrix. 
rawdeid.cor <- cor(data.matrix(rawdeid.sam),use = 'pairwise');

#'And now let's create a convenient color-generating function to use in the next several examples.
colorfun <- colorRamp(c("#3366CC","white","#CC0000"), space="Lab");

#'### Default Heatmap in R
#'Here we use our color function. Blue means correlation red means inverse correlation.
heatmap(rawdeid.cor,symm = TRUE
        ,col = rgb(colorfun((rawdeid.cor+1)/2),maxColorValue = 255));

#'### Plotting Correlations with `plotcorr`
#'Here is another way to visualize correlation. For this we will need the `ellipse` library.

#+ warning=FALSE
#install.packages('ellipse'); 
library(ellipse);
plotcorr(rawdeid.cor, col=rgb(colorfun((rawdeid.cor+1)/2), maxColorValue=255));

#'### K-Means Clustering
#'The idea behind K-means is to optimally divide the data into K clusters. A good library for doing this is `fpc`.
#+ warning=FALSE
#install.packages('fpc'); 
library(fpc);
deidk <- pamk(data.matrix(rawdeid.sam)
              ,krange=2:5,usepam = F,scaling = T,critout = T);
deidk$nc;
plot(deidk$pamobject);

#'# Classification and Regression Trees
#'Trees are an alternative to regression models-- here, instead of the response being a function
#'of predictors, a set of rules based on the predictors is derived that gives most probable outcome 
#'(for discrete response variables) or range of outcomes (for continuous response variables).
#'There are many decision-tree packages. You can read about them in 
#'[this overview](http://statistical-research.com/a-brief-tour-of-the-trees-and-forests/?utm_source=rss&utm_medium=rss&utm_campaign=a-brief-tour-of-the-trees-and-forests) 
#'by Wes Stevenson and in the [CRAN Task View](http://cran.r-project.org/web/views/) on 
#'[Machine Learning](http://cran.r-project.org/web/views/MachineLearning.html)
#'Today we will use the `partykit` package because it can run fast enought to be practically
#'usable in the classroom and draws reasonably interpretable plots. Its syntax is typical of such
#'packages, so if you experiment with some of the others, you'll know what to expect.
#'The following code loads the `partykit` package if you already
#'have it, and if you do not, it installs it and then loads it.
#'This can be done with any package.
if(!require(partykit)){
  install.packages('partykit',repos = 'https://cran.rstudio.com/');
  require(partykit);
}

#'Now, let's create a conditional inference tree (`ctree`). The first argument
#'is a model formula which should be familiar by now. For `ctree()`, though, 
#'interactions are not permitted-- only additive terms. The second argument is 
#'a `data.frame`, again as usual.
rawdeid.ct <-ctree(Y~AGE+BMI+TEMPERATURE+SEX+RACE+VAR01+VAR02+VAR03
                   # We set the optional `na.action` argument to `na.omit` because
                   # otherwise this command will give an error the first time
                   # it encounters a missing value. With `na.omit` is just 
                   # skips them
                   ,data=rawdeid.sam,na.action=na.omit
                   # The optional argument `control` is passed a list of 
                   # settings generated by the `ctree_control()` command. We 
                   # are using it because there is one setting we want to not
                   # be at its default value: `maxdepth`. Its default value is
                   # infinite, and it may produce really huge and messy results
                   ,control = ctree_control(maxdepth = 3));
#'Let's see what we got...
rawdeid.ct;
#'We can also plot it.
#+fig.width=10
plot(rawdeid.ct);
#'Let's use the plotting functions from last time to reality-check these 
#'predictions
if(!require(ggplot2)){
  install.packages('ggplot2',repos = 'https://cran.rstudio.com/');
  require(ggplot2);
}

#'In Black patients, Y goes down with age, with males having a higher baseline
#'than females.
ggplot(
  subset(rawdeid.sam,RACE=='black') # we plot just the Black patients
  ,aes(x=AGE                      # tell ggplot that AGE goes on the x-axis
       ,y=Y                       # Y goes on the y-axis
       ,col=SEX)                  # and assign color based on SEX
  ) + geom_point()                # then 'add' to this a scatter-plot style

#'In elderly White and Other patients females have a higher Y than males
ggplot(subset(rawdeid.sam,RACE!='black'&AGE>68.211)
       ,aes(x=SEX,y=Y,col=SEX)      # now both color and x-axis represent SEX
       ) + geom_jitter()            # geom_jitter is for scatter-plots with discrete x-axes
#'In non-elderly White and Other patients, Y increases with BMI
ggplot(subset(rawdeid.sam,RACE!='black'&AGE<68.211)
       ,aes(x=BMI,y=Y)) + geom_point()
#'Less consistent than the others but the right overall trend.
#'#Bonus Section: Imputation
#'In many of the above cases we quietly drop the missing values. Usually the 
#'entire row that contains one or more missing values. Of course this 
#'drastically reduces the sample size. What's more, it can introduce a bias if
#'the data is not missing completely at random, but rather is more likely to be
#'missing in certain groups of patients than in others.
#'
#'Imputation techniques use non-missing data to estimate the missing data.
#'For this we will use a very powerful and flexible imputation library called
#'`mice`
if(!require(mice)){
  install.packages('mice',repos = 'https://cran.rstudio.com/');
  require(mice);
}
#'Let's create some missing data, with more of it missing in male patients
rawdeid.mis <- rawdeid.sam;
