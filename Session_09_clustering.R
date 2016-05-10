#' ---
#' title: "Session 09: Clustering and Decision Trees"
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

#'# Decision Trees
#'To be continued!
#'