#' ---
#' title: "Visualization, Clustering, Decision Trees"
#' author: "Alex F. Bokov"
#' date: "04/25/2016"
#' ---
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

#'# Part 1: Visualization in R Using ggplot2
#'We will be using a package called `ggplot2`, which should already come pre-installed with your RStudio/R instances, but if not, you can get it from RStudio with `install.packages('ggplot2')`. `ggplot2` allows you to build up plots in a modular manner, making it easier (once you understand how it works) to quickly create new plots and modify existing ones.
#'
#'The main components of a `ggplot2` are:
#'
#'* Data: what you are trying to visualize
#'* Aesthetic Mapping: what visual attributes will represent which features of your data (will be explained in a moment, hold tight)
#'* Geometry: what actually gets drawn on your plot
#'* Scales: modifying the default settings that various aesthetics use (will be explained in a moment, hold tight)
#'* Facets: how a single plot should get broken up into multiple plots to make it easier to understand
#'* Themes: how to change global default values of your plot
#'* Labels: where/how to write text and what the text should say
#'* Statistics: how your data is transformed when it is plotted (not all plots will use this)
#'
#'## The `ggplot()` function
#'There are several plotting functions in `ggplot2`, but we will focus on the generic one called `ggplot()`. It is the "bottom" layer that can take a `data` argument which specifies a `data.frame` and a `mapping` argument which specifies output from an `aes()` function. Like this:
library(ggplot2);
mygg <- ggplot(data=rawdeid.sam,aes(x=AGE,y=Y));

#'What the above code says amounts to: "bundle up my data, `rawdeid.sam`, together an abstract rule saying that in whatever way we end up plotting this data, the x-axis will represent age and the y-axis will represent the `Y` variable from sessions 5 and 6".
#'
#'## The geometry functions
#'But the `mygg` object we created is not by itself going to be plot-able. We specified the x and y axes, but we have not yet said what type of plot we want. For this we need one or more geometries. Like this:
#+ warning=FALSE
# Scatter plot
mygg + geom_point();

#'Or this:
#+ warning=FALSE,message=FALSE
# Smoothed line plot
mygg + geom_smooth();

#'Notice you don't actually use a plot command. Simply adding the output of a `geom_FOO()` to `mygg` plots the data contained in `mygg` using the `FOO` method, interpreting the x and y aesthetic mappings in some manner specific to that method. There are many geometries in `ggplot2` and you can add them to the same underlying `ggplot` object...
#+ warning=FALSE,message=FALSE
mygg + geom_bin2d();
mygg + geom_line();
mygg + geom_quantile();
mygg + geom_density2d();

#'Not every geometry looks good or makes sense for every dataset...
#+ warning=FALSE
mygg + geom_area();
mygg + geom_boxplot();
mygg + geom_violin();
mygg + geom_step();
mygg + geom_dotplot();
mygg + geom_polygon();

#'It's up to you to decide which geometries are most appropriate to your data. Many of them require additional arguments to the `aes()` function. Still, it's pretty convenient that `ggplot2` is smart enough to do something reasonable when you type `mygg + geom_point()` without having to specify any additional arguments. You can add multiple geometries to the same `ggplot` object:
#+ warning=FALSE
mygg + geom_density2d();
mygg + geom_density2d() + geom_point(shape='.');
mygg + geom_density2d() + geom_point(shape='.') + geom_smooth();

#'## More Aesthetic Mappings
#'In the above example, we use two arguments to the `aes()` command: `x` and `y`, for two of the ways a plot is capable of communicating meaning. Here are some other possible arguments (different geometries may use the same argument in different ways, and some geometries ignore some arguments).
#'
#' * `color` (line or outline color)
#' * `fill` (internal color, where applicable)
#' * shape (shape of points, where applicable)
#' * linetype (dotted, dashed, etc. where applicable)
#' * size (size of points or thickness of lines)
#' * alpha (transparency-- the closer to 0, the more transparent)

#' Let's try color...
#+ warning=FALSE
mygg <- ggplot(data=rawdeid.sam,aes(x=AGE,y=Y,color=SEX));
mygg + geom_point();

#' But we don't have to re-create `mygg` each time. The `geom_FOO()` functions can take an optional `aes()` argument which overrides any existing mappings from the baseline one, adds any new ones and keeps the rest. So the below gives the same plot:
#+ warning=FALSE
mygg <- ggplot(data=rawdeid.sam,aes(x=AGE,y=Y));
mygg + geom_point(aes(color=SEX));

#'Now let's try shape.
#+ warning=FALSE
mygg + geom_point(aes(shape=SEX));

#'Size.
#+ warning=FALSE
mygg + geom_point(aes(size=SEX));

#'Um no. How about alpha?
#+warning=FALSE
mygg + geom_point(aes(alpha=SEX));

#'That might be useful for emphasizing one group and then the other, in turn.
#'
#'Let's try linetype.
#+ warning=FALSE
mygg + geom_point(aes(linetype=SEX));

#'Doesn't seem to do anything... because `geom_point()` doesn't plot any lines, so it ignores the `linetype` argument. So let's try something that does plot lines...
#+ warning=FALSE
mygg + geom_density2d(aes(linetype=SEX));

#'This could be useful in some scenarios. We can better differentiate the two groups by using multiple aesthetic mappings.
#+ warning=FALSE
mygg + geom_density2d(aes(linetype=SEX,color=SEX));

#'Or we can simultaneously distinguish multiple variables with a different aesthetic mapping for each.
#+ warning=FALSE
mygg + geom_point(aes(color=BMI_BIN,shape=SEX));

#'## Representing a series of observations
#'As we discussed in sessions 5 and 6, these data points represent many visits from a smaller number of patients. So a scatter plot omits important information: which visits are linked to a single patient. A line-plot doesn't do the trick either, because it joins _all_ the points within a group into single line (except for where it skips missing observations)...
#+ warning=FALSE
mygg + geom_line(aes(color=SEX));

#'We could join them by `PATIENT_NUM` but then it looks like Rainbow Dash threw up on your screen.
#+ warning=FALSE
mygg + geom_line(aes(color=factor(PATIENT_NUM)));

#'Which points are joined by a line can be directly specified by an `aes()` argument called `group` as follows:
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM));

#'This frees up `color` so we can again use it to indicate some other property of the data.
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX));

#'It would have taken a lot of web searching and inference to figure out on your own how to plot time series from many individuals on the same axis... and now you know!
#'
#'## Scales
#'Let's say you don't want the default brick-red and turquoise colors for your lines. You can override them by adding on a scale function.
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX)) + 
  scale_color_manual(values = c(m='red',f='darkgreen'));


#'Most of the `aes()` arguments have corresponding `scale_FOO_BAR()` functions, often several to choose from. Including ones for numeric variables, such as the `x` axis in our example. You can override the default linear scale of the `x` axis and put it on the log scale.
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX)) + 
  scale_x_log10();

#'You can also use `scale_FOO_BAR()` functions to specify attributes such as `limits`, `breaks`, and `name` of the scale (here we are back to using the default `scale_x_continuous()` but these arguments work with many types of scales).
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX)) + 
  scale_x_continuous(name='Age in Days'
                     ,limits=c(20,80)
                     ,breaks=seq(0,100,by=20));

#'The same scales exist for the `y` argument to `aes()`. But you don't have to use them all. Often you might use a scale function just to change the label, and leave everything else at its default value.
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX)) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response')

#'## Facets
#'You may remember from sessions 5 and 6 that a specific combination of sex and race exhibited a differential treatment effect. Now, we could visualize this using some combination of `color`, `linetype`, `size`, `alpha`, and maybe even add on top of the `geom_line()` a `geom_point()` and thus enable the use of `shape`. But the plot starts to get busy. Instead of doing that, you can use one of the variables break it out into a set of plots that use the same axes using the `facet_wrap()` function. 
#+ warning=FALSE
# Break them out by race...
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE);
# Or by sex
mygg + geom_line(aes(group=PATIENT_NUM,color=RACE),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~SEX);

#'### Constant Aesthetic Mappings
#'You may have noticed that in the above plot we have `alpha=0.7` instead of having it be determined by one of the variables. Earlier we similarly had a plot where we used `geom_point(shape='.')`.  This means that the opacity of all the lines is 70%, or in the other example, the shape of all the points is `.`, regardless of what values the variables are. These constant aesthetic mappings are set by arguments _outside_ `aes()` function whereas the data-dependent aesthetic mappings are set by arguments _inside_ `aes()`. Compare...
#+ warning=FALSE,fig.width=3,fig.height=3,fig.show='hold'
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX));
mygg + geom_line(aes(group=PATIENT_NUM),color='indianred2');

#'## Themes
#'What if you don't like the default gray background with white gridlines? You can change it using themes.
#+ warning=FALSE
# Black & White
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE) +
  theme_bw();
# Minimal
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE) +
  theme_minimal();
# Light
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE) +
  theme_light();
# Classic
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE) +
  theme_classic();
# Linedraw
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE) +
  theme_linedraw();

#'You can then fine-tune the defaults for a particular theme using the `theme()` function.
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  scale_x_continuous(name='Age in Days') +
  scale_y_continuous(name='Response') +
  facet_wrap(~RACE) +
  theme_minimal() +
  theme(
  plot.background=element_rect(fill = 'gray')
  ,panel.background=element_rect(fill='white')
  ,text=element_text(colour = 'white',face = 'bold'));

#'## Labels
#'We don't have to invoke, for example, `scale_x_continuous()` just to change the name of the axis. We can use the `labs()` function instead.
#+ warning=FALSE
mygg + geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  facet_wrap(~RACE) + theme_minimal() +
  labs(x='Age in Days',y='Response',legend='Sex',title='Response vs. Age')

#'## Statistics
#'As a finishing touch, we can visually summarize the raw data by using the `stat_FOO()` functions.
#+ warning=FALSE,message=FALSE
# Bounding Ellipses
mygg + 
  geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  facet_wrap(~RACE) + theme_minimal() +
  labs(x='Age in Days'
       ,y='Response'
       ,legend='Sex'
       ,title='Response vs. Age') +
  stat_ellipse(aes(group=SEX,color=SEX));  

# Or Smoothed Curves
mygg + 
  geom_line(aes(group=PATIENT_NUM,color=SEX),alpha=0.7) + 
  facet_wrap(~RACE) + theme_minimal() +
  labs(x='Age in Days'
       ,y='Response'
       ,legend='Sex'
       ,title='Response vs. Age') +
  stat_smooth(aes(group=SEX,color=SEX));  

#'By the way, remember how `x` is an `aes()` variable? What if we override it inside of `geom_line()` and `stat_smooth`? Will that work? Let's find out.
#+ warning=FALSE,message=FALSE
mygg + 
  geom_line(aes(group=PATIENT_NUM,color=SEX,x=BMI),alpha=0.7) + 
  facet_wrap(~RACE) + theme_minimal() +
  labs(x='BMI'
       ,y='Response'
       ,legend='Sex'
       ,title='Response vs. BMI') +
  stat_smooth(aes(group=SEX,color=SEX,x=BMI));  

#'
#'So here, in one plot is the visual accompaniment of the statistical story that unfolded in sessions 5 and 6.
#'
#'## More Information
#'There is a lot more to `ggplot2` than we covered here, but hopefully this will give you a starting point from which to expand your skills. Here are sources of helpful information about `ggplot2`, with examples:
#'
#'* http://docs.ggplot2.org/current/ (the author's website)
#'* http://stackoverflow.com/questions/tagged/ggplot2
#'* https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#'
#'## Challenge: find someone who thinks it's a good idea to spend $1000/seat on MS Tableau and see how long it will take them to reproduce the above results.


