**About me**

I am [Leander De Schutter](https://www.linkedin.com/in/leander-de-schutter), a PhD Candidate at the Rotterdam School of Management, Erasmus University. In my research I examine how people maintain cooperative relationships at the workplace. I am also interested in how methodological techniques can be made more accessible. The latter is exactly the purpose of the shiny application I wrote: ConMET. 
Feel free to reach out to me at [deschutter@rsm.nl](mailto:deschutter@rsm.nl) if you encounter any bugs, have any comments, questions or change requests.

\
\
**ConMET: Short Description**

When you conduct survey research, reviewers often ask you to perform confirmatory factor analyses (CFAs) to check whether the measured constructs are statistically different from each other or whether there is a common method bias. Conducting and evaluating CFAs, however, is a tedious task. Even after becoming familiar with the technique, you still spend a lot of time repeatedly creating, comparing, and reporting on different CFA models.

I wrote ConMET, a shiny application, that makes it easier to analyze typical CFA models that you have to report in most journals. With this app you can work much faster because it automatically creates, tests, and compares nested measurement models. It also provides you with a table that summarizes the results, which you can insert directly into your manuscript. Another advantage is that the application has a point-and-click user interface, which means you don't have to program to run CFAs.

\
\
**How to access the application:** 
 - You can open this link: https://leanderdeschutter.shinyapps.io/conmet/. This is an online version of the application and works slower than when your run the app locally on your desktop. The app is hosted on shinyapps.io, and data you upload is [removed](https://docs.rstudio.com/shinyapps.io/Storage.html) after you close your session. Best thing you can do is to load the app in your local R console.
 - You can also run the following line of code in your R console. This will install the required packages for the application and then open the app in R. The function just retrieves the code in the 'myapp'-folder and then runs it on your computer.
	```{r}
	# install the R shiny package if you havent installed it before. 
	# install.packages("shiny")
	library(shiny)
	runGitHub("conmet", "leander-ds", subdir = "myapp")
   	 ```
\
\
**Tutorial**

In the [tutorial](Tutorial) folder you can find information about how to use the application. You can also test ConMET by using the [example datasets](Example%20Data). 

\
\
**More information**

ConMET relies heavily on the lavaan package (Rosseel, 2012) and uses functions from the semTools package (Jorgensen et al., 2020) to obtain reliabilities. Please make sure to cite these packages when using the output of the application.

Other packages that the app uses are:

 - foreign (R Core Team, 2020) to read datasets
 - shiny (Chang et al., 2021), shinydashboard (Chang & Ribeiro, 2018),
   waiter (Coene, 2021), DT (Xie, Cheng, & Tan, 2020) and shinyWidgets
   (Perrier, Meyer & Granjon, 2021) for the web application framework
   and features.
 - purrr (Henry & Wickham, 2020), stringr (Wickham, 2018), dplyr
   (Wickham, François, Henry, & Müller, 2020) for data and string
   wrangling.
 - summarytools (Comtois, 2021) for the neat data summary.
 - Hmisc (Harrel, 2020). To get correlation matrix with p-values.
 - Openxlsx (Schauwberber & Walker, 2020). To write dataframes into an
   Excel sheet.
