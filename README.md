
**ConMET: Short Description**

When you conduct survey research, reviewers often ask you to perform confirmatory factor analyses (CFAs) to check whether the measured constructs are statistically different from each other or whether there is a common method bias. Conducting and evaluating CFAs, however, is a tedious task. Even after becoming familiar with the technique, you still spend a lot of time repeatedly creating, comparing, and reporting on different CFA models.

I wrote ConMET, a shiny application, that makes it easier to analyze typical CFA models that you have to report in most journals. With this app you can work much faster because it automatically creates, tests, and compares nested measurement models. It also provides you with a table that summarizes the results, which you can insert directly into your manuscript. Another advantage is that the application has a point-and-click user interface, which means you don't have to program to run CFAs.

**How to access the application:** 
 - You can open this link:  https://leanderdeschutter.shinyapps.io/conmet/. This is an online version of the application and works slower than when your run the app locally on your desktop. 
 - You can also run the following line of code in your R console:
	```{r}
	# install the R shiny package if you havent installed it before. 
	# install.packages("shiny")
	library(shiny)
	runGitHub("conmet", "leander-ds")
   	 ```

	This will install the required packages for the application and then open the app in R. 


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
