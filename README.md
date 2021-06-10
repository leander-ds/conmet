**Short Description**

When you conduct survey research, reviewers often ask you to perform confirmatory factor analyses (CFAs) to check whether the measured constructs are statistically different from each other or whether there is a common method bias. Conducting and evaluating CFAs, however, is a tedious task. Even after becoming familiar with the technique, you still spend a lot of time repeatedly creating, comparing, and reporting on different CFA models.

I wrote an application that makes it easier to analyze typical CFA models that you have to report in most journals. With this app you can work much faster because it automatically creates, tests, and compares nested measurement models. It also provides you with a table that summarizes the results, which you can insert directly into your manuscript. Another advantage is that the application has a point-and-click user interface, which means you don't have to program to run CFAs.

**How to access the application:** 
 - You can open this link:  https://leanderdeschutter.shinyapps.io/conmet/. This is an online version of the application and works slower than when your run the app locally on your desktop. 
 - You can also run the following line of code: 
	  `install.packages("shiny")`
	  `library(shiny)`
	  `runGithub("leander-ds", "conmet")`
	  This will install the required packages for the application and then open the app in R. 
