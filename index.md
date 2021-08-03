<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  {%- seo -%}
  <link rel="stylesheet" href="{{ "/assets/css/style.css" | relative_url }}">
<meta name="google-site-verification" content="rvUPFYzH66qBL1bdUgI_L2lmSEmaLXfWxYGbaIV4ysc">
</head>

# ConMET: Facilitating Confirmatory Factor Analyses

When you conduct survey research, reviewers often ask you to perform confirmatory factor analyses (CFAs) to check whether the measured constructs are statistically different from each other or whether there is a common method bias. Conducting and evaluating CFAs, however, is a tedious task. Even after becoming familiar with the technique, you still spend a lot of time repeatedly creating, comparing, and reporting on different CFA models.

I wrote ConMET, a shiny application, that makes it easier to analyze typical CFA models that you have to report in most journals. With this app you can work much faster because it automatically creates, tests, and compares nested measurement models. It also provides you with a table that summarizes the results, which you can insert directly into your manuscript. Another advantage is that the application has a point-and-click user interface, which means you don't have to program to run CFAs.

ConMET relies heavily on the lavaan package (Rosseel, 2012) and uses functions from the semTools package (Jorgensen et al., 2020) to obtain reliabilities. Please make sure to cite these packages when using the output of the application.


## Tutorial

In the [tutorial](https://github.com/leander-ds/conmet/tree/master/Tutorial) folder you can find information about how to use the application. You can also test ConMET by using the [example datasets](https://github.com/leander-ds/conmet/tree/master/Example%20Data). 


## How to access the application:
 - You can open this link: https://leanderdeschutter.shinyapps.io/conmet/. This is an online version of the application and works slower than when your run the app locally on your computer. The app is hosted on shinyapps.io, and the data you upload is stored in a temporary directory and [removed](https://docs.rstudio.com/shinyapps.io/Storage.html) once you close the session. I use the free version which only has limited uses every month, so there is a chance that the link will not work.
 - You can also run the app locally on your computer. You can do this by running the code below in your R console. This will install the required packages for the application and then open the app in R. The function just retrieves the code in the 'myapp'-folder from this GitHub page and then runs it on your computer.
	
 ```{r}
	# install the R shiny package if you havent installed it before. 
	# install.packages("shiny")
	library(shiny)
	runGitHub("conmet", "leander-ds", subdir = "myapp")
```

# About me

I am [Leander De Schutter](https://www.linkedin.com/in/leander-de-schutter), a PhD Candidate at the Rotterdam School of Management, Erasmus University. In my research I examine how people maintain cooperative relationships at the workplace. I am also interested in how methodological techniques can be made more accessible. The latter is exactly the purpose of the shiny application I wrote: ConMET. 
Feel free to reach out to me at [deschutter@rsm.nl](mailto:deschutter@rsm.nl) if you encounter any bugs, have any comments, questions or change requests.
