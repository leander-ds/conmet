# Package for loading datasets
if(!require(foreign)){
  install.packages("foreign")
}

# Base package for app. 
if(!require(shiny)){
  install.packages("shiny")
 library(shiny)
}

# For nicer shiny app layout. 
if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library(shinydashboard)
}

# For the loader
if(!require(waiter)){
  install.packages("waiter")
  library(waiter)
}

# Needed for nicer buttons and picker input. . 
if(!require(shinyWidgets)){
  install.packages("shinyWidgets")
  library(shinyWidgets)
}

# useful for more efficient functions .
if(!require(purrr)){
  install.packages("purrr")
  library(purrr)
}

# Needed for sem
if(!require(lavaan)){
  install.packages("lavaan")
  library(lavaan)
}

if(!require(summarytools)){
  install.packages("summarytools")
  library(summarytools)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

#
if(!require(Hmisc)){
  install.packages("Hmisc")
  library(Hmisc)
}

#if(!require(highcharter)){
#  install.packages("highcharter")
#  library(highcharter)
#}

if(!require(semTools)){
  install.packages("semTools")
  library(semTools)
}

if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}

if(!require(DT)){
  install.packages("DT")
  library(DT)
}

#library(foreign)
#library(shiny)
#library(lavaan)
#library(shinydashboard)
#library(shinyWidgets)
#library(corrplot)
#library(summarytools)
#library(tidyverse)
#library(purrr)
#library(Hmisc)
#library(openxlsx)
#library(waiter)
#library(highcharter)


ui <- dashboardPage(
  dashboardHeader(title = HTML("ConMET")),
  
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      id = "tabs",
      menuItem("READ ME", tabName = "Intro", icon = icon("book")),
      menuItem("Load Data and Select Items", icon = icon("upload"), tabName = "LoadData", selected = TRUE),
      menuItem("Descriptive Statistics", icon = icon("chart-bar"), tabName = "Descript_tab",
               menuSubItem("Item Correlations", icon = icon("table"), tabName = "CorrelationTable_tab"),
               menuSubItem("Item Summary Statistics", icon =  icon("equalizer", lib = "glyphicon"), tabName = "dfSummary_tab")
      ),
      menuItem("Confirmatory Factor Analysis", icon = icon("bezier-curve"), tabName = "Settings_tab",
               menuSubItem("CFA Specifications", icon = icon("cogs"), tabName = "Spec_tab"),
               menuSubItem("Measurement Table", icon = icon("table"), tabName = "Tables_tab"),
               menuSubItem("Detailed CFA Summary", icon = icon("file-alt"), tabName = "CFA_tab"),
               menuSubItem("Syntax Lavaan Models", icon = icon("file-code"), tabName = "Lavaan_tab"),
               menuSubItem("Reliabilities, AVE, SV and ICC", icon = icon("bullseye"), tabName = "AVE")
               
               
      ),
      menuItem("About", tabName = "about", icon = icon("user-astronaut"))
      
      
    )
  ),
  
  dashboardBody(
    use_waiter(),
    use_hostess(),
    
    
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    tabItems(
      tabItem(tabName = "Intro",
              fluidPage(
                fluidRow(
                         box(
                           width = 12, title = "Getting Started and Automatic Factor Detection", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE, collapsed = TRUE,
                           includeHTML("HowTo.html")
                         ) # end of box. 
                                  ), # end of fluidRow. 
                fluidRow(
                          box(
                           width = 12, title = "CFA Specifications", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE, collapsed = TRUE,
                           includeHTML("Estimator.html")
                           
                              ) # end box.
              ), # end fluidRow
              fluidRow(
                       box(
                         width = 12, title = "Interpreting CFA Table", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE,
                          includeHTML("fit_indices.html")
                           )# end of box.
                       ), # end of fluidRow. 
                fluidRow( 
                       box(
                         width = 12, title = "Common Method Bias", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE,
                         includeHTML("CMV.html")
                         
                       ) # end box.
                ), # end fluidRow
              fluidRow( 
                box(
                  width = 12, title = "Reliabilities, AVE, SV, and ICCs", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  includeHTML("reliabilities_tab.html")
                  
                ) # end box.
          ) # end fluidPage
      )
      ),
      tabItem(tabName = "about",
              includeHTML("about_tab.html")
              
      ),
      ##Loading Data and Selecting Variables
      
      
      tabItem(tabName = "LoadData",
              fluidPage(
                fluidRow(
                  column(4, offset = 0, fileInput("file1", "Choose an SPSS or Excel file", accept = c(".sav", ".xlsx")))
                ),
                fluidRow(
                  tabBox(
                    side = "left", selected = "Automatically Search for Factors",
                    tabPanel("Automatically Search for Factors", fluidPage(
                      fluidRow(
                        column(4, uiOutput("VarsSelect")),
                        column(8, verbatimTextOutput("proposed_model_auto_extract"))
                      ),
                      fluidRow(column(4,  uiOutput("Action_Items")))
                    )
                    ),
                    tabPanel("Manually Select Factors", 
                             fluidPage(
                               fluidRow(
                                 column(4,
                                        uiOutput("ui_numb_fact"),
                                        (HTML("<br/>")),
                                        (HTML("<br/>")),
                                        uiOutput("manual_items")),
                                 column(8, verbatimTextOutput("proposed_model_auto_extract2")  )
                               ),
                               fluidRow(
                                 column(4, uiOutput("Action_Items_manual")
                                        
                                 ))
                             )
                    )
                    
                    , width = 12)
                )
              )
      ), 
      
      
      tabItem(tabName = "CorrelationTable_tab",
              fluidPage(
                fluidRow(
                  column(4, uiOutput("cor_columns_input")),
                  column(8, fluidRow(
                    column(4, (HTML("<br/>")),
                           downloadButton(outputId = "downloadCor_Full", label = "Download Correlation Table")),
                    column(4, (HTML("<br/>")),
                           downloadButton(outputId = "downloadCor_LowerT", label = "Download APA-style Correlation Table"),
                           (HTML("<br/>"))   ))
                  )
                ),
                fluidRow(
                  dataTableOutput("CorrelationTable")
                )
              )
      ),
      
      tabItem(tabName = "dfSummary_tab",
              fluidRow(column(12, uiOutput("data_summary")), width = 12)
      ),
      
      tabItem(tabName = "CFA_tab",
              fluidPage(
                fluidRow(column(4, uiOutput("model_selector")),
                         column(8, (HTML("<br/>")), uiOutput("ui_cfa_raw_model_download") )
                ),
                fluidRow(column(10, verbatimTextOutput("CFA_Model")))
              )
      ),
      
      
      
      tabItem(tabName = "Lavaan_tab",
              fluidPage(
                fluidRow(
                  column(4, uiOutput("lavaan_model_selector")),
                  column(8, (HTML("<br/>")), uiOutput("ui_cfa_lavaan_model_download") )
                ),
                fluidRow(
                  column(10, verbatimTextOutput("lavaan_models"))
                ))
      ),
      
      tabItem(tabName = "Tables_tab",
              fluidPage(
                fluidRow(
                  column(4, uiOutput("cfatable_columns_input") ),
                  column(8,     (HTML("<br/>")), uiOutput("ui_table_download"))
                ),
                fluidRow(
                  column(10,dataTableOutput("CFA_Table_DF"))
                )
              )
      ),
      
      
      
      
      tabItem(tabName = "Spec_tab",
              fluidPage(
                fluidRow(
                  column(4, uiOutput("get_estimator")), 
                  column(8, (HTML("<br/>")), uiOutput("Settings_Action"))),
                fluidRow(
                  column(4, uiOutput("is_there_cluster"))),
                fluidRow(
                  column(4, uiOutput("get_cluster"))),
                fluidRow(
                  column(4, uiOutput("orthogonal_ui"))),
                fluidRow(
                  column(4, uiOutput("mimic_ui"))),
                fluidRow(
                  column(12, uiOutput("Which_Models_Selected")))
                
                
                
              )
      ),
      
      tabItem(tabName = "AVE",
              fluidPage(
                fluidRow(
                  column(4, uiOutput("ave_model_selector")),
                  column(8, 
                         fluidRow(
                           column(4, (HTML("<br/>")), uiOutput("ui_ave_download")),
                           column(4, (HTML("<br/>")), uiOutput("ui_ICC_download"))
                         ) 
                         
                  )),
                fluidRow(
                  column(4,uiOutput("ui_ave_title"), dataTableOutput("ave_table_DF"))
                ),
                fluidRow(
                  column(4, textOutput("alpha_text"))
                ),
                fluidRow(
                  column(4,(HTML("<br/>")), (HTML("<br/>")), uiOutput("ui_ICC_title"), dataTableOutput("icc_table_DF"))
                )
              ))
      
      
      
    )
  )  
  # Boxes need to be put in a row (or column)
) 




#data <- lavaan::HolzingerSwineford1939
#' # Changing names to reflect different factors
#names(data)[7:15] <- c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")





##### Changing the type of the png function used in the dfSummary package.####


#### Start Server Function ####


server <- 
  function(input, output, session) {
    
    
    # Adapted Corstars #### from: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
    corstars1 <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                         result=c("none", "html", "latex")){
      #Compute correlation matrix
      x <- as.matrix(x)
      correlation_matrix<-rcorr(x, type=method[1])
      R <- correlation_matrix$r # Matrix of correlation coeficients
      R <- round(R, 2)
      rownames(R) <- colnames(x)
      colnames(R) <- paste(colnames(x), "", sep="")
      
      ## remove last column and return the correlation matrix
      return(R)
    }
    corstars2 <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                         result=c("none", "html", "latex")){
      #Compute correlation matrix
      x <- as.matrix(x)
      correlation_matrix<-rcorr(x, type=method[1])
      R <- correlation_matrix$r # Matrix of correlation coeficients
      p <- correlation_matrix$P # Matrix of p-value 
      
      ## Define notions for significance levels; spacing is important.
      mystars <- ifelse(p < .001, "***", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
      
      ## trunctuate the correlation matrix to two decimal
      R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
      
      ## build a new matrix that includes the correlations with their apropriate stars
      Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
      diag(Rnew) <- paste(diag(R), " ", sep="")
      rownames(Rnew) <- colnames(x)
      colnames(Rnew) <- paste(colnames(x), "", sep="")
      
      ## remove last column and return the correlation matrix
      return(Rnew)
    } 
    
    corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                        result=c("none", "html", "latex")){
      #Compute correlation matrix
      require(Hmisc)
      x <- as.matrix(x)
      correlation_matrix<-rcorr(x, type=method[1])
      R <- correlation_matrix$r # Matrix of correlation coeficients
      p <- correlation_matrix$P # Matrix of p-value 
      
      ## Define notions for significance levels; spacing is important.
      mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
      
      ## trunctuate the correlation matrix to two decimal
      R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
      
      ## build a new matrix that includes the correlations with their apropriate stars
      Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
      diag(Rnew) <- paste(diag(R), " ", sep="")
      rownames(Rnew) <- colnames(x)
      colnames(Rnew) <- paste(colnames(x), "", sep="")
      
      ## remove upper triangle of correlation matrix
      if(removeTriangle[1]=="upper"){
        Rnew <- as.matrix(Rnew)
        Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
        Rnew <- as.data.frame(Rnew)
      }
      
      ## remove lower triangle of correlation matrix
      else if(removeTriangle[1]=="lower"){
        Rnew <- as.matrix(Rnew)
        Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
        Rnew <- as.data.frame(Rnew)
      }
      
      ## remove last column and return the correlation matrix
      Rnew <- cbind(Rnew[1:length(Rnew)-1])
      if (result[1]=="none") return(Rnew)
      else{
        if(result[1]=="html") print(xtable(Rnew), type="html")
        else print(xtable(Rnew), type="latex") 
      }
    } 
    
    
    # CFA Tables Functions ####
    
    #Function that extracts Factor Names
    extract_factor_names <- function(var_names){
      var_names_char <- gsub('_R$', '', var_names)
      var_names_char <- gsub('R$', '', var_names_char)
      var_names_char <- gsub('_Rev$', '', var_names_char)
      var_names_char <- gsub('Rev$', '', var_names_char)
      var_names_char <- gsub('_REV$', '', var_names_char)
      var_names_char <- gsub('REV$', '', var_names_char)
      var_names_char <- gsub('[[:digit:]]+', '', var_names_char)

      
      
      # For when the end of the name is not an alpha numeric character.  
      var_names_char <- map_chr(var_names_char, function(x){
        test <- substr(x,nchar(x), nchar(x))
        test2 <- (gsub("[^[:alnum:]]", "", test))
        if(test == test2){
          return(x)
        } else {
          x <- substr(x, 1, nchar(x)-1)
          return(x)
        }
      })  
      
      #factor_names <- unique(var_names_char)
      var_names_char
    }
    
    
    
    extract_set_of_items <- function(var_names){
      factor_names_fulllist <- extract_factor_names(var_names)
      factor_names <- unique(factor_names_fulllist)
      items_and_names <- map(factor_names, function(x){
        single_factor_name <- x 
        Dummy <- which(factor_names_fulllist == single_factor_name)
        Dummy <- var_names[Dummy]
        set_of_items <- paste(Dummy, collapse = "+")
        set_of_items
        
      })
      
      return(items_and_names) 
    }
    
    
    
    
    
    
    
    
    #############################
    
    
    
    
    
    
    
    
    
    
    ### Function that creates the proposed cfa model.  
    proposed_cfa_model <- function(factor_names, set_of_items){
      #factor_names <- unique(extract_factor_names(var_names))
      #set_of_items <- extract_set_of_items(var_names = var_names)
      single_indicator <- map_lgl(set_of_items, function(x) !str_detect(x, "\\+"))
      
      factor_names[single_indicator] <- paste(factor_names[single_indicator], "_single", sep = "")
      
      ProposedModel <- paste(factor_names, set_of_items, sep = "=~")
      
      #Add error
      if(any(single_indicator)){
        zero_variance <- paste(set_of_items[single_indicator], "~~", "0*", set_of_items[single_indicator], sep = "")
        ProposedModel <- c(ProposedModel, zero_variance)
      }
      ProposedModel <- paste(ProposedModel, collapse = '\n')
      ProposedModel
    }
    
    
    
    combined_cfa_models <- function(factor_names, set_of_items){
      #factor_names <- unique(extract_factor_names(var_names))
      #set_of_items <- extract_set_of_items(var_names = var_names)
      CombinedModels <- vector(mode = "list", ncol(utils::combn(length(factor_names), 2)))
      Counter = 1
      if(length(set_of_items) > 2){
        for (z in 1:length(set_of_items)){
          for( i in 1:(length(set_of_items))){
            if (z >= i){ next
            } else {
              dummy_items <- paste(set_of_items[z], set_of_items[i], sep = "+")
              dummy_name <- paste(factor_names[z], factor_names[i], sep = "_and_")
              dummy_lavaan_model <- paste(dummy_name, dummy_items, sep = "=~")
              set_of_items_other <- set_of_items[c(-i,-z)]
              factor_names_other <- factor_names[c(-i,-z)]
              ###
              single_indicator <- map_lgl(set_of_items_other, function(x) !str_detect(x, "\\+"))
              factor_names_other[single_indicator] <- paste(factor_names_other[single_indicator], "_single", sep = "")
              ProposedModel_Other <- paste(factor_names_other, set_of_items_other, sep = "=~")
              #Add error
              if(any(single_indicator)){
                zero_variance <- paste(set_of_items_other[single_indicator], "~~", "0*", set_of_items_other[single_indicator], sep = "")
                ProposedModel_Other <- c(ProposedModel_Other, zero_variance)
              }
              dummy_lavaan_model_Final <- c(dummy_lavaan_model, ProposedModel_Other)
              dummy_lavaan_model_Final <- paste(dummy_lavaan_model_Final, collapse = '\n')
              
              ###
              
              CombinedModels[Counter] <- dummy_lavaan_model_Final
              Counter = Counter + 1
            }
          }
        }
      } else {
        dummy_items <- paste(set_of_items[1], set_of_items[2], sep = "+")
        dummy_name <- paste(factor_names[1], factor_names[2], sep = "_and_")
        dummy_lavaan_model <- paste(dummy_name, dummy_items, sep = "=~")
        CombinedModels[Counter] <- dummy_lavaan_model
      }
      
      
      
      CombinedModels
    }
    
    
    one_cfa_models <- function(factor_names, set_of_items){
      #factor_names <- unique(extract_factor_names(var_names))
      #set_of_items <- extract_set_of_items(var_names = var_names)
      single_indicator <- map_lgl(set_of_items, function(x) !str_detect(x, "\\+"))
      factor_names[single_indicator] <- paste(factor_names[single_indicator], "_single", sep = "")
      
      
      OneFactorModels <- vector(mode = "list", 3)
      
      set_of_items_one <- paste(set_of_items, collapse = '+')
      one_factor_code <- paste("Harmans_OneFactor", set_of_items_one, sep = "=~")
      OneFactorModels[1] <- one_factor_code
      
      ### latent one factor model
      # LatentOneFactor; First we fix all effects in the latent factor to be equal
      set_of_items_one_latent <- set_of_items_one
      LatentOneFactor <- paste("LatentCMV", set_of_items_one_latent, sep = "=~")
      ProposedModel <- proposed_cfa_model(factor_names, set_of_items)
      LatentOneFactorModel <- paste(LatentOneFactor, ProposedModel, sep = "\n")
      ##### uncorrelated to other constructs
      uncorrelated_to_latent <- map_chr(factor_names, function(x) paste("LatentCMV ~~ 0*", x, sep = ""))
      uncorrelated_to_latent <- paste(uncorrelated_to_latent, collapse = '\n')
      LatentOneFactorModel <- paste(LatentOneFactorModel, uncorrelated_to_latent, sep = "\n")
      OneFactorModels[2] <- LatentOneFactorModel
      
      
      ### latent one factor model
      # LatentOneFactor; First we fix all effects in the latent factor to be equal
      set_of_items_one_latent_fixed <- set_of_items_one
      set_of_items_one_latent_fixed <- paste("1*", set_of_items_one, sep = "")
      set_of_items_one_latent_fixed <- gsub("[+]", "+1*", set_of_items_one_latent_fixed)
      LatentOneFactor_fixed <- paste("LatentCMV_fixed", set_of_items_one_latent_fixed, sep = "=~")
      LatentOneFactorModel_fixed <- paste(LatentOneFactor_fixed, ProposedModel, sep = "\n")
      ##### uncorrelated to other constructs
      uncorrelated_to_latent_fixed <- map_chr(factor_names, function(x) paste("LatentCMV_fixed ~~ 0*", x, sep = ""))
      uncorrelated_to_latent_fixed <- paste(uncorrelated_to_latent_fixed, collapse = '\n')
      LatentOneFactorModel_fixed <- paste(LatentOneFactorModel_fixed, uncorrelated_to_latent_fixed, sep = "\n")
      OneFactorModels[3] <- LatentOneFactorModel_fixed
      OneFactorModels
    }
    
    
    
    all_models <- function(factor_names, set_of_items){
      #factor_names <- unique(extract_factor_names(var_names))
      #set_of_items <- extract_set_of_items(var_names = var_names)
      
      if(length(factor_names) == 1) {
        number_of_combined_models <- 0
        CFA_Models <- vector(mode = "list", (number_of_combined_models + 4) )
        CFA_Models[[1]] <- proposed_cfa_model(var_names)
        OneFactorModels <- one_cfa_models(var_names)
        CFA_Models[[(length(CFA_Models)-2)]] <- OneFactorModels[[1]]
        CFA_Models[[(length(CFA_Models)-1)]] <- OneFactorModels[[2]]
        CFA_Models[[(length(CFA_Models))]] <- OneFactorModels[[3]]
        
        
      } else{
        number_of_combined_models <- ncol(utils::combn(length(factor_names), 2))
        CFA_Models <- vector(mode = "list", (number_of_combined_models + 4) )
        CFA_Models[[1]] <- proposed_cfa_model(var_names)
        Combined_Models <- combined_cfa_models(var_names)
        for(t in 1:number_of_combined_models){
          CFA_Models[[t+1]] <- Combined_Models[[t]]
        }
        OneFactorModels <- one_cfa_models(var_names)
        CFA_Models[[(length(CFA_Models)-2)]] <- OneFactorModels[[1]]
        CFA_Models[[(length(CFA_Models)-1)]] <- OneFactorModels[[2]]
        CFA_Models[[(length(CFA_Models))]] <- OneFactorModels[[3]]
        
      }
      return(CFA_Models)
    }
    
    
    ####################################
    output$Which_Models_Selected <- renderUI({ 
      awesomeCheckboxGroup( 
        inputId = "WhichModelsLavaan",
        label = "Which models do you want to include?", 
        choices = c("Proposed Measurement Model",
                    "All models where two constructs are merged into one",
                    "Harman's single factor test",
                    "Proposed model with unmeasured latent methods factor",
                    "Proposed model with unmeasured latent methods factor with fixed factor loadings"),
        selected = c("Proposed Measurement Model", "All models where two constructs are merged into one", "Harman's single factor test"),
        width = 800
      )
      
    }) 
    
    
    #####################################
    
    
    Get_all_cfa_models <- reactive({
      ############ Hier kan ik van die checks toevoegen.
      #if(input$isThereCluster == "No")
      #var_names = InputVariables()   
      #factor_names <- unique(extract_factor_names(var_names))
      #set_of_items <- extract_set_of_items(var_names = var_names)
      factor_names = factor_names$factor_names
      set_of_items = set_of_items$set_of_items
      
      if(any(input$WhichModelsLavaan == "Proposed Measurement Model")){
        x_prop <- proposed_cfa_model(factor_names, set_of_items)
      } else {
        x_prop <- NULL
      } 
      
      if(any(input$WhichModelsLavaan == "All models where two constructs are merged into one")){
        if(length(factor_names) != 1) {
          x_two_comb <- combined_cfa_models(factor_names, set_of_items)
        } else{
          x_two_comb <- NULL
        }
        
      } else {
        x_two_comb <- NULL
      } 
      
      if(any(input$WhichModelsLavaan == "Harman's single factor test")){
        x_harman <- one_cfa_models(factor_names, set_of_items)
        x_harman <- x_harman[[1]]
      } else {
        x_harman <- NULL
      }
      
      if(any(input$WhichModelsLavaan == "Proposed model with unmeasured latent methods factor")){
        x_cmv <- one_cfa_models(factor_names, set_of_items)
        x_cmv <- x_cmv[[2]]
      } else {
        x_cmv <- NULL
      }
      
      if(any(input$WhichModelsLavaan == "Proposed model with unmeasured latent methods factor with fixed factor loadings")){
        x_cmv_fixed <- one_cfa_models(factor_names, set_of_items)
        x_cmv_fixed <- x_cmv_fixed[[3]]
      } else {
        x_cmv_fixed <- NULL
      }
      
      
      CFA_Models <- append(x_prop, x_two_comb)
      CFA_Models <- append(CFA_Models, x_harman)
      CFA_Models <- append(CFA_Models, x_cmv)
      CFA_Models <- append(CFA_Models, x_cmv_fixed)
      
      return(CFA_Models)
      
    })
    
    
    
    names_models <- function(){
      model_names <- Get_all_cfa_models()
      model_names <- sub('=~.*', '', model_names)
      if(any(input$WhichModelsLavaan == "Proposed Measurement Model")){
        model_names[1] <- "ProposedModel"
      }
      model_names
    }
    
    
    get_names_models <- reactive({
      x <- names_models()
      return(x)
    })
    
    
    
    ##### Estimating all CFA models ####
    
    run_cfa <- function(data,
                        mimic = "lavaan",
                        estimator = "ML",
                        orthogonal = FALSE,
                        cluster = NULL
                        
    ){
      # Generating all models
      All_CFA_Models =  Get_all_cfa_models()
      host <- Hostess$new("loader")
      
      w <- Waiter$new(html = tagList(span("Running CFA models..."), host$get_loader(stroke_color = "#ffffff", 
                                                                                    text_color = "white",
                                                                                    progress_type = "stroke", 
                                                                                    center_page = FALSE, 
                                                                                    value = 0))
      )
      
      
      # Creating empty output lists
      Fitted_Models <- vector(length = length(All_CFA_Models), mode = "list")
      pb = utils::txtProgressBar(min = 0, max = length(Fitted_Models), initial = 0, style = 3)
      # Running the CFA
      w$show()
      host$set(1)
      
      for (i in seq_along(All_CFA_Models)){
        Fitted_Models[[i]] <- cfa(All_CFA_Models[[i]], data=data, mimic = mimic, estimator = estimator, orthogonal = orthogonal, cluster = cluster, meanstructure=TRUE, check.lv.names = FALSE)
        #cat(round((i/length(All_CFA_Models))*100, 2), "%\n")
        host$set((i/length(All_CFA_Models)*100) )
        Sys.sleep(.3)
        
      }
      w$hide()
      cat("\n\n")
      names(Fitted_Models) <- get_names_models()
      Fitted_Models
    }
    
    
    
    
    extract_fit <- function(x){
      x <- tryCatch(lavaan::summary(x, fit.measures=TRUE, standardized = TRUE), error=function(e) NULL)
      if (is.null(x)){
        return(NULL)
      } else{
        x$FIT
      }
    }
    
    function_to_data_frame <- function(x, n){
      if(length(x) == 0) {
        x <- matrix(ncol = n)
        t(as.data.frame(x))
      } else {
        t(as.data.frame(x))
      }
    }
    
    #### Put the output into a dataframe. 
    
 output_in_data_frame <- function(fitted_models){
      output_fits <- map(fitted_models, extract_fit)
      output_fits <-  output_fits[lapply(output_fits,length)>0]
      
      if(length(output_fits[[1]]) != length(output_fits[[length(output_fits)]])){
        #### remove some elements so that the columns are equal in lenght. 
        full_names <- names(output_fits[[1]])
        partial_names <- names(output_fits[[length(output_fits)]])
        overlap_names <- intersect(full_names, partial_names)
        
        for(i in 1:length(output_fits)){
          output_fits[[i]] <-  output_fits[[i]][overlap_names]
        }
      }
      #output_in_df <- purrr::map_df(output_fits, function_to_data_frame, n = length(output_fits[[1]]))
      output_in_df <- function_to_data_frame(x = output_fits, n = length(output_fits[[1]]))
      transposed_output_in_df <- as.data.frame(output_in_df)
      #transposed_output_in_df <- t(output_in_df)
      #transposed_output_in_df <- as.data.frame(transposed_output_in_df)
      #names(transposed_output_in_df) <- names(output_fits[[1]])
      if(any(names(transposed_output_in_df)== "chisq.scaled") ){
        transposed_output_in_df$chisqBydf <- transposed_output_in_df$chisq.scaled/transposed_output_in_df$df
      } else {
        transposed_output_in_df$chisqBydf <- transposed_output_in_df$chisq/transposed_output_in_df$df
        
      }
      transposed_output_in_df
    }
    
       
    
    
    
    
    get_cfa_data_frame <- reactive({
      x <- get_run_cfa()
      
      dataframe.x <- output_in_data_frame(x)
      
      to_compare <- intersect(rownames(dataframe.x), names(x))
      to_compare_models <- x[to_compare]
      
      if (length(to_compare_models) > 1){
      Comparisons2 <- map_df(to_compare_models[2:length(to_compare_models)], function(x){
        model_name <- names(x)
        model1 <- to_compare_models[[1]]
        comp <- anova(model1,x)
        return_this <- data.frame(comp, attr(comp, "heading"))
        return(return_this)
        
      })
      Comparisons3 <- Comparisons2[which(!is.na(Comparisons2$Df.diff) ),]
      R <- Comparisons3$Chisq.diff
      p =  Comparisons3$Pr..Chisq.
      mystars <- ifelse(is.na(p), " ", ifelse(p < .001, "***", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
      Rnew <- paste(round(R, 3), mystars, sep="")
      Rnew <- c(NA, Rnew)
      
      dataframe.x <- round(dataframe.x, 3)
      
      
      if(grepl("satorra.bentler.2001", Comparisons2$attr.comp...heading..[1]
               , fixed = TRUE)){
        dataframe.x$Chisq.Diff_satorra.bentler.2001 <- Rnew
        
      } else if (grepl("satorra.2000", Comparisons2$attr.comp...heading..[1]
                       , fixed = TRUE)){
        dataframe.x$Chisq.Diff_satorra.2000 <- Rnew
        
      } else {
        dataframe.x$Chisq.Diff <- Rnew
        
      }
      } else{
        dataframe.x <- round(dataframe.x, 3)
        
      }
      return(dataframe.x)
    })
    
    
    
    #####################################
    #####################################
    
     dataInput <- reactive({
      req(input$file1)
      if (substr(input$file1$datapath, nchar(input$file1$datapath)-3+1, nchar(input$file1$datapath)) == "sav" )
      {
        df <- foreign::read.spss(input$file1$datapath, to.data.frame = TRUE, use.value.labels = FALSE)
        
        for (i in 1:length(names(df))){
          attributes(df[,i])$label <- attributes(df)$variable.labels[i]
        }
      } else if (substr(input$file1$datapath, nchar(input$file1$datapath)-4+1, nchar(input$file1$datapath)) == "xlsx" ){
        df <- openxlsx::read.xlsx(xlsxFile = input$file1$datapath, colNames = TRUE)
      }
 
      return(df)
    }) 
    
    
    
    # UI Loading data and selecting items ####
    ### Items Selected by User and then given to auto extract.
    output$VarsSelect <- renderUI({
      req(input$file1)
      df <- dataInput()
      column_names <- colnames(df)
      pickerInput(
        inputId = "Id094",
        label = "Select all items for the measurement model", 
        choices = column_names,
        options = list(
          `actions-box` = TRUE), 
        multiple = TRUE
      )
      
    })
    ### Action button - do this then use the items from the auto extract. 
    output$Action_Items <- renderUI({
      req(input$file1)
      actionButton("do_items", "Extract Factors")
      
    })
    
    ### These are the variables to be used for the analyses.
    
    
    ##################################################
    #
    #
    #Manual Output Vars Select. 
    #
    #############################
    output$ui_numb_fact <- renderUI({
      req(input$file1)
      tagList(numericInput(inputId = 'how_many_factors', label = "How many latent constructs are in the model?",
                           value = 1, min = 1, step = 1))
    })
    
    
    
    
    output$manual_items <- renderUI({
      req(input$file1)
      req(input$how_many_factors)
      df <- dataInput()
      
      column_names <- colnames(df)
      #if(is.null(input$num_factor_input)){
      #  return(NULL)
      #}
      
      list_of_factors <- 'tagList('
      for (i in seq_len(input$how_many_factors) ){
        list_of_factors <- paste(list_of_factors,'pickerInput(
      inputId = "manual_indicators',toString(i),
                                 '",label="Choose indicators for Factor ',toString(i),
                                 '",choices = column_names,
                             options = list(
        `actions-box` = TRUE), multiple=TRUE),',sep='')
      }
      
      substr(list_of_factors, nchar(list_of_factors), nchar(list_of_factors)) <- ')'
      eval(parse(text = list_of_factors))
    })
    
    
    
    
    #
    #
    #
    #
    #
    #
    #
    ###################################################
    InputVariables <- eventReactive( c(input$do_items, input$do_items_manual), {
      return((input$Id094)) 
    })
    
    
    First_Factor_Manual <- eventReactive( c(input$do_items, input$do_items_manual), { 
      return((input$manual_indicators1)) 
    })
    
    factor_names <- reactiveValues()
    set_of_items <- reactiveValues()
    
    observeEvent(input$do_items, {
      factor_names$factor_names <- unique(extract_factor_names(InputVariables() ))
    })
    
    observeEvent(input$do_items, {
      set_of_items$set_of_items <- extract_set_of_items(var_names = InputVariables() )
    })
    
    
    
    observeEvent(input$do_items_manual, {
      factor_names$factor_names <- paste(rep("Factor", input$how_many_factors), seq_len(input$how_many_factors), sep = "")
      
    })
    
    observeEvent(input$do_items_manual, {
      x <- rep("", length = input$how_many_factors)
      for(i in seq_len(input$how_many_factors) ){
        e <-  paste('input$manual_indicators',toString(i),sep='')
        e2 <- eval(parse(text = e))
        e2 <- paste(e2, collapse = "+")
        x[i] <- e2
        
      }
      set_of_items$set_of_items <- x
    })
    
    
    #####
    Selected_Items <- reactiveValues()
    
    observeEvent(input$do_items,{
      Selected_Items$items <-input$Id094
    })
    
    observeEvent(input$do_items_manual,{
      x <- vector(mode = "list", length = input$how_many_factors)
      for(i in seq_len(input$how_many_factors) ){
        e <-  paste('input$manual_indicators',toString(i),sep='')
        e2 <- eval(parse(text = e))
        x[[i]] <- e2
      }
      x2 <- unlist(x)
      Selected_Items$items <- x2
    })
    
    
    
    #####
    
    
    #unique(extract_factor_names(var_names))
    #set_of_items <- extract_set_of_items(var_names = var_names)
    
    
    output$Action_Items_manual <- renderUI({
      req(input$file1)
      actionButton("do_items_manual", "Extract Factors")
      
    })
    
    
    
    
    ### These are the variables to be used for the analyses.
    
    
    
    
    #####################
    
    
    ### Proposed CFA Model, based on the auto extract.
    
    output$proposed_model_auto_extract <- renderText({ 
      req(input$file1)
      proposed.cfa <- proposed_cfa_model(factor_names = factor_names$factor_names, set_of_items = set_of_items$set_of_items)
      x <- paste("Preview of Proposed Measurement Model\n\n", proposed.cfa, sep = "")
      return(x)
    }) 
    
    output$proposed_model_auto_extract2 <- renderText({ 
      req(input$file1)
      proposed.cfa <- proposed_cfa_model(factor_names = factor_names$factor_names, set_of_items = set_of_items$set_of_items)
      x <- paste("Preview of Proposed Measurement Model\n\n", proposed.cfa, sep = "")
      return(x)
      
    }) 
    
    
    
    
    # UI Descriptives and Correlations ####
    output$cor_columns_input <- renderUI({ 
      column_names <- c("No", "Variable", "M", "SD", Selected_Items$items)
      pickerInput(
        selected = c("No", "Variable", "M", "SD", Selected_Items$items[1:9]),
        inputId = "cor_columns",
        label = "Please select the columns you want to display", 
        choices = column_names,
        options = list(
          `actions-box` = TRUE), 
        multiple = TRUE
      )
      
    }) 
    
    
    relabeling_function <- function(cor_x, corlabels){
      final_postion <- sort(order(cor_x, decreasing = TRUE))
      current_position <- order(cor_x, decreasing = TRUE)
      corlabels2 <- corlabels
      for(i in seq_along(corlabels)){
        corlabels2[final_postion[i]] <- corlabels[current_position[i]]
      }
      return(corlabels2)
    }
    
    
    CorrelationTableDT <- reactive({
      req(input$file1)
      req(Selected_Items$items)
      df <- dataInput()
      if (is.null(Selected_Items$items)){
        
      } else {
        df.subset <- df[, Selected_Items$items]
        
        R_Final <- corstars1(df.subset) 
        R_Final <- as.data.frame(R_Final)
        Rnew <- corstars2(df.subset) 
        Rnew <- as.data.frame(Rnew)
        
        x_new <- R_Final
        
        for(i in seq_along(names(R_Final))){
          x_new[,i] = factor(rank(-R_Final[,i], ties.method= "first"),
                             labels = relabeling_function(R_Final[,i], Rnew[,i]),
                             levels = unique(sort(order(R_Final[,i], decreasing = TRUE))))
          
        }
        
        
        mean_data <- map_df(df.subset, function(x) {
          x1 <- mean(x, na.rm = TRUE)
          return(x1)
        })
        sd_data <- map_df(df.subset, function(x) {
          x1 <- sd(x, na.rm = TRUE)
          return(x1)
        })
        
        mean_data <- round(t(mean_data), 2) 
        sd_data <- round(t(sd_data), 2) 
        data_to_return <- cbind(mean_data, sd_data, x_new) 
        names(data_to_return)[1:2] <- c("M", "SD") 
        Variable <- row.names(data_to_return)
        No <- c((seq_along(row.names(data_to_return))))
        data_to_return <- cbind(No, Variable, data_to_return)
        
        return(data_to_return)
      } 
    })
    
    output$CorrelationTable <- renderDataTable({ 
      data_to_return <- CorrelationTableDT()
      subset_table <- data_to_return[, input$cor_columns, drop = F]
      return(subset_table)
    }, options = list(pageLength = 50), rownames = FALSE)  
    
    
    
    CorrelationTableDT_Full <- reactive({
      x <- CorrelationTableDT() 
      x[,3] <- as.character(x[,3]) 
      x[,4] <- as.character(x[,4])
      return(x)
    })
    
    
    
    output$downloadCor_Full <- downloadHandler(
      filename = function() {
        paste("cor-table-full", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(CorrelationTableDT_Full(), file, asTable = FALSE, 
                   row.names = FALSE, 
                   col.names = TRUE,
                   firstActiveRow  = 2, 
                   firstActiveCol = 3,
                   headerStyle = createStyle(textDecoration = "ITALIC")
        )
      }
    )
    
    
    CorTab_LowerTriang <- reactive({
      req(input$file1)
      df <- dataInput()
      if (is.null(Selected_Items$items)){
        
      } else {
        df.subset <- df[, Selected_Items$items]
        
        R_Final <- corstars(df.subset) 
        R_Final <- as.data.frame(R_Final)
        
        mean_data <- map_df(df.subset, function(x) {
          x1 <- mean(x, na.rm = TRUE)
          return(x1)
        })
        sd_data <- map_df(df.subset, function(x) {
          x1 <- sd(x, na.rm = TRUE)
          return(x1)
        })
        
        mean_data <- round(t(mean_data), 2) 
        sd_data <- round(t(sd_data), 2) 
        data_to_return <- cbind(mean_data, sd_data, R_Final) 
        names(data_to_return)[1:2] <- c("M", "SD") 
        Variable <- row.names(data_to_return)
        No <- c((seq_along(row.names(data_to_return))))
        data_to_return <- cbind(No, Variable, data_to_return)
        
        data_to_return[,3] <- as.character(data_to_return[,3]) 
        data_to_return[,4] <- as.character(data_to_return[,4])
        names(data_to_return)[5:length(names(data_to_return))] <- c((1:(length(row.names(data_to_return))-1) ))
        return(data_to_return)
      } 
    })
    
    output$downloadCor_LowerT <- downloadHandler(
      filename = function() {
        paste("cor-table", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(CorTab_LowerTriang(), file, asTable = FALSE, 
                   row.names = FALSE, 
                   col.names = TRUE, 
                   firstActiveRow  = 2, 
                   firstActiveCol = 3,
                   headerStyle = createStyle(textDecoration = "ITALIC")
        )
      }
    )
    
    
    
    output$data_summary <- renderPrint({
      st_css()
      
      req(input$file1)
      df <- dataInput()
      if (is.null(Selected_Items$items)){
        
      } else {
        df.subset <- df[, Selected_Items$items]
        out <- print(dfSummary(df.subset, round.digits = 3, graph.magnif = 1.2, valid.col = FALSE), omit.headings = FALSE, method = 'render')
        out[[3]][[2]][[5]] <- gsub("IQR [(]CV[)]", "<br/> IQR (CV)", out[[3]][[2]][[5]])
        out[[3]][[2]][[5]] <- gsub("Mean [(]sd[)]", "Mean (SD)", out[[3]][[2]][[5]])
        out[[3]][[2]][[5]] <- gsub("min < med < max", "<br/>Min < Med < Max < ", out[[3]][[2]][[5]])
        out <- out[[3]][[2]][[5]]
        return(out)
      } 
    })
    
    # CFA MODEL CODE #####
    
    
    ########################################
    # Specifications
    ########################################
    # mimic = "lavaan",
    # estimator = "ML",
    # orthogonal = FALSE,
    # cluster = NULL
    output$get_estimator <- renderUI({
      
      pickerInput(
        inputId = "which_estimator",
        selected = "ML",
        label = "Select the estimator",
        choices = c("ML", "MLR", "MLM","MLF",
                    "MLMVS", "MLMV", "GLS", "WLS","DWLS", "ULS",
                    "WLSM", "WLSMVS",
                    "WLSMV", "ULSM", "ULSMVS", "ULSMV"
        ),
        choicesOpt = list(
          subtext = c("Maximum likelihood",
                      "ML with robust (Huber-White) SE's and (asymptotically) Yuan-Bentler test statistic.",
                      "ML with robust SE's and Satorra-Bentler scaled (mean adjusted) test statistic",
                      "ML with SE's based on the first-order derivatives, and a conventional test statistic",
                      "ML with robust SE's and a mean- and variance adjusted test statistic (aka the Satterthwaite approach)",
                      "ML with robust SE's and a mean- and variance adjusted test statistic (using a scale-shifted approach)",
                      "Generalized least squares",
                      "Weighted least squares - or asymptopically distribution free estimation",
                      "Diagonally Weighted Least Squares", 
                      "Unweighted least squares",
                      "Weighted least squares with robust SE's and mean-adjusted test statistic", 
                      "Weighted least squares with robust SE's and mean- and variance-adjusted test statistic (Satterthwaite)",
                      "Weighted least squares with robust SE's and mean- and variance adjusted test statistic (scale-shifted)", 
                      "Unweighted least squares with robust SE's and mean-adjusted test statistic", 
                      "Unweighted least squares with robust SE's and mean- and variance-adjusted test statistic (Satterthwaite)", 
                      "Unweighted least squares with robust SE's and mean- and variance-adjusted test statistic (scale-shifted)")
        )
      )
    })
    
    
    output$get_estimator <- renderUI({
      req(input$isThereCluster)
      if(input$isThereCluster == "No"){
        choices = c("ML", "MLR", "MLM","MLF",
                    "MLMVS", "MLMV", "GLS", "WLS","DWLS", "ULS",
                    "WLSM", "WLSMVS",
                    "WLSMV", "ULSM", "ULSMVS", "ULSMV"
        )
        
        subtext = c("Maximum likelihood",
                    "ML with robust (Huber-White) SE's and (asymptotically) Yuan-Bentler test statistic.",
                    "ML with robust SE's and Satorra-Bentler scaled (mean adjusted) test statistic",
                    "ML with SE's based on the first-order derivatives, and a conventional test statistic",
                    "ML with robust SE's and a mean- and variance adjusted test statistic (aka the Satterthwaite approach)",
                    "ML with robust SE's and a mean- and variance adjusted test statistic (using a scale-shifted approach)",
                    "Generalized least squares",
                    "Weighted least squares - or asymptopically distribution free estimation",
                    "Diagonally Weighted Least Squares", 
                    "Unweighted least squares",
                    "Weighted least squares with robust SE's and mean-adjusted test statistic", 
                    "Weighted least squares with robust SE's and mean- and variance-adjusted test statistic (Satterthwaite)",
                    "Weighted least squares with robust SE's and mean- and variance adjusted test statistic (scale-shifted)", 
                    "Unweighted least squares with robust SE's and mean-adjusted test statistic", 
                    "Unweighted least squares with robust SE's and mean- and variance-adjusted test statistic (Satterthwaite)", 
                    "Unweighted least squares with robust SE's and mean- and variance-adjusted test statistic (scale-shifted)"
        )
      }
      
      if(input$isThereCluster == "Yes"){
        choices = c("MLR", "MLM","MLF",
                    "MLMVS", "MLMV",
                    "WLSM", "WLSMVS",
                    "WLSMV", "ULSM", "ULSMVS", "ULSMV"
        )
        
        subtext = c("ML with robust (Huber-White) SE's and (asymptotically) Yuan-Bentler test statistic.",
                    "ML with robust SE's and Satorra-Bentler scaled (mean adjusted) test statistic",
                    "ML with SE's based on the first-order derivatives, and a conventional test statistic",
                    "ML with robust SE's and a mean- and variance adjusted test statistic (aka the Satterthwaite approach)",
                    "ML with robust SE's and a mean- and variance adjusted test statistic (using a scale-shifted approach)",
                    "Weighted least squares with robust SE's and mean-adjusted test statistic", 
                    "Weighted least squares with robust SE's and mean- and variance-adjusted test statistic (Satterthwaite)",
                    "Weighted least squares with robust SE's and mean- and variance adjusted test statistic (scale-shifted)", 
                    "Unweighted least squares with robust SE's and mean-adjusted test statistic", 
                    "Unweighted least squares with robust SE's and mean- and variance-adjusted test statistic (Satterthwaite)", 
                    "Unweighted least squares with robust SE's and mean- and variance-adjusted test statistic (scale-shifted)"
        )
      }
      
      pickerInput(
        inputId = "which_estimator",
        selected = "ML",
        label = "Select the estimator",
        choices = choices,
        choicesOpt = list(
          subtext = subtext
        )
      )
    })
    
    
    output$get_cluster <- renderUI({
      req(input$isThereCluster)
      req(input$file1)
      if(input$isThereCluster == "Yes"){
        df <- dataInput()
        column_names <- colnames(df)
        pickerInput(
          inputId = "get_cluster",
          label = "Which variable represents the cluster?", 
          choices = c(column_names),
          options = list(
            `actions-box` = TRUE), 
          multiple = FALSE    )
      } 
    })
    
    output$is_there_cluster <- renderUI({
      prettyRadioButtons(
        inputId = "isThereCluster",
        label = "Is the data clustered?", 
        choices = c("No", "Yes"),
        inline = TRUE, 
        status = "primary",
        fill = TRUE,
        selected = "No"
      )
    })
    
    
    use_this_cluster <- reactive({
      req(input$isThereCluster)
      if(input$isThereCluster == "Yes"){
        return(input$get_cluster)
      } else{
        return(NULL)
      }
    })
    
    
    output$orthogonal_ui <- renderUI({
      prettyRadioButtons(
        inputId = "orthogonal",
        label = "Orthogonal", 
        choices = c(FALSE, TRUE),
        inline = TRUE, 
        status = "primary",
        fill = TRUE
      )
    })
    
    output$mimic_ui <- renderUI({
      prettyRadioButtons(
        inputId = "Mimic",
        label = "Mimic", 
        choices = c("lavaan", "Mplus", "EQS"),
        inline = TRUE, 
        status = "primary",
        fill = TRUE
      )
    })
    
    
    ########################################
    ### Action button - do this then use the items from the auto extract. 
    output$Settings_Action <- renderUI({
      req(input$file1)
      if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
        
        actionButton("do_TheseSettings", "Run CFA with these settings")
      }
    })
    
    observeEvent(input$do_TheseSettings, {
      newtab <- switch(input$tabs, "Spec_tab" = "Tables_tab","Tables_tab" = "Spec_tab")
      updateTabItems(session, "tabs", newtab)
    })
    
    
    get_run_cfa <- eventReactive(input$do_TheseSettings, {
      if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
        x <- run_cfa(data = dataInput(), estimator = input$which_estimator, cluster = use_this_cluster(), orthogonal = input$orthogonal,
                     mimic = input$Mimic)
        return(x)    
      }
    })
    
    
    
    
    
    ### Proposed CFA Model, based on the auto extract.
    
    Which_CFA_Model <- eventReactive(input$do_items, { 
      return((input$Id094)) 
    })
    
    
    output$model_selector <- renderUI({
      req(input$file1)
      x <- get_run_cfa()
      column_names <- names(x)
      pickerInput(
        inputId = "Model_to_Display",
        label = "Please select a measurement model", 
        choices = column_names
      )
      
    })
    
    #######################
    
    output$CFA_Model <- renderText({
      req(input$Model_to_Display)
      x <- get_run_cfa()
      name_model <- input$Model_to_Display
      text_out <- paste(utils::capture.output(lavaan::summary(x[[input$Model_to_Display]], rsquare = TRUE, fit = TRUE,standardize = TRUE)),collapse = "\n")
      text_out <- paste("###\n", "Measurement Model:", name_model,"\n###\n\n" , text_out)
      return(text_out)
      
    })
    
    output$cfa_raw_model_download <- downloadHandler(
      filename = function() {
        paste("summary-all-models ", Sys.Date(), ".txt", sep="")
      },
      content = function(file) {
        x <- get_run_cfa()
        x2 <- map(x, function(x){
          return_this <- paste(utils::capture.output(lavaan::summary(x, rsquare = TRUE, fit = TRUE,standardize = TRUE)),collapse = "\n")
          return(return_this)
        }
        )
        
        test_names <- names(x)
        test_names2 <- paste("#### Model: ", test_names, "", sep = "")
        testb <- paste(x2, "\n\n\n", sep = "")
        test3 <- paste(test_names2, testb, sep = "\n\n")
        write_this <- test3
        writeLines(write_this, file, sep = "\n")
      }
    )
    
    output$ui_cfa_raw_model_download <- renderUI({
      req(input$file1)
      if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
        downloadButton(outputId = "cfa_raw_model_download", label = "Download summary output of all models")    
      }
      
      
    })
    
    
    ##########################
    # Display Lavaan Code
    output$lavaan_model_selector <- renderUI({
      req(input$file1)
      names_x <- get_names_models()
      column_names <- names_x
      pickerInput(
        inputId = "lavaan_model_to_Display",
        label = "Please select a measurement model", 
        choices = column_names
      )
      
    })
    
    
    text_lavaan <- reactive({
      x <- Get_all_cfa_models()
      names_x <- get_names_models()
      names(x) <- names_x 
      return(x)
    })
    
    
    lavaan_models_reactive <- eventReactive(c(input$do_TheseSettings, input$lavaan_model_to_Display),{
      x <- text_lavaan()
      name_model <- input$lavaan_model_to_Display
      text_out <- (x[[input$lavaan_model_to_Display]])
      text_out <- paste("###\n", "Measurement Model:", name_model,"\n###\n\n" , text_out, sep = "")
      return(text_out)
    }) 
    
    output$lavaan_models <- renderText({
      req(input$lavaan_model_to_Display)
      lavaan_models_reactive()
    })
    
    output$cfa_lavaan_model_download <- downloadHandler(
      
      filename = function() {
        paste("lavaan-models ", Sys.Date(), ".txt", sep="")
      },
      content = function(file) {
        test <- text_lavaan()
        test_names <- names(text_lavaan() )
        test_names2 <- paste("#### Model: ", test_names, "", sep = "")
        testb <- paste(test, "\n\n\n", sep = "")
        test3 <- paste(test_names2, testb, sep = "\n\n")
        write_this <- test3
        writeLines(write_this, file, sep = "\n")
      }
    )
    
    output$ui_cfa_lavaan_model_download <- renderUI({
      req(input$file1)
      if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
        downloadButton(outputId = "cfa_lavaan_model_download", label = "Download all lavaan models")
      }
      
    })
    
    
    
    
    #########################
 
    basic_indices <- reactive({
      if( any(input$which_estimator == c("MLMVS", "MLMV","WLSM","WLSMVS",
                                         "WLSMV", "ULSM", "ULSMVS", "ULSMV") ) ){
        x <- c("chisq.scaled",
                           "df",
                           "chisqBydf",
                           "cfi.scaled",
                           "rmsea.scaled",
                           "srmr",
                           "Chisq.Diff_satorra.2000")
      } else if (any(input$which_estimator == c("MLR", "MLM") ) ){
        x <- c("chisq.scaled",
                           "df",
                           "chisqBydf",
                           "cfi.robust",
                           "cfi.scaled",
                           "rmsea.robust",
                           "srmr",
                           "Chisq.Diff_satorra.bentler.2001")
      } else{
        x <- c("chisq",
                           "df",
                           "chisqBydf",
                           "cfi",
                           "rmsea",
                           "srmr",
                           "Chisq.Diff")
      }
      return(x)
    })   
    
    
    #### Put the output into a dataframe. 
    ## Input for CFA Table
    
    
    
      output$cfatable_columns_input <- renderUI({
      x <- get_cfa_data_frame()
      
      select_these <- basic_indices()
      #select_these <- intersect(basic_indices() , names(x))
      column_names <- names(x)
      pickerInput(
        inputId = "cfa_columns",
        selected = select_these,
        label = "Please select the columns you want to display", 
        choices = column_names,
        options = list(
          `actions-box` = TRUE), 
        multiple = TRUE
      )
      
    }) 
    
    
    
    edited_cfa_table <- reactive({
      req(input$cfa_columns)
      fit_indices = input$cfa_columns
      x <- get_cfa_data_frame()
      fit_indices <- intersect(all_of(fit_indices) , names(x))
      x <- select(x, fit_indices)
      return(x)
      
    })
    
    output$CFA_Table_DF <- renderDataTable({
      req(input$cfa_columns)
      x <- edited_cfa_table()
      return(x)
    }, options = list(pageLength = 50), rownames = TRUE)
    
    
    output$CFA_Table_download <- downloadHandler(
      filename = function() {
        paste("cfa-table-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(edited_cfa_table(), file, asTable = FALSE, 
                   row.names = TRUE, 
                   col.names = TRUE,
                   headerStyle = createStyle(textDecoration = "ITALIC")
                   
        )
      }
    )
    ############# Waiter
    # specify the id 
    
    
    #################
    
    
    output$ui_table_download <- renderUI({
      req(input$file1)
      if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
        downloadButton(outputId = "CFA_Table_download", label = "Download Measurement Table")   
      }
      
    })
    
    
    #########################################################
    ################# AVE
    output$ave_model_selector <- renderUI({
      req(input$file1)
      names_x <- get_names_models()
      column_names <- names_x
      pickerInput(
        inputId = "ave_lavaan_model_to_show",
        label = "Please select a measurement model", 
        choices = column_names
      )
    })
    
    extract_reliabilities <-   function(cfa_model){
      relia_x <- reliability(cfa_model)
      
      # first check if there was only one variable that had a reliability. 
      if(is.null(nrow(relia_x))){
        relia_x <- t(as.matrix(relia_x))
        x <- lavInspect(cfa_model,"partable")
        x <- x$lambda
        x <- data.frame(x)
        l <- map_dbl(x, function(x){
          x <- as.factor(x)
          return(summary(x)[[1]])
          
              })
        row.names(relia_x) <- names(which.min(l))
        trelia_x <- relia_x
        trelia_x <- round(as.data.frame(trelia_x),3)
        trelia_x <- trelia_x[,-c(3,4)]
        
        names(trelia_x)[3] <- "AVE"
        cors_x <- round(as.data.frame(lavInspect(cfa_model,"cor.lv")^2),3)
        names(cors_x) <- paste("SV_", names(cors_x), sep = "")
        cors_x$Factor <- row.names(cors_x)
        trelia_x$Factor <- row.names(trelia_x)
        
        cors_x$order <- c(1:nrow(cors_x)) # to reorder rows. 
        test_merge2 <- merge(trelia_x, cors_x, by='Factor', all=TRUE) # merge cors with relia. Missing values will be added
        row.names(test_merge2) <- test_merge2$Factor # put factor as row names
        x1 <- test_merge2[,-c(1)] # Delete factor column
        x2 <- x1[order(x1$order),] # Reorder rows
        x <- x2[, c(-ncol(x2))] # delete reordering variable. 
        
            } else if (ncol(relia_x != 0)) {
         
              trelia_x <- t(relia_x)
              trelia_x <- round(as.data.frame(trelia_x),3)
              trelia_x <- trelia_x[,-c(3,4)]
        
              if(any(rownames(trelia_x) == "total") ){
              trelia_x   <- trelia_x[row.names(trelia_x) != "total",]
              }
              names(trelia_x)[3] <- "AVE"
              cors_x <- round(as.data.frame(lavInspect(cfa_model,"cor.lv")^2),3)
              names(cors_x) <- paste("SV_", names(cors_x), sep = "")
              cors_x$Factor <- row.names(cors_x)
              trelia_x$Factor <- row.names(trelia_x)
              
              cors_x$order <- c(1:nrow(cors_x)) # to reorder rows. 
              test_merge2 <- merge(trelia_x, cors_x, by='Factor', all=TRUE) # merge cors with relia. Missing values will be added
              row.names(test_merge2) <- test_merge2$Factor # put factor as row names
              x1 <- test_merge2[,-c(1)] # Delete factor column
              x2 <- x1[order(x1$order),] # Reorder rows
              x <- x2[, c(-ncol(x2))] # delete reordering variable. 
              
              
              
            } else {
              cors_x <- round(as.data.frame(lavInspect(cfa_model,"cor.lv")^2),3)
              names(cors_x) <- paste("SV_", names(cors_x), sep = "")
              x <- cors_x
        
      } 
      return(x)
    }
    
    
    should_these_be_reversed <- function(cfa_model){
      x <- cfa_model@ParTable$rhs[c(which(cfa_model@ParTable$est[which(cfa_model@ParTable$user == 1)] < 0))]
      return(x)
    }
    
    
    ave_table <- reactive({   
      x <- get_run_cfa()
      ave_output <- map(x, function(x) {
        output_return <- extract_reliabilities(x)
        return(output_return)
      })
      name_model <- input$ave_lavaan_model_to_show
      ave_display <- ave_output[[name_model]]
      return(ave_display)
    })    
    
    output$ave_table_DF <- renderDataTable({ 
      req(input$ave_lavaan_model_to_show)
      x <- ave_table()
      return(x)
    }, options = list(pageLength = 50, dom = "t", ordering=F), rownames = TRUE)
    
    
    text_reversed <- reactive({
      x <- get_run_cfa()
      reverse_output <- map(x, function(x) {
        return_reversed <- should_these_be_reversed(x)
        return(return_reversed)
      })
      name_model <- input$ave_lavaan_model_to_show  
      reverse_display <- reverse_output[[name_model]] 
      return(reverse_display)
    }) 
    
    
    output$alpha_text <- renderText({
      req(input$ave_lavaan_model_to_show)
      x <- text_reversed()
      if(is_empty(x)) {
        return("")
      } else {  
      return(c("Reversed items need to be reverse scored prior to the analysis to get the correct Cronbach's Alpha and McDonald's Omega. Perhaps the following item(s) need to be reverse scored: ", x) )
      }
    })
    
    
    output$ui_ave_download <- renderUI({
      req(input$file1)
      if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
        downloadButton(outputId = "AVE_Table_download", label = "Download Reliability Table")  
      }
      
      
    })
    
    output$AVE_Table_download <- downloadHandler(
      filename = function() {
        paste("reliabilities-", input$ave_lavaan_model_to_show,"-" , Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(ave_table(), file, asTable = FALSE, 
                   row.names = TRUE, 
                   col.names = TRUE,
                   headerStyle = createStyle(textDecoration = "ITALIC")
                   
        )
      }
    )
    
                        
    #####

    ####
       
    ICC1 <- function(object){
      MOD <- summary(object)
      MSB <- MOD[[1]][1, 3]
      MSW <- MOD[[1]][2, 3]
      GSIZE <- (MOD[[1]][2, 1] + (MOD[[1]][1, 1] + 1))/(MOD[[1]][1, 
                                                                 1] + 1)
      OUT <- (MSB - MSW)/(MSB + ((GSIZE - 1) * MSW))
      return(OUT)
    }
    
    ICC2 <- function(object) 
    {
      MOD <- summary(object)
      MSB <- MOD[[1]][1, 3]
      MSW <- MOD[[1]][2, 3]
      OUT <- (MSB - MSW)/MSB
      return(OUT)
    }
    
    extract_icc <- function(cfa_model){
      factor_Scores <- lavPredict(cfa_model, method = "ML")
      cluster <- cfa_model@Data@Lp[[1]][7]$cluster.idx[[2]]
      icc_data_Frame <- data.frame(factor_Scores, cluster)
      t1 <- ncol(factor_Scores)
      t2 <- ncol(icc_data_Frame)
      if(t1 > 1){
        y <- map_df(icc_data_Frame[, 1:t1], function(x){
          model1 <- aov(x~ as.factor(icc_data_Frame[,t2]))
          one <- ICC1(model1)
          two <- ICC2(model1)
          to_return <- round(c(one, two), 3)
          return(to_return)
        })
        y <- as.data.frame(y)
        row.names(y) <- c("ICC1", "ICC2")
        return(y)
      } else{
        model1 <- aov(icc_data_Frame[, 1:t1]~ as.factor(icc_data_Frame[,t2]))
        one <- ICC1(model1)
        two <- ICC2(model1)
        to_return <- c(one, two)
        to_return <- round(as.data.frame(to_return), 3)
        names(to_return) <- colnames(factor_Scores)
        row.names(to_return) <- c("ICC1", "ICC2")
        return(to_return)
      }
    }
    
    icc_table <- reactive({   
      req(input$isThereCluster)
      if(input$isThereCluster == "Yes"){
        x <- get_run_cfa()
        
        icc_output <- map(x, function(x) {
          output_return <- extract_icc(x)
          return(output_return)
        })
        names(icc_output) <- get_names_models()
        name_model <- input$ave_lavaan_model_to_show
        icc_display <- icc_output[[name_model]]
        return(icc_display)
      } 
    })  
    
    output$icc_table_DF <- renderDataTable({ 
      req(input$ave_lavaan_model_to_show)
      req(input$isThereCluster)
      if(input$isThereCluster == "Yes"){
        x <- icc_table()
        return(x)}
    }, options = list(pageLength = 50, dom = "t", ordering=F), rownames = TRUE)
    
    
    output$ui_ICC_download <- renderUI({
      req(input$file1)
      req(input$isThereCluster)
      if(input$isThereCluster == "Yes"){
        if( length(InputVariables() ) != 0 | length(First_Factor_Manual() ) != 0 ){
          downloadButton(outputId = "ICC_Table_download", label = "Download ICC Table")  
        }
      }
      
    })
    
    output$ICC_Table_download <- downloadHandler(
      filename = function() {
        paste("icc-", input$ave_lavaan_model_to_show,"-" , Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(icc_table(), file, asTable = FALSE, 
                   row.names = TRUE, 
                   col.names = TRUE,
                   headerStyle = createStyle(textDecoration = "ITALIC")
                   
        )
      }
    )
    
    
    output$ui_ICC_title <- renderUI({
      req(input$file1)
      if(input$isThereCluster == "Yes"){
        h4(tags$b("Intraclass correlations"))
      }
    }) 
    
    output$ui_ave_title <- renderUI({
      req(input$file1)
      h4(tags$b("Reliabilities"))
    }) 
    
    
    
    ################
    
    #output$about_tab <- renderUI({
    #  url1 <- a("Leander De Schutter", href="https://nl.linkedin.com/in/leander-de-schutter")
    #  tagList("Written by: ", url)
    #})
    
    
    ##############################
    
    
    
  }



shinyApp(ui, server)
