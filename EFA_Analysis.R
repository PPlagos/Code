#### ---------- Emotional Dependency on AI Chatbots Scale (EDCS) ---------- ####
#### ---------- MSc Cyberpsychology Thesis ---------- ####
#### ---------- Panagiotis Plagos ---------- ####
#### ---------- Exploratory Factor Analysis ---------- ####

#### Load Dependencies ####
# install.packages("tidyverse") #data handling
# install.packages("psych") #psychometrics
# install.packages("car") #VIF Metric
# install.packages("EFAtools") #Parallel Analysis-required package
# install.packages("GPArotation") #Rotation methods for factanal
# install.packages("ggpattern") #For Visualization
# install.packages("rempsyc")
# install.packages("flextable")
# install.packages("apa7")

library(tidyverse)
library(psych)
library(car)
library(EFAtools)
library(GPArotation)
library(rempsyc)
library(flextable)
library(apa7)
library(officer)

#### Loading Data ####
dataset <- read.csv("-- path to the file containing the respondents data", sep=",", fileEncoding="latin1") #read csv file with numeric data
dataset <- as.data.frame(dataset) #set the data as dataframe

#### Cleaning Data ###

#Delete First Row (Item Descriptions)
dataset <- dataset[-1,]

#Inspect Dataset
dataset |> head()
summary(dataset)

#Convert character columns to numeric
df <- as.data.frame(apply(dataset, 2, as.numeric))

#Inspect the Dataset
df |> summary()
df |> dim()
df |> str()
df |> colnames()


#Deleting Random Answers - Respondents that failed both tests (IRI, Bogus)
df_valid <- df |> 
  #Q19 is the IRI
  filter(Q19 == 2) |> 
  #Q48 Bogus Item
  filter(Q48 == 1) |>
  #Deletion of the Answers from both Bogus and IRI
  select(!Q19 & !Q48)

#### Descriptive Statistics ####

#Create function that inspects the descriptive metrics of our data
describe_data <- function(df_valid) {
  descriptive <- describe(df_valid)
  
  #Observe the number of rows, mean, standard deviation, minimum and maximum valuem, skewness, and kurtosis of all items' values' distributions
  
  descriptives_table_raw <- as.data.frame(round(descriptive[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")], 3))
  descriptives_table <- descriptives_table_raw |> #from the raw data,
    rownames_to_column("Item") |> #set items from rows to column
    rename(
      ITEM = Item, N = n, MEAN = mean, SD = sd, #rename the column names 
      MIN = min, MAX = max, SKEW = skew, KURTOSIS = kurtosis 
    )
  descriptives_table <- nice_table(descriptives_table, #set the dataframe into a table with a note, APA style
                                   note = "
                                  SD, Standard Deviation;
                                  MIN, Minimum Value;
                                  MAX, Maximum Value")
  
  return(list( #Return from the function:
    descriptives_table = descriptives_table, #The final apa table
    descriptives_table_raw = descriptives_table_raw, #the raw data
    
    cat("This function returns both the table with the raw descriptive data of the dataset, 
as well as a more clean version of the table created with the nice_table package.
Use either descriptives_table or descriptives_table_raw"))) #describe what happens when using the function
  
}

descriptives <- describe_data(df_valid) #apply the function into our data

#Inspect the Table
descriptives$descriptives_table 

#Q29 has no standard Deviation

#Possible Floor Effect (American Psychological Association, 2018 - full citation in the paper), too strict of an item

#Necessary to be deleted to conduct factor analysis because it uses a correlation matrix
df_valid <- df_valid |> 
  select(!Q29) #delete Q29


#Correlation Matrix
corr_matrix <- cor(df_valid) #Inspect Correlation Matrix from our data
corrmatrix <- round(as.data.frame(round(corr_matrix)),2) #round correlations values to two digits
corr_matrix_table <- nice_table(corrmatrix, note = "Pearson Correlations") #set correlations into a table with a note
corr_matrix_table #inspect the table

prerequisites <- function(df_valid) { #creating a function that, when passing a dataset, checks the prerequisites that need to be fulfilled to run a factor analysis properly
  
  #Inspect Sampling Adequacy withcorrmatrix#Inspect Sampling Adequacy with Kaiser-Meyer-Olkin (KMO)
  # KMO > 0.70 means we have a good sample, > 0.80 is great, 0.90 is superb (Field et al., 2012; Howard, 2016 - full citations in the paper, bibliography section)
  KMO_Test <- KMO(df_valid)
  
  #Inspect Problematic Correlations with Bartlett's Sphericity Test
  #Identity Matrix or not?
  #Significant P Value means we can Continue (Field et al., 2012 - full citations in the paper, bibliography section)
  Bartlett_Test <- cortest.bartlett(df_valid)

  #Inspect VIF values to evaluate multicollinearity problem.
  #Items with VIF < 3 warrant no further action (Kyriazos & Poga, 2023 - full citations in the paper, bibliography section)
  names <- colnames(df_valid) #take the colnames
  dependent <- names[1] #set the first as a dependent variable
  predictors <- names[-1] #all variables except the first one acting as predicots
  formula <- as.formula(paste(dependent, "~", paste(predictors, collapse = "+"))) #set the formula to run VIF analysis
  vifmodel <- lm(formula, data = df_valid) #linear model, with our data and formula
  vif_values <- vif(vifmodel) #apply vif analysis with our model
  problematic_vif_values <- vif_values[vif_values > 10] #check values above 10 as problematic 
  concerning_vif_values <- vif_values[vif_values > 5] #check values above 5 as concerning 
  acceptable_vif_values <- vif_values[vif_values > 3] #check values above 3 as acceptable
  #values below 3 are acceptable (Kyriazos & Poga, 2023 - full citations in the paper, bibliography section)
  
  #Inspect Normality of the data. 
  Mardia_Results <- psych::mardia(df_valid) #apply the mardia test in the dataset

  if(Mardia_Results$p.skew < 0.05 & Mardia_Results$p.kurt < 0.05) { #Inspect significance of both kurtosis and skew result
    
    #message if significant values are recorded for both p values
    cat("Mardia's test showcases that the data did not meet multivariate normality, as shown by the significant multivariate skewness (b1 =", round(Mardia_Results$b1p,3),
        ",χ² =", round(Mardia_Results$skew,3), ",p < .001), 
and by the significant multivariate kurtosis (b2 =", round(Mardia_Results$b2p,3),", z =", round(Mardia_Results$kurtosis,3), ", p < .001).
These results justify the use of Principal Axis Factoring instead of Maximum Likelihood extraction method for Exploratory Factor Analysis. \n \n")
  } else {
    #message if no significance is reached for both p values
    cat("Mardia's test showcases that the data meet the multivariate normality condition,
    as shown by the non significant multivariate skewness (b1 =", round(Mardia_Results$b1p,3),
        ",χ² =", round(Mardia_Results$skew,3), ", p =",round(Mardia_Results$p.skew,3), ").",
        "and by the non significant multivariate kurtosis (b2 =", round(Mardia_Results$b2p,3),
        ", z =", round(Mardia_Results$kurtosis,3), ", p =", round(Mardia_Results$p.kurt,3),").
    These results justify the use of Maximum Likelihood extraction method for Exploratory Factor Analysis.")
  }
  
  #KMO Test and Bartlett's test message
  #acceptable KMO and Bartlett's test message
  if(KMO_Test$KMO > .7 & Bartlett_Test$p.value < 0.05) {
    cat("Additionally, Bartlett's Test of Sphericity wielded a significant value (χ² =", 
        round(Bartlett_Test$chisq,3), "p > .001). KMO also wielded an acceptable level (", round(KMO_Test$KMO,3), "). 
  These results mean that our data are suitable for exploratory factor analysis (Field et al., 2012; Howard, 2016).\n \n")
  } else {
    #not acceptable levels of KMO and bartlett's test message
    cat("Bartlett's Test of Sphericity wielded a non significant value (χ² =", 
        round(Bartlett_Test$chisq,3), "p =", round(Bartlett_Test$p.value, 3), ").
    KMO also did not have an acceptable level (", round(KMO_Test$KMO,3), ").")
  }
  
  #Inspect determinant of the correlation matrix for extreme multicollinearity (Field et al., 2012 - full citations in the paper, bibliography section)
  if(det(cor(df_valid)) < 0.00001) { #if exceeding the threshold of .00001, then...
    if(!any(vif_values[vif_values >3] == 0)){ #Inspect VIF values exceeding 3 to reassess
      
      #message in case of no values surpassing 3
            cat("Finally, Extreme Multicollinearity was tested through the calculation of VIF values for all items.
This was due to the fact that the determinant was lower than the suggested .00001 value (Field et al., 2012).
Items showcased no problematic levels of multicollinearity, with no item surpassing the 3 VIF threshold (Kyriazos & Poga, 2023).")
    } else {
      #message in case of any value surpassing 3
      cat("Finally, Multicollinearity was tested through the calculation of VIF values for all items.
      Items showcased problematic levels of multicollinearity, 
      with some items(", vif_values[vif_values > 3] ,") surpassing the 3 VIF threshold.")
    }
  }
}

#running the function on dataset
prerequisites(df_valid) 
#### Factor Analysis ####

#Function for Analysis
factor_analysis_check <- function(df_edcs) {
  while (TRUE){
    #Determining number of Factors through Parallel Analysis
    parallel_analysis <- PARALLEL(df_edcs, n_datasets = 5000) 
    fa_model <- fa(df_edcs, #Dataset
                   #Factors Determined by Parallel Analysis
                   nfactors = parallel_analysis$n_fac_EFA,
                   #Rotation Method - Promax for Correlated Factors (In Theory)
                   #Goretzko et al., 2019; Howard, 2016 - full citations in the paper, bibliography section
                   rotate = "promax", 
                   #Extraction Method - Principal Axis Factoring (non normal data)
                   #Fabrigar et al., 1999; Goretzko et al., 2021; Watkins, 2018 - full citations in the paper, bibliography section
                   fm = "pa", 
                   #Statistical Iterations Principal Axis must go through
                   max.iter = 1000 
    )
    
    #Uniqueness Check - Delete Items, One by One, by highest uniqueness values
    uniqueness <- as.data.frame(fa_model$uniquenesses) |> setNames("uniqueness")
    if (any(uniqueness$uniqueness >= 0.7)) { #Uniqueness > .70
      droplet <- rownames(uniqueness)[which.max(uniqueness$uniqueness)]
      df_edcs <- df_edcs |> 
        select(-all_of(droplet))
      #restard loop until if is not triggered, then proceed
      next 
    } 
    #Loadings Check
    loadings <- as.data.frame(fa_model$loadings[,]) #Convert Loadings to DataFrame
    loadings[abs(loadings) < 0.4] <- NA #Delete loadings below 0.4
    loadings <- loadings[rowSums(!is.na(loadings)) > 0, ] # Keep items with at least one loading above 0.4 (non NA value)
    loadings <- t(apply(loadings, 1, function(row){ 
      if(sum(!is.na(row)) > 1) { #If a row has more than one NA value (cross loadings)
        max_col <- which.max(abs(row)) 
        row[-max_col] <- ifelse(abs(row[-max_col]) < 0.4, NA, row[-max_col]) #Keep lowest ones only if they don't rise above 0.4
      }
      return(row)
    }))
    loadings <- as.data.frame(loadings)
    
    #Drop items with no loading >= 0.4 on any factor
    all_na_items <- rownames(loadings)[rowSums(!is.na(loadings)) == 0]
    if (length(all_na_items) > 0) {
      df_edcs <- df_edcs |> 
        select(-all_of(all_na_items))
      next #restard loop intil the if statement is not triggered, then proceed
    }
    return(fa_model) 
    #Returns the model (fa_model)
  }
}

#### Manual Models Inspection - One by One ####
#First EFA Model
model <- factor_analysis_check(df_valid)

#Inspect model's Loadings
model$loadings

#Inspect Uniqueness Values
model$uniquenesses

#Inspect Fit Indices
model$TLI
model$CFI
model$RMSEA
#message stating the Inspect Fit indices of the model 
cat("First model's Inspect Fit indices are TLI = ", round(model$TLI, 2), "CFI =", round(model$CFI, 2),
"RMSEA = 0.48") 


#Selecting the most appropriate items to continue with second model based on:
#low and cross loadings
#Remove: Q1.1, Q38, Q45. Chose to remove 3 items instead of one due to the high amount of contained items
df_valid2 <- df_valid |> 
  select(Q2.2, Q3.3, Q4.4, Q5.1, Q6.2, Q7.3,Q10.2, Q12, Q14.1, Q16.4, Q17, Q18.2, Q22,
         Q23.2, Q24.3, Q25, Q26.1, Q27.2, Q28, Q30.1, Q31.2, Q33, Q34.1, Q35.2, Q39.2, Q40.3,
         Q41.4, Q46.2, Q47.3)

#Parallel to determine factors
PARALLEL(df_valid2, n_datasets = 5000)

#Second EFA Model
model2 <- fa(df_valid2, 
             nfactors = 7, 
             rotate = "promax", 
             fm = "pa", 
             max.iter = 1000)

#Inspect loadings
model2$loadings

#Inspecting Uniquenesses
model2$uniquenesses

#Inspect Fit indices
model2$TLI
model2$CFI
model2$RMSEA
#message stating the Inspect Fit indices of the model 
cat("Second model's Inspect Fit indices are TLI = ", round(model2$TLI, 2), "CFI =", round(model2$CFI, 2),
    "RMSEA = 0.52") 



#Appropriate Items for the Third Model
#Removed Items based on Cross Loadings and low loadings:
#Q5.1. Remove only 1 item.
df_valid3 <- df_valid2 |> 
  select(Q2.2, Q3.3, Q4.4, Q6.2, Q7.3, Q10.2, Q12, Q14.1, Q16.4, Q17, Q18.2, Q22,
         Q23.2, Q24.3, Q25, Q26.1, Q27.2, Q28, Q31.2, Q30.1, Q31.2, Q33, Q34.1, Q35.2, Q39.2, Q40.3, Q41.4, Q46.2,
         Q47.3)

#Parallel Analysis with the third dataset
PARALLEL(df_valid3, n_datasets = 5000)

#Third EFA Model
model3 <- fa(df_valid3,
             nfactors = 7,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect third model loadings
model3$loadings

#Inspect third model communalities
model3$uniquenesses

#Inspect Fit Indices
model3$CFI
model3$TLI
model3$RMSEA
#Appropriate Items for the Fourth Model
#Items Removed Based on Low Loadings and Cross Loadings
#Additionally, uniqueness is considered
#Removed Items:Q17, Q39.2. Chose two due to both having extremely low quality loadings
df_valid4 <- df_valid3 |> 
  select(!Q17 & !Q39.2)

#Parallel Analysis for the Fourth Model
PARALLEL(df_valid4, n_datasets = 5000)

#Fourth EFA Model
model4 <- fa(df_valid4,
             nfactors = 7,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect Fourth Model Loadings
model4$loadings

#Inspect Fourth Model Uniquenesses
model4$uniquenesses

#Inspect Fourth Model Fit Indices
model4$CFI
model4$RMSEA
model4$TLI

#Selecting appropriate items for the fifth model
#Deleting Items Based on Cross Loadings and Low Loadings
#Also Inspect uniquenesses
#Items Removed: Q33 
df_valid5 <- df_valid4 |> 
  select(!Q33)

#Parallel Analysis for the fifth Model
PARALLEL(df_valid5, n_datasets = 5000)

#Fifth EFA Model
model5 <- fa(df_valid5,
             nfactors = 7,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspec fifth Model Loadings
model5$loadings

#Inspect fifth Model Communalities
model5$uniquenesses

#Inspect fifth model Fit indices
model5$CFI
model5$RMSEA
model5$TLI

#Selecting Appropriate Items for the sixth Model
#Removing Items based on Cross and Low Loadings
#Additionally considering uniquenesses
#Removed Items: Q4.4
df_valid6 <- df_valid5 |> 
  select(!Q4.4)

PARALLEL(df_valid6, n_datasets = 5000)

#Sixth EFA Model
model6 <- fa(df_valid6,
             nfactors = 7,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect Sixth Model Loadings
model6$loadings

#Inspect Sixth Model Uniquenesses
model6$uniquenesses

#Inspect Sixth Model Inspect Fit Indices
model6$CFI
model6$RMSEA
model6$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items:Q30.1. 
df_valid7 <- df_valid6 |> 
  select(!Q30.1)

PARALLEL(df_valid7, n_datasets = 5000)

#Seventh EFA Model
model7 <- fa(df_valid7,
             nfactors = 6,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect Seventh Model Loadings
model7$loadings

#Inspect Seventh Model Uniquenesses
model7$uniquenesses

#Inspect Seventh Model Inspect Fit Indices
model7$CFI
model7$RMSEA
model7$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items:Q26.1. 
df_valid8 <- df_valid7 |> 
  select(!Q26.1)

PARALLEL(df_valid8, n_datasets = 5000)

#Eighth EFA Model
model8 <- fa(df_valid8,
             nfactors = 6,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect Eighth Model Loadings
model8$loadings

#Inspect Eighth Model Uniquenesses
model8$uniquenesses

#Inspect Eighth Model Fit Indices
model8$CFI
model8$RMSEA
model8$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items: Q22, 
df_valid9 <- df_valid8 |> 
  select(!Q22)


PARALLEL(df_valid9, n_datasets = 5000)

#Ninth EFA Model
model9 <- fa(df_valid9,
             nfactors = 6,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect Ninth Model Loadings
model9$loadings

#Inspect Ninth Model Uniquenesses
model9$uniquenesses

#Inspect Ninth Model Inspect Fit Indices
model9$CFI
model9$RMSEA
model9$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items: Q25
df_valid10 <- df_valid9 |> 
  select(!Q25)

PARALLEL(df_valid10, n_datasets = 5000)

#Tenth EFA Model
model10 <- fa(df_valid10,
             nfactors = 4,
             rotate = "promax",
             fm = "pa",
             max.iter = 1000)

#Inspect Tenth Model Loadings
model10$loadings

#Inspect Tenth Model Uniquenesses
model10$uniquenesses

#Inspect Tenth Model Inspect Fit Indices
model10$CFI
model10$RMSEA
model10$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items: Q3.3, q46.2
df_valid11 <- df_valid10 |> 
  select(!Q46.2)

PARALLEL(df_valid11, n_datasets = 5000)

#Eleventh EFA Model
model11 <- fa(df_valid11,
              nfactors = 4,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Eleventh Model Loadings
model11$loadings

#Sixth Model Uniquenesses
model11$uniquenesses

#Inspect Eleventh Model Inspect Fit Indices
model11$CFI
model11$RMSEA
model11$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items: Q12, 
df_valid12 <- df_valid11 |> 
  select(!Q12)

PARALLEL(df_valid12, n_datasets = 5000)

#Twelfth EFA Model
model12 <- fa(df_valid12,
              nfactors = 5,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Twelfth Model Loadings
model12$loadings

#Inspect Twelfth Uniquenesses
model12$uniquenesses

#Inspect Twelfth Model Inspect Fit Indices
model12$CFI
model12$RMSEA
model12$TLI

#Removing Items based on Cross and Low Loadings
#Additionally Considering Uniqueness Values
#Removed Items: Q3.3, 
df_valid13 <- df_valid12 |> 
  select(!Q3.3)

PARALLEL(df_valid13, n_datasets = 5000)

#Thirteenth EFA Model
model13 <- fa(df_valid13,
              nfactors = 4,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Thirteenth Model Loadings
model13$loadings

#Inspect Thirteenth Model Uniquenesses
model13$uniquenesses

#Inspect Thirteenth Model Inspect Fit Indices
model13$CFI
model13$RMSEA
model13$TLI

#Removing Items based on Low Loadings, Theory, and Uniqueness Values
#Removed Items: Q2.2 was removed, not Inspect Fit with its factor, very high uniqueness
#(.78)
df_valid14 <- df_valid13 |> 
  select(!Q2.2)

PARALLEL(df_valid14, n_datasets = 5000)

#Fourteenth EFA Model
model14 <- fa(df_valid14,
              nfactors = 4,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Fourteenth Model Loadings
model14$loadings

#Inspect Fourteenth Uniquenesses
model14$uniquenesses

#Inspect Fourteenth Model Inspect Fit Indices
model14$CFI
model14$RMSEA
model14$TLI

#Removing Items based on Theory and Statistical Principles: No Factor can
#exist with only 2 items.
#Removed Items: Q16.4, Q28
df_valid15 <- df_valid14 |> 
  select(!Q28)

PARALLEL(df_valid15, n_datasets = 5000)

#Fifteenth EFA Model
model15 <- fa(df_valid15,
              nfactors = 4,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Fifteenth Model Loadings
model15$loadings

#Inspect Fifteenth Model Uniquenesses
model15$uniquenesses

#Inspect Fifteenth Model Inspect Fit Indices - #All three are within the acceptable limits - Improved over all previous models
model15$CFI
model15$RMSEA
model15$TLI
model15$PVAL

#Removing Items based on Theory and Statistical Principles: No Factor can
#exist with only 1 items.
#Removed Items: Q16.4
df_valid16 <- df_valid15 |> 
  select(!Q16.4)

PARALLEL(df_valid16, n_datasets = 5000)

#Sixteenth EFA Model
model16 <- fa(df_valid16,
              nfactors = 3,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Sixteenth Model Loadings
model16$loadings

#Inspect Sixteenth Uniquenesses
model16$uniquenesses

#Inspect Sixteenth Model Inspect Fit Indices - Improved compared to last acceptable model_15
model16$CFI
model16$RMSEA
model16$TLI

#Removing Items based on Uniqueness
#Removed Items: Q6.2
df_valid17 <- df_valid16 |> 
  select(!Q6.2)

PARALLEL(df_valid17, n_datasets = 5000)

#Seventeenth EFA Model
model17 <- fa(df_valid17,
              nfactors = 3,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Sevebteenth Model Loadings
model17$loadings

#Inspect Seventeenth Model Uniquenesses
model17$uniquenesses

#Inspect Seventeenth Model Inspect Fit Indices - Very High Levels of All 3 Inspect Fit indices
model17$CFI
model17$RMSEA
model17$TLI

#Removing Items based on Uniqueness
#Removed Items: Q27.2
df_valid18 <- df_valid17 |> 
  select(!Q27.2)

parallel_figure <- PARALLEL(df_valid18, n_datasets = 5000)

#Eighteenth EFA Model
model18 <- fa(df_valid18,
              nfactors = 3,
              rotate = "promax",
              fm = "pa",
              max.iter = 1000)

#Inspect Eighteenth Model Loadings
model18$loadings

#Inspect Eigtheenth Model Uniquenesses
model18$uniquenesses

#Inspect Eighteenth Model Inspect Fit Indices
model18$CFI
model18$RMSEA
model18$TLI
model18$loadings
model18$fit

#Report of Eighteenth model's metrics 
cat("Model 18 is a highly significant improvement over all previous models tested.
A three factor model is drived from the Dataset, by executing multiple iterations of 
factor analysis, using Principal Axis Factoring and Promax Rotation. \n
Specifically, the Inspect Fit indices of model 18 are the following:
1) CFI = ", round(model18$CFI, 3),
"\n2) TLI =", round(model18$TLI, 3),
"\n3) RMSEA =", round(model18$RMSEA, 3),
"\n4) Inspect Fit =", round(model18$Fit.off, 3),
"\nThe model is significant with p > 0.001.",
"\nIn all instances of new number of factors evaluation, in each model iteration,
parallel analysis was used, with a bootstrap = 5000.")

# Inter-Item Correlations for Each Factor
f1_items <- c("Q10.2", "Q18.2", "Q23.2", "Q31.2", "Q35.2")
f2_items <- c("Q7.3", "Q24.3", "Q40.3", "Q47.3")
f3_items <- c("Q41.4", "Q34.1", "Q14.1")

#Calculate correlations for each separate factor
cor_f1 <- round(cor(df_valid18[,f1_items]),2)
cor_f2 <- round(cor(df_valid18[,f2_items]),2)
cor_f3 <- round(cor(df_valid18[,f3_items]),2)

#combine all correlations into a correlation matrix called mean_cor
mean_cor <- function(cor_matrix){
  vals <- cor_matrix[lower.tri(cor_matrix)] #keep only the lower side
  round(mean(vals),3) #round values
}

#save each factor's correlations into separate variables
inter_cor_1 <- mean_cor(cor_f1)
inter_cor_2 <- mean_cor(cor_f2)
inter_cor_3 <- mean_cor(cor_f3)

#Report for correlations into each factor
cat("The Inter-ite correlations for each factor are the following:
1) Factor 1:", round(inter_cor_1,2),
"\n2) Factor 2:", round(inter_cor_2,2),
"\n3) Factor 3:", round(inter_cor_3,2))

#Reliability - Omega calculation
omega_scale <- psych::omega(df_valid18, nfactors = 3, fm = "pa",rotate = "promax") #set the model
omega_scale$omega_h #report of omega for general variable's explanation of variance
omega_scale$omega.group #report of general omega (whole scale's reliability)



#### Visualization ####

#Matrix for the Factor Loadings
loadings_matrix <- model18$loadings
loadings_df <- as.data.frame(unclass(loadings_matrix))
loadings_df$Item <- rownames(loadings_df)



#Reshape to Long Format
loadings_long <- loadings_df |> 
  pivot_longer(
    cols = starts_with("PA"), #Select the Columns (PA1, PA2, PA3)
    names_to = "Factor", #Factor Names in one column
    values_to = "Loading") |> #Loadings in second column
  filter(abs(Loading) >= 0.3) |>  #Cutoff Value
  mutate(Loading = round(Loading, 2)) |> #Rounding to Two Digits
  arrange(Factor, Item)
loadings_long$Factor <- factor(loadings_long$Factor,
                               levels = c("PA1", "PA2", "PA3"),
                               labels = c("Factor 1", "Factor 2", "Factor3"))


# Figure Plotting
loadings_figure<- ggplot(loadings_long, aes(x = Loading, y = Item)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 1) +
  geom_vline(xintercept = 0.3, colour = "grey65", linewidth = 0.3, linetype = "dashed") +
  
  # Bars - plain grey
  geom_col(fill = "grey40", colour = "grey30", linewidth = 0.25, width = 0.65) +
  
  # Bar Labels
  geom_text(aes(label = Loading,
                hjust = ifelse(Loading >= 0, -0.15, 1.15)),
            size = 4, colour = "grey20", fontface = "plain") +
  
  # Scales
  scale_x_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  
  # Facet
  facet_wrap(~ Factor, ncol = 3, scales = "free_y") +
  
  # Labels
  labs(
    title = "Factor Loadings - Emotional Dependency on AI Chatbots Scale (EDCS)",
    subtitle = "Exploratory Factor Analysis - Principal Axis Factoring - Promax Rotation - \nNumber of Factors through Parallel Analysis",
    x = "Interitem Correlations: \n Factor 1 = .42; Factor 2 = .42; Factor 3 = .42",
    y = NULL,
    caption = "Note. N = 268. \nOmega Total = 0.84. \nOmega for: Factor 1 = 0.79, Factor 2 = 0.74, Factor 3 = 0.71"
  ) +
  
  # Theme
  theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = "grey35", hjust = 0, margin = margin(b = 12)),
    plot.caption = element_text(size = 8, colour = "grey40", hjust = 0, margin = margin(t = 10)),
    axis.title.x = element_text(size = 10, margin = margin(t = 8)),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.ticks = element_line(colour = "grey70", linewidth = 0.3),
    strip.text = element_text(size = 11, face = "bold", colour = "white"),
    strip.background = element_rect(fill = "grey25", colour = NA),
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(14, 16, 10, 14)
  )

loadings_figure

#### Factor Loadings Matrix ####

loadings_table <- model18 |> 
  apa_loadings() |> #apply apa styled loadings
  mutate(h2 = formatC(model18$communality, digits = 2, format = "f")) |>  #create h2 column for communalities
  rename( #rename columns into readable labels according to theory
    "Active Nurture of the Chatbot" = PA1,
    "Relational Anthropomorhism" = PA2,
    "Dysfunctional Attachment to the Chatbot" = PA3) |> 
  apa_flextable(no_format_columns = Variable) |> 
  add_footer_lines( #add note at the end of the table
                "Note.Principal Axis Extraction was used. \n
                N = 260. \n
                Promax Rotation Method \n
                Parallel Analysis for Factor Number Extraction.") |> 
  compose(part = "footer", i = 1, j = 1,
          value = as_paragraph( 
            as_chunk("Note. ", props = fp_text(italic = TRUE)),
            as_chunk("N = 268. Principal Axis Extraction with Promax Rotation were used. Parallel Analysis estimated the number of factors.")))

read_docx() |> #create word file
  body_add_flextable(loadings_table) |>  #pass the loadings table
  print(target = "factor_loadings_communalities.docx") #save with a specific name for the file

model18$Phi

#### Correlations Matrix Table ####
factor_correlations <- as.data.frame(round(model18$Phi,2)) #factor correlations into a dataframe

#APA 7 Table with nice_table
factor_correlations <- cbind(Factor = rownames(factor_correlations), factor_correlations)
factor_correlations <- nice_table(factor_correlations, note = "Promax Rotation.") #add apa 7 note
factor_correlations 

read_docx() |> 
  body_add_flextable(factor_correlations) |> 
  print(target = "factor_correlations.docx")

#### Final Items Descriptives Table ####
descriptives_final <- describe_data(df_valid18)

read_docx() |> #create word file
  body_add_flextable(descriptives_final$descriptives_table) |> #add the descriptives table of the final items into it
  print(target = "Descriptives_Final_Items.docx") #save with its corresponding name

#### Initial Items Descriptives Table ####
read_docx() |> #create word file
  body_add_flextable(descriptives$descriptives_table) |> #Add the descriptives table of the initial full scale items into it
  print(target = "Descriptives_Initial_Items.docx") #save with its corresponding name


#### Structure Matrix Table ####
structure_loadings <- as.data.frame(unclass(model18$Structure)) 
structure_loadings_table <- nice_table(structure_loadings, note = "Structure Matrix is similar to Pattern Matrix.") #add note and format into a table
read_docx() |> #create word file
  body_add_flextable(structure_loadings_table) |> #add the structure matrix table into it
  print(target = "structure_loadings.docx") #save it with a corresponding name


structure_table <- as.data.frame(unclass(model18$Structure)) |> #structure matrix into a dataframe variable
  rownames_to_column("Variable") |> #variable into a column
  mutate(across(where(is.numeric), ~ formatC(.x, digits = 2, format = "f"))) |> #set digits to 2 for all loadings
  rename( #rename factors into relevant names according to theory
    "Active Nurture of the Chatbot" = PA1,
    "Relational Anthropomorphism" = PA2,
    "Dysfunctional Attachment to the Chatbot" = PA3) |>
  flextable() |> #set in flextable format
  theme_apa() |> #set apa themed style
  add_footer_lines("") |> #add note in the end
  compose(part = "footer", i = 1, j = 1,
          value = as_paragraph(
            as_chunk("Note. ", props = fp_text(italic = TRUE)),
            as_chunk("N = 268. Principal Axis Extraction with Promax Rotation were used. Structure matrix shown."))) #add relevant information in the note section

read_docx() |> #create word file
  body_add_flextable(structure_table) |> #add into it the structure matrix table
  print(target = "structure_matrix.docx") #save the word file with a corresponding name
  
