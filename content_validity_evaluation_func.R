#### Converting the CVI and AIKEN'S V calculations into a Pipeline ####
content_validity_evaluation <- function(data, n_experts, lo = 1, hi = 4, z_val = 1.96){
  
  # 1. Input Validation: Ensure data is numeric
  if(!all(sapply(data, is.numeric))){
    stop("Error: Dataframe contains non-numeric columns. Please pass only the item columns.")
  }
  
  # 2. Calculate k automatically to prevent human error
  k_range <- hi - lo
  
  # 3. Analysis
  results_table <- data |> 
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Rating") |> 
    group_by(Item) |> 
    summarise(
      #CVI (Lynn, 1986)
      Agreements = sum(Rating >= 3),
      I_CVI = Agreements / n_experts,
      
      #Aiken's V (Aiken 1980)
      S_sum = sum(Rating-lo),
      Aikens_V = S_sum / (n_experts * k_range)
    ) |> 
    
    mutate(
      #Modified Kappa (Polit et al., 2007)
      Pc  = choose(n_experts, Agreements) * (0.5^n_experts),
      Kappa = (I_CVI - Pc) / (1-Pc),
      Kappa_Eval = case_when(
        Kappa > 0.74 ~ "Excellent",
        Kappa >= 0.60 ~ "Good",
        Kappa >= 0.40 ~ "Fair",
        TRUE ~ "Poor"
      ),
      #Confidence Intervals (Penfield & Giacobbi, 2009)
      A = (2 * n_experts * k_range * Aikens_V) + (z_val^2),
      B = z_val * sqrt((4 * n_experts * k_range * Aikens_V * (1 - Aikens_V)) + z_val^2),
      C = 2 * ((n_experts * k_range) + z_val^2),
      
      Lower_CI = (A-B)/C,
      Upper_CI = (A+B)/C,
      
      #Decisions
      Decision_CVI = ifelse(I_CVI >= 0.78, "KEEP", "REJECT"), #Lynn (1986)
      Decision_V_CI = ifelse(Lower_CI >= 0.7, "KEEP", "REJECT"), #Penfield & Giacobbi (2004)
      Decision_V = ifelse(Aikens_V >= 0.81, "KEEP", "REJECT")
      ) |> 
    
    #Final Formatting
    mutate(across(c(I_CVI, Aikens_V, Lower_CI, Upper_CI, Kappa), \(x) round(x, 3))) |> 
    select(Item, I_CVI, Decision_CVI, Aikens_V, Lower_CI, Upper_CI, Decision_V, Decision_V_CI, Kappa, Kappa_Eval)
  return(results_table)
}

#### ------------------------------------------------ ####
# Usage: 
# source("content_validity_evaluation_func.R")
#### ------------------------------------------------ ####