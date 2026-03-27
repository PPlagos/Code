#### ---------- Emotional Dependency on AI Chatbots Scale (EDCS) ---------- ####
#### ---------- MSc Cyberpsychology Thesis ---------- ####
#### ---------- Panagiotis Plagos ---------- ####
#### ---------- Content Validity Evaluation for EDCS ---------- ####

#### Load Dependencies ####
#install.packages("tidyverse")
#install.packages("rcompanion")
#install.packages("psych")
#install.packages("forcats")
library(tidyverse)
library(rcompanion)
library(psych)
library(forcats)

#### Loading Data from the Questionnaire
getwd()
setwd("D:/ΠΑΝΑΓΙΩΤΗΣ ΠΛΑΓΟΣ/ΚΥΒΕΡΝΟΨΥΧΟΛΟΓΙΑ ΕΚΠΑ/ΔΙΠΛΩΜΑΤΙΚΗ/code/Content Validity/")
df <- read.csv("content_validity_final.csv")
head(df)

items_df <- df |> 
  select(starts_with("Q")) #select only the items from the dataset

#import function for CVI, Aiken's V, and Kappa Calculation
source("content_validity_evaluation_func.R")

#### Running Function to get a dataframe with the results ####
results <- content_validity_evaluation(items_df, n_experts = 9, z_val = 1.645)
# Specifying the number of experts = 9
# Specifying the confidence interval for Aiken's V, z value  = 1.645 for 90%
# 90% Confidence Interval for  9 Judges was suggested by Penfield & Giacobbi, 2004
#Full citations present in the paper

results #inspect results of the function

#### Plot to Scan CVI Values (Lynn, 1986; Polit & Beck, 2006; Polit et al., 2007 - Full citations present in the paper) ####
CVI_graph <- results |> 
  ggplot2::ggplot(aes(x = Item, y = I_CVI)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_hline(yintercept = 0.78, linetype = "dashed", color = "red") +
  coord_flip() +
  facet_wrap(~Decision_CVI, scales = "free_y") +
  labs(title = "I-CVI Values for each Item",
       subtitle = "Left, Acceptable Items, Right, Non-Acceptable Items",
       x = "Items",
       y = "CVI Values",
       caption = "S-CVI = 0.96, 0.97 without Non-Acceptable Items") +
  theme_minimal()
CVI_graph #Inspect CVI Graph

#### Plot to inspect V Values (Aiken, 1980, 1985 - Full citations present in the paper) ####

V_graph <- results |> 
  ggplot2::ggplot(aes(x = Item, y = Aikens_V)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_hline(yintercept = 0.81, linetype = "dashed", color = "red") +
  coord_flip() +
  facet_wrap(~Decision_V, scales = "free_y") +
  labs(title = "Aiken's V Evaluation",
       subtitle = "Left, Acceptable Items, Right, Non-Acceptable Items",
       x = "Items",
       y = "Aiken's V Levels") +
  theme_minimal()
V_graph

#### Plot to Inspect Confidence Interval V Values (Penfield & Giacobbi, 2004) ####
V_graph_CI <- results |> 
  ggplot2::ggplot(aes(x = Item, y = Lower_CI)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") + 
#Acceptance Threshold of 0.7 on lower Confidence Interval (Penfield & Giacobbi, 2004)
  coord_flip() +
  facet_wrap(~Decision_V_CI, scales = "free_y") +
  labs(title = "Confidence Interval Aiken's V Evaluation",
       subtitle = "Left, Acceptable Items, Right, Non-Acceptable Items",
       x = "Items",
       y = "Lower Confidence Interval Levels") +
  theme_minimal()
V_graph_CI

#### Overseeing Rejected Items ####

#Possible Rejections
Items_Reject_Possible <- results |> 
  filter(Decision_CVI == "REJECT" | Decision_V == "REJECT" | Decision_V_CI == "REJECT")

Items_Reject_Possible

#Items Rejected by All Three Metrics
Items_Reject_Definite <- results |> 
  filter(Decision_CVI == "REJECT" & Decision_V == "REJECT" & Decision_V_CI == "REJECT")

Items_Reject_Definite

#### Full Scale CVI Calculation - Excluding rejected item Q11 (Lynn, 1986; Polit & Beck, 2006; Polit et al., 2007 - Full citations present in the paper) ####
s_cvi <- sum(results$I_CVI) / 48 #full scale cvi calculated
round(s_cvi, 2) #round full scale cvi to two digits
s_cvi_accepted_only <- results |> #only accepted items full scale cvi calculated
  filter(!Item %in% c("Q11"))
s_cvi_accepted_only <- round(sum(s_cvi_accepted_only$I_CVI) / 47, 2) #round it to two digits
s_cvi_accepted_only #inspect accepted items-full scale cvi

#### Full Scale Weighted Kappa Evaluation - No Item Q11 (Polit et al., 2007 - Full citation present in the paper) ####
Average_Kappa = mean(results$Kappa)
Average_Kappa #average kappa
results_no_q11 <- results |> filter(Item != "Q11") |> 
  summarise(Kappa_Excellent_Count = sum(Kappa_Eval == "Excellent"),
            Percentage_Excellent = (Kappa_Excellent_Count = Kappa_Excellent_Count / 47) * 100)
results_no_q11 #weigthed kappa values for all items except the reject one (q11)


#### APA 7 CVI Plot ####
CVI_plot <- results |> 
  mutate(Item = fct_reorder(Item, I_CVI)) |> 
  ggplot(aes(x = Item, y = I_CVI, fill  = Decision_CVI)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", I_CVI)), hjust = -0.2, size = 3)+
  coord_flip() +
  geom_hline(yintercept = 0.78, linetype = "dashed", colour = "black", linewidth = 0.7) +
  scale_fill_manual(
    name = "Criterion of Choice",
    values = c("KEEP" = "#0072B2", "REJECT" = "#E69F00"),
    labels = c("KEEP" = "Αποδοχή (Ι-CVI > 0.78)", "REJECT" = "Απόρριψη (I-CVI =< 0.78)")
  ) +
  labs(
    x = "Items",
    y = "Interitem Content Validity Index (I-CVI)"
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.15)))
CVI_plot #inspect cvi plot

#### APA 7 Aiken's V Plot ####
V_plot <- results |> 
  mutate(Item = fct_reorder(Item, Aikens_V)) |> 
  ggplot(aes(x = Item, y = Aikens_V, fill = Decision_V)) +
  geom_col(width = 0.6)+
  geom_text(aes(label = sprintf("%.2f", results$Aikens_V)), hjust = -0.2, size = 3) +
  coord_flip() +
  geom_hline(yintercept = 0.81, linetype = "dashed", colour = "black", linewidth = 0.7) +
  scale_fill_manual(
    name = "Criterion of Choice",
    values = c("KEEP" = "#0072B2", "REJECT" = "#E69F00"),
    labels = c("KEEP" = "Αποδοχή (V > 0.81", "REJECT" = "Απόρριψη (V =< 0.81)")
  ) +
  labs(
    x = "Items",
    y = "Aiken's V Index"
  ) +
  theme_classic(base_size = 11, base_family = "sans") + 
  theme(
    axis.text.y = element_text(size = 8),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.15)))
V_plot #inspect Aiken's V plot

#### APA 7 Aiken's V Lower Confidence Intervals Plot ####
V_CI_plot <- results |> 
  mutate(Item = fct_reorder(Item, Aikens_V)) |> 
  ggplot(aes(x = Item,y = Lower_CI, fill = Decision_V_CI)) +
  geom_col(width = 0.6) + 
  geom_text(aes(label = sprintf("%.2f", results$Lower_CI)), hjust = -0.97, size = 3) +
  coord_flip() +
  geom_hline(yintercept = 0.7, linetype = "dashed", colour = "black", linewidth = 0.7) +
  scale_fill_manual(
    name= "Criterion of Choice",
    values = c("KEEP" = "#0072B2", "REJECT" = "#E69F00"),
    labels = c("KEEP" = "Αποδοχή (Lower Confidence Interval > 0.7", "REJECT" = "Απόρριψη (Lower Confidence Interval =< 0.7")
  ) +
  labs(
    x = "Items",
    y = "Aiken's V Lower Confidence Interval"
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.15)))
V_CI_plot # Inspect Aiken's V Lower Confidence Intervals plot


#### APA 7 Weighted Kappa Evaluation Plot ####
Kappa_plot <- results |> 
  mutate(Item = fct_reorder(Item, Kappa)) |> 
  ggplot(aes(x = Item, y = Kappa, fill = Kappa_Eval)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", results$Kappa)), hjust = -0.97, size = 3) +
  coord_flip() +
  geom_hline(yintercept = 0.74, linetype = "dashed", colour = "black", linewidth = 0.7) +
  scale_fill_manual(
    name = "Criterion of Choice",
    values = c("Excellent" = "#0072B2"),
    labels = c("Excellent" = "Εξαιρετικά Επίπεδα Kappa Evaluation (Τυχαία Συμφωνία Ειδικών)" )
  ) +
  labs(
    x = "Items",
    y = "Weighted Kappa Scores"
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.15)))
Kappa_plot #Inspect Weighted Kappa Evaluation Plot
