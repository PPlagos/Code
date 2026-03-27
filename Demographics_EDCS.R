#### ---------- Emotional Dependency on AI Chatbots Scale (EDCS) ---------- ####
#### ---------- MSc Cyberpsychology Thesis ---------- ####
#### ---------- Panagiotis Plagos ---------- ####
#### ---------- Analysis of Demographics and Chatbot Use Traits ---------- ####

#### Loading Dependencies ####

#Install the packages if not already 
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("rempsyc")
#install.packages("flextable")
#install.packages("officer")

#Load packages
library(tidyverse)
library(readxl)
library(rempsyc)
library(flextable)
library(officer)

#### Load Demographics Dataset ####
demographics <- read_excel("Professional/ΚΥΒΕΡΝΟΨΥΧΟΛΟΓΙΑ ΕΚΠΑ/ΔΙΠΛΩΜΑΤΙΚΗ/code/EFA/Book1.xlsx")

####Inspect Dataset ####
View(demographics)


#### Prefered Chatbot Model Table ####

target_column <- "Ποια chatbot χρησιμοποιείς κυρίως;" #set the target column

chatbot_freq <- demographics |> 
  select(target_column) |> #select to work with our column of interest
  
  drop_na() |> #Drop Invalid Answers if present - not present here
  
  #Participants were able to choose multiple values, and they are separated by commas, therefore...
  separate_rows(all_of(target_column), sep = ",") |> #seperate values with comma
  
  mutate(!!sym(target_column) := str_trim(!!sym(target_column))) |> #Remove various spaces before and after values
  
  mutate(!!sym(target_column) := str_to_title(!!sym(target_column))) |> #Set all values to capitalized first letter
  
  mutate(target_column  = if_else(str_detect(target_column, "Copilot"), "Copilot", target_column)) |>  #Copilot and Microsoft Copilot set as the same value
  
  count(!!sym(target_column), name = "Frequency") |> #Count the frequency
  
  arrange(desc(Frequency)) #Set in in decreasing order 
chatbot_freq <- chatbot_freq[c(-8),]

chatbot_prop <- prop.table(chatbot_freq$Frequency) #Calculate proportions of each value based on their frequencies

#Pass the proportions into a table with frequencies
chatbot_freq <- chatbot_freq |> 
  mutate(Percentage = round(chatbot_prop * 100, 2)) #Round the values to 2 digits to have it in reportable format 

colnames(chatbot_freq) <- c("Preferred Chatbot", "Frequency", "Percentage (%)") #Set the Column names

#Create an APA 7 style table 
chatbot_freq_table <- chatbot_freq |>
  nice_table(note = "N = 298. ",spacing = 2) |> #Note of sample, with spacing
  theme_apa() #Set apa theme.

#### Communication Method Table ####

#Follows the same logic as the one to build the preferred chatbot table.

comm_freq <- demographics |> 
  select(`Ποιος είναι ο κύριος τρόπος που αλληλεπιδράς και επικοινωνείς με τα chatbot;`) |> 
  drop_na() |>
  count(`Ποιος είναι ο κύριος τρόπος που αλληλεπιδράς και επικοινωνείς με τα chatbot;`, name = "Frequency") |> 
  arrange(desc(Frequency))
comm_prop <- prop.table(comm_freq$Frequency)
comm_freq <- comm_freq |> 
  mutate(Percentage = round(comm_prop * 100, 2))
colnames(comm_freq) <- c("Way of Communication", "Frequency", "Percentage (%)")
comm_table <- comm_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> 
  theme_apa()

#### Reason of Use ####

#Set the possible responses into a vector
responses <- c(
  "Παραγωγικότητα",
  "Δημιουργικότητα",
  "Διασκέδαση και Παιχνίδι",
  "Κοινωνικοί Λόγοι",
  "Συναισθηματική Επικοινωνία",
  "Γνώση, Εκπαίδευση και Υποστήριξη στη Μελέτη",
  "Αναζήτηση Πληροφοριών"
)

#
df_reason <- demographics |> 
  mutate(id = row_number()) |> #Create a numeric column named "ID"
  bind_cols(
    #Creates a single column for each possible response as defined by the responses vector.
    map_dfc(responses, ~ tibble(!!.x := as.integer(str_detect(
      demographics$`Ποιοι είναι οι κύριοι λόγοι που χρησιμοποιείς τα chatbot;`, fixed(.x)))))
    #Adds 1 on each column for each corresponding value appearing under the target column, and 0 for non appearing values (dummy variables)
  ) #With bind_cols all created columns are attached to the far right of the dataframe
    #"fixed()" ensures that its a string column

reason_freq <- df_reason |>
  
  select(all_of(responses)) |> #Work only with the 0/1 columns
  
  summarise(across(everything(), sum)) |> #Sum per column (therefore adds up all the 1 values)
  
  pivot_longer(everything(), names_to = "Reason", values_to = "n") |> #shifts observations from rows to columns 
  
  arrange(desc(n)) #Descending order

reason_prop <- prop.table(reason_freq$n) #Sets up Proportions based on frequencies

reason_freq <- reason_freq |> 
  mutate(Percentage = round(reason_prop * 100, 2)) #Adds percentage column based on the created proportions

colnames(reason_freq) <- c("Purpose of Communication", "Frequency", "Percentage (%)") #Sets the names of the columns for the dataframe

reason_table <- reason_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> #Dataframe is now a table, with note and spacing according to APA rules
  theme_apa() #APA theme

### Hours of use ####
col_name <- names(demographics)[1]  #Selects the relevant use time variable from the dataset 

#Set up a function to parse the hours since the used values are vary and are in different forms
parse_hours <- function(raw) {
  x <- tolower(trimws(as.character(raw))) #all to low characters and with no white spaces, 
  x <- gsub(",", ".", x)       # "0,5" → "0.5" 
  x <- gsub("'", "", x)        # "10'" → "10"  (cuts apostrophe)
  x <- trimws(x) #final no white space cleanin, to ensure non remain after the two previous changes
  
  # Sets varying inconclusive and vague responses (e.g. "δεν χρησιμοποιώ καθημερινά") to NA values
  if (grepl(
    "δεν.*(καθημερ|χρησιμοποι)|οχι καθημ|σχεδόν μηδ|1 φορ.*εβδ|
     λιγα.*οχι|μιση.*οχι|δεν συμπλ|αν χρει|ανάλογα.*δουλ|
     εβδομαδ|αν εινσι|μπωρει|^λεπτά$|σχεδόν", x)) return(NA_real_)
  
  # Convert to NA any other values that the excel might read as Dates - This was also taken care of inside excel
  if (grepl("^20[0-9]{2}-", x)) return(NA_real_)
  
  # Converts values that excel might have formatted as time to usable values
  if (grepl("^\\d{1,2}:\\d{2}", x)) {
    parts <- as.numeric(strsplit(x, ":")[[1]])
    return(parts[1] + parts[2] / 60)
  }
  
  # Handles values with "0h05"-like format that are present in the dataset
  if (grepl("^0h(\\d+)", x)) { #detect the pattern
    m <- as.numeric(regmatches(x, regexpr("(?<=0h)\\d+", x, perl = TRUE))) #keep only the digits after 0h
    return(m / 60) #convert the digits to decimal hours
  }
  
  # Handles responses like "10-15 λεπτα"
  if (grepl("^\\d+-\\d+ ?(λεπτ|lepta|λ |$)", x)) { #detect the pattern, "?" makes space before the label optional
    nums <- as.numeric(regmatches(x, gregexpr("\\d+", x))[[1]]) #Collect the digits and convert to numeric
    return(mean(nums) / 60) #select the midpoint (10-15 = 12.5) and turn to hours (12.5 = 0.208 of the hour)
  }
  
  # Handles responses like "10 λεπτά"
  if (grepl("λεπτ|lepta|\\d+ ?λ($| )", x)) { 
    nums <- as.numeric(regmatches(x, gregexpr("[0-9]+\\.?[0-9]*", x))[[1]]) #Scan the values
    nums <- nums[!is.na(nums) & nums < 120]   #exclude outliers (no one would write 120 minutes, for example)
    if (length(nums) == 0) return(NA_real_) #Turn empty values into NAs
    return(mean(nums) / 60) #turn minutes into hour values
  }
  
  #Responses like "λίγα λεπτά" are converted to 0.04 hours (2.5 minutes)
  if (grepl("λιγ.*λεπτ|λεπτ.*καθ|^- ?\\d+ λεπ", x)) return(0.04)
  
  #Responses like "μισή ώρα" converted to half an hour (0.5)
  if (grepl("μισ.*ώρα|μισ.*ωρα|μισή ωρα|half", x)) return(0.5)
  
  #Responses like "1/4" converted to 0.25 hours (15 minutes)
  if (grepl("1/4", x)) return(0.25)
  
  #Range of hours: "0-1", "0-2", "0.5-1", "1-1.30", "1-2h", "1-2 ώρες"
  if (grepl("^0 ?[-–] ?1$|^0[-–]1$", x)) return(0.5) #0-1 converted to 0.5 hours (30 minutes)
  
  if (grepl("^0 ?[-–] ?2$", x)) return(1.0) #0-2 converted to 1 hour
  
  if (grepl("0\\.5 ?[-–] ?1", x)) return(0.75) #0.5-1 converted to 0.75 (45 minutes)
  
  if (grepl("1[-–]1\\.?3|1[-–]2", x)) return(1.5) #1-1.30, 1-2, converted to 1.5 hours 
  
  if (grepl("από καθ.*μισ|^0.*μισ", x)) return(0.25) #από καθημερινά μισή, 0 και μισή, converted to 0.25 hours (15 minutes)
  
  #Values like "λιγότερο από μία ώρα" and "<1 ώρα" converted to 0.5 hours (30 minutes)
  if (grepl("< ?1 ?(ώρα|ωρα)?|λιγ.*από.*1|λιγότ.*1", x)) return(0.5)
  if (grepl("^0\\.5", x)) return(0.5)
  
  #Values like "1 ώρα" converted to purely numeric
  if (grepl("μια ?ώρα|μια ?ωρα", x)) return(1) #"μία ώρα" converted to 1
  
  if (grepl("[0-9] ?(ώρα|ωρα|ώρες|ωρες)", x)) { 
    n <- suppressWarnings(as.numeric(regmatches(x, regexpr("[0-9]+\\.?[0-9]*", x)))) # number + ώρα converted to keep only the number
    
    return(ifelse(is.na(n), NA_real_, n)) #if value is a number keep it or else return NA
  }
  
  #Values such as "20 λεπτά" convert to 0.33 hours (20 minutes)
  if (grepl("20 λεπτ|20 λεπ", x)) return(20 / 60)
  
  n <- suppressWarnings(as.numeric(x)) #Convert the remaining numbers to numerics 
  if (!is.na(n)) { #If the number is not NA, then...
    if (n > 24) return(NA_real_)   # Convert to NA if higher than 24
    return(n) #return the number
  }
  
  return(NA_real_) #If nothing else matches and its not a plain number, return NA as the final resort
}

demographics <- demographics |> #take our dataset 
  mutate(
    usage_hours = sapply(.data[[col_name]], parse_hours), #apply the previously defined function to the relevant time column
    
    usage_cat = case_when( 
      is.na(usage_hours)        ~ "Αδιόριστο / Μη καθημερινό", #If NA values then place in the category "Αδιόριστο / Μη καθημερινό"
      usage_hours == 0          ~ "Καθόλου (0 ώρες)", #if equals 0 then place into "Καθόλου (0 ώρεςς)"
      usage_hours <  0.25       ~ "< 15 λεπτά", #if less than 0.25 of hour place into "< 15 λεπτά"
      usage_hours <  0.5        ~ "15–30 λεπτά", #if less than half an hour place into "15 - 30 λεπτά"
      usage_hours <= 1          ~ "30 λεπτά – 1 ώρα", #if less than one hour place into "30 λεπτά - 1 ώρα"
      usage_hours <= 2          ~ "1–2 ώρες", #If equals or bigger than one hour place into "1-2 ώρες" 
      TRUE                      ~ "> 2 ώρες" #If bigger than two hours place into "> 2 ώρες"
    ),
    #Different groups into usage_cat
    usage_cat = factor(usage_cat, levels = c(
      "Καθόλου (0 ώρες)", "< 15 λεπτά", "15–30 λεπτά",
      "30 λεπτά – 1 ώρα", "1–2 ώρες", "> 2 ώρες",
      "Αδιόριστο / Μη καθημερινό"
    ))
  )

# Create frequency of time of use
freq_table <- demographics |>
  #Count the appears frequency of the different groups' values
  count(usage_cat, .drop = FALSE) |> 
  #Create the percentage column from the sum of appearances
  mutate(pct = round(n / sum(n) * 100, 1)) |> 
  #Place into descending order
  arrange(desc(pct))
  #Define the names of the table's columns
colnames(freq_table) <- c("Time of Communication", "Frequency", "Percentage (%)")

#Create the table, apa style
freq_table <- freq_table |> nice_table(note = "N = 298") |>  theme_apa()

#### Gender Table ####

#Follows the same logic as the previous table development (e.g., preferred chatbot table in the beginning)
gender_freq <- demographics |> 
  select(`Φύλο`) |> 
  drop_na() |> 
  count(`Φύλο`, name = "Frequency") |> 
  arrange(desc(Frequency))
gender_freq
gender_prop <- prop.table(gender_freq$Frequency)
gender_freq
gender_freq <- gender_freq |> 
  mutate(Percentage = round(gender_prop * 100, 2))
colnames(gender_freq) <- c("Gender", "Frequency", "Percentage (%)")
gender_table <- gender_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> 
  theme_apa()

#### Education Status Table ####

#Follows the same logic as the previous table development (e.g., preferred chatbot table in the beginning)

edu_freq <- demographics |> 
  select(`Εκπαιδευτικό Επίπεδο`) |> 
  drop_na() |> 
  count(`Εκπαιδευτικό Επίπεδο`, name = "Frequency") |> 
  arrange(desc(Frequency))
edu_prop <- prop.table(edu_freq$Frequency)
edu_freq <- edu_freq |> 
  mutate(Percentage = round(edu_prop * 100, 2))
colnames(edu_freq) <- c("Education Level", "Frequency", "Percentage (%)")
edu_table <- edu_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> 
  theme_apa()


#### Work Status Table ####

#Follows the same logic as the previous table development (e.g., preferred chatbot table in the beginning)

job_freq <- demographics |> 
  select(`Επαγγελματική Κατάσταση`) |> 
  drop_na() |> 
  count(`Επαγγελματική Κατάσταση`, name = "Frequency") |> 
  arrange(desc(Frequency))
job_prop <- prop.table(job_freq$Frequency)
job_freq <- job_freq |> 
  mutate(Percentage = round(job_prop * 100, 2))
colnames(job_freq) <- c("Work Status", "Frequency", "Percentage (%)")
job_table <- job_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> 
  theme_apa()

#### Relationship Status Table ####

#Follows the same logic as the previous table development (e.g., preferred chatbot table in the beginning)

relationship_freq <- demographics |> 
  select(`Σχεσιακή Κατάσταση`) |> 
  drop_na() |> 
  count(`Σχεσιακή Κατάσταση`, name = "Frequency") |> 
  arrange(desc(Frequency))
relationship_prop <- prop.table(relationship_freq$Frequency)
relationship_freq <- relationship_freq |> 
  mutate(Percentage = round(relationship_prop * 100, 2))
colnames(relationship_freq) <- c("Relationship", "Frequency", "Percentage (%)")
relationship_table <- relationship_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> 
  theme_apa()

#### Age Table ####

#Follows the same logic as the previous table development (e.g., preferred chatbot table in the beginning)

age_group_df <- demographics |> 
  mutate(age_group = 
           cut(`Ηλικία`,
               breaks = c(18,25,30,35,40,45,50,55,60,65),
               labels = c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-65"),
               right = FALSE))

age_freq <- age_group_df |> 
  select(age_group) |> 
  drop_na() |> 
  count(age_group, name = "Frequency") |> 
  arrange(desc(Frequency))
age_prop <- prop.table(age_freq$Frequency)
age_freq <- age_freq |> 
  mutate(Percentage = round(age_prop * 100,2))
colnames(age_freq) <- c("Age", "Frequency", "Percentage (%)")
age_table <- age_freq |> 
  nice_table(note = "N = 298", spacing = 2) |> 
  theme_apa()
age_table

#### Merged Demographics Table ####

#Merges all the demographic tables

gender_combined <- gender_freq |> rename(Category = Gender) #rename the Gender column into category
age_combined <- age_freq |> rename(Category = Age) #rename the Age column into category
relationship_combined <- relationship_freq |> rename(Category = Relationship) #rename the Relationship column into category
education_combined <- edu_freq |> rename(Category = "Education Level") #rename the Education Level column into category
employment_combined <- job_freq |> rename(Category = "Work Status") #rename the Work Status column into category

demographics_table <- bind_rows( #combine the following rows of the corresponding data frames 
  gender_combined |> mutate(Variable = "Gender"), #Adds a label column (Gender)
  age_combined |> mutate(Variable = "Age"),  #Adds a label column
  relationship_combined |> mutate(Variable = "Relationship"),  #Adds a label column
  education_combined |>  mutate(Variable = "Education Level"),  #Adds a label column
  employment_combined |> mutate(Variable = "Work Status")  #Adds a label column
) |> 
  select(Variable, Category, Frequency, `Percentage (%)`) |> #select the relevant columns to include (frequencies, percentages, and the category)
  nice_table(note = "N = 298", spacing = 2) #place note into the table, and set the spacing into 2, per APA
  

demographics_table <- demographics_table |> 
  merge_v(j = "Variable") |> #merge the repeated values in the variable column, so that each one (e.g. Gender) appears only once
  theme_apa() |>  #apply apa style
  valign(j = "Variable", valign = "top") |> #Merged labels (e.g. Age, Gender, etc) appear only once on the topc cel
  autofit() #Adjusts width of the columns to fit the present content

demographics_table #call the demographics table to inspect it

read_docx() |> # Creates word document
  body_add_flextable(demographics_table) |> #Inserts the table into the word document 
  print(target = "demographics_table.docx") #Writes it into the disk, saving the file with print, with its name.


#### Chatbot Use Profile ####

#Follows exactly the same logic as the demographics table

reason_combined <- reason_freq |> rename(Category = "Purpose of Communication")
freq_combined <- freq_table |> rename(Category = "Time of Communication")
comm_combined <- comm_freq |> rename(Category = "Way of Communication")
chatbot_combined <- chatbot_freq |> rename(Category = "Preferred Chatbot")

chatbot_profile_table <- bind_rows(
  reason_combined |> mutate(Variable = "Purpose of Use"),
  freq_combined |> mutate(Variable = "Duration of Communication (per day)"),
  comm_combined |> mutate(Variable = "Way of Communication"),
  chatbot_combined |> mutate(Variable = "Preferred Chatbot Model")
) |> 
  select(Variable, Category, Frequency, `Percentage (%)`) |> 
  nice_table(note = "N = 298.", spacing = 2)

chatbot_profile_table <- chatbot_profile_table |> 
  merge_v(j = "Variable") |> 
  theme_apa() |> 
  valign(j = "Variable", valign = "top",) |> 
  autofit()

#Call the chatbot profile table to inspect it
chatbot_profile_table

read_docx() |> 
  body_add_flextable(chatbot_profile_table) |> 
  print(target = "chatbot_profile_table.docx")