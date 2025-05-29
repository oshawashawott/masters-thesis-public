#README


#This is the official file containing all analytics and processing relevant to the Masters Thesis:
#Multivariate Insights into Discrimination: Analyzing Differences in Human Values among University-Educated Young Adults
#The sections will proceed with Data processing and description, and will be followed by the data analysis section
#Relevant Notes will accompany each of the code sections
#Some information is redacted for the purposes of anonymity

#relevant libraries for the analysis
library(dplyr)
library(tidyverse)
library(pheatmap)
library(ggplot2)
library(patchwork)
library(reshape2)
library(cluster)
library(MASS)
set.seed(314159)
#Data processing

#The dataset is referred to as "YARG" throughout the code
#This dataset consists of the following columns:
#Respondent ID: unique identifier for each respondent to the survey
#country_language: unique combination of country and the language (most of) the survey was administered in
#date: date of survey administration
#"In which country are you studying at the moment?": the country where the survey was administered/the respondent currently lives

#13 questions regarding discrimination where the respondents can check whether they feel discriminated based on 
#belonging to a certain group. There are 10 main groups, 1 Other group column, and 1 free text column which 
#is intended for describing the "Other" group. The final column is to be checked if the respondent does not feel that they experience discrimination

#57 questions corresponding to the Portrait Values Questionnaire developed by Schwartz (see reference). This response allows for selection of integers from 1 to 6.
#1 column called PVQmRat which is the mean score across all 57 values for each respondent. This is used for centering values.
#19 columns summarizing a value identified by Schwartz, some of which are sub-values of Schwartz's 10 basic human values
#6 additional columns which indicate a higher order value identified by Schwartz for the 10 basic human values.
#Summary of the data first:
summary(YARG)
#The summary gives the nature of the data, though we are concerned about missing data:
#1. Rename Columns for readability
colnames(YARG)[4] <- "Country"
colnames(YARG)[5] <- "No Discrimination"
colnames(YARG)[6] <- "Discrimination by Colour or Race"
colnames(YARG)[7] <- "Discrimination by Nationality"
colnames(YARG)[8] <- "Discrimination by Religion"
colnames(YARG)[9] <- "Discrimination by Political Orientation"
colnames(YARG)[10] <- "Discrimination by Language"
colnames(YARG)[11] <- "Discrimination by Ethnic Group"
colnames(YARG)[12] <- "Discrimination by Age"
colnames(YARG)[13] <- "Discrimination by Gender"
colnames(YARG)[14] <- "Discrimination by Sexuality"
colnames(YARG)[15] <- "Discrimination by Disability"
colnames(YARG)[16] <- "Discrimination by Other (not listed)"
colnames(YARG)[17] <- "Other Discrimination Type (written)"
colnames(YARG)[53] <- "36. It is important to him to enjoy life's pleasures."

#We also have some issues with the strings in the data itself being difficult to be read.
#To handle this, we will remove them altogether from the data and put as a palceholder "1" and "0"
#This does not remove anything from the data as these columns were coded versions of a checkbox in the survey
#2. Convert Discrimination columns into check-boxes (did check discrimination by this method vs not)
YARG$"No Discrimination" <- ifelse(grepl("discriminated", iconv(YARG$"No Discrimination", from = "", to = "UTF-8", sub = ""), ignore.case = TRUE), 1, 0)
YARG$"Discrimination by Colour or Race" <- ifelse(grepl("Colour",YARG$"Discrimination by Colour or Race"),1,0)
YARG$"Discrimination by Nationality" <- ifelse(grepl("Nationality",YARG$"Discrimination by Nationality"),1,0)
YARG$"Discrimination by Religion" <- ifelse(grepl("Religion",YARG$"Discrimination by Religion"),1,0)
YARG$"Discrimination by Political Orientation" <- ifelse(grepl("Politic",YARG$"Discrimination by Political Orientation"),1,0)
YARG$"Discrimination by Language" <- ifelse(grepl("Language",YARG$"Discrimination by Language"),1,0)
YARG$"Discrimination by Ethnic Group" <- ifelse(grepl("Ethnic",YARG$"Discrimination by Ethnic Group"),1,0)
YARG$"Discrimination by Age" <- ifelse(grepl("Age",YARG$"Discrimination by Age"),1,0)
YARG$"Discrimination by Gender" <- ifelse(grepl("Gender",YARG$"Discrimination by Gender"),1,0)
YARG$"Discrimination by Sexuality" <- ifelse(grepl("Sexuality",YARG$"Discrimination by Sexuality"),1,0)
YARG$"Discrimination by Disability" <- ifelse(grepl("Disability",YARG$"Discrimination by Disability"),1,0)
YARG$"Discrimination by Other (not listed)" <- ifelse(grepl("Other",YARG$"Discrimination by Other (not listed)"),1,0)

#The run of this function tells us quite a bit about our data:
#Missing Values:
#In regard to missing data, we have some columns with empty entries to consider.
#The most important and clearest missing entries come from questions 55 through 57 on the PVQ section,
#where each of those questions has 307 empty responses.
#This is because the Philippines data set did not include those three questions in the results. 
#this will necessarily have an impact on the higher order values calculated from the Philippines, but they do not make the data entirely unusable, 
#as each higher order value is calculated using at least 3 PVQ responses. The particular higher order values associated with these will be noted

#Outside of these columns, about half of the PVQ entries are complete and the other half are missing one or two entries, which is good on the whole.
#Some of the higher order values are also missing one entry, which we will investigate to make sure are due to missing calculations from the rest of the options

#NEW RULE
#Each of the 19 values must be missing no more than 1 of the entries used to calculate that value to be counted in our data.
#Some of the higher order values (of which there are 10) are made up of multiple entries (6 and/or 9 PVQ questions), 
#which means if they are missing more than 1 of the 6 or 9, we will remove the entry from the analysis
#We can note which of the ones are missing a value in their higher order value and calculate a number for reporting purposes
#Just need to find a justification for including them

#3. Process "Others" Discrimination Column

#In our data, we have a column of discrimination type called "Other", for which there are 122 entries.
#When viewing the text inputs for these individuals,
#it seems that some of them wrote down one of the discrimination types we have listed. For this reason, 
#we need to re-code these individuals as not being "Other", but rather one of our listed discrimination types.
#While the respondent may be trying to indicate something more specific with this information, for the purposes of our analysis, as "Other"
#is not a cohesive group, it may be better to group these respondents in with the proper corresponding group. 

#We have determined 31 respondents who need the "Other Discrimination" response unchecked and either left as is, or replaced with another category in our existing list
#16 of these 31 need to have another category indicated, while 15 only need the "Other" response removed
summary(as.logical(YARG$`Discrimination by Other (not listed)`))
#Uncheck the "Other" box
YARG$`Discrimination by Other (not listed)` <- ifelse(YARG$RespondentID %in% c(4598874185,4370931546,4667742715,4762512989,4799621413, ##Recode as Religion
                                                                               4421555877, #Recode as Political Orientation
                                                                               4387543740, #Recode as Nationality
                                                                               5124233828,4452548905, #Recode as Gender
                                                                               4584593304,4366898778,4367294875,4439962651,4582011555,4593274620,4669787781, #Recode as Disability
                                                                               4706106422,4370263971,4517262366,4355930454,4370067611,4375778970,4520337609,4879182357, #No Recode necessary, just need to be cleared
                                                                               4587310877,4350021045,4587367318,4585598487,4554742107,4691167398,4688198563), 
                                                      0,
                                                      YARG$`Discrimination by Other (not listed)`)

#Recode the necessary responses
YARG$`Discrimination by Religion` <- ifelse(YARG$RespondentID %in% c(4598874185,4370931546,4667742715,4762512989,4799621413), ##Recode as Religion
                                            1,
                                            YARG$`Discrimination by Religion`)

YARG$`Discrimination by Political Orientation` <- ifelse(YARG$RespondentID %in% c(4421555877), #Recode as Political Orientation
                                                         1,
                                                         YARG$`Discrimination by Political Orientation`)

YARG$`Discrimination by Nationality` <- ifelse(YARG$RespondentID %in% c(4387543740), #Recode as Nationality
                                               1,
                                               YARG$`Discrimination by Nationality`)

YARG$`Discrimination by Gender` <- ifelse(YARG$RespondentID %in% c(5124233828,4452548905), #Recode as Gender
                                          1,
                                          YARG$`Discrimination by Gender`)

YARG$`Discrimination by Disability` <- ifelse(YARG$RespondentID %in% c( 4584593304,4366898778,4367294875,4439962651,4582011555,4593274620,4669787781), #Recode as Disability
                                              1,
                                              YARG$`Discrimination by Disability`)


#The other 91 Other responses are fine as they are

#4. Process Contrasting checks (checks no discrimination but also another etc.)
#To ensure our data is functioning as intended, we need to make sure that no respondent clicked both the "No Discrimination" column and any of the other 
#discrimination types. 

YARG <- YARG %>% mutate('Number of Discrimination Types' = rowSums(across(c(6:16))))
#YARG %>% filter(YARG$`No Discrimination`== 1 & YARG$`Number of Discrimination Types` > 0) %>% select(6:16,last_col())
YARG <- YARG %>% mutate(inconsistent_discrimination = (YARG$`No Discrimination`== 1 & YARG$`Number of Discrimination Types` > 0))
summary(YARG$inconsistent_discrimination)
YARG_inconsistant <- YARG %>% filter(YARG$inconsistent_discrimination == TRUE)
summary(YARG_inconsistant[,c(6:16)])
#We have 71 entries (about 1.4%) who indicated both that they do not experience discrimination AND 
#that they do feel discriminated against based on one of the 11 discrimination types.
#This is a difficult case to deal with. 
#It does not appear that there is any pattern among the inconsistent group in terms of which discrimination types are selected, though it should be noted that
#in the "Other" category, about 13 respondents chose both No Discrimination and Other discrimination type possibly to indicate that they do not think they actually 
#experience discrimination though they belong to groups that "should"

#For the purposes of analysis, we will first consider that this group is actually not going to be considered as "No Discrimination"
#and will default to the specific discrimination type. (This means if inconsistent_discrimination is TRUE and "No Discrimination" is flagged as 1,
#then we will consider the "No Discrimination" flag as '0' for the purposes of the analysis.

##We will call the proper field for flagging 'No Discrimination" as "No Discrimination Updated"

YARG <- YARG %>% mutate('No Discrimination Updated' = ifelse(inconsistent_discrimination == 1,0,YARG$`No Discrimination` ))

summary(as.logical(YARG$`No Discrimination`))
summary(as.logical(YARG$`No Discrimination Updated`))

#The numbers now check-out
YARG %>%
  filter(if_any(18:71, ~ is.na(.) | . == "" | . == " " | . == 0)) %>%
  nrow()


# the following Portrait value questions are missing at least one entry: 
#4,6,7,10,11,16,19,21,27,30,31,33,34,38,39,41,43,44,48,53,55,56,57

#We also observe that we have missing values among Benevolence: Dependability, Stimulation, and Benevolence (also ben21 and sti21, which are repeats)
##This cleaning process includes some details which are not meant to be public, thus the process will just be described
##All Philippines respondents missing any of the 1-54 items are processed manually
## If the rule is maintained, then respondents are not excluded
#21 respondents were manually checked, 10 were found to need to be excluded
##FINAL Missing values total
##It seems all of the missing PVQ incidences come from the Philippines data, and according to the rule of not letting values
#be calculated with more than one missing component, we now are missing the following:
#5 entries of Self Direction
#3 entries of Self-Direction:Action
#3 entries of Benevolence
#2 entries of Benevolence:Dependability
#2 entries of Universalism
#1 entry of Universalism: Tolerance
#1 entry of Power
#1 entry of Power: Dominance
##This comprises 10 individuals, one is missing enough data to not calculate 2 values

#All of these missing values come from the Philippines.
#This should not significantly impact our results given the large sample size.
#Due to the functionality below of the various methods (clustering or otherwise) we will exclude any individual who is missing any of the 10 values:
#There are 10 of them as indicated in the missing value section above

YARG <- YARG %>% mutate(numdiscrimanswered = rowSums(across(c(5:16))))

YARG[,c(1,4:16,114)] %>% filter(numdiscrimanswered == 0)

## We have 24 individuals who did not answer any of the discrimination questions
## Since we dont have any indication either way whether these people are discriminated against or not,
## we must exclude them from our data analysis.
YARG <- YARG %>% filter(numdiscrimanswered != 0)
#Now exclude the remaining 10 PH members
YARG[!complete.cases(YARG), ]
YARG <- na.omit(YARG)

### Split Israel sample into Israel_Arabic and Israel_Hebrew
YARG$Country_update <- ifelse(YARG$Country == "Israel", YARG$country_language, YARG$Country)


### All figures and associated analyses are now shown below in order of appearance

##Figure 1: Number of Respondents by Level of Discrimination
#create a new field with updated discirmination numbers (as the prior calculation includes the incomplete group)
YARG$numDiscrimination_updated <- rowSums(YARG[,6:16])
#Update to just have multiple groups
YARG$numDiscrimination_updated <-
  ifelse(YARG$numDiscrimination_updated >= 2, "2+", YARG$numDiscrimination_updated)
#initial plot informs the data frame below with the colors:
ggplot(YARG, aes(x = factor(numDiscrimination_updated))) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Number of Respondents Experiencing X Number of Discrimination Types",
       x = "Number of Discrimination Types",
       y = "Frequency") +
  theme_minimal()

#Actual Figure creation
df <- data.frame(
  Discrimination = factor(c("No Discrimination", 
                            "One Type of Discrimination", 
                            "Multiple Types of Discrimination"),
                          levels = c("No Discrimination", 
                                     "One Type of Discrimination", 
                                     "Multiple Types of Discrimination")),
  Respondents = c(3250, 958, 989)
)

custom_colors <- c("No Discrimination" = "#7A1E1E",  
                   "One Type of Discrimination" = "#007377",
                   "Multiple Types of Discrimination" = "#E1A200")

ggplot(df, aes(x = Discrimination, y = Respondents, fill = Discrimination)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Respondents), vjust = -0.5, size = 5) +
  labs(y = "Respondents", x = NULL) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none")


#Figure 2: Percentage of Respondents who Experience each Discrimination Type
#get the totals for each group
discrimination_totals <- rep(0,11)
for(i in 6:16){
  discrimination_totals[i-5] <- sum(YARG[, i])
}
names(discrimination_totals) <- c("Colour or Race", "Nationality","Religion","Political orientation","Language","Ethnic group","Age","Gender","Sexuality","Disability","Other")
discrimination_percent <- discrimination_totals/nrow(YARG)
#actual figure
ggplot(data.frame(
  type = names(discrimination_percent),
  value = discrimination_percent * 100
), aes(x = reorder(type, -value), y = value)) +
  geom_bar(stat = "identity", fill = "#5465A0") +
  geom_text(aes(label = sprintf("%.1f%%", value)), 
            vjust = -0.5, size = 3.5) + 
  labs(
    x = "Discrimination Type",
    y = "Percentage(%) of Respondents"
  ) +
  ylim(0, 20) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#Figure 3: Percentage of Respondents by Level of Discrimination in each Country (Sampling Group)
#data frame creation
country_discrim <- YARG %>%
  group_by(Country_update, numDiscrimination_updated) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Country_update) %>%
  mutate(Percentage = Count / sum(Count) * 100)
#Actual figure
ggplot(country_discrim, aes(x = Country_update, y = Percentage, fill = numDiscrimination_updated)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200"),
                    labels = c("0" = "No Discrimination", 
                               "1" = "One Type of Discrimination", 
                               "2+" = "Multiple Types of Discrimination")) +
  labs(
    x = "Country (Sampling Group)",
    y = "Percentage(%) of Respondents",
    fill = "Experiences..."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Figure 4: QQ Plots using Mahalanobis Distance by Level of Discrimination

# First, isolate the value names in the data (note the 21 is just the label given in the dataframe)
value_names <- c("ben21", "con21", "pow21", "sdi21", "sec21", 
                 "uni21", "ach21", "sti21", "hed21", "tra21")
#isolate three data frames with the relevant information
YARG_values <- YARG[, c("numDiscrimination_updated", value_names)]
YARG_values_0 <- YARG_values %>%
  filter(numDiscrimination_updated == "0")
YARG_values_1 <- YARG_values %>%
  filter(numDiscrimination_updated == "1")
YARG_values_2 <- YARG_values %>%
  filter(numDiscrimination_updated == "2+")
## Calculate Mahalanobis distance for each group of discrimination level and make the qq plots
md_group0 <- mahalanobis(
  x = YARG_values_0[,value_names],
  center = colMeans(YARG_values_0[,value_names]),
  cov = cov(YARG_values_0[,value_names])
)
YARG_values_0$md <- md_group0
font_size <- 24
qqp1 <- ggplot(YARG_values_0, aes(sample = md)) +
  stat_qq(distribution = qchisq, dparams = list(df = length(value_names))) +
  stat_qq_line(distribution = qchisq, dparams = list(df = length(value_names)), color = "red") +
  labs(title = "No Discrimination",
       x = "Theoretical Quantiles", y = "Mahalanobis Distance") +
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size * 0.8)
  )


md_group1 <- mahalanobis(
  x = YARG_values_1[,value_names],
  center = colMeans(YARG_values_1[,value_names]),
  cov = cov(YARG_values_1[,value_names])
)
YARG_values_1$md <- md_group1

qqp2 <- ggplot(YARG_values_1, aes(sample = md)) +
  stat_qq(distribution = qchisq, dparams = list(df = length(value_names))) +
  stat_qq_line(distribution = qchisq, dparams = list(df = length(value_names)), color = "red") +
  labs(title = "One Type of Discrimination",
       x = "Theoretical Quantiles", y = "Mahalanobis Distance")+
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size * 0.8)
  )

md_group2 <- mahalanobis(
  x = YARG_values_2[,value_names],
  center = colMeans(YARG_values_2[,value_names]),
  cov = cov(YARG_values_2[,value_names])
)
YARG_values_2$md <- md_group2

qqp3 <- ggplot(YARG_values_2, aes(sample = md)) +
  stat_qq(distribution = qchisq, dparams = list(df = length(value_names))) +
  stat_qq_line(distribution = qchisq, dparams = list(df = length(value_names)), color = "red") +
  labs(title = "Multiple Types of Discrimination",
       x = "Theoretical Quantiles", y = "Mahalanobis Distance")+
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size * 0.8)
  )
#combine qq plots
combined_plot_qq_1 <- qqp1 + qqp2 + qqp3
combined_plot_qq_1

## Outlier checks code
cutoff <- qchisq(0.999, df = length(value_names))

YARG_values_0$outlier <- YARG_values_0$md > cutoff
YARG_values_1$outlier <- YARG_values_1$md > cutoff
YARG_values_2$outlier <- YARG_values_2$md > cutoff



#Table 1: Variances for Values by Level of Discrimination and Levene's Test Results
## First Levene's test using the "car" package
levene_p_vals <- rep(0,10)
test1 <- leveneTest(ben21 ~ numDiscrimination_updated, data = YARG)
test2 <- leveneTest(con21 ~ numDiscrimination_updated, data = YARG)
test3 <- leveneTest(pow21 ~ numDiscrimination_updated, data = YARG)
test4 <- leveneTest(sdi21 ~ numDiscrimination_updated, data = YARG)
test5 <- leveneTest(sec21 ~ numDiscrimination_updated, data = YARG)
test6 <- leveneTest(uni21 ~ numDiscrimination_updated, data = YARG)
test7 <- leveneTest(ach21 ~ numDiscrimination_updated, data = YARG)
test8 <- leveneTest(sti21 ~ numDiscrimination_updated, data = YARG)
test9 <- leveneTest(hed21 ~ numDiscrimination_updated, data = YARG)
test10 <- leveneTest(tra21 ~ numDiscrimination_updated, data = YARG)
#we isolate the p-values and apply the correction (based on number)
evene_p_vals[1] <- test1[1, "Pr(>F)"]
levene_p_vals[2] <- test2[1, "Pr(>F)"]
levene_p_vals[3] <- test3[1, "Pr(>F)"]
levene_p_vals[4] <- test4[1, "Pr(>F)"]
levene_p_vals[5] <- test5[1, "Pr(>F)"]
levene_p_vals[6] <- test6[1, "Pr(>F)"]
levene_p_vals[7] <- test7[1, "Pr(>F)"]
levene_p_vals[8] <- test8[1, "Pr(>F)"]
levene_p_vals[9] <- test9[1, "Pr(>F)"]
levene_p_vals[10] <- test10[1, "Pr(>F)"]
levene_p_vals <- p.adjust(levene_p_vals, method = "bonferroni")

## Get the variances (not the best way but uses cov function to get the matrix and then this gets the variances diagonal)
cov_group1 <- diag(cov(YARG_values_0[,value_names]))
cov_group2 <- diag(cov(YARG_values_1[,value_names]))
cov_group3 <- diag(cov(YARG_values_2[,value_names]))

table_1 <- rbind(
  "No Discrimination Variance" = cov_group1,
  "One Type of Discrimination Variance" = cov_group2,
  "Multiple Types of Discrimination Variance" = cov_group3,
  "Bonferroni-corrected Levene's Test P-Value" = levene_p_vals
)

colnames(table_1) <- c("Benevolence", "Conformity", "Power", "Self-Direction", "Security","Universalism", "Achievement", "Stimulation", "Hedonism", "Tradition"
)

#write.csv(table_1 , "table1.csv", row.names = TRUE)

#Table 2:MANOVA Results for Values by Level of Discrimination
#MANOVA time, base manova just on discrimination overall
base_manova <- manova(cbind(ben21,con21,pow21,sdi21,sec21,uni21,ach21,sti21,hed21,tra21) ~
                        YARG$numDiscrimination_updated, data = YARG)

base_pillai <- summary(base_manova, test = "Pillai")$stats
base_wilks <- summary(base_manova, test = "Wilks")$stats
summary(base_manova)
base_manova_table <- data.frame(
  Test = c("Wilks' Lambda", "Pillai's Trace"),
  'Degrees of Freedom' = c(base_wilks[1, "Df"], base_pillai[1, "Df"]),
  Statistic = c(base_wilks[1, "Wilks"], base_pillai[1, "Pillai"]),
  'F Statistic' = c(base_wilks[1, "approx F"], base_pillai[1, "approx F"]),
  p_value = c(base_wilks[1, "Pr(>F)"], base_pillai[1, "Pr(>F)"])
)

base_manova_table[, -1] <- lapply(base_manova_table[, -1], function(x) signif(x, 5))
#write.csv(base_manova_table , "table2.csv", row.names = TRUE)

### Figure 5: Coefficients of LD1 and LD2 for Values by Level of Discrimination from LDA Results
#Linear Discriminant Analysis follow up:
base_lda_model <- lda(numDiscrimination_updated ~ ben21 +con21 +pow21 +sdi21 +sec21 +uni21 +ach21+sti21+hed21 +tra21, data = YARG)
lda_1_data <- as.data.frame(base_lda_model$scaling)
lda_1_data$names <- c("Benevolence (BEN)", "Conformity (CON)", "Power (POW)", "Self-Direction (SDI)", "Security (SEC)",
                      "Universalism (UNI)", "Achievement (ACH)", "Stimulation (STI)", "Hedonism (HED)", "Tradition (TRA)"
)
lda_1_data <- lda_1_data %>%
  pivot_longer(cols = starts_with("LD"), names_to = "LDA", values_to = "Coefficient")
lda_1_data$names <- factor(lda_1_data$names, levels = rev(c("Benevolence (BEN)", "Conformity (CON)", "Power (POW)", "Self-Direction (SDI)", "Security (SEC)",
                                                            "Universalism (UNI)", "Achievement (ACH)", "Stimulation (STI)", "Hedonism (HED)", "Tradition (TRA)"
)))
font_size = 14
#Actual figure
ggplot(lda_1_data, aes(x = names, y = Coefficient, fill = LDA)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(
    values = c("LD1" = "#7A1E1E", "LD2" = "#007377"),
    labels = c("LD1" = "LD1 (~87%)", "LD2" = "LD2 (~13%)")
  ) +
  labs(x = "Values", y = "Coefficient (Measure of Importance of Value)", fill = "LD Functions") +
  theme_minimal()  +
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text.x = element_text(size = font_size * 0.8, vjust = 0.5)
  )

#Figure 6:LDA Biplot: Level of Discrimination Groups and Vectorized Coefficient Contribution of Values
#Get the data from the lda to plot
base_lda_predictions <- predict(base_lda_model)

plot_df <- data.frame(base_lda_predictions$x, numDiscrimination_updated = YARG$numDiscrimination_updated)
#Initial plot
ggplot(plot_df, aes(x = LD1, y = LD2, color = numDiscrimination_updated)) +
  geom_point(size = 2, alpha = 0.5) +
  stat_ellipse(aes(group = numDiscrimination_updated), type = "norm", linetype = 2) +
  theme_minimal() +
  labs(title = "Linear Discriminant Function Plot",
       x = "LD1", y = "LD2")
#Well it looks like maybe the 2+ value is a bit different on the LD1 dimension, though it's very hard to tell

scaling <- as.data.frame(base_lda_model$scaling)
scaling$values <- rownames(scaling)
### add variable contributions to plot itself
# scaling factor of 2 for visibility
vector_scale <- 2
loadings_scaled <- scaling
loadings_scaled$LD1 <- scaling$LD1 * vector_scale
loadings_scaled$LD2 <- scaling$LD2 * vector_scale

loadings_scaled$values <- c("BEN","CON","POW","SDI","SEC","UNI","ACH","STI","HED","TRA")

# actual figure
custom_colors <- c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200")
legend_labels <- c("0" = "No Discrimination", "1" = "One Type", "2+" = "Multiple Types")

ggplot(plot_df, aes(x = LD1, y = LD2, color = numDiscrimination_updated)) +
  geom_point(alpha = 0.3) +
  stat_ellipse(type = "norm", level = 0.95, linetype = "dashed") +
  geom_segment(data = loadings_scaled,
               aes(x = 0, y = 0, xend = LD1, yend = LD2),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = loadings_scaled,
            aes(x = LD1, y = LD2, label = values), 
            hjust = 1.2, vjust = 1.2, size = 3.5, color = "black") +
  scale_color_manual(values = custom_colors,
                     labels = legend_labels,
                     name = "Experiences...") +
  theme_minimal() +
  labs(x = "LD1 (~87%)", y = "LD2 (~13%)")

#Figure 7:Boxplots of Conformity and Security Values by Level of Discrimination
#Separate box plots and join them together
#first one
font_size = 12
boxp1 <- ggplot(YARG, aes(x = numDiscrimination_updated, y = con21, fill = numDiscrimination_updated)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200")) +
  scale_x_discrete(
    labels = c("0" = "No\nDiscrimination", 
               "1" = "One Type of\nDiscrimination", 
               "2+" = "Multiple Types\nof Discrimination")
  ) +
  labs(y = "Conformity", x = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size * 0.8),
    legend.position = "none"
  )

# boxplot for Security
boxp2 <- ggplot(YARG, aes(x = numDiscrimination_updated, y = sec21, fill = numDiscrimination_updated)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200")) +
  scale_x_discrete(
    labels = c("0" = "No\nDiscrimination", 
               "1" = "One Type of\nDiscrimination", 
               "2+" = "Multiple Types\nof Discrimination")
  ) +
  labs(y = "Security", x = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text.x = element_text(size = font_size * 0.8, vjust = 0.5),
    legend.position = "none"
  )

combined_plot_box_1 <- boxp1 + boxp2

# final figure
combined_plot_box_1

## Figure 8: Co-incidence of Discrimination Types
heatmap_data <- YARG[,c(6:16)]
#get rid of names for plot clarity
names(heatmap_data) <- gsub("^Discrimination by\\s*", "", names(heatmap_data))

co_occurrence <- t(as.matrix(heatmap_data)) %*% as.matrix(heatmap_data)

melted_cooccurrence <- melt(co_occurrence)
ggplot(melted_cooccurrence, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "", y = "", fill = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Figure 9: Heatmap of Respondent's Discrimination Type by Cluster
## Need to perform the clustering using the 10 discrim types, have to filter the data to just the discrim group
clustered_rows <- rowSums(YARG[, 6:15]) > 0
#need to do the discrim columns separately:
discrim_numbers <- YARG[clustered_rows, 6:15]
diss <- daisy(discrim_numbers, metric = "gower", type = list(asymm = 1:10))
hc <- hclust(diss, method = "average")
##initial plot, not super useful
plot(hc, labels = FALSE, main = "Hierarchical Clustering with Jaccard Distance")
#experimented with cuts, but generally gave similar results
clusters <- cutree(hc, k = 2)

#re-ordering the matrix by the new clusters
binary_clustered <- discrim_numbers[order(clusters), ]
names(binary_clustered) <- gsub("^Discrimination by\\s*", "", names(binary_clustered))
#then we produce the annotation
annotation <- data.frame(Cluster = factor(clusters[order(clusters)],
                                          levels = c("1", "2"),
                                          labels = c("EI", "SCI")))
rownames(annotation) <- rownames(binary_clustered)

ann_colors <- list(
  Cluster = c("EI" = "#007377", "SCI" = "#E1A200")
)
#actual figure heatmap
pheatmap(as.matrix(binary_clustered),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = FALSE,
         annotation_row = annotation,
         annotation_colors = ann_colors,
         color = c("white", "black"))

## Assign the clusters: 
full_clusters <- rep(0, nrow(YARG))  
full_clusters[clustered_rows] <- clusters     
YARG$discrim_clusters <- full_clusters


#Table 3: Percentage of each Discrimination Type by Cluster
cluster_sums <- YARG %>%
  group_by(discrim_clusters) %>%
  summarise(across(6:15, sum))
column_totals <- colSums(YARG[, 6:15])
percent_matrix <- cluster_sums %>%
  mutate(across(-discrim_clusters, ~ .x / column_totals[cur_column()] * 100)) 
names(percent_matrix) <- gsub("^Discrimination by\\s*", "", names(percent_matrix))
names(percent_matrix)[1] <- "Cluster"
table3 <- percent_matrix[c(2:3),]
#manually re-ordering the table 3 results
tbl3 <- data.frame(
  Cluster = c("EI Cluster", "SCI Cluster"),
  Gender = c(93.5, 6.47),
  Sexuality = c(90.9, 9.06),
  Disability = c(90.4, 9.6),
  Age = c(85.9, 14.1),
  "Colour or Race" = c(76.3, 23.7),
  "Political Orientation" = c(41.7, 58.3),
  "Ethnic Group" = c(27.4, 72.6),
  Religion = c(27.1, 72.9),
  Language = c(21.7, 78.3),
  Nationality = c(19.6, 80.4)
)
rownames(tbl3) <- tbl3$Cluster
tbl3$Cluster <- NULL

#write.csv(tbl3 , "table3.csv", row.names = TRUE)

#The clusters mostly operate as follows:
#Cluster 1: Primarily Gender, Age, Sexuality, Disability, Colour or Race
#Cluster 2: Primarily Nationality, Religion, Political Orientation, Language, Ethnic Group

## Table 4: Number of Respondents by Discrimination Type Clusters and Country Sampling Group
#Let's look at the clusters based on which country dominates the values
table(YARG$discrim_clusters, YARG$Country_update)
tbl4 <- table(YARG$discrim_clusters, YARG$Country_update)[2:3,]
rownames(tbl4) <- c("EI Cluster","SCI Cluster")

table_4 <- t(tbl4)
#write.csv(tbl4, "table4.csv", row.names = TRUE)


## Figure 10: QQ Plots using Mahalanobis Distance for Embodied Identity (EI) and Sociocultural Identity (SCI) Clusters
value_names <- c("ben21", "con21", "pow21", "sdi21", "sec21", 
                 "uni21", "ach21", "sti21", "hed21", "tra21")
#get rid of the Others that aren't no discrimination
YARG_values_types <- YARG %>% filter(YARG$numDiscrimination_updated != 1 | YARG$discrim_clusters != 0)
YARG_values_types <- YARG_values_types[, c("discrim_clusters","Country_update", value_names)]
YARG_values_c1 <- YARG_values_types %>%
  filter(discrim_clusters == 1)
YARG_values_c2 <- YARG_values_types %>%
  filter(discrim_clusters == 2)

md_groupc1 <- mahalanobis(
  x = YARG_values_c1[,value_names],
  center = colMeans(YARG_values_c1[,value_names]),
  cov = cov(YARG_values_c1[,value_names])
)
YARG_values_c1$md <- md_groupc1
font_size <- 24
qqp3 <- ggplot(YARG_values_c1, aes(sample = md)) +
  stat_qq(distribution = qchisq, dparams = list(df = length(value_names))) +
  stat_qq_line(distribution = qchisq, dparams = list(df = length(value_names)), color = "red") +
  labs(title = "Embodied Identity (EI) Cluster",
       x = "Theoretical Quantiles", y = "Mahalanobis Distance") + theme(
         plot.title = element_text(size = font_size, face = "bold"),
         axis.title = element_text(size = font_size),
         axis.text = element_text(size = font_size * 0.8)
       )

md_groupc2 <- mahalanobis(
  x = YARG_values_c2[,value_names],
  center = colMeans(YARG_values_c2[,value_names]),
  cov = cov(YARG_values_c2[,value_names])
)
YARG_values_c2$md <- md_groupc2

qqp4 <- ggplot(YARG_values_c2, aes(sample = md)) +
  stat_qq(distribution = qchisq, dparams = list(df = length(value_names))) +
  stat_qq_line(distribution = qchisq, dparams = list(df = length(value_names)), color = "red") +
  labs(title = "Sociocultural Identity (SCI) Cluster",
       x = "Theoretical Quantiles", y = "Mahalanobis Distance") +
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size * 0.8)
  )

combined_plot_qq_2 <- qqp3 + qqp4
#actual plot
combined_plot_qq_2

#Outlier views
YARG_values_c1$outlier <- YARG_values_c1$md > cutoff
YARG_values_c2$outlier <- YARG_values_c2$md > cutoff

## Table 5: Variances for Values by Discrimination Type Clusters and No Discrimination Group and Levene's Test Results
levene_p_vals_c <- rep(0,10)
YARG_values_types$discrim_clusters <- as.factor(YARG_values_types$discrim_clusters)
test1c <- leveneTest(ben21 ~ discrim_clusters, data = YARG_values_types)
test2c <- leveneTest(con21 ~ discrim_clusters, data = YARG_values_types)
test3c <- leveneTest(pow21 ~ discrim_clusters, data = YARG_values_types)
test4c <- leveneTest(sdi21 ~ discrim_clusters, data = YARG_values_types)
test5c <- leveneTest(sec21 ~ discrim_clusters, data = YARG_values_types)
test6c <- leveneTest(uni21 ~ discrim_clusters, data = YARG_values_types)
test7c <- leveneTest(ach21 ~ discrim_clusters, data = YARG_values_types)
test8c <- leveneTest(sti21 ~ discrim_clusters, data = YARG_values_types)
test9c <- leveneTest(hed21 ~ discrim_clusters, data = YARG_values_types)
test10c <- leveneTest(tra21 ~ discrim_clusters, data = YARG_values_types)

levene_p_vals_c[1] <- test1c[1, "Pr(>F)"]
levene_p_vals_c[2] <- test2c[1, "Pr(>F)"]
levene_p_vals_c[3] <- test3c[1, "Pr(>F)"]
levene_p_vals_c[4] <- test4c[1, "Pr(>F)"]
levene_p_vals_c[5] <- test5c[1, "Pr(>F)"]
levene_p_vals_c[6] <- test6c[1, "Pr(>F)"]
levene_p_vals_c[7] <- test7c[1, "Pr(>F)"]
levene_p_vals_c[8] <- test8c[1, "Pr(>F)"]
levene_p_vals_c[9] <- test9c[1, "Pr(>F)"]
levene_p_vals_c[10] <- test10c[1, "Pr(>F)"]



levene_p_vals_c <- p.adjust(levene_p_vals_c, method = "bonferroni")

cov_group1_c <- diag(cov(YARG_values_0[,value_names]))
cov_group2_c <- diag(cov(YARG_values_c1[,value_names]))
cov_group3_c <- diag(cov(YARG_values_c2[,value_names]))

table_5 <- rbind(
  "No Discrimination Variance" = cov_group1_c,
  "EI Cluster Variance" = cov_group2_c,
  "SCI Cluster Variance" = cov_group3_c,
  "Bonferroni-corrected Levene's Test P-Value" = levene_p_vals_c
)

colnames(table_5) <- c("Benevolence", "Conformity", "Power", "Self-Direction", "Security",
                       "Universalism", "Achievement", "Stimulation", "Hedonism", "Tradition"
)

#write.csv(table_5 , "table5.csv", row.names = TRUE)

## Table 6: MANOVA Results for Values by Discrimination Type Clusters and No Discrimination Group
rejoined_YARG_clusters <- rbind(YARG_values_0, YARG_values_c1, YARG_values_c2)

rejoined_YARG_clusters$discrim_clusters <- as.factor(rejoined_YARG_clusters$discrim_clusters)
add_manova <- manova(cbind(ben21,con21,pow21,sdi21,sec21,uni21,ach21,sti21,hed21,tra21) ~
                       discrim_clusters, data = YARG_values_types)
add_manova_out <- manova(cbind(ben21,con21,pow21,sdi21,sec21,uni21,ach21,sti21,hed21,tra21) ~
                           rejoined_YARG_clusters$discrim_clusters, data = rejoined_YARG_clusters)
summary(add_manova)

add_pillai <- summary(add_manova, test = "Pillai")$stats
add_wilks <- summary(add_manova, test = "Wilks")$stats
summary(add_manova)
add_manova_table <- data.frame(
  Test = c("Wilk's Lambda", "Pillai's Trace"),
  'Degrees of Freedom' = c(add_wilks[1, "Df"], add_pillai[1, "Df"]),
  Statistic = c(add_wilks[1, "Wilks"], add_pillai[1, "Pillai"]),
  'F Statistic' = c(add_wilks[1, "approx F"], add_pillai[1, "approx F"]),
  p_value = c(add_wilks[1, "Pr(>F)"], add_pillai[1, "Pr(>F)"])
)

add_manova_table[, -1] <- lapply(add_manova_table[, -1], function(x) signif(x, 5))
write.csv(add_manova_table , "table6.csv", row.names = TRUE)

## Figure 11:Coefficients of LD1 and LD2 for Values by Discrimination Type Clusters and No Discrimination Group from LDA Results
YARG$discrim_clusters <- as.factor(YARG$discrim_clusters)

add_lda_model <- lda(discrim_clusters ~ ben21 +con21 +pow21 +sdi21 +sec21 +uni21 +ach21+sti21+hed21 +tra21, data = YARG_values_types)

lda_2_data <- as.data.frame(add_lda_model$scaling)
lda_2_data$names <- c("Benevolence (BEN)", "Conformity (CON)", "Power (POW)", "Self-Direction (SDI)", "Security (SEC)",
                      "Universalism (UNI)", "Achievement (ACH)", "Stimulation (STI)", "Hedonism (HED)", "Tradition (TRA)"
)
lda_2_data <- lda_2_data %>%
  pivot_longer(cols = starts_with("LD"), names_to = "LDA", values_to = "Coefficient")
lda_2_data$names <- factor(lda_2_data$names, levels = rev(c("Benevolence (BEN)", "Conformity (CON)", "Power (POW)", "Self-Direction (SDI)", "Security (SEC)",
                                                            "Universalism (UNI)", "Achievement (ACH)", "Stimulation (STI)", "Hedonism (HED)", "Tradition (TRA)"
)))
font_size <- 14
ggplot(lda_2_data, aes(x = names, y = Coefficient, fill = LDA)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(
    values = c("LD1" = "#7A1E1E", "LD2" = "#007377"),
    labels = c("LD1" = "LD1 (~76%)", "LD2" = "LD2 (~24%)")
  ) +
  labs(x = "Values", y = "Coefficient (Measure of Importance of Value)", fill = "LD Functions") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = font_size, face = "bold"),
    axis.title = element_text(size = font_size),
    axis.text.x = element_text(size = font_size * 0.8, vjust = 0.5)
  )

## Figure 12: LDA Biplot: Discrimination Type Clusters and No Discrimination Group with Vectorized Coefficient Contribution of Values
add_lda_predictions <- predict(add_lda_model)
add_plot_df <- data.frame(add_lda_predictions$x, discrim_clusters = as.factor(YARG_values_types$discrim_clusters))

vector_scale <- 2
add_scaling <- as.data.frame(add_lda_model$scaling)
add_scaling$values <- rownames(add_scaling)

add_loadings_scaled <- add_scaling
add_loadings_scaled$LD1 <- add_scaling$LD1 * vector_scale
add_loadings_scaled$LD2 <- add_scaling$LD2 * vector_scale
add_loadings_scaled$values <- c("BEN","CON","POW","SDI","SEC","UNI","ACH","STI","HED","TRA")

custom_colors <- c("0" = "#7A1E1E", "1" = "#007377", "2" = "#E1A200")
legend_labels <- c("0" = "No Discrimination", "1" = "EI Cluster", "2" = "SCI Cluster")
#actual figure
ggplot(add_plot_df, aes(x = LD1, y = LD2, color = discrim_clusters)) +
  geom_point(alpha = 0.4) +
  stat_ellipse(type = "norm", level = 0.95, linetype = "dashed") +
  geom_segment(data = add_loadings_scaled,
               aes(x = 0, y = 0, xend = LD1, yend = LD2),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = add_loadings_scaled,
            aes(x = LD1, y = LD2, label = values), 
            hjust = 1.2, vjust = 1.2, size = 3.5, color = "black") +
  scale_color_manual(values = custom_colors,
                     labels = legend_labels,
                     name = "Discrimination Type") +
  theme_minimal() +
  labs(x = "LD1 (~76%)", y = "LD2 (~24%)")

## Figure 13: Boxplots of Universalism and Tradition Values across Discrimination Type Clusters and No Discrimination Group
font_size = 12
#first one
boxp3 <- ggplot(YARG_values_types, aes(x = discrim_clusters, y = uni21, fill = discrim_clusters)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2" = "#E1A200")) +
  scale_x_discrete(labels = c("0" = "No\nDiscrimination", 
                              "1" = "EI Cluster", 
                              "2" = "SCI Cluster")) +
  labs(y = "Universalism", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = font_size, face = "bold"),
        axis.title = element_text(size = font_size),
        axis.text = element_text(size = font_size * 0.8))

# second boxplot
boxp4 <- ggplot(YARG_values_types, aes(x = discrim_clusters, y = tra21, fill = discrim_clusters)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2" = "#E1A200")) +
  scale_x_discrete(labels = c("0" = "No\nDiscrimination", 
                              "1" = "EI Cluster", 
                              "2" = "SCI Cluster")) +
  labs(y = "Tradition",x = "") +
  theme_minimal()+
  theme(plot.title = element_text(size = font_size, face = "bold"),
        axis.title = element_text(size = font_size),
        axis.text = element_text(size = font_size * 0.8),
        legend.position = "none")

combined_plot_box_2 <- boxp3 + boxp4

combined_plot_box_2


## Prep a df for Figure 14 and 15
YARG_long <- YARG[,c(5:16,106,110)]
names(YARG_long)[2:12] <- gsub("^Discrimination by\\s*", "", names(YARG_long)[2:12])
YARG_long <- YARG_long %>%
  pivot_longer(
    cols = 1:12,
    names_to = "DiscriminationType",
    values_to = "include"
  ) %>%
  filter(include == 1)

cluster1 <- c("Gender", "Sexuality", "Disability", "Age", "Colour or Race")
cluster2 <- c("Religion", "Nationality", "Ethnic Group", "Language", "Political Orientation")
other <- "Other (not listed)"
no_discrim <- "No Discrimination"


## Figure 14: Boxplot of Universalism Value across Discrimination Types
YARG_long_uni <- YARG_long %>%
  mutate(
    ClusterGroup = case_when(
      DiscriminationType %in% cluster1 ~ "Primarily EI Cluster",
      DiscriminationType %in% cluster2 ~ "Primarily SCI Cluster",
      DiscriminationType == no_discrim ~ "No Disc.",
      DiscriminationType == other ~ "Other",
      TRUE ~ "Unknown"
    )
  )

mean_order <- YARG_long_uni %>%
  group_by(DiscriminationType) %>%
  summarise(mean_uni = -mean(uni21, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    YARG_long_uni %>% dplyr::select(DiscriminationType, ClusterGroup) %>% distinct(),
    by = "DiscriminationType"
  ) %>%
  arrange(factor(ClusterGroup, levels = c("Primarily EI Cluster", "No Disc.", "Primarily SCI Cluster", "Other")), mean_uni) %>%
  pull(DiscriminationType)

YARG_long_uni <- YARG_long_uni %>%
  mutate(
    ClusterGroup = factor(ClusterGroup, levels = c("Primarily EI Cluster", "No Disc.", "Primarily SCI Cluster", "Other")),
    DiscriminationType = factor(DiscriminationType, levels = mean_order)
  )
#actual figure
ggplot(YARG_long_uni, aes(x = DiscriminationType, y = uni21)) +
  geom_boxplot() +
  facet_grid(~ ClusterGroup, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    x = "Type of Discrimination",
    y = "Universalism"
  )
## Figure 15: Boxplot of Tradition Value across Discrimination Types
YARG_long_tra <- YARG_long %>%
  mutate(
    ClusterGroup = case_when(
      DiscriminationType %in% cluster1 ~ "Primarily EI Cluster",
      DiscriminationType %in% cluster2 ~ "Primarily SCI Cluster",
      DiscriminationType == no_discrim ~ "No Disc.",
      DiscriminationType == other ~ "Other",
      TRUE ~ "Unknown"
    )
  )

mean_order <- YARG_long_tra %>%
  group_by(DiscriminationType) %>%
  summarise(mean_tra = mean(tra21, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    YARG_long_tra %>% dplyr::select(DiscriminationType, ClusterGroup) %>% distinct(),
    by = "DiscriminationType"
  ) %>%
  arrange(factor(ClusterGroup, levels = c("Primarily EI Cluster", "No Disc.", "Primarily SCI Cluster", "Other")), mean_tra) %>%
  pull(DiscriminationType)

YARG_long_tra <- YARG_long_tra %>%
  mutate(
    ClusterGroup = factor(ClusterGroup, levels = c("Primarily EI Cluster", "No Disc.", "Primarily SCI Cluster", "Other")),
    DiscriminationType = factor(DiscriminationType, levels = mean_order)
  )
#actual figure
ggplot(YARG_long_tra, aes(x = DiscriminationType, y = tra21)) +
  geom_boxplot() +
  facet_grid(~ ClusterGroup, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    x = "Type of Discrimination",
    y = "Tradition"
  )

## Figure 16:Dendrogram of Country Sampling Groups by Hierarchical Clustering using Mean of Schwartz 19 Values
#need summary data on eman values to cluster the countries
country_centered_summary <- YARG %>%
  group_by(Country_update) %>%
  summarise(across(76:94, mean)) %>%
  ungroup()

centered_matrix <- as.data.frame(country_centered_summary)
rownames(centered_matrix) <- centered_matrix$Country_update
centered_matrix <- centered_matrix[, -1] 
distance_matrix_centered <- dist(centered_matrix, method = "euclidean")

centered_clusters <- hclust(distance_matrix_centered, method = "ward.D2")
#actual figure
plot(centered_clusters, main = "",  , sub = "", xlab = "",
     labels = rownames(centered_matrix))

#viewed for choosing cluster number
summary(centered_clusters$height)
#actual cluster assignments
YARG <- YARG %>%
  mutate(
    Country_clusters = case_when(
      Country_update %in% c("The Philippines", "Ghana", "India") ~ "A (PH,GH,IN)",
      Country_update %in% c("China", "Japan", "Israel_Arabic", "Turkey") ~ "B (CH,JP,IL_A,TR)",
      Country_update %in% c("Russia", "Israel_Hebrew", "Poland") ~ "C (RU,IL_H,PL)",
      Country_update %in% c("Peru", "The United States", "Canada", "Finland", "Sweden") ~ "D (PE,US,CA,FI,SE)",
      TRUE                                            ~ "wrong"
    )
  )

## Figure 17:Number of Respondents by Level of Discrimination and Country Cluster

country_c_discrim <- YARG %>%
  group_by(Country_clusters, numDiscrimination_updated) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Country_clusters)
#Figure 17
ggplot(country_c_discrim, aes(x = Country_clusters, y = Count, fill = numDiscrimination_updated)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200"),
                    labels = c("0" = "No Discrimination", 
                               "1" = "One Type of Discrimination", 
                               "2+" = "Multiple Types of Discrimination")) +
  labs(
    x = "Country Clusters",
    y = "Number of Respondents",
    fill = "Experiences..."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Figure 18:Values of Conformity and Security by Discrimination Type Clusters and No Discrimination Group and Country Clusters
#first box plot
boxp5 <- ggplot(YARG, aes(x = numDiscrimination_updated, y = con21, fill = numDiscrimination_updated)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200")) +
  facet_wrap(~ Country_clusters) + 
  labs(y = "Conformity", x = "") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank(), 
        axis.ticks.x = element_blank() )

#second box plot
boxp6 <- ggplot(YARG, aes(x = numDiscrimination_updated, y = sec21, fill = numDiscrimination_updated)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(
    values = c("0" = "#7A1E1E", "1" = "#007377", "2+" = "#E1A200"),
    labels = c("0" = "No Discrimination", 
               "1" = "One Type of Discrimination", 
               "2+" = "Multiple Types of Discrimination"),
    name = "Experiences..."
  ) +
  facet_wrap(~ Country_clusters) +
  labs(y = "Security", x = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()  
  )

#actual figure
combined_plot_box_3 <- boxp5 + boxp6

## Table 7:Number of Respondents by Discrimination Type Clusters and Country Clusters

table(YARG$discrim_clusters, YARG$Country_clusters)
tbl7 <- table(YARG$discrim_clusters, YARG$Country_clusters)[2:3,]
rownames(tbl7) <- c("EI Cluster","SCI Cluster")
colnames(tbl7) <- c("Cluster A (PH,GH,IN)", "Cluster B (CH,JP,IL_A,TR)", "Cluster C (RU,IL_H,PL)", "Cluster D (PE,US,CA,FI,SE)")
tbl7 <- t(tbl7)
write.csv(tbl7, "table7.csv", row.names = TRUE)

## Figure 19:Values of Universalism and Tradition by Discrimination Type Clusters and No Discrimination Group and Country Clusters
YARG_values_types <- YARG_values_types %>%
  mutate(
    Country_clusters = case_when(
      Country_update %in% c("The Philippines", "Ghana", "India") ~ "A (PH,GH,IN)",
      Country_update %in% c("China", "Japan", "Israel_Arabic", "Turkey") ~ "B (CH,JP,IL_A,TR)",
      Country_update %in% c("Russia", "Israel_Hebrew", "Poland") ~ "C (RU,IL_H,PL)",
      Country_update %in% c("Peru", "The United States", "Canada", "Finland", "Sweden") ~ "D (PE,US,CA,FI,SE)",
      TRUE                                            ~ "wrong"
    )
  )

#actual figure
#first one
boxp7 <- ggplot(YARG_values_types, aes(x = discrim_clusters, y = uni21, fill = discrim_clusters)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2" = "#E1A200")) +
  scale_x_discrete(labels = c("0" = "No Discrimination", 
                              "1" = "EI Cluster", 
                              "2" = "SCI Cluster")) +
  labs(y = "Universalism", x = "") +
  facet_wrap(~ Country_clusters) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank() )

# second boxplot
boxp8 <- ggplot(YARG_values_types, aes(x = discrim_clusters, y = tra21, fill = discrim_clusters)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#7A1E1E", "1" = "#007377", "2" = "#E1A200"),
                    labels = c("0" = "No Discrimination", 
                               "1" = "EI Cluster", 
                               "2" = "SCI Cluster")) +
  scale_x_discrete(labels = c("0" = "No Discrimination", 
                              "1" = "EI Cluster", 
                              "2" = "SCI Cluster")) +
  facet_wrap(~ Country_clusters) +
  labs(y = "Tradition",x = "" ,fill = "Discrimination Type Clusters") +
  theme_minimal()  +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

combined_plot_box_4 <- boxp7 + boxp8

