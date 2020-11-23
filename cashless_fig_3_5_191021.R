# cashless_fig_3_5_191021.R Figures 3 and 5 (by income), cashless_55.tex
# cashless_income_191007.R Figures 2 and 4: w.r.t to income category. 

### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
#library(ggplot2)
#library(spatstat) # for weighted.median
#library(mfx)
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
#library(mlogit)
#library(mnlogit)# Where I replicate the existing mlogit regressions and experiment with individual effects. 
#library(tibble)
setwd("~/Documents/Papers/cashless/cashless_coding")# NOTE, your working directory is different. Adjust accordingly!!! 
dir()

### Reading RDS datasets (merged data: merged by a separate R-file)
d1 = readRDS("cashless_merged_2017_2018_190928.rds")
objects()
names(d1)
### Data preparations
# NOTE: The paper itself (section-by-section) starts on line xxx

length(unique(d1$uasid)) # num of unique respondents
dim(d1) # num trans times num variables
#

## Restricting transactions to in-person only
names(d1)
dim(d1)
table(d1$in_person)
sum(is.na(d1$in_person))# how many NAs under in person
sum(!is.na(d1$in_person))# Not NAs in person
d2 = subset(d1,  in_person %in% 1)# restriction to in-person payments only
dim(d2)# num trans
length(unique(d2$uasid))# num of respondents (in-person only)
d2 = subset(d2,  type == "expenditure")# restriction expenditures only
dim(d2)# num trans
length(unique(d2$uasid))# num of respondents (in-person only)
# restricting to to expenditures only

## restricting to merchant type: 1:7 only
table(d2$merch)
d3 = subset(d2, merch %in% 1:7)# changed from 6 to 7 190927
# 1 - Grocery stores, convenience stores without gas stations, pharmacies
# 2 - Gas stations
# 3 - Sit-down restaurants and bars
# 4 - Fast food restaurants, coffee shops, cafeterias, food trucks
# 5 - General merchandise stores, department stores, other stores, online shopping
# 6 - General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.
# 7 - Arts, entertainment, recreation
dim(d3)# num transactions in merchants 1:7
length(unique(d3$uasid))# num respondents
d4 = d3 %>% filter(merch %in% 1:7) %>% droplevels()# delete unused levels (0s)
table(d4$merch)
sum(table(d4$merch))
#
## Examining and restricting to dominant payment instruments (PI)
# 0 - Multiple payment methods
# 1 - Cash
# 2 - Check
# 3 - Credit card
# 4 - Debit card
# 5 - Prepaid/gift/EBT card
table(d4$pi)# distribution of transactions by PI
percent(prop.table(table(d4$pi)))
dim(d4)
# Below, removing PI used less than 1%
d5 = subset(d4, pi %in% 1:5)
dim(d5)
length(unique(d5$uasid))
nrow(d5) - nrow(d4)# num payments lost by restricting PI
length(unique(d5$uasid)) - length(unique(d4$uasid)) # num respondents lost by restricting PI
table(d5$pi)
sum(table(d5$pi))# Number of trans 
length(unique(d5$uasid))# Number of respondents 
# Below, select only 4 used variables from the transaction dataset and xxx variables from the survey (adoption and assessments)
names(d5)
d6 = subset(d5, select = c("uasid", "weight_1", "weight_2", "amnt", "pi", "merch", "date", "hh_size", "age", "gender", "employed", "married", "education", "income", "year", "bnk_acnt_adopt", "chk_adopt", "cc_adopt", "dc_adopt", "svc_adopt", "assess_cost_cash", "assess_cost_check", "assess_cost_debit", "assess_cost_credit", "assess_cost_prepaid", "assess_acceptance_cash", "assess_acceptance_check", "assess_acceptance_debit", "assess_acceptance_credit", "assess_acceptance_prepaid", "assess_convenience_cash", "assess_convenience_check", "assess_convenience_debit", "assess_convenience_credit", "assess_convenience_prepaid", "assess_security_cash", "assess_security_check", "assess_security_debit", "assess_security_credit", "assess_security_prepaid", "assess_setup_cash", "assess_setup_check", "assess_setup_debit", "assess_setup_credit", "assess_setup_prepaid", "assess_record_cash", "assess_record_check", "assess_record_debit", "assess_record_credit", "assess_record_prepaid"))

# Below, give names to 5 PI (removing other PI levels)
table(d6$pi)
str(d6$pi)
head(d6$pi)
d7 = d6 %>% filter(pi %in% 1:5) %>% droplevels()# delete unused levels (0s)
table(d7$pi)
dim(d7)
levels(d7$pi)
d8 = d7
levels(d8$pi) =  c("cash", "check", "credit", "debit", "prepaid")
table(d8$pi)
dim(d8)

## adding var with respondent's adoption profile: Both_cards, DC_only, CC_only, None. Then, removing NAs. 
# Note: Adopt is first used in Figure 2 (not in any Table)
# Note: From cashless_190613b, changed adoption profile to match those in the tables. 
d9 = d8
table(d9$bnk_acnt_adopt)
length(unique(d9))
# num trans by banked and unbanked. 
unbanked_tran = subset(d9, bnk_acnt_adopt == 0)
nrow(unbanked_tran)# trans by unbanked
length(unique(unbanked_tran$uasid))# num of unbanked
length(unique(unbanked_tran$uasid))/length(unique(d9$uasid))# fraction of unbanked respondents
table(unbanked_tran$pi)# some inconsistency => need to redefine unbanked as those who also don't have credit and debit cards. Those who have debit card trans may have mistakenly stated that they don't have a bank account. 
#
d9$adopt[d9$dc_adopt==1 & d9$cc_adopt==1] = "Both_cards"
d9$adopt[d9$cc_adopt==0 & pi != "credit"] = "No_cc"
d9$adopt[d9$dc_adopt==0 & pi != "debit"] = "No_dc"
# d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0] = "None" # split below
d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0  & d9$bnk_acnt_adopt==1 & d9$pi != "credit" & d9$pi != "debit"] = "None_banked"
d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0  & d9$bnk_acnt_adopt==0 & d9$pi != "credit" & d9$pi != "debit"] = "None_unbanked"
table(d9$adopt)# trans vol vs. adoption 
d9$adopt = as.factor(d9$adopt)
levels(d9$adopt)
summary(d9$adopt)
#
#
# removing NAs from adopt
dim(d9)
d10 = d9[!is.na(d9$adopt), ]
dim(d10)
nrow(d10) - nrow(d9) # num trans lost be removing adopt==NA
d10_unique = d10[!duplicated(d10$uasid), ] # data set containing each resp only once (not to be used for trans stats, only adoption stats)
dim(d10_unique)

# Below, create a df with one row per respondent
# All (Full) in_person (all merchant) sample (d10)
(all_num_resp = length(unique(d10$uasid)))# num respondents
(all_num_resp_perc = percent(all_num_resp/all_num_resp))# % num resp
(all_val_per_resp = sum(d10$amnt, na.rm = T)/all_num_resp)# avg trans value per resp
# below, I create a df with unique id and weight (not needed for ALL set, but just testing)
all_unique_id = d10[!duplicated(d10$uasid),  ]
 dim(all_unique_id)
(all_avg_hhincome = mean(all_unique_id$income, na.rm = T))# avg income
(all_median_hhincome = median(all_unique_id$income, na.rm = T))# median income
(all_avg_age = mean(all_unique_id$age, na.rm = T))# avg age
(all_median_age = median(all_unique_id$age, na.rm = T))# median age
# #

### Figure 3: Sequal to Figure 2, box plot w.r.t income
names(d10)
dim(d10)# num payments
length(unique(d10$uasid))# num respondents
# Dividing into income groups
income25 = subset(d10, income < 25000)
income49 = subset(d10, income >= 25000 & income < 49999)
income74 = subset(d10, income >= 50000 & income < 74999)
income99 = subset(d10, income >= 75000 & income < 99999)
incomeinf = subset(d10, income >= 100000)
nrow(income25)
nrow(income49)
nrow(income74)
nrow(income99)
nrow(incomeinf)
length(unique(income25$uasid))
length(unique(income49$uasid))
length(unique(income74$uasid))
length(unique(income99$uasid))
length(unique(incomeinf$uasid))

frac_cash_all = d10 %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_all)
nrow(frac_cash_all)
#
frac_cash_income25 = income25 %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_income25)
nrow(frac_cash_income25)# num payments
#
frac_cash_income49 = income49 %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_income49)
nrow(frac_cash_income49)# num payments
#
frac_cash_income74 = income74 %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_income74)
nrow(frac_cash_income74)# num payments
#
frac_cash_income99 = income99 %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_income99)
nrow(frac_cash_income99)# num payments
#
frac_cash_incomeinf = incomeinf %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_incomeinf)
nrow(frac_cash_incomeinf)# num payments
#
frac_cash_all_vec = frac_cash_all %>% pull(2)# converting tibble col to vec
frac_cash_income25_vec = frac_cash_income25 %>% pull(2)# converting tibble col to vec
frac_cash_income49_vec = frac_cash_income49 %>% pull(2)# converting tibble col to vec
frac_cash_income74_vec = frac_cash_income74 %>% pull(2)# converting tibble col to vec
frac_cash_income99_vec = frac_cash_income99 %>% pull(2)# converting tibble col to vec
frac_cash_incomeinf_vec = frac_cash_incomeinf %>% pull(2)# converting tibble col to vec
#
(frac_cash_all_med = median(frac_cash_all_vec))# median
(frac_cash_income25_med = median(frac_cash_income25_vec))# median
(frac_cash_income49_med = median(frac_cash_income49_vec))# median
(frac_cash_income74_med = median(frac_cash_income74_vec))# median
(frac_cash_income99_med = median(frac_cash_income99_vec))# median
(frac_cash_incomeinf_med = median(frac_cash_incomeinf_vec))# median
#
(frac_cash_all_avg = mean(frac_cash_all_vec))# mean
(frac_cash_income25_avg = mean(frac_cash_income25_vec))# mean
(frac_cash_income49_avg = mean(frac_cash_income49_vec))# mean
(frac_cash_income74_med = mean(frac_cash_income74_vec))# mean
(frac_cash_income99_med = mean(frac_cash_income99_vec))# mean
(frac_cash_incomeinf_med = mean(frac_cash_incomeinf_vec))# mean
#
(frac_cash_all_1st = quantile(frac_cash_all_vec, 0.25))# 1st quantile
(frac_cash_income25_1st = quantile(frac_cash_income25_vec, 0.25))# quantile
(frac_cash_income49_1st = quantile(frac_cash_income49_vec, 0.25))# quantile
(frac_cash_income74_1st = quantile(frac_cash_income74_vec, 0.25))# quantile
(frac_cash_income99_1st = quantile(frac_cash_income99_vec, 0.25))# quantile
(frac_cash_incomeinf_1st = quantile(frac_cash_incomeinf_vec, 0.25))# 1st quantile
#
(frac_cash_all_3rd = quantile(frac_cash_all_vec, 0.75))# 3rd quantile
(frac_cash_income25_3rd = quantile(frac_cash_income25_vec, 0.75))# 3rd quantile
(frac_cash_income49_3rd = quantile(frac_cash_income49_vec, 0.75))# 3rd quantile
(frac_cash_income74_3rd = quantile(frac_cash_income74_vec, 0.75))# 3rd quantile
(frac_cash_income99_3rd = quantile(frac_cash_income99_vec, 0.75))# 3rd quantile
(frac_cash_incomeinf_3rd = quantile(frac_cash_incomeinf_vec, 0.75))# 3rd quantile
#
summary(frac_cash_all_vec)
summary(frac_cash_income25_vec)
summary(frac_cash_income49_vec)
summary(frac_cash_income74_vec)
summary(frac_cash_income99_vec)
summary(frac_cash_incomeinf_vec)
#
# Figure 3 (sequel to Fig.2): Box plots by income groups, # started Line 233 Ends line 259
par(mar = c(2.5,4,1,2))
boxplot(100*frac_cash_all_vec, 100*frac_cash_income25_vec, 100*frac_cash_income49_vec, 100*frac_cash_income74_vec, 100*frac_cash_income99_vec, 100*frac_cash_incomeinf_vec, axes=F, col = c("grey","red","magenta","yellow", "cyan", "green"),  names = c("Full sample","Both cards","No credit", "No debit", "None banked", "None unbanked"), notch = T, ylab = "Distribution of cash use (%) by  respondent")
axis(2, at=seq(0,100, 10),las=2)
axis(1,at=c(1:6), labels=c("All","$25k or less","$25k-$49.9k", "$50k-$74.9k", "$75k-$99.9k", "$100k +"), cex.axis=0.95)
# add text 1st quantile value (above line)
text(1, 100*frac_cash_all_1st + 3, labels = 100*round(frac_cash_all_1st, digits = 3))
text(2, 100*frac_cash_income25_1st + 3, labels = 100*round(frac_cash_income25_1st, digits = 3))
text(3, 100*frac_cash_income49_1st + 3, labels = 100*round(frac_cash_income49_1st, digits = 3))
text(4, 100*frac_cash_income74_1st + 3, labels = 100*round(frac_cash_income74_1st, digits = 3))
text(5, 100*frac_cash_income99_1st + 3, labels = 100*round(frac_cash_income99_1st, digits = 3))
text(6, 100*frac_cash_incomeinf_1st + 3, labels = 100*round(frac_cash_incomeinf_1st, digits = 3))
# add text median value (below line)
text(1, 100*frac_cash_all_med - 3, labels = 100*round(frac_cash_all_med, digits = 3))
text(2, 100*frac_cash_income25_med - 3, labels = 100*round(frac_cash_income25_med, digits = 3))
text(3, 100*frac_cash_income49_med - 3, labels = 100*round(frac_cash_income49_med, digits = 3))
text(4, 100*frac_cash_income74_med - 3, labels = 100*round(frac_cash_income74_med, digits = 3))
text(5, 100*frac_cash_income99_med - 3, labels = 100*round(frac_cash_income99_med, digits = 3))
text(6, 100*frac_cash_incomeinf_med - 3, labels = 100*round(frac_cash_incomeinf_med, digits = 3))
# add text 3rd quantile value (above line)
text(1, 100*frac_cash_all_3rd - 3, labels = 100*round(frac_cash_all_3rd, digits = 3))
text(2, 100*frac_cash_income25_3rd - 3, labels = 100*round(frac_cash_income25_3rd, digits = 3))
text(3, 100*frac_cash_income49_3rd - 3, labels = 100*round(frac_cash_income49_3rd, digits = 3))
text(4, 100*frac_cash_income74_3rd - 3, labels = 100*round(frac_cash_income74_3rd, digits = 3))
text(5, 100*frac_cash_income99_3rd - 3, labels = 100*round(frac_cash_income99_3rd, digits = 3))
text(6, 100*frac_cash_incomeinf_3rd - 3, labels = 100*round(frac_cash_incomeinf_3rd, digits = 3))
# End of Figure 3 started Line 233 Ends line 259

## Section 5, Figure 5 sequel to Figure 4: Cost assessments by income (instead of card adoption), starts Line 260

all_unique_id = d10[!duplicated(d10$uasid),  ]
dim(all_unique_id)
(all_avg_hhincome = mean(all_unique_id$income, na.rm = T))# avg income
(all_median_hhincome = median(all_unique_id$income, na.rm = T))# median income
(all_avg_age = mean(all_unique_id$age, na.rm = T))# avg age
(all_median_age = median(all_unique_id$age, na.rm = T))# median age
#
income25_unique_id = income25[!duplicated(income25$uasid),  ]
dim(income25_unique_id)
(income25_avg_hhincome = mean(income25_unique_id$income, na.rm = T))# avg income
(income25_median_hhincome = median(income25_unique_id$income, na.rm = T))# median income
(income25_avg_age = mean(income25_unique_id$age, na.rm = T))# avg age
(income25_median_age = median(income25_unique_id$age, na.rm = T))# median age
#
income49_unique_id = income49[!duplicated(income49$uasid),  ]
dim(income49_unique_id)
(income49_avg_hhincome = mean(income49_unique_id$income, na.rm = T))# avg income
(income49_median_hhincome = median(income49_unique_id$income, na.rm = T))# median income
(income49_avg_age = mean(income49_unique_id$age, na.rm = T))# avg age
(income49_median_age = median(income49_unique_id$age, na.rm = T))# median age
#
income74_unique_id = income74[!duplicated(income74$uasid),  ]
dim(income74_unique_id)
(income74_avg_hhincome = mean(income74_unique_id$income, na.rm = T))# avg income
(income74_median_hhincome = median(income74_unique_id$income, na.rm = T))# median income
(income74_avg_age = mean(income74_unique_id$age, na.rm = T))# avg age
(income74_median_age = median(income74_unique_id$age, na.rm = T))# median age
#
income99_unique_id = income99[!duplicated(income99$uasid),  ]
dim(income99_unique_id)
(income99_avg_hhincome = mean(income99_unique_id$income, na.rm = T))# avg income
(income99_median_hhincome = median(income99_unique_id$income, na.rm = T))# median income
(income99_avg_age = mean(income99_unique_id$age, na.rm = T))# avg age
(income99_median_age = median(income99_unique_id$age, na.rm = T))# median age
#
incomeinf_unique_id = incomeinf[!duplicated(incomeinf$uasid),  ]
dim(incomeinf_unique_id)
(incomeinf_avg_hhincome = mean(incomeinf_unique_id$income, na.rm = T))# avg income
(incomeinf_median_hhincome = median(incomeinf_unique_id$income, na.rm = T))# median income
(incomeinf_avg_age = mean(incomeinf_unique_id$age, na.rm = T))# avg age
(incomeinf_median_age = median(incomeinf_unique_id$age, na.rm = T))# median age
#
## Cash ranking begins
# All starts: ranking cost of cash: 1 to 5
(all_cash_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 1, ]))
(all_cash_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 2, ]))
(all_cash_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 3, ]))
(all_cash_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 4, ]))
(all_cash_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 5, ]))
# income25 starts: ranking cost of cash: 1 to 5
(income25_cash_vote1 = nrow(income25_unique_id[income25_unique_id$assess_cost_cash == 1, ]))
(income25_cash_vote2 = nrow(income25_unique_id[income25_unique_id$assess_cost_cash == 2, ]))
(income25_cash_vote3 = nrow(income25_unique_id[income25_unique_id$assess_cost_cash == 3, ]))
(income25_cash_vote4 = nrow(income25_unique_id[income25_unique_id$assess_cost_cash == 4, ]))
(income25_cash_vote5 = nrow(income25_unique_id[income25_unique_id$assess_cost_cash == 5, ]))
# income49 starts: ranking cost of cash: 1 to 5
(income49_cash_vote1 = nrow(income49_unique_id[income49_unique_id$assess_cost_cash == 1, ]))
(income49_cash_vote2 = nrow(income49_unique_id[income49_unique_id$assess_cost_cash == 2, ]))
(income49_cash_vote3 = nrow(income49_unique_id[income49_unique_id$assess_cost_cash == 3, ]))
(income49_cash_vote4 = nrow(income49_unique_id[income49_unique_id$assess_cost_cash == 4, ]))
(income49_cash_vote5 = nrow(income49_unique_id[income49_unique_id$assess_cost_cash == 5, ]))
# income74 starts: ranking cost of cash: 1 to 5
(income74_cash_vote1 = nrow(income74_unique_id[income74_unique_id$assess_cost_cash == 1, ]))
(income74_cash_vote2 = nrow(income74_unique_id[income74_unique_id$assess_cost_cash == 2, ]))
(income74_cash_vote3 = nrow(income74_unique_id[income74_unique_id$assess_cost_cash == 3, ]))
(income74_cash_vote4 = nrow(income74_unique_id[income74_unique_id$assess_cost_cash == 4, ]))
(income74_cash_vote5 = nrow(income74_unique_id[income74_unique_id$assess_cost_cash == 5, ]))
# income99 starts: ranking cost of cash: 1 to 5
(income99_cash_vote1 = nrow(income99_unique_id[income99_unique_id$assess_cost_cash == 1, ]))
(income99_cash_vote2 = nrow(income99_unique_id[income99_unique_id$assess_cost_cash == 2, ]))
(income99_cash_vote3 = nrow(income99_unique_id[income99_unique_id$assess_cost_cash == 3, ]))
(income99_cash_vote4 = nrow(income99_unique_id[income99_unique_id$assess_cost_cash == 4, ]))
(income99_cash_vote5 = nrow(income99_unique_id[income99_unique_id$assess_cost_cash == 5, ]))
# incomeinf starts: ranking cost of cash: 1 to 5
(incomeinf_cash_vote1 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_cash == 1, ]))
(incomeinf_cash_vote2 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_cash == 2, ]))
(incomeinf_cash_vote3 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_cash == 3, ]))
(incomeinf_cash_vote4 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_cash == 4, ]))
(incomeinf_cash_vote5 = nrow(incomeinf_unique_id[income99_unique_id$assess_cost_cash == 5, ]))

## Check ranking begins
# All starts: ranking cost of cash: 1 to 5
(all_check_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_check == 1, ]))
(all_check_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_check == 2, ]))
(all_check_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_check == 3, ]))
(all_check_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_check == 4, ]))
(all_check_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_check == 5, ]))
# income25 starts: ranking cost of check: 1 to 5
(income25_check_vote1 = nrow(income25_unique_id[income25_unique_id$assess_cost_check == 1, ]))
(income25_check_vote2 = nrow(income25_unique_id[income25_unique_id$assess_cost_check == 2, ]))
(income25_check_vote3 = nrow(income25_unique_id[income25_unique_id$assess_cost_check == 3, ]))
(income25_check_vote4 = nrow(income25_unique_id[income25_unique_id$assess_cost_check == 4, ]))
(income25_check_vote5 = nrow(income25_unique_id[income25_unique_id$assess_cost_check == 5, ]))
# income49 starts: ranking cost of check: 1 to 5
(income49_check_vote1 = nrow(income49_unique_id[income49_unique_id$assess_cost_check == 1, ]))
(income49_check_vote2 = nrow(income49_unique_id[income49_unique_id$assess_cost_check == 2, ]))
(income49_check_vote3 = nrow(income49_unique_id[income49_unique_id$assess_cost_check == 3, ]))
(income49_check_vote4 = nrow(income49_unique_id[income49_unique_id$assess_cost_check == 4, ]))
(income49_check_vote5 = nrow(income49_unique_id[income49_unique_id$assess_cost_check == 5, ]))
# income74 starts: ranking cost of check: 1 to 5
(income74_check_vote1 = nrow(income74_unique_id[income74_unique_id$assess_cost_check == 1, ]))
(income74_check_vote2 = nrow(income74_unique_id[income74_unique_id$assess_cost_check == 2, ]))
(income74_check_vote3 = nrow(income74_unique_id[income74_unique_id$assess_cost_check == 3, ]))
(income74_check_vote4 = nrow(income74_unique_id[income74_unique_id$assess_cost_check == 4, ]))
(income74_check_vote5 = nrow(income74_unique_id[income74_unique_id$assess_cost_check == 5, ]))
# income99 starts: ranking cost of check: 1 to 5
(income99_check_vote1 = nrow(income99_unique_id[income99_unique_id$assess_cost_check == 1, ]))
(income99_check_vote2 = nrow(income99_unique_id[income99_unique_id$assess_cost_check == 2, ]))
(income99_check_vote3 = nrow(income99_unique_id[income99_unique_id$assess_cost_check == 3, ]))
(income99_check_vote4 = nrow(income99_unique_id[income99_unique_id$assess_cost_check == 4, ]))
(income99_check_vote5 = nrow(income99_unique_id[income99_unique_id$assess_cost_check == 5, ]))
# incomeinf starts: ranking cost of check: 1 to 5
(incomeinf_check_vote1 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_check == 1, ]))
(incomeinf_check_vote2 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_check == 2, ]))
(incomeinf_check_vote3 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_check == 3, ]))
(incomeinf_check_vote4 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_check == 4, ]))
(incomeinf_check_vote5 = nrow(incomeinf_unique_id[income99_unique_id$assess_cost_check == 5, ]))

## Credit ranking begins
# All starts: ranking cost of cash: 1 to 5
(all_credit_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 1, ]))
(all_credit_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 2, ]))
(all_credit_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 3, ]))
(all_credit_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 4, ]))
(all_credit_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 5, ]))
# income25 starts: ranking cost of credit: 1 to 5
(income25_credit_vote1 = nrow(income25_unique_id[income25_unique_id$assess_cost_credit == 1, ]))
(income25_credit_vote2 = nrow(income25_unique_id[income25_unique_id$assess_cost_credit == 2, ]))
(income25_credit_vote3 = nrow(income25_unique_id[income25_unique_id$assess_cost_credit == 3, ]))
(income25_credit_vote4 = nrow(income25_unique_id[income25_unique_id$assess_cost_credit == 4, ]))
(income25_credit_vote5 = nrow(income25_unique_id[income25_unique_id$assess_cost_credit == 5, ]))
# income49 starts: ranking cost of credit: 1 to 5
(income49_credit_vote1 = nrow(income49_unique_id[income49_unique_id$assess_cost_credit == 1, ]))
(income49_credit_vote2 = nrow(income49_unique_id[income49_unique_id$assess_cost_credit == 2, ]))
(income49_credit_vote3 = nrow(income49_unique_id[income49_unique_id$assess_cost_credit == 3, ]))
(income49_credit_vote4 = nrow(income49_unique_id[income49_unique_id$assess_cost_credit == 4, ]))
(income49_credit_vote5 = nrow(income49_unique_id[income49_unique_id$assess_cost_credit == 5, ]))
# income74 starts: ranking cost of credit: 1 to 5
(income74_credit_vote1 = nrow(income74_unique_id[income74_unique_id$assess_cost_credit == 1, ]))
(income74_credit_vote2 = nrow(income74_unique_id[income74_unique_id$assess_cost_credit == 2, ]))
(income74_credit_vote3 = nrow(income74_unique_id[income74_unique_id$assess_cost_credit == 3, ]))
(income74_credit_vote4 = nrow(income74_unique_id[income74_unique_id$assess_cost_credit == 4, ]))
(income74_credit_vote5 = nrow(income74_unique_id[income74_unique_id$assess_cost_credit == 5, ]))
# income99 starts: ranking cost of credit: 1 to 5
(income99_credit_vote1 = nrow(income99_unique_id[income99_unique_id$assess_cost_credit == 1, ]))
(income99_credit_vote2 = nrow(income99_unique_id[income99_unique_id$assess_cost_credit == 2, ]))
(income99_credit_vote3 = nrow(income99_unique_id[income99_unique_id$assess_cost_credit == 3, ]))
(income99_credit_vote4 = nrow(income99_unique_id[income99_unique_id$assess_cost_credit == 4, ]))
(income99_credit_vote5 = nrow(income99_unique_id[income99_unique_id$assess_cost_credit == 5, ]))
# incomeinf starts: ranking cost of credit: 1 to 5
(incomeinf_credit_vote1 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_credit == 1, ]))
(incomeinf_credit_vote2 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_credit == 2, ]))
(incomeinf_credit_vote3 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_credit == 3, ]))
(incomeinf_credit_vote4 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_credit == 4, ]))
(incomeinf_credit_vote5 = nrow(incomeinf_unique_id[income99_unique_id$assess_cost_credit == 5, ]))

## Debit ranking begins
# All starts: ranking cost of debit: 1 to 5
(all_debit_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 1, ]))
(all_debit_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 2, ]))
(all_debit_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 3, ]))
(all_debit_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 4, ]))
(all_debit_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 5, ]))
# income25 starts: ranking cost of debit: 1 to 5
(income25_debit_vote1 = nrow(income25_unique_id[income25_unique_id$assess_cost_debit == 1, ]))
(income25_debit_vote2 = nrow(income25_unique_id[income25_unique_id$assess_cost_debit == 2, ]))
(income25_debit_vote3 = nrow(income25_unique_id[income25_unique_id$assess_cost_debit == 3, ]))
(income25_debit_vote4 = nrow(income25_unique_id[income25_unique_id$assess_cost_debit == 4, ]))
(income25_debit_vote5 = nrow(income25_unique_id[income25_unique_id$assess_cost_debit == 5, ]))
# income49 starts: ranking cost of debit: 1 to 5
(income49_debit_vote1 = nrow(income49_unique_id[income49_unique_id$assess_cost_debit == 1, ]))
(income49_debit_vote2 = nrow(income49_unique_id[income49_unique_id$assess_cost_debit == 2, ]))
(income49_debit_vote3 = nrow(income49_unique_id[income49_unique_id$assess_cost_debit == 3, ]))
(income49_debit_vote4 = nrow(income49_unique_id[income49_unique_id$assess_cost_debit == 4, ]))
(income49_debit_vote5 = nrow(income49_unique_id[income49_unique_id$assess_cost_debit == 5, ]))
# income74 starts: ranking cost of debit: 1 to 5
(income74_debit_vote1 = nrow(income74_unique_id[income74_unique_id$assess_cost_debit == 1, ]))
(income74_debit_vote2 = nrow(income74_unique_id[income74_unique_id$assess_cost_debit == 2, ]))
(income74_debit_vote3 = nrow(income74_unique_id[income74_unique_id$assess_cost_debit == 3, ]))
(income74_debit_vote4 = nrow(income74_unique_id[income74_unique_id$assess_cost_debit == 4, ]))
(income74_debit_vote5 = nrow(income74_unique_id[income74_unique_id$assess_cost_debit == 5, ]))
# income99 starts: ranking cost of debit: 1 to 5
(income99_debit_vote1 = nrow(income99_unique_id[income99_unique_id$assess_cost_debit == 1, ]))
(income99_debit_vote2 = nrow(income99_unique_id[income99_unique_id$assess_cost_debit == 2, ]))
(income99_debit_vote3 = nrow(income99_unique_id[income99_unique_id$assess_cost_debit == 3, ]))
(income99_debit_vote4 = nrow(income99_unique_id[income99_unique_id$assess_cost_debit == 4, ]))
(income99_debit_vote5 = nrow(income99_unique_id[income99_unique_id$assess_cost_debit == 5, ]))
# incomeinf starts: ranking cost of debit: 1 to 5
(incomeinf_debit_vote1 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_debit == 1, ]))
(incomeinf_debit_vote2 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_debit == 2, ]))
(incomeinf_debit_vote3 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_debit == 3, ]))
(incomeinf_debit_vote4 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_debit == 4, ]))
(incomeinf_debit_vote5 = nrow(incomeinf_unique_id[income99_unique_id$assess_cost_debit == 5, ]))

## prepaid ranking begins
# All starts: ranking cost of prepaid: 1 to 5
(all_prepaid_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 1, ]))
(all_prepaid_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 2, ]))
(all_prepaid_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 3, ]))
(all_prepaid_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 4, ]))
(all_prepaid_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 5, ]))
# income25 starts: ranking cost of prepaid: 1 to 5
(income25_prepaid_vote1 = nrow(income25_unique_id[income25_unique_id$assess_cost_prepaid == 1, ]))
(income25_prepaid_vote2 = nrow(income25_unique_id[income25_unique_id$assess_cost_prepaid == 2, ]))
(income25_prepaid_vote3 = nrow(income25_unique_id[income25_unique_id$assess_cost_prepaid == 3, ]))
(income25_prepaid_vote4 = nrow(income25_unique_id[income25_unique_id$assess_cost_prepaid == 4, ]))
(income25_prepaid_vote5 = nrow(income25_unique_id[income25_unique_id$assess_cost_prepaid == 5, ]))
# income49 starts: ranking cost of prepaid: 1 to 5
(income49_prepaid_vote1 = nrow(income49_unique_id[income49_unique_id$assess_cost_prepaid == 1, ]))
(income49_prepaid_vote2 = nrow(income49_unique_id[income49_unique_id$assess_cost_prepaid == 2, ]))
(income49_prepaid_vote3 = nrow(income49_unique_id[income49_unique_id$assess_cost_prepaid == 3, ]))
(income49_prepaid_vote4 = nrow(income49_unique_id[income49_unique_id$assess_cost_prepaid == 4, ]))
(income49_prepaid_vote5 = nrow(income49_unique_id[income49_unique_id$assess_cost_prepaid == 5, ]))
# income74 starts: ranking cost of prepaid: 1 to 5
(income74_prepaid_vote1 = nrow(income74_unique_id[income74_unique_id$assess_cost_prepaid == 1, ]))
(income74_prepaid_vote2 = nrow(income74_unique_id[income74_unique_id$assess_cost_prepaid == 2, ]))
(income74_prepaid_vote3 = nrow(income74_unique_id[income74_unique_id$assess_cost_prepaid == 3, ]))
(income74_prepaid_vote4 = nrow(income74_unique_id[income74_unique_id$assess_cost_prepaid == 4, ]))
(income74_prepaid_vote5 = nrow(income74_unique_id[income74_unique_id$assess_cost_prepaid == 5, ]))
# income99 starts: ranking cost of prepaid: 1 to 5
(income99_prepaid_vote1 = nrow(income99_unique_id[income99_unique_id$assess_cost_prepaid == 1, ]))
(income99_prepaid_vote2 = nrow(income99_unique_id[income99_unique_id$assess_cost_prepaid == 2, ]))
(income99_prepaid_vote3 = nrow(income99_unique_id[income99_unique_id$assess_cost_prepaid == 3, ]))
(income99_prepaid_vote4 = nrow(income99_unique_id[income99_unique_id$assess_cost_prepaid == 4, ]))
(income99_prepaid_vote5 = nrow(income99_unique_id[income99_unique_id$assess_cost_prepaid == 5, ]))
# incomeinf starts: ranking cost of prepaid: 1 to 5
(incomeinf_prepaid_vote1 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_prepaid == 1, ]))
(incomeinf_prepaid_vote2 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_prepaid == 2, ]))
(incomeinf_prepaid_vote3 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_prepaid == 3, ]))
(incomeinf_prepaid_vote4 = nrow(incomeinf_unique_id[incomeinf_unique_id$assess_cost_prepaid == 4, ]))
(incomeinf_prepaid_vote5 = nrow(incomeinf_unique_id[income99_unique_id$assess_cost_prepaid == 5, ]))

## start summarizing votes by *income* group
# All score of cost by PI
(all_cash_vote = 1*all_cash_vote1 + 2*all_cash_vote2 + 3*all_cash_vote3 + 4*all_cash_vote4 + 5*all_cash_vote5)
(all_check_vote = 1*all_check_vote1 + 2*all_check_vote2 + 3*all_check_vote3 + 4*all_check_vote4 + 5*all_check_vote5)
(all_credit_vote = 1*all_credit_vote1 + 2*all_credit_vote2 + 3*all_credit_vote3 + 4*all_credit_vote4 + 5*all_credit_vote5)
(all_debit_vote = 1*all_debit_vote1 + 2*all_debit_vote2 + 3*all_debit_vote3 + 4*all_debit_vote4 + 5*all_debit_vote5)
(all_prepaid_vote = 1*all_prepaid_vote1 + 2*all_prepaid_vote2 + 3*all_prepaid_vote3 + 4*all_prepaid_vote4 + 5*all_prepaid_vote5)
rank(c(all_cash_vote, all_check_vote, all_credit_vote, all_debit_vote, all_prepaid_vote))# Note: Highest rank = lowest cost
(all_vote_vec = c(all_cash_vote, all_check_vote, all_credit_vote, all_debit_vote, all_prepaid_vote))# vector of ALL votes for PI
(all_vote_score = all_vote_vec/nrow(all_unique_id))
#
# income25 score of cost by PI
(income25_cash_vote = 1*income25_cash_vote1 + 2*income25_cash_vote2 + 3*income25_cash_vote3 + 4*income25_cash_vote4 + 5*income25_cash_vote5)
(income25_check_vote = 1*income25_check_vote1 + 2*income25_check_vote2 + 3*income25_check_vote3 + 4*income25_check_vote4 + 5*income25_check_vote5)
(income25_credit_vote = 1*income25_credit_vote1 + 2*income25_credit_vote2 + 3*income25_credit_vote3 + 4*income25_credit_vote4 + 5*income25_credit_vote5)
(income25_debit_vote = 1*income25_debit_vote1 + 2*income25_debit_vote2 + 3*income25_debit_vote3 + 4*income25_debit_vote4 + 5*income25_debit_vote5)
(income25_prepaid_vote = 1*income25_prepaid_vote1 + 2*income25_prepaid_vote2 + 3*income25_prepaid_vote3 + 4*income25_prepaid_vote4 + 5*income25_prepaid_vote5)
# income25 score of cost by PI
(income25_cash_vote = 1*income25_cash_vote1 + 2*income25_cash_vote2 + 3*income25_cash_vote3 + 4*income25_cash_vote4 + 5*income25_cash_vote5)
(income25_check_vote = 1*income25_check_vote1 + 2*income25_check_vote2 + 3*income25_check_vote3 + 4*income25_check_vote4 + 5*income25_check_vote5)
(income25_credit_vote = 1*income25_credit_vote1 + 2*income25_credit_vote2 + 3*income25_credit_vote3 + 4*income25_credit_vote4 + 5*income25_credit_vote5)
(income25_debit_vote = 1*income25_debit_vote1 + 2*income25_debit_vote2 + 3*income25_debit_vote3 + 4*income25_debit_vote4 + 5*income25_debit_vote5)
(income25_prepaid_vote = 1*income25_prepaid_vote1 + 2*income25_prepaid_vote2 + 3*income25_prepaid_vote3 + 4*income25_prepaid_vote4 + 5*income25_prepaid_vote5)
rank(c(income25_cash_vote, income25_check_vote, income25_credit_vote, income25_debit_vote, income25_prepaid_vote))# Note: Highest rank = lowest cost
(income25_vote_vec = c(income25_cash_vote, income25_check_vote, income25_credit_vote, income25_debit_vote, income25_prepaid_vote))# vector of income25 votes for PI
(income25_vote_score = income25_vote_vec/nrow(income25_unique_id))
#
# income49 score of cost by PI
(income49_cash_vote = 1*income49_cash_vote1 + 2*income49_cash_vote2 + 3*income49_cash_vote3 + 4*income49_cash_vote4 + 5*income49_cash_vote5)
(income49_check_vote = 1*income49_check_vote1 + 2*income49_check_vote2 + 3*income49_check_vote3 + 4*income49_check_vote4 + 5*income49_check_vote5)
(income49_credit_vote = 1*income49_credit_vote1 + 2*income49_credit_vote2 + 3*income49_credit_vote3 + 4*income49_credit_vote4 + 5*income49_credit_vote5)
(income49_debit_vote = 1*income49_debit_vote1 + 2*income49_debit_vote2 + 3*income49_debit_vote3 + 4*income49_debit_vote4 + 5*income49_debit_vote5)
(income49_prepaid_vote = 1*income49_prepaid_vote1 + 2*income49_prepaid_vote2 + 3*income49_prepaid_vote3 + 4*income49_prepaid_vote4 + 5*income49_prepaid_vote5)
# income49 score of cost by PI
(income49_cash_vote = 1*income49_cash_vote1 + 2*income49_cash_vote2 + 3*income49_cash_vote3 + 4*income49_cash_vote4 + 5*income49_cash_vote5)
(income49_check_vote = 1*income49_check_vote1 + 2*income49_check_vote2 + 3*income49_check_vote3 + 4*income49_check_vote4 + 5*income49_check_vote5)
(income49_credit_vote = 1*income49_credit_vote1 + 2*income49_credit_vote2 + 3*income49_credit_vote3 + 4*income49_credit_vote4 + 5*income49_credit_vote5)
(income49_debit_vote = 1*income49_debit_vote1 + 2*income49_debit_vote2 + 3*income49_debit_vote3 + 4*income49_debit_vote4 + 5*income49_debit_vote5)
(income49_prepaid_vote = 1*income49_prepaid_vote1 + 2*income49_prepaid_vote2 + 3*income49_prepaid_vote3 + 4*income49_prepaid_vote4 + 5*income49_prepaid_vote5)
rank(c(income49_cash_vote, income49_check_vote, income49_credit_vote, income49_debit_vote, income49_prepaid_vote))# Note: Highest rank = lowest cost
(income49_vote_vec = c(income49_cash_vote, income49_check_vote, income49_credit_vote, income49_debit_vote, income49_prepaid_vote))# vector of income49 votes for PI
(income49_vote_score = income49_vote_vec/nrow(income49_unique_id))
#
# income74 score of cost by PI
(income74_cash_vote = 1*income74_cash_vote1 + 2*income74_cash_vote2 + 3*income74_cash_vote3 + 4*income74_cash_vote4 + 5*income74_cash_vote5)
(income74_check_vote = 1*income74_check_vote1 + 2*income74_check_vote2 + 3*income74_check_vote3 + 4*income74_check_vote4 + 5*income74_check_vote5)
(income74_credit_vote = 1*income74_credit_vote1 + 2*income74_credit_vote2 + 3*income74_credit_vote3 + 4*income74_credit_vote4 + 5*income74_credit_vote5)
(income74_debit_vote = 1*income74_debit_vote1 + 2*income74_debit_vote2 + 3*income74_debit_vote3 + 4*income74_debit_vote4 + 5*income74_debit_vote5)
(income74_prepaid_vote = 1*income74_prepaid_vote1 + 2*income74_prepaid_vote2 + 3*income74_prepaid_vote3 + 4*income74_prepaid_vote4 + 5*income74_prepaid_vote5)
# income74 score of cost by PI
(income74_cash_vote = 1*income74_cash_vote1 + 2*income74_cash_vote2 + 3*income74_cash_vote3 + 4*income74_cash_vote4 + 5*income74_cash_vote5)
(income74_check_vote = 1*income74_check_vote1 + 2*income74_check_vote2 + 3*income74_check_vote3 + 4*income74_check_vote4 + 5*income74_check_vote5)
(income74_credit_vote = 1*income74_credit_vote1 + 2*income74_credit_vote2 + 3*income74_credit_vote3 + 4*income74_credit_vote4 + 5*income74_credit_vote5)
(income74_debit_vote = 1*income74_debit_vote1 + 2*income74_debit_vote2 + 3*income74_debit_vote3 + 4*income74_debit_vote4 + 5*income74_debit_vote5)
(income74_prepaid_vote = 1*income74_prepaid_vote1 + 2*income74_prepaid_vote2 + 3*income74_prepaid_vote3 + 4*income74_prepaid_vote4 + 5*income74_prepaid_vote5)
rank(c(income74_cash_vote, income74_check_vote, income74_credit_vote, income74_debit_vote, income74_prepaid_vote))# Note: Highest rank = lowest cost
(income74_vote_vec = c(income74_cash_vote, income74_check_vote, income74_credit_vote, income74_debit_vote, income74_prepaid_vote))# vector of income74 votes for PI
(income74_vote_score = income74_vote_vec/nrow(income74_unique_id))
#
# income99 score of cost by PI
(income99_cash_vote = 1*income99_cash_vote1 + 2*income99_cash_vote2 + 3*income99_cash_vote3 + 4*income99_cash_vote4 + 5*income99_cash_vote5)
(income99_check_vote = 1*income99_check_vote1 + 2*income99_check_vote2 + 3*income99_check_vote3 + 4*income99_check_vote4 + 5*income99_check_vote5)
(income99_credit_vote = 1*income99_credit_vote1 + 2*income99_credit_vote2 + 3*income99_credit_vote3 + 4*income99_credit_vote4 + 5*income99_credit_vote5)
(income99_debit_vote = 1*income99_debit_vote1 + 2*income99_debit_vote2 + 3*income99_debit_vote3 + 4*income99_debit_vote4 + 5*income99_debit_vote5)
(income99_prepaid_vote = 1*income99_prepaid_vote1 + 2*income99_prepaid_vote2 + 3*income99_prepaid_vote3 + 4*income99_prepaid_vote4 + 5*income99_prepaid_vote5)
# income99 score of cost by PI
(income99_cash_vote = 1*income99_cash_vote1 + 2*income99_cash_vote2 + 3*income99_cash_vote3 + 4*income99_cash_vote4 + 5*income99_cash_vote5)
(income99_check_vote = 1*income99_check_vote1 + 2*income99_check_vote2 + 3*income99_check_vote3 + 4*income99_check_vote4 + 5*income99_check_vote5)
(income99_credit_vote = 1*income99_credit_vote1 + 2*income99_credit_vote2 + 3*income99_credit_vote3 + 4*income99_credit_vote4 + 5*income99_credit_vote5)
(income99_debit_vote = 1*income99_debit_vote1 + 2*income99_debit_vote2 + 3*income99_debit_vote3 + 4*income99_debit_vote4 + 5*income99_debit_vote5)
(income99_prepaid_vote = 1*income99_prepaid_vote1 + 2*income99_prepaid_vote2 + 3*income99_prepaid_vote3 + 4*income99_prepaid_vote4 + 5*income99_prepaid_vote5)
rank(c(income99_cash_vote, income99_check_vote, income99_credit_vote, income99_debit_vote, income99_prepaid_vote))# Note: Highest rank = lowest cost
(income99_vote_vec = c(income99_cash_vote, income99_check_vote, income99_credit_vote, income99_debit_vote, income99_prepaid_vote))# vector of income99 votes for PI
(income99_vote_score = income99_vote_vec/nrow(income99_unique_id))
#
# incomeinf score of cost by PI
(incomeinf_cash_vote = 1*incomeinf_cash_vote1 + 2*incomeinf_cash_vote2 + 3*incomeinf_cash_vote3 + 4*incomeinf_cash_vote4 + 5*incomeinf_cash_vote5)
(incomeinf_check_vote = 1*incomeinf_check_vote1 + 2*incomeinf_check_vote2 + 3*incomeinf_check_vote3 + 4*incomeinf_check_vote4 + 5*incomeinf_check_vote5)
(incomeinf_credit_vote = 1*incomeinf_credit_vote1 + 2*incomeinf_credit_vote2 + 3*incomeinf_credit_vote3 + 4*incomeinf_credit_vote4 + 5*incomeinf_credit_vote5)
(incomeinf_debit_vote = 1*incomeinf_debit_vote1 + 2*incomeinf_debit_vote2 + 3*incomeinf_debit_vote3 + 4*incomeinf_debit_vote4 + 5*incomeinf_debit_vote5)
(incomeinf_prepaid_vote = 1*incomeinf_prepaid_vote1 + 2*incomeinf_prepaid_vote2 + 3*incomeinf_prepaid_vote3 + 4*incomeinf_prepaid_vote4 + 5*incomeinf_prepaid_vote5)
# incomeinf score of cost by PI
(incomeinf_cash_vote = 1*incomeinf_cash_vote1 + 2*incomeinf_cash_vote2 + 3*incomeinf_cash_vote3 + 4*incomeinf_cash_vote4 + 5*incomeinf_cash_vote5)
(incomeinf_check_vote = 1*incomeinf_check_vote1 + 2*incomeinf_check_vote2 + 3*incomeinf_check_vote3 + 4*incomeinf_check_vote4 + 5*incomeinf_check_vote5)
(incomeinf_credit_vote = 1*incomeinf_credit_vote1 + 2*incomeinf_credit_vote2 + 3*incomeinf_credit_vote3 + 4*incomeinf_credit_vote4 + 5*incomeinf_credit_vote5)
(incomeinf_debit_vote = 1*incomeinf_debit_vote1 + 2*incomeinf_debit_vote2 + 3*incomeinf_debit_vote3 + 4*incomeinf_debit_vote4 + 5*incomeinf_debit_vote5)
(incomeinf_prepaid_vote = 1*incomeinf_prepaid_vote1 + 2*incomeinf_prepaid_vote2 + 3*incomeinf_prepaid_vote3 + 4*incomeinf_prepaid_vote4 + 5*incomeinf_prepaid_vote5)
rank(c(incomeinf_cash_vote, incomeinf_check_vote, incomeinf_credit_vote, incomeinf_debit_vote, incomeinf_prepaid_vote))# Note: Highest rank = lowest cost
(incomeinf_vote_vec = c(incomeinf_cash_vote, incomeinf_check_vote, incomeinf_credit_vote, incomeinf_debit_vote, incomeinf_prepaid_vote))# vector of incomeinf votes for PI
(incomeinf_vote_score = incomeinf_vote_vec/nrow(incomeinf_unique_id))

## Convert this cost ranking to graphs
# Below ranking by 5 subsamples of respondents (5 = lowest cost)
all_vote_score
income25_vote_score
income49_vote_score
income74_vote_score
income99_vote_score
incomeinf_vote_score
# Below, reversing the score (1 = lowest score, 5 = highest) revised 190709, line 1003
(all_vote_score_rev = 6- all_vote_score)# skip All in graph
(income25_vote_score_rev = 6- income25_vote_score)
(income49_vote_score_rev = 6- income49_vote_score)
(income74_vote_score_rev = 6- income74_vote_score)
(income99_vote_score_rev = 6- income99_vote_score)
(incomeinf_vote_score_rev = 6- incomeinf_vote_score)
#
plot(income25_vote_score_rev, pch = 15, col = "darkgreen", cex = 3, axes = F, ylim = c(1.2, 3.4), ylab = "Payment insturment cost ratings", xlab = "")
points(income49_vote_score_rev, pch = 16, col = "magenta", cex = 3)
points(income74_vote_score_rev, pch = 18, col = "black", cex = 3)
points(income99_vote_score_rev, pch = 17, col = "blue", cex = 3)
points(incomeinf_vote_score_rev, pch = 17, col = "red", cex = 3)
axis(2, at=seq(1.2, 3.4, 0.2),las=2)
axis(1,at=c(1:5), labels=c("Cash","Check","Credit card", "Debit card", "Prepaid card"))
#axis(4, at=seq(0,100, 10),las=2)
legend(2.5,2.0,c("$25k or less","$25k-$49.9k","$50k-$74.9k", "$75k-$99.9k", "$100k +"), col = c("darkgreen", "magenta", "black", "blue", "red"), pch = c(15, 16, 18, 17), pt.cex = 2)# pt.cex increases symbol size only

# End of Section 5, Figure 5 sequel to Figure 4: Cost assessments by income (instead of card adoption), starts Line 260


### End of cashless_fig_3_5_xxxxxx.R code ###
