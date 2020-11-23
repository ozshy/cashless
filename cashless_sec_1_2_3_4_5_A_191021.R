# cashless_sec_1_2_3_4_5_A_191020.R for cashless-54.tex submitted AEJ policy
# cashless_main_190930.R Moving random utility (multinomial, Section 5) analysis to a separte R-file
# cashless_190927-8.R Major revision. Adding 7th merch type. Other changes from the 3 referee reports. Removing mnlogit. New merged file with bnk_acnt_adopt 
# cashless_190826.R Also, (line 75-76) create table num tran by merchant type (6 types)
# cashless_190826.R Experimenting with mnlogit to replace mlogit. I simply replicate mnlogit after existing mlogit and then try run with individual effects. mlogit starts line 1266. Overall, same results for both, slighly different results for noccyesdc and none (perhaps, because I don't delete the unused pi observations). Stick to mlogit is OK. 
# cashless_190801.R Changing random utility mlogit to non-check transactions, Line 1264 
# cashless_190731.R Last version with random utility mlogit all tranactions
# cashless_190722.R Major revision of the cashless paper 
# 1. combining 2017 with 2018 & 2. adding weights
#cashless_190707.R Adding Discrete choice cash vs. prepaid. Starts Line 1226. New section 5: Estimating the "price" coefficient ratios as an indicator of cost of substitution from cash to PP. 
#cashless_190709.R Revising Figure 4  (1 = lowest score, 5 = highest) revised 190709, line 1003, revised on cashless_34.tex
#cashless_190613b.R revising Figure 2 (adoption profile now matches the tables)
#cashless_190609.R Line 156 creating d10 from inst3 before Figure 1 and Table 1 to remove NAs for card adoption profile => revising all figs and tables. Adding regressions new section 4.
#cashless_190607.R improving Figure 3 (adding more stat on Figure itself)
#cashless_190413.R Preparing final version to follow the order of sections, tables, and graphs in the paper (cashless-17.tex). Also, using latest 2017 DCPC data
#cashless_190411.R New assessment Table 3 in paper
#cashless_190410 restricting data to merch=1:6 (note: this paper is not about cashless society, it is about cashless stores)
#cashless_190403.R switching to 2017 DCSC/SCPC: (uasid instead of uasid, "work_oth" instead of "work_other", "income" needs to be converted from categories to numbers)
#cashless_190327.R Last 2018 data. Fixing income, now using "income_hh" from indlevel (instead of from tranlevel that had many NAs). Using Revised RDS data Kevin sent today. 
#cashless_190315.R start writing the paper cashless_1.tex
#cashless_190312.R Looking at in-person for all merchant types. Moving merchants 1:5 and later 1&5 to separate sections (if any)
#cashless_190310.R 
# who is paying cash, and estimate welfare loss of going cashless

### The following packages are used:
library(formattable)# has percent function
library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
library(ggplot2)
library(spatstat) # for weighted.median
library(mfx)
library(regclass) # for confusion_matrix
library(nnet) # for multinomial logit
library(AER) # for p-values of nnet multinom logit coeftest(regression)
library(mlogit)
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

### Section 2: Data info
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
d9$adopt[d9$cc_adopt==0 & d9$pi != "credit"] = "No_cc"
d9$adopt[d9$dc_adopt==0 & d9$pi != "debit"] = "No_dc"
# d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0] = "None" # split below
d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0 & d9$bnk_acnt_adopt==1 & d9$pi != "credit" & d9$pi != "debit"] = "None_banked"
d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0 & d9$bnk_acnt_adopt==0 & d9$pi != "credit" & d9$pi != "debit"] = "None_unbanked"
table(d9$adopt)# trans vol vs. adoption 
d9$adopt = as.factor(d9$adopt)
levels(d9$adopt)
summary(d9$adopt)
#
# removing NAs from adopt
dim(d9)
d10 = d9[!is.na(d9$adopt), ]
dim(d10)
nrow(d10) - nrow(d9) # num trans lost be removing adopt==NA
d10_unique = d10[!duplicated(d10$uasid), ] # data set containing each resp only once (not to be used for trans stats, only adoption stats)
dim(d10_unique)
table(d10$adopt)

### Section 1 begins 
#Note: For Figure 1 see below, although moved to Section 1)
# stats stated at the beginning of the section
# nrow(trans3)# num in-person trans (all merchants)
# length(unique(trans3$uasid))# num of respondents (in-person all merchants)
# nrow(trans6)# Number of trans (restricted to: in person, 6 merchant types, 5 PI)
# length(unique(trans6$uasid))# Number of respondents 
# #
# nrow(d10)# num trans after removing NAs on those not reporting card adoption
# d10_unique = d10[!duplicated(d10$uasid), ]
# nrow(d10_unique)# num resp after removing NAs for those not reporting card adoption

## Figure 1: PI w.r.t. payment amounts (Now in Section 1)
# Top panel: amount <= $100
(seq100 = seq(0, 100, 5))
plot(pi~amnt, data = subset(d10, d10$amnt<=100), col=rainbow(5),
     ylab = "Method of payment (Instrument)", xlab="Payment dollar value (amount below or equal $100)", axes=T, breaks = seq100) # generates $5 bin sizes 
legend("bottomleft", c("Cash", "Check", "Credit card", "Debit card", "Prepaid card"), fill=rainbow(5))
# How many trans and resp in the $0-$100 range?
nrow(subset(d10, amnt <= 100 ))# num trans
length(unique(subset(d10, amnt <= 100)$uasid))# num of resp
#
# Bottom panel:  $100 < amnt <= $400
(seq100400 = seq(100, 400, 50))
plot(pi~amnt, data = subset(d10, amnt>100 & amnt <=400), col=rainbow(5),
     ylab = "Method of payment (Instrument)", xlab="Payment dollar value (amount between $100 and $400)",
     axes = T, breaks = seq100400) # generates $5 bin sizes
legend("bottomleft", c("Cash", "Check", "Credit card", "Debit card", "Prepaid card"), fill=rainbow(5))
#
# Verifying quoted numbers in the caption of Figure 1
# Below, num of trans below $5 paid with cash
nrow(d10[d10$amnt <= 100, ])# num trans below $100
length(unique(d10[d10$amnt <= 100, ]$uasid)) # resp for the above
nrow(d10[d10$amnt > 100 & d10$amnt <= 400, ])# num trans betweem $100 and $400
length(unique(d10[d10$amnt > 100 & d10$amnt <= 400, ]$uasid)) # resp for the above

## Verifying text in Section 1 explaining Figure 1
percent(nrow(d10[d10$amnt <= 5 & d10$pi == "cash", ])/nrow(d10[d10$amnt <= 5, ]))
percent(nrow(d10[d10$amnt > 5 & d10$amnt <= 10 & d10$pi == "cash", ])/nrow(d10[d10$amnt <= 5 & d10$amnt <= 10, ]))
percent(nrow(d10[d10$amnt > 100 & d10$amnt <= 400 & d10$pi == "cash", ])/nrow(d10[d10$amnt > 100 & d10$amnt <= 400, ]))# share of cash for trans between $100 and $400
#percent(nrow(d10[d10$amnt >= 300 & d10$amnt <= 400 & d10$pi == "cash", ])/nrow(d10[d10$amnt >= 300 & d10$amnt <= 400, ]))

### Section 3 (Who doesn't have a card?) begins

### Table 1 in paper: Finding the number and % of Rs with and without cc and dc (weighted and unweighted). Starts Line 197 Table printed on line 467 
# Below, rescaling the weights
nrow(d10)
sum(d10$weight_1)# verify that sum of weights = num transactions
# 
#Rescaling weight_1 for all_trans for transaction stats
d10$weight_1_trans = nrow(d10) * d10$weight_1/sum(d10$weight_1)
sum(d10$weight_1_trans)
nrow(d10)
#
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
#
#Rescaling weight_1 for all_unique_id subset. Use these for num resp stats
all_unique_id$weight_1_id = nrow(all_unique_id) * all_unique_id$weight_1/sum(all_unique_id$weight_1)
sum(all_unique_id$weight_1_id)
# doing above stats with weights
(all_resp_w = sum(all_unique_id$weight_1_id))
(all_resp_perc_w = percent(all_resp_w/all_resp_w))
(all_avg_hhincome_w = mean(all_unique_id$income*all_unique_id$weight_1_id, na.rm = T))# avg hhincome weighted
(all_avg_age_w = mean(all_unique_id$age*all_unique_id$weight_1_id, na.rm = T))# avg age weighted

# All transaction stats
(all_num_trans = nrow(d10))# number transactions in 3 days
(all_num_trans_perc = percent(all_num_trans/all_num_trans))
(all_num_trans_w = sum(d10$weight_1_trans))
(all_num_trans_perc_w =  percent(all_num_trans_w/all_num_trans_w))
(all_num_trans_per_resp = all_num_trans/all_num_resp)
(all_num_trans_per_resp_w = all_num_trans_w/all_resp_w)
(all_val_trans = sum(d10$amnt, na.rm = T)/length(unique(d10$uasid)))# value per resp in 3 days
(all_val_trans_w = sum(d10$weight_1_trans*d10$amnt, na.rm = T)/length(unique(d10$uasid)))# value per resp in 3 days
(all_val_per_trans = sum(d10$amnt, na.rm = T)/nrow(d10))# val per trans
(all_val_per_trans_w = sum(d10$weight_1_trans*d10$amnt, na.rm = T)/sum(d10$weight_1_trans))# val per trans weighted
#
# summarizing All
(cash_all = c(all_num_resp, 100*all_num_resp_perc, all_resp_w, 100*all_resp_perc_w, all_num_trans, 100*all_num_trans_perc, all_num_trans_w, 100*all_num_trans_perc_w, all_num_trans_per_resp*31/3, all_num_trans_per_resp_w*31/3, all_val_per_resp*31/3,  all_val_per_trans, all_val_trans_w*31/3, all_val_per_trans_w, all_avg_hhincome, all_avg_hhincome_w, all_median_hhincome, all_avg_age, all_avg_age_w, all_median_age))
length(cash_all)

## Both: have cc and dc
both = subset(d10, cc_adopt == 1 & dc_adopt == 1)# subset of trans for adopters of both cards
dim(both)
# below, creating a subset of all_unique_id
both_unique_id = subset(all_unique_id, cc_adopt == 1 & dc_adopt == 1)#
(both_resp = length(unique(both$uasid)))# num resp
(both_resp_perc = percent(both_resp/all_num_resp))# % num resp from all resp
(both_val_per_resp = sum(both$amnt, na.rm = T)/both_resp)# avg trans value per resp
(both_avg_hhincome = mean(both_unique_id$income, na.rm = T))# avg income
(both_median_hhincome = median(both_unique_id$income, na.rm = T))# median income
(both_avg_age = mean(both_unique_id$age, na.rm = T))# avg age
(both_median_age = median(both_unique_id$age, na.rm = T))# median age
#
# doing above stats with weights
(both_resp_w = sum(both_unique_id$weight_1_id))
(both_resp_perc_w = percent(both_resp_w/all_resp_w))
(both_avg_hhincome_w = mean(both_unique_id$income*both_unique_id$weight_1_id, na.rm = T))# avg income weighted
(both_avg_age_w = mean(both_unique_id$age*both_unique_id$weight_1_id, na.rm = T))# avg age weighted
#
# Both transaction stats
(both_num_trans = nrow(both))
(both_num_trans_perc = percent(both_num_trans/all_num_trans))
(both_val_per_trans = sum(both$amnt, na.rm = T)/both_num_trans)# value $ per transaction
(both_num_trans_w = sum(both$weight_1_trans))
(both_num_trans_perc_w =  percent(both_num_trans_w/all_num_trans_w))
(both_num_trans_per_resp = both_num_trans/both_resp)
(both_num_trans_per_resp_w = both_num_trans_w/both_resp_w)
(both_val_trans_w = sum(both$weight_1_trans*both$amnt, na.rm = T)/length(unique(both$uasid)))# value per resp in 3 days
(both_val_per_trans = sum(both$amnt, na.rm = T)/nrow(both))# val per trans
(both_val_per_trans_w = sum(both$weight_1_trans*both$amnt, na.rm = T)/sum(both$weight_1_trans))# val per trans weighted
#
# summarizing Both
(cash_both = c(both_resp, 100*both_resp_perc, both_resp_w, 100*both_resp_perc_w, both_num_trans, 100*both_num_trans_perc, both_num_trans_w, 100*both_num_trans_perc_w, both_num_trans_per_resp*31/3, both_num_trans_per_resp_w*31/3, both_val_per_resp*31/3, both_val_per_trans, both_val_trans_w*31/3, both_val_per_trans_w, both_avg_hhincome, both_avg_hhincome_w, both_median_hhincome, both_avg_age, both_avg_age_w, both_median_age))
length(cash_both)

## No cc: non cc adopters
no_cc = subset(d10, cc_adopt == 0 & pi != "credit")# subset of trans for non cc adopters
dim(no_cc)
# below, creating a subset of all_unique_id
no_cc_unique_id = subset(all_unique_id, cc_adopt == 0)#
(no_cc_resp = length(unique(no_cc$uasid)))# num resp
(no_cc_resp_perc = percent(no_cc_resp/all_num_resp))# % num resp from all resp
(no_cc_val_per_resp = sum(no_cc$amnt, na.rm = T)/no_cc_resp)# avg trans value per resp
(no_cc_avg_hhincome = mean(no_cc_unique_id$income, na.rm = T))# avg income
(no_cc_median_hhincome = median(no_cc_unique_id$income, na.rm = T))# median income
(no_cc_avg_age = mean(no_cc_unique_id$age, na.rm = T))# avg age
(no_cc_median_age = median(no_cc_unique_id$age, na.rm = T))# median income
#
# doing above stats with weights
(no_cc_resp_w = sum(no_cc_unique_id$weight_1_id))
(no_cc_resp_perc_w = percent(no_cc_resp_w/all_resp_w))
(no_cc_avg_hhincome_w = mean(no_cc_unique_id$income*no_cc_unique_id$weight_1_id, na.rm = T))# avg income weighted
(no_cc_avg_age_w = mean(no_cc_unique_id$age*no_cc_unique_id$weight_1_id, na.rm = T))# avg age weighted
#
# no cc transaction stats
(no_cc_num_trans = nrow(no_cc))
(no_cc_num_trans_perc = percent(no_cc_num_trans/all_num_trans))
(no_cc_val_per_trans = sum(no_cc$amnt, na.rm = T)/no_cc_num_trans)# value $ per transaction
(no_cc_num_trans_w = sum(no_cc$weight_1_trans))
(no_cc_num_trans_perc_w =  percent(no_cc_num_trans_w/all_num_trans_w))
(no_cc_num_trans_per_resp = no_cc_num_trans/no_cc_resp)
(no_cc_val_trans_w = sum(no_cc$weight_1_trans*no_cc$amnt, na.rm = T)/length(unique(no_cc$uasid)))# value per resp in 3 days
(no_cc_val_per_trans = sum(no_cc$amnt, na.rm = T)/nrow(no_cc))# val per trans
(no_cc_val_per_trans_w = sum(no_cc$weight_1_trans*no_cc$amnt, na.rm = T)/sum(no_cc$weight_1_trans))# val per trans weighted

(no_cc_num_trans_per_resp_w = no_cc_num_trans_w/no_cc_resp_w)
# summarizing no_cc
(cash_no_cc = c(no_cc_resp, 100*no_cc_resp_perc, no_cc_resp_w, 100*no_cc_resp_perc_w, no_cc_num_trans, 100*no_cc_num_trans_perc, no_cc_num_trans_w, 100*no_cc_num_trans_perc_w, no_cc_num_trans_per_resp*31/3, no_cc_num_trans_per_resp_w*31/3, no_cc_val_per_resp*31/3, no_cc_val_per_trans, no_cc_val_trans_w*31/3, no_cc_val_per_trans_w, no_cc_avg_hhincome, no_cc_avg_hhincome_w, no_cc_median_hhincome, no_cc_avg_age, no_cc_avg_age_w, no_cc_median_age))
length(cash_no_cc)

## No dc: non dc adopters
no_dc = subset(d10, dc_adopt == 0 & pi != "debit")# subset of trans for non dc adopters
dim(no_dc)
# below, creating a subset of all_unique_id
no_dc_unique_id = subset(all_unique_id, dc_adopt == 0)#
(no_dc_resp = length(unique(no_dc$uasid)))# num resp
(no_dc_resp_perc = percent(no_dc_resp/all_num_resp))# % num resp from all resp
(no_dc_val_per_resp = sum(no_dc$amnt, na.rm = T)/no_dc_resp)# avg trans value per resp
(no_dc_avg_hhincome = mean(no_dc_unique_id$income, na.rm = T))# avg income
(no_dc_median_hhincome = median(no_dc_unique_id$income, na.rm = T))# median income
(no_dc_avg_age = mean(no_dc_unique_id$age, na.rm = T))# avg age
(no_dc_median_age = median(no_dc_unique_id$age, na.rm = T))# median age
#
# doing above stats with weights
(no_dc_resp_w = sum(no_dc_unique_id$weight_1_id))
(no_dc_resp_perc_w = percent(no_dc_resp_w/all_resp_w))
(no_dc_avg_hhincome_w = mean(no_dc_unique_id$income*no_dc_unique_id$weight_1_id, na.rm = T))# avg income weighted
(no_dc_avg_age_w = mean(no_dc_unique_id$age*no_dc_unique_id$weight_1_id, na.rm = T))# avg age weighted
#
## no dc transaction stats
(no_dc_num_trans = nrow(no_dc))
(no_dc_num_trans_perc = percent(no_dc_num_trans/all_num_trans))
(no_dc_val_per_trans = sum(no_dc$amnt, na.rm = T)/no_dc_num_trans)# value $ per transaction
(no_dc_num_trans_w = sum(no_dc$weight_1_trans))
(no_dc_num_trans_perc_w =  percent(no_dc_num_trans_w/all_num_trans_w))
(no_dc_num_trans_per_resp = no_dc_num_trans/no_dc_resp)
(no_dc_num_trans_per_resp_w = no_dc_num_trans_w/no_dc_resp_w)
(no_dc_val_trans_w = sum(no_dc$weight_1_trans*no_dc$amnt, na.rm = T)/length(unique(no_dc$uasid)))# value per resp in 3 days
(no_dc_val_per_trans = sum(no_dc$amnt, na.rm = T)/nrow(no_dc))# val per trans
(no_dc_val_per_trans_w = sum(no_dc$weight_1_trans*no_dc$amnt, na.rm = T)/sum(no_dc$weight_1_trans))# val per trans weighted
# 
# summarizing no_dc
(cash_no_dc = c(no_dc_resp, 100*no_dc_resp_perc, no_dc_resp_w, 100*no_dc_resp_perc_w, no_dc_num_trans, 100*no_dc_num_trans_perc, no_dc_num_trans_w, 100*no_dc_num_trans_perc_w, no_dc_num_trans_per_resp*31/3, no_dc_num_trans_per_resp_w*31/3, no_dc_val_per_resp*31/3, no_dc_val_per_trans, no_dc_val_trans_w*31/3, no_dc_val_per_trans_w, no_dc_avg_hhincome, no_dc_avg_hhincome_w, no_dc_median_hhincome, no_dc_avg_age, no_dc_avg_age_w, no_dc_median_age))
length(cash_no_dc)

## No dc & no cc [split between banked and unbanked]
# no_dc_cc = subset(d10, dc_adopt == 0 & cc_adopt == 0 & pi != "credit" & pi != "debit")# subset of trans for non dc & non cc adopters
# dim(no_dc_cc)
# # below, creating a subset of all_unique_id
# no_dc_cc_unique_id = subset(all_unique_id, dc_adopt == 0 & cc_adopt == 0)#
# (no_dc_cc_resp = length(unique(no_dc_cc$uasid)))# num resp
# (no_dc_cc_resp_perc = percent(no_dc_cc_resp/all_num_resp))# % num resp from all resp
# (no_dc_cc_val_per_resp = sum(no_dc_cc$amnt, na.rm = T)/no_dc_cc_resp)# avg trans value per resp
# (no_dc_cc_avg_hhincome = mean(no_dc_cc_unique_id$income, na.rm = T))# avg income
# (no_dc_cc_median_hhincome = median(no_dc_cc_unique_id$income, na.rm = T))# avg income
# (no_dc_cc_avg_age = mean(no_dc_cc_unique_id$age, na.rm = T))# avg age
# (no_dc_cc_median_age = median(no_dc_cc_unique_id$age, na.rm = T))# avg age
# #
# # doing above stats with weights
# (no_dc_cc_resp_w = sum(no_dc_cc_unique_id$weight_1_id))
# (no_dc_cc_resp_perc_w = percent(no_dc_cc_resp_w/all_resp_w))
# (no_dc_cc_avg_hhincome_w = mean(no_dc_cc_unique_id$income*no_dc_cc_unique_id$weight_1_id, na.rm = T))# avg income weighted
# (no_dc_cc_avg_age_w = mean(no_dc_cc_unique_id$age*no_dc_cc_unique_id$weight_1_id, na.rm = T))# avg age weighted
# #
# # no cc & no dc transaction stats
# (no_dc_cc_num_trans = nrow(no_dc_cc))
# (no_dc_cc_num_trans_perc = percent(no_dc_cc_num_trans/all_num_trans))
# (no_dc_cc_val_per_trans = sum(no_dc_cc$amnt, na.rm = T)/no_dc_cc_num_trans)# value $ per transaction
# (no_dc_cc_num_trans_w = sum(no_dc_cc$weight_1_trans))
# (no_dc_cc_num_trans_perc_w =  percent(no_dc_cc_num_trans_w/all_num_trans_w))
# (no_dc_cc_num_trans_per_resp = no_dc_cc_num_trans/no_dc_cc_resp)
# (no_dc_cc_num_trans_per_resp_w = no_dc_cc_num_trans_w/no_dc_cc_resp_w)
# (no_dc_cc_val_trans_w = sum(no_dc_cc$weight_1_trans*no_dc_cc$amnt, na.rm = T)/length(unique(no_dc_cc$uasid)))# value per resp in 3 days
# (no_dc_cc_val_per_trans = sum(no_dc_cc$amnt, na.rm = T)/nrow(no_dc_cc))# val per trans
# (no_dc_cc_val_per_trans_w = sum(no_dc_cc$weight_1_trans*no_dc_cc$amnt, na.rm = T)/sum(no_dc_cc$weight_1_trans))# val per trans weighted
# #
# # summarizing no_dc & no_cc
# (cash_no_dc_cc = c(no_dc_cc_resp, 100*no_dc_cc_resp_perc, no_dc_cc_resp_w, 100*no_dc_cc_resp_perc_w, no_dc_cc_num_trans, 100*no_dc_cc_num_trans_perc, no_dc_cc_num_trans_w, 100*no_dc_cc_num_trans_perc_w, no_dc_cc_num_trans_per_resp*31/3, no_dc_cc_num_trans_per_resp_w*31/3, no_dc_cc_val_per_resp*31/3, no_dc_cc_val_per_trans, no_dc_cc_val_trans_w*31/3, no_dc_cc_val_per_trans_w, no_dc_cc_avg_hhincome, no_dc_cc_avg_hhincome_w, no_dc_cc_median_hhincome, no_dc_cc_avg_age, no_dc_cc_avg_age_w, no_dc_cc_median_age))
# length(cash_no_dc_cc)

## No dc & no cc (banked)
none_banked = subset(d10, dc_adopt == 0 & cc_adopt == 0 & bnk_acnt_adopt==1 & pi != "credit" & pi != "debit")# subset of trans for non dc & non cc adopters
dim(none_banked)
# below, creating a subset of all_unique_id
none_banked_unique_id = subset(all_unique_id, dc_adopt == 0 & cc_adopt == 0 & bnk_acnt_adopt==1 )#
(none_banked_resp = length(unique(none_banked$uasid)))# num resp
(none_banked_resp_perc = percent(none_banked_resp/all_num_resp))# % num resp from all resp
(none_banked_val_per_resp = sum(none_banked$amnt, na.rm = T)/none_banked_resp)# avg trans value per resp
(none_banked_avg_hhincome = mean(none_banked_unique_id$income, na.rm = T))# avg income
(none_banked_median_hhincome = median(none_banked_unique_id$income, na.rm = T))# avg income
(none_banked_avg_age = mean(none_banked_unique_id$age, na.rm = T))# avg age
(none_banked_median_age = median(none_banked_unique_id$age, na.rm = T))# avg age
#
# doing above stats with weights
(none_banked_resp_w = sum(none_banked_unique_id$weight_1_id))
(none_banked_resp_perc_w = percent(none_banked_resp_w/all_resp_w))
(none_banked_avg_hhincome_w = mean(none_banked_unique_id$income*none_banked_unique_id$weight_1_id, na.rm = T))# avg income weighted
(none_banked_avg_age_w = mean(none_banked_unique_id$age*none_banked_unique_id$weight_1_id, na.rm = T))# avg age weighted
#
# no cc & no dc banked transaction stats
(none_banked_num_trans = nrow(none_banked))
(none_banked_num_trans_perc = percent(none_banked_num_trans/all_num_trans))
(none_banked_val_per_trans = sum(none_banked$amnt, na.rm = T)/none_banked_num_trans)# value $ per transaction
(none_banked_num_trans_w = sum(none_banked$weight_1_trans))
(none_banked_num_trans_perc_w =  percent(none_banked_num_trans_w/all_num_trans_w))
(none_banked_num_trans_per_resp = none_banked_num_trans/none_banked_resp)
(none_banked_num_trans_per_resp_w = none_banked_num_trans_w/none_banked_resp_w)
(none_banked_val_trans_w = sum(none_banked$weight_1_trans*none_banked$amnt, na.rm = T)/length(unique(none_banked$uasid)))# value per resp in 3 days
(none_banked_val_per_trans = sum(none_banked$amnt, na.rm = T)/nrow(none_banked))# val per trans
(none_banked_val_per_trans_w = sum(none_banked$weight_1_trans*none_banked$amnt, na.rm = T)/sum(none_banked$weight_1_trans))# val per trans weighted
#
# summarizing no_dc & no_cc (banked)
(cash_none_banked = c(none_banked_resp, 100*none_banked_resp_perc, none_banked_resp_w, 100*none_banked_resp_perc_w, none_banked_num_trans, 100*none_banked_num_trans_perc, none_banked_num_trans_w, 100*none_banked_num_trans_perc_w, none_banked_num_trans_per_resp*31/3, none_banked_num_trans_per_resp_w*31/3, none_banked_val_per_resp*31/3, none_banked_val_per_trans, none_banked_val_trans_w*31/3, none_banked_val_per_trans_w, none_banked_avg_hhincome, none_banked_avg_hhincome_w, none_banked_median_hhincome, none_banked_avg_age, none_banked_avg_age_w, none_banked_median_age))
length(cash_none_banked)

## No dc & no cc (UNbanked, unb)
none_unb = subset(d10, dc_adopt == 0 & cc_adopt == 0 & bnk_acnt_adopt==0 & pi != "credit" & pi != "debit")# subset of trans for non dc & non cc adopters
dim(none_unb)
# below, creating a subset of all_unique_id
none_unb_unique_id = subset(all_unique_id, dc_adopt == 0 & cc_adopt == 0 & bnk_acnt_adopt==0 )#
(none_unb_resp = length(unique(none_unb$uasid)))# num resp
(none_unb_resp_perc = percent(none_unb_resp/all_num_resp))# % num resp from all resp
(none_unb_val_per_resp = sum(none_unb$amnt, na.rm = T)/none_unb_resp)# avg trans value per resp
(none_unb_avg_hhincome = mean(none_unb_unique_id$income, na.rm = T))# avg income
(none_unb_median_hhincome = median(none_unb_unique_id$income, na.rm = T))# avg income
(none_unb_avg_age = mean(none_unb_unique_id$age, na.rm = T))# avg age
(none_unb_median_age = median(none_unb_unique_id$age, na.rm = T))# avg age
#
# doing above stats with weights
(none_unb_resp_w = sum(none_unb_unique_id$weight_1_id))
(none_unb_resp_perc_w = percent(none_unb_resp_w/all_resp_w))
(none_unb_avg_hhincome_w = mean(none_unb_unique_id$income*none_unb_unique_id$weight_1_id, na.rm = T))# avg income weighted
(none_unb_avg_age_w = mean(none_unb_unique_id$age*none_unb_unique_id$weight_1_id, na.rm = T))# avg age weighted
#
# no cc & no dc banked transaction stats
(none_unb_num_trans = nrow(none_unb))
(none_unb_num_trans_perc = percent(none_unb_num_trans/all_num_trans))
(none_unb_val_per_trans = sum(none_unb$amnt, na.rm = T)/none_unb_num_trans)# value $ per transaction
(none_unb_num_trans_w = sum(none_unb$weight_1_trans))
(none_unb_num_trans_perc_w =  percent(none_unb_num_trans_w/all_num_trans_w))
(none_unb_num_trans_per_resp = none_unb_num_trans/none_unb_resp)
(none_unb_num_trans_per_resp_w = none_unb_num_trans_w/none_unb_resp_w)
(none_unb_val_trans_w = sum(none_unb$weight_1_trans*none_unb$amnt, na.rm = T)/length(unique(none_unb$uasid)))# value per resp in 3 days
(none_unb_val_per_trans = sum(none_unb$amnt, na.rm = T)/nrow(none_unb))# val per trans
(none_unb_val_per_trans_w = sum(none_unb$weight_1_trans*none_unb$amnt, na.rm = T)/sum(none_unb$weight_1_trans))# val per trans weighted
#
# summarizing no_dc & no_cc (UNbanked, unb)
(cash_none_unb = c(none_unb_resp, 100*none_unb_resp_perc, none_unb_resp_w, 100*none_unb_resp_perc_w, none_unb_num_trans, 100*none_unb_num_trans_perc, none_unb_num_trans_w, 100*none_unb_num_trans_perc_w, none_unb_num_trans_per_resp*31/3, none_unb_num_trans_per_resp_w*31/3, none_unb_val_per_resp*31/3, none_unb_val_per_trans, none_unb_val_trans_w*31/3, none_unb_val_per_trans_w, none_unb_avg_hhincome, none_unb_avg_hhincome_w, none_unb_median_hhincome, none_unb_avg_age, none_unb_avg_age_w, none_unb_median_age))
length(cash_none_unb)

## Start combine the above into Table 1 in paper
cash_list = c("Number of respondents", "% of total", "Number of respondents (w)", "% of total (w)", "Number of payments", "% of total","Number of payments (w)", "% of total (w)", "Monthly payments per respondent", "Monthly payments per respondent (w)", "Monthly value $ per respondent", "Average payment $ value", "Monthly value $ per respondent (w)", "Average payment $ value (w)", "Average HH income", "Average HH income (w)", "Median HH income", "Average age", "Average age (w)", "Median age")
cash_list
length(cash_list)
#
cash_table = data.frame("Variable" = cash_list, "All" = cash_all, "CC_DC" = cash_both, "No_CC" = cash_no_cc, "No_DC" = cash_no_dc, "None_B" = cash_none_banked, "None_UB" = cash_none_unb)
cash_table# Table 1 in paper
dim(cash_table)
# below, create matrix w\ 1 extra column to indicate number of digits for each row
(digitm = matrix(c(rep(0,8), 0, rep(1,7), rep(0,8), 0, rep(1,7), rep(0,8), 0, rep(1,7), rep(0,8), 0, rep(1,7), 0, rep(2,7), 0, rep(2,7), rep(0,8), 0, rep(2,7), rep(0,8), 0, rep(2,7), rep(0,8), rep(0,8), rep(0,8), rep(0,8), rep(0,8), rep(0,8)), nrow = 20, ncol = 8, byrow = T))
#
print(xtable(cash_table, digits = digitm), include.rownames = F, hline.after = c(0,4,10,14,17,20))
# Starts Line 197 Table 1 printed on line 467


### Section 4 in paper
### start preparing for Table 2: Use of PI by card and noncard holders (unweighted, in_person). Table 2: Starts line 471, ends line 705
# Note: Some Table 1 variables are used below
(pi_all_cash = nrow(d10[d10$pi == "cash", ]))# number of cash trans
(pi_all_check = nrow(d10[d10$pi == "check", ]))# number of check trans
(pi_all_credit = nrow(d10[d10$pi == "credit", ]))# number of credit trans
(pi_all_debit = nrow(d10[d10$pi == "debit", ]))# number of debit trans
(pi_all_prepaid = nrow(d10[d10$pi == "prepaid", ]))# number of prepaid trans
(pi_all = c(pi_all_cash, pi_all_check, pi_all_credit, pi_all_debit, pi_all_prepaid))# vector of trans by pi
(pi_all_perc = percent(pi_all/nrow(d10)))
#
# Do the above weighted [note: should use weight_1_trans]
nrow(d10)
sum(d10$weight_1)
sum(d10$weight_1_trans)
#
(pi_all_cash_w = sum(d10[d10$pi == "cash", ]$weight_1_trans))# cash trans
(pi_all_check_w = sum(d10[d10$pi == "check", ]$weight_1_trans))# check trans
(pi_all_credit_w = sum(d10[d10$pi == "credit", ]$weight_1_trans))# credit trans
(pi_all_debit_w = sum(d10[d10$pi == "debit", ]$weight_1_trans))# debit trans
(pi_all_prepaid_w = sum(d10[d10$pi == "prepaid", ]$weight_1_trans))# prepaid trans
(pi_all_w = c(pi_all_cash_w, pi_all_check_w, pi_all_credit_w, pi_all_debit_w, pi_all_prepaid_w))# vector of trans by pi
(pi_all_perc_w = percent(pi_all_w/sum(d10$weight_1_trans)))
#
(pi_all_per_resp = (31/3)*pi_all/all_num_resp)# monthly num trans per resp by pi
(pi_all_cash_value_per_trans = sum(d10[d10$pi == "cash", ]$amnt, na.rm = T)/pi_all[1])# Avg trans value
(pi_all_check_value_per_trans = sum(d10[d10$pi == "check", ]$amnt, na.rm = T)/pi_all[2])# Avg trans value
(pi_all_credit_value_per_trans = sum(d10[d10$pi == "credit", ]$amnt, na.rm = T)/pi_all[3])# Avg trans value
(pi_all_debit_value_per_trans = sum(d10[d10$pi == "debit", ]$amnt, na.rm = T)/pi_all[4])# Avg trans value
(pi_all_prepaid_value_per_trans = sum(d10[d10$pi == "prepaid", ]$amnt, na.rm = T)/pi_all[5])# Avg trans value
(pi_all_value_per_trans = as.numeric(c(pi_all_cash_value_per_trans, pi_all_check_value_per_trans, pi_all_credit_value_per_trans, pi_all_debit_value_per_trans, pi_all_prepaid_value_per_trans)))
##
#(pi_both = table(both$pi))# Num trans for each pi
(pi_both_cash = nrow(both[both$pi == "cash", ]))# number of cash trans
(pi_both_check = nrow(both[both$pi == "check", ]))# number of check trans
(pi_both_credit = nrow(both[both$pi == "credit", ]))# number of credit trans
(pi_both_debit = nrow(both[both$pi == "debit", ]))# number of debit trans
(pi_both_prepaid = nrow(both[both$pi == "prepaid", ]))# number of prepaid trans
(pi_both = c(pi_both_cash, pi_both_check, pi_both_credit, pi_both_debit, pi_both_prepaid))# vector of trans by pi
(pi_both_perc = percent(pi_both/nrow(both)))
# Do the above weighted
nrow(both)
sum(both$weight_1_trans)
(pi_both_cash_w = sum(both[both$pi == "cash", ]$weight_1_trans))# cash trans
(pi_both_check_w = sum(both[both$pi == "check", ]$weight_1_trans))# check trans
(pi_both_credit_w = sum(both[both$pi == "credit", ]$weight_1_trans))# credit trans
(pi_both_debit_w = sum(both[both$pi == "debit", ]$weight_1_trans))# debit trans
(pi_both_prepaid_w = sum(both[both$pi == "prepaid", ]$weight_1_trans))# prepaid trans
(pi_both_w = c(pi_both_cash_w, pi_both_check_w, pi_both_credit_w, pi_both_debit_w, pi_both_prepaid_w))# vector of trans by pi
(pi_both_perc_w = percent(pi_both_w/sum(both$weight_1_trans)))
#
(pi_both_per_resp = (31/3)*pi_both/both_resp)# monthly num trans per resp by pi
(pi_both_cash_value_per_trans = sum(both[both$pi == "cash", ]$amnt, na.rm = T)/pi_both[1])# Avg trans value
(pi_both_check_value_per_trans = sum(both[both$pi == "check", ]$amnt, na.rm = T)/pi_both[2])# Avg trans value
(pi_both_credit_value_per_trans = sum(both[both$pi == "credit", ]$amnt, na.rm = T)/pi_both[3])# Avg trans value
(pi_both_debit_value_per_trans = sum(both[both$pi == "debit", ]$amnt, na.rm = T)/pi_both[4])# Avg trans value
(pi_both_prepaid_value_per_trans = sum(both[both$pi == "prepaid", ]$amnt, na.rm = T)/pi_both[5])# Avg trans value
(pi_both_value_per_trans = as.numeric(c(pi_both_cash_value_per_trans, pi_both_check_value_per_trans, pi_both_credit_value_per_trans, pi_both_debit_value_per_trans, pi_both_prepaid_value_per_trans)))
#
#(pi_no_cc = table(no_cc$pi))# Num trans for each pi
(pi_no_cc_cash = nrow(no_cc[no_cc$pi == "cash", ]))# number of cash trans
(pi_no_cc_check = nrow(no_cc[no_cc$pi == "check", ]))# number of check trans
(pi_no_cc_credit = nrow(no_cc[no_cc$pi == "credit", ]))# number of credit trans
(pi_no_cc_debit = nrow(no_cc[no_cc$pi == "debit", ]))# number of debit trans
(pi_no_cc_prepaid = nrow(no_cc[no_cc$pi == "prepaid", ]))# number of prepaid trans
(pi_no_cc = c(pi_no_cc_cash, pi_no_cc_check, pi_no_cc_credit, pi_no_cc_debit, pi_no_cc_prepaid))# vector of trans by pi
(pi_no_cc_perc = percent(pi_no_cc/nrow(no_cc)))
# Do the above weighted
nrow(no_cc)
sum(no_cc$weight_1_trans)
(pi_no_cc_cash_w = sum(no_cc[no_cc$pi == "cash", ]$weight_1_trans))# cash trans
(pi_no_cc_check_w = sum(no_cc[no_cc$pi == "check", ]$weight_1_trans))# check trans
(pi_no_cc_credit_w = sum(no_cc[no_cc$pi == "credit", ]$weight_1_trans))# credit trans
(pi_no_cc_debit_w = sum(no_cc[no_cc$pi == "debit", ]$weight_1_trans))# debit trans
(pi_no_cc_prepaid_w = sum(no_cc[no_cc$pi == "prepaid", ]$weight_1_trans))# prepaid trans
(pi_no_cc_w = c(pi_no_cc_cash_w, pi_no_cc_check_w, pi_no_cc_credit_w, pi_no_cc_debit_w, pi_no_cc_prepaid_w))# vector of trans by pi
(pi_no_cc_perc_w = percent(pi_no_cc_w/sum(no_cc$weight_1_trans)))
#
#(pi_no_cc_perc = percent(prop.table(table(no_cc$pi))))
(pi_no_cc_per_resp = (31/3)*pi_no_cc/no_cc_resp)# monthly num trans per resp by pi
(pi_no_cc_cash_value_per_trans = sum(no_cc[no_cc$pi == "cash", ]$amnt, na.rm = T)/pi_no_cc[1])# Avg trans value
(pi_no_cc_check_value_per_trans = sum(no_cc[no_cc$pi == "check", ]$amnt, na.rm = T)/pi_no_cc[2])# Avg trans value
#(pi_no_cc_credit_value_per_trans = sum(no_cc[no_cc$pi == "credit", ]$amnt, na.rm = T)/pi_no_cc[3])# Avg trans value
(pi_no_cc_credit_value_per_trans = 0)# Avg trans value
(pi_no_cc_debit_value_per_trans = sum(no_cc[no_cc$pi == "debit", ]$amnt, na.rm = T)/pi_no_cc[4])# Avg trans value
(pi_no_cc_prepaid_value_per_trans = sum(no_cc[no_cc$pi == "prepaid", ]$amnt, na.rm = T)/pi_no_cc[5])# Avg trans value
(pi_no_cc_value_per_trans = as.numeric(c(pi_no_cc_cash_value_per_trans, pi_no_cc_check_value_per_trans, pi_no_cc_credit_value_per_trans, pi_no_cc_debit_value_per_trans, pi_no_cc_prepaid_value_per_trans)))
##
#(pi_no_dc = table(no_dc$pi))# Num trans for each pi
(pi_no_dc_cash = nrow(no_dc[no_dc$pi == "cash", ]))# number of cash trans
(pi_no_dc_check = nrow(no_dc[no_dc$pi == "check", ]))# number of check trans
(pi_no_dc_credit = nrow(no_dc[no_dc$pi == "credit", ]))# number of credit trans
(pi_no_dc_debit = nrow(no_dc[no_dc$pi == "debit", ]))# number of debit trans
(pi_no_dc_prepaid = nrow(no_dc[no_dc$pi == "prepaid", ]))# number of prepaid trans
(pi_no_dc = c(pi_no_dc_cash, pi_no_dc_check, pi_no_dc_credit, pi_no_dc_debit, pi_no_dc_prepaid))# vector of trans by pi
(pi_no_dc_perc = percent(pi_no_dc/nrow(no_dc)))
# Do the above weighted
nrow(no_dc)
sum(no_dc$weight_1_trans)
(pi_no_dc_cash_w = sum(no_dc[no_dc$pi == "cash", ]$weight_1_trans))# cash trans
(pi_no_dc_check_w = sum(no_dc[no_dc$pi == "check", ]$weight_1_trans))# check trans
(pi_no_dc_credit_w = sum(no_dc[no_dc$pi == "credit", ]$weight_1_trans))# credit trans
(pi_no_dc_debit_w = sum(no_dc[no_dc$pi == "debit", ]$weight_1_trans))# debit trans
(pi_no_dc_prepaid_w = sum(no_dc[no_dc$pi == "prepaid", ]$weight_1_trans))# prepaid trans
(pi_no_dc_w = c(pi_no_dc_cash_w, pi_no_dc_check_w, pi_no_dc_credit_w, pi_no_dc_debit_w, pi_no_dc_prepaid_w))# vector of trans by pi
(pi_no_dc_perc_w = percent(pi_no_dc_w/sum(no_dc$weight_1_trans)))
#
(pi_no_dc_per_resp = (31/3)*pi_no_dc/no_dc_resp)# monthly num trans per resp by pi
(pi_no_dc_cash_value_per_trans = sum(no_dc[no_dc$pi == "cash", ]$amnt, na.rm = T)/pi_no_dc[1])# Avg trans value
(pi_no_dc_check_value_per_trans = sum(no_dc[no_dc$pi == "check", ]$amnt, na.rm = T)/pi_no_dc[2])# Avg trans value
(pi_no_dc_credit_value_per_trans = sum(no_dc[no_dc$pi == "credit", ]$amnt, na.rm = T)/pi_no_dc[3])# Avg trans value
#(pi_no_dc_debit_value_per_trans = sum(no_dc[no_dc$pi == "debit", ]$amnt, na.rm = T)/pi_no_dc[4])# Avg trans value
(pi_no_dc_debit_value_per_trans = 0)# Avg trans value
(pi_no_dc_prepaid_value_per_trans = sum(no_dc[no_dc$pi == "prepaid", ]$amnt, na.rm = T)/pi_no_dc[5])# Avg trans value
(pi_no_dc_value_per_trans = c(pi_no_dc_cash_value_per_trans, pi_no_dc_check_value_per_trans, pi_no_dc_credit_value_per_trans, pi_no_dc_debit_value_per_trans, pi_no_dc_prepaid_value_per_trans))
## swithing from no_dc_cc to 1) none_banked and 2) none_unb
# (pi_no_dc_cc_cash = nrow(no_dc_cc[no_dc_cc$pi == "cash", ]))# number of cash trans
# (pi_no_dc_cc_check = nrow(no_dc_cc[no_dc_cc$pi == "check", ]))# number of check trans
# (pi_no_dc_cc_credit = nrow(no_dc_cc[no_dc_cc$pi == "credit", ]))# number of credit trans
# (pi_no_dc_cc_debit = nrow(no_dc_cc[no_dc_cc$pi == "debit", ]))# number of debit trans
# (pi_no_dc_cc_prepaid = nrow(no_dc_cc[no_dc_cc$pi == "prepaid", ]))# number of prepaid trans
# (pi_no_dc_cc = c(pi_no_dc_cc_cash, pi_no_dc_cc_check, pi_no_dc_cc_credit, pi_no_dc_cc_debit, pi_no_dc_cc_prepaid))# vector of trans by pi
# (pi_no_dc_cc_perc = percent(pi_no_dc_cc/nrow(no_dc_cc)))
# # Do the above weighted
# nrow(no_dc_cc)
# sum(no_dc_cc$weight_1_trans)
# (pi_no_dc_cc_cash_w = sum(no_dc_cc[no_dc_cc$pi == "cash", ]$weight_1_trans))# cash trans
# (pi_no_dc_cc_check_w = sum(no_dc_cc[no_dc_cc$pi == "check", ]$weight_1_trans))# check trans
# (pi_no_dc_cc_credit_w = sum(no_dc_cc[no_dc_cc$pi == "credit", ]$weight_1_trans))# credit trans
# (pi_no_dc_cc_debit_w = sum(no_dc_cc[no_dc_cc$pi == "debit", ]$weight_1_trans))# debit trans
# (pi_no_dc_cc_prepaid_w = sum(no_dc_cc[no_dc_cc$pi == "prepaid", ]$weight_1_trans))# prepaid trans
# (pi_no_dc_cc_w = c(pi_no_dc_cc_cash_w, pi_no_dc_cc_check_w, pi_no_dc_cc_credit_w, pi_no_dc_cc_debit_w, pi_no_dc_cc_prepaid_w))# vector of trans by pi
# (pi_no_dc_cc_perc_w = percent(pi_no_dc_cc_w/sum(no_dc_cc$weight_1_trans)))
# #
# (pi_no_dc_cc_per_resp = (31/3)*pi_no_dc_cc/no_dc_cc_resp)# monthly num trans per resp by pi
# (pi_no_dc_cc_cash_value_per_trans = sum(no_dc_cc[no_dc_cc$pi == "cash", ]$amnt, na.rm = T)/pi_no_dc_cc[1])# Avg trans value
# (pi_no_dc_cc_check_value_per_trans = sum(no_dc_cc[no_dc_cc$pi == "check", ]$amnt, na.rm = T)/pi_no_dc_cc[2])# Avg trans value
# #(pi_no_dc_cc_credit_value_per_trans = sum(no_dc_cc[no_dc_cc$pi == "credit", ]$amnt, na.rm = T)/pi_no_dc_cc[3])# Avg trans value
# (pi_no_dc_cc_credit_value_per_trans = 0)# Avg trans value
# #(pi_no_dc_cc_debit_value_per_trans = sum(no_dc_cc[no_dc_cc$pi == "debit", ]$amnt, na.rm = T)/pi_no_dc_cc[4])# Avg trans value
# (pi_no_dc_cc_debit_value_per_trans = 0)# Avg trans value
# (pi_no_dc_cc_prepaid_value_per_trans = sum(no_dc_cc[no_dc_cc$pi == "prepaid", ]$amnt, na.rm = T)/pi_no_dc_cc[5])# Avg trans value
# (pi_no_dc_cc_value_per_trans = as.numeric(c(pi_no_dc_cc_cash_value_per_trans, pi_no_dc_cc_check_value_per_trans, pi_no_dc_cc_credit_value_per_trans, pi_no_dc_cc_debit_value_per_trans, pi_no_dc_cc_prepaid_value_per_trans)))
## 
# none_banked, table 2 continued
(pi_none_banked_cash = nrow(none_banked[none_banked$pi == "cash", ]))# number of cash trans
(pi_none_banked_check = nrow(none_banked[none_banked$pi == "check", ]))# number of check trans
(pi_none_banked_credit = nrow(none_banked[none_banked$pi == "credit", ]))# number of credit trans
(pi_none_banked_debit = nrow(none_banked[none_banked$pi == "debit", ]))# number of debit trans
(pi_none_banked_prepaid = nrow(none_banked[none_banked$pi == "prepaid", ]))# number of prepaid trans
(pi_none_banked = c(pi_none_banked_cash, pi_none_banked_check, pi_none_banked_credit, pi_none_banked_debit, pi_none_banked_prepaid))# vector of trans by pi
(pi_none_banked_perc = percent(pi_none_banked/nrow(none_banked)))
# Do the above weighted
nrow(none_banked)
sum(none_banked$weight_1_trans)
(pi_none_banked_cash_w = sum(none_banked[none_banked$pi == "cash", ]$weight_1_trans))# cash trans
(pi_none_banked_check_w = sum(none_banked[none_banked$pi == "check", ]$weight_1_trans))# check trans
(pi_none_banked_credit_w = sum(none_banked[none_banked$pi == "credit", ]$weight_1_trans))# credit trans
(pi_none_banked_debit_w = sum(none_banked[none_banked$pi == "debit", ]$weight_1_trans))# debit trans
(pi_none_banked_prepaid_w = sum(none_banked[none_banked$pi == "prepaid", ]$weight_1_trans))# prepaid trans
(pi_none_banked_w = c(pi_none_banked_cash_w, pi_none_banked_check_w, pi_none_banked_credit_w, pi_none_banked_debit_w, pi_none_banked_prepaid_w))# vector of trans by pi
(pi_none_banked_perc_w = percent(pi_none_banked_w/sum(none_banked$weight_1_trans)))
#
(pi_none_banked_per_resp = (31/3)*pi_none_banked/none_banked_resp)# monthly num trans per resp by pi
(pi_none_banked_cash_value_per_trans = sum(none_banked[none_banked$pi == "cash", ]$amnt, na.rm = T)/pi_none_banked[1])# Avg trans value
(pi_none_banked_check_value_per_trans = sum(none_banked[none_banked$pi == "check", ]$amnt, na.rm = T)/pi_none_banked[2])# Avg trans value
#(pi_none_banked_credit_value_per_trans = sum(none_banked[none_banked$pi == "credit", ]$amnt, na.rm = T)/pi_none_banked[3])# Avg trans value
(pi_none_banked_credit_value_per_trans = 0)# Avg trans value
#(pi_none_banked_debit_value_per_trans = sum(none_banked[none_banked$pi == "debit", ]$amnt, na.rm = T)/pi_none_banked[4])# Avg trans value
(pi_none_banked_debit_value_per_trans = 0)# Avg trans value
(pi_none_banked_prepaid_value_per_trans = sum(none_banked[none_banked$pi == "prepaid", ]$amnt, na.rm = T)/pi_none_banked[5])# Avg trans value
(pi_none_banked_value_per_trans = as.numeric(c(pi_none_banked_cash_value_per_trans, pi_none_banked_check_value_per_trans, pi_none_banked_credit_value_per_trans, pi_none_banked_debit_value_per_trans, pi_none_banked_prepaid_value_per_trans)))
##
# none_unb, table 2 continued
(pi_none_unb_cash = nrow(none_unb[none_unb$pi == "cash", ]))# number of cash trans
(pi_none_unb_check = nrow(none_unb[none_unb$pi == "check", ]))# number of check trans
(pi_none_unb_credit = nrow(none_unb[none_unb$pi == "credit", ]))# number of credit trans
(pi_none_unb_debit = nrow(none_unb[none_unb$pi == "debit", ]))# number of debit trans
(pi_none_unb_prepaid = nrow(none_unb[none_unb$pi == "prepaid", ]))# number of prepaid trans
(pi_none_unb = c(pi_none_unb_cash, pi_none_unb_check, pi_none_unb_credit, pi_none_unb_debit, pi_none_unb_prepaid))# vector of trans by pi
(pi_none_unb_perc = percent(pi_none_unb/nrow(none_unb)))
# Do the above weighted
nrow(none_unb)
sum(none_unb$weight_1_trans)
(pi_none_unb_cash_w = sum(none_unb[none_unb$pi == "cash", ]$weight_1_trans))# cash trans
(pi_none_unb_check_w = sum(none_unb[none_unb$pi == "check", ]$weight_1_trans))# check trans
(pi_none_unb_credit_w = sum(none_unb[none_unb$pi == "credit", ]$weight_1_trans))# credit trans
(pi_none_unb_debit_w = sum(none_unb[none_unb$pi == "debit", ]$weight_1_trans))# debit trans
(pi_none_unb_prepaid_w = sum(none_unb[none_unb$pi == "prepaid", ]$weight_1_trans))# prepaid trans
(pi_none_unb_w = c(pi_none_unb_cash_w, pi_none_unb_check_w, pi_none_unb_credit_w, pi_none_unb_debit_w, pi_none_unb_prepaid_w))# vector of trans by pi
(pi_none_unb_perc_w = percent(pi_none_unb_w/sum(none_unb$weight_1_trans)))
#
(pi_none_unb_per_resp = (31/3)*pi_none_unb/none_unb_resp)# monthly num trans per resp by pi
(pi_none_unb_cash_value_per_trans = sum(none_unb[none_unb$pi == "cash", ]$amnt, na.rm = T)/pi_none_unb[1])# Avg trans value
(pi_none_unb_check_value_per_trans = sum(none_unb[none_unb$pi == "check", ]$amnt, na.rm = T)/pi_none_unb[2])# Avg trans value
#(pi_none_unb_credit_value_per_trans = sum(none_unb[none_unb$pi == "credit", ]$amnt, na.rm = T)/pi_none_unb[3])# Avg trans value
(pi_none_unb_credit_value_per_trans = 0)# Avg trans value
#(pi_none_unb_debit_value_per_trans = sum(none_unb[none_unb$pi == "debit", ]$amnt, na.rm = T)/pi_none_unb[4])# Avg trans value
(pi_none_unb_debit_value_per_trans = 0)# Avg trans value
(pi_none_unb_prepaid_value_per_trans = sum(none_unb[none_unb$pi == "prepaid", ]$amnt, na.rm = T)/pi_none_unb[5])# Avg trans value
(pi_none_unb_value_per_trans = as.numeric(c(pi_none_unb_cash_value_per_trans, pi_none_unb_check_value_per_trans, pi_none_unb_credit_value_per_trans, pi_none_unb_debit_value_per_trans, pi_none_unb_prepaid_value_per_trans)))
#

## finalizing Table 2
(pi_table1 = rbind(pi_all, 100*pi_all_perc, 100*pi_all_perc_w, pi_all_per_resp, pi_all_value_per_trans, pi_both, 100*pi_both_perc, 100*pi_both_perc_w, pi_both_per_resp, pi_both_value_per_trans, pi_no_cc, 100*pi_no_cc_perc, 100*pi_no_cc_perc_w, pi_no_cc_per_resp, pi_no_cc_value_per_trans, pi_no_dc, 100*pi_no_dc_perc, 100*pi_no_dc_perc_w, pi_no_dc_per_resp, pi_no_dc_value_per_trans,   pi_none_banked, 100*pi_none_banked_perc, 100*pi_none_banked_perc_w, pi_none_banked_per_resp, pi_none_banked_value_per_trans, pi_none_unb, 100*pi_none_unb_perc, 100*pi_none_unb_perc_w, pi_none_unb_per_resp, pi_none_unb_value_per_trans))
rownames(pi_table1) = NULL
dim(pi_table1)
# below, create a column with cc and dc adoption patterns
#(pi_adoption_list = c(rep("Full_sample", 4), rep("CC_and_DC", 4), rep("No_CC", 4), rep("No_DC", 4), rep("No_CC_no_DC", 4)))
(pi_adoption_list = c( "", "", "All", "", "", 
                       "", "", "CC_DC", "", "", 
                       "", "", "No_CC", "", "",
                       "", "", "No_DC", "", "",
                       "",  "", "None_B", "", "",
                       "",  "", "None_U", "", ""))
(pi_var_list = rep(c("Number of payments", "%", "% (w)", "Monthly number per respondent", "Average payment $ value"), 6))
(pi_table2 =  cbind(pi_adoption_list, pi_var_list, pi_table1))
dim(pi_table2)
#
(pi_table3 = data.frame(pi_table2))
colnames(pi_table3) = c("Subsample", "Variable", "Cash", "Check", "Credit", "Debit", "Prepaid")
pi_table3
dim(pi_table3)
# Above table lists numbers as factors. Below, convert to numeric with 2 digits
pi_table4 = pi_table3
colnames(pi_table4)
(pi_table4$Cash = round(as.numeric(as.character(pi_table3$Cash)), digits = 2))
(pi_table4$Check = round(as.numeric(as.character(pi_table3$Check)), digits = 2))
(pi_table4$Credit = round(as.numeric(as.character(pi_table3$Credit)), digits = 2))
(pi_table4$Debit = round(as.numeric(as.character(pi_table3$Debit)), digits = 2))
(pi_table4$Prepaid = round(as.numeric(as.character(pi_table3$Prepaid)), digits = 2))
# below, create matrix w\ 1 extra column to indicate number of digits for each row
(pi_digitm = matrix(rep(c(rep(0,8), rep(1,8), rep(1,8), rep(2, 2*8)),6), nrow = 30, ncol = 8, byrow = T))
dim(pi_digitm)
print(xtable(pi_table4, digits = pi_digitm), include.rownames = F, hline.after = c(0,5,10,15,20,25))
# End of Table 2: Starts line 471, ends line 705

### Subsection 4.2 begins

### Figure 2 (box plots) starts line 709 (ends on line xxx)
## Building shares of INDIVIDUALs' cash-use (as fucntion of CC and DC adoption)
# Note: This is at the individual level (not transaction level)
## Full sample
# below, fraction of cash trans for EACH unique respondent (full sample)
frac_cash_all = d10 %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_all)
nrow(frac_cash_all)
# below, fraction of cash trans for EACH unique respondent (both cards)
frac_cash_both = both %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_both)
nrow(frac_cash_both)
# below, fraction of cash trans for EACH unique respondent (no cc)
frac_cash_no_cc = no_cc %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_no_cc)
nrow(frac_cash_no_cc)
# below, fraction of cash trans for EACH unique respondent (no dc)
frac_cash_no_dc = no_dc %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_no_dc)
nrow(frac_cash_no_dc)
# below, fraction of cash trans for EACH unique respondent (no dc no cc)
# frac_cash_no_dc_cc = no_dc_cc %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
# head(frac_cash_no_dc_cc)
# nrow(frac_cash_no_dc_cc)
# below, fraction of cash trans for EACH unique respondent (none_banked)
frac_cash_none_banked = none_banked %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_none_banked)
nrow(frac_cash_none_banked)
# below, fraction of cash trans for EACH unique respondent (none_unb)
frac_cash_none_unb = none_unb %>% group_by(uasid) %>% summarize(mean(pi == "cash"))
head(frac_cash_none_unb)
nrow(frac_cash_none_unb)
# 
frac_cash_all_vec = frac_cash_all %>% pull(2)# converting tibble col to vec
frac_cash_both_vec = frac_cash_both %>% pull(2)# converting tibble col to vec
frac_cash_no_cc_vec = frac_cash_no_cc %>% pull(2)# converting tibble col to vec
frac_cash_no_dc_vec = frac_cash_no_dc %>% pull(2)# converting tibble col to vec
frac_cash_none_banked_vec = frac_cash_none_banked %>% pull(2)# converting tibble col to vec
frac_cash_none_unb_vec = frac_cash_none_unb %>% pull(2)# converting tibble col to vec
#
(frac_cash_all_med = median(frac_cash_all_vec))# median
(frac_cash_both_med = median(frac_cash_both_vec))# median
(frac_cash_no_cc_med = median(frac_cash_no_cc_vec))# median
(frac_cash_no_dc_med = median(frac_cash_no_dc_vec))# median
(frac_cash_none_banked_med = median(frac_cash_none_banked_vec))# median
(frac_cash_none_unb_med = median(frac_cash_none_unb_vec))# median
#
(frac_cash_all_avg = mean(frac_cash_all_vec))# mean
(frac_cash_both_avg = mean(frac_cash_both_vec))# mean
(frac_cash_no_cc_avg = mean(frac_cash_no_cc_vec))# mean
(frac_cash_no_dc_avg = mean(frac_cash_no_dc_vec))# mean
(frac_cash_none_banked_avg = mean(frac_cash_none_banked_vec))# mean
(frac_cash_none_unb_avg = mean(frac_cash_none_unb_vec))# mean
# 
(frac_cash_all_1st = quantile(frac_cash_all_vec, 0.25))# 1st quartile
(frac_cash_both_1st = quantile(frac_cash_both_vec, 0.25))# 1st quartile
(frac_cash_no_cc_1st = quantile(frac_cash_no_cc_vec, 0.25))# 1st quartile
(frac_cash_no_dc_1st = quantile(frac_cash_no_dc_vec, 0.25))# 1st quartile
(frac_cash_none_banked_1st = quantile(frac_cash_none_banked_vec, 0.25))# 1st quartile
(frac_cash_none_unb_1st = quantile(frac_cash_none_unb_vec, 0.25))# 1st quartile
#
(frac_cash_all_3rd = quantile(frac_cash_all_vec, 0.75))# 3rd quartile
(frac_cash_both_3rd = quantile(frac_cash_both_vec, 0.75))# 3rd quartile
(frac_cash_no_cc_3rd = quantile(frac_cash_no_cc_vec, 0.75))# 3rd quartile
(frac_cash_no_dc_3rd = quantile(frac_cash_no_dc_vec, 0.75))# 3rd quartile
(frac_cash_none_banked_3rd = quantile(frac_cash_none_banked_vec, 0.75))# 3rd quartile
#
summary(frac_cash_all_vec)
summary(frac_cash_both_vec)
summary(frac_cash_no_cc_vec)
summary(frac_cash_no_dc_vec)
summary(frac_cash_none_banked_vec)
summary(frac_cash_none_unb_vec)
#
par(mar = c(2.5,4,1,2))
boxplot(100*frac_cash_all_vec, 100*frac_cash_both_vec, 100*frac_cash_no_cc_vec, 100*frac_cash_no_dc_vec, 100*frac_cash_none_banked_vec, 100*frac_cash_none_unb_vec, axes=F, col = c("cyan","green","magenta","yellow", "red", "red"),  names = c("Full sample","Both cards","No credit", "No debit", "None banked", "None unbanked"), notch = T, ylab = "Distribution of cash use (%) by  respondent")
axis(2, at=seq(0,100, 10),las=2)
axis(1,at=c(1:6), labels=c("All","Both cards","No credit", "No debit", "None banked", "None unbanked"), cex.axis=0.95)
# add text 1st quantile value (above line)
text(1, 100*frac_cash_all_1st + 3, labels = 100*round(frac_cash_all_1st, digits = 3))
text(2, 100*frac_cash_both_1st + 3, labels = 100*round(frac_cash_both_1st, digits = 3))
text(3, 100*frac_cash_no_cc_1st + 3, labels = 100*round(frac_cash_no_cc_1st, digits = 3))
text(4, 100*frac_cash_no_dc_1st + 3, labels = 100*round(frac_cash_no_dc_1st, digits = 3))
text(5, 100*frac_cash_none_banked_1st + 3, labels = 100*round(frac_cash_none_banked_1st, digits = 3))
text(6, 100*frac_cash_none_unb_1st + 3, labels = 100*round(frac_cash_none_unb_1st, digits = 3))
# add text median value (below line)
text(1, 100*frac_cash_all_med - 3, labels = 100*round(frac_cash_all_med, digits = 3))
text(2, 100*frac_cash_both_med - 3, labels = 100*round(frac_cash_both_med, digits = 3))
text(3, 100*frac_cash_no_cc_med - 3, labels = 100*round(frac_cash_no_cc_med, digits = 3))
text(4, 100*frac_cash_no_dc_med - 3, labels = 100*round(frac_cash_no_dc_med, digits = 3))
text(5, 100*frac_cash_none_banked_med - 3, labels = 100*round(frac_cash_none_banked_med, digits = 3))
# add text 3rd quantile value (above line)
text(1, 100*frac_cash_all_3rd - 3, labels = 100*round(frac_cash_all_3rd, digits = 3))
text(2, 100*frac_cash_both_3rd - 3, labels = 100*round(frac_cash_both_3rd, digits = 3))
text(3, 100*frac_cash_no_cc_3rd - 3, labels = 100*round(frac_cash_no_cc_3rd, digits = 3))
text(4, 100*frac_cash_no_dc_3rd - 3, labels = 100*round(frac_cash_no_dc_3rd, digits = 3))
text(5, 100*frac_cash_none_banked_1st + 3, labels = 100*round(frac_cash_none_banked_1st, digits = 3))
text(6, 100*frac_cash_none_unb_1st + 3, labels = 100*round(frac_cash_none_unb_1st, digits = 3))

# Stats in subsection 4.2 based on Fig 2 above
# Starting cashless-28.tex cashless_190607.R added these stats into the plot, thus, not quoted separately. 
summary(frac_cash_all_vec)# all resp
summary(frac_cash_both_vec)# have both cards
summary(frac_cash_no_cc_vec)# don't have cc
summary(frac_cash_no_dc_vec)# don't have dc
summary(frac_cash_none_banked_vec)# none banked
summary(frac_cash_none_unb_vec)# none unbanked
# End of Figure 2 in paper starts line 709 (ends on line 816)


### Section 5: Start Figure 4: Cost assessments by PI
# Fig 4 Starts on Line 819, ends on line 1116
# All starts: ranking cost of cash: 1 to 5
(all_cash_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 1, ]))
(all_cash_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 2, ]))
(all_cash_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 3, ]))
(all_cash_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 4, ]))
(all_cash_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_cash == 5, ]))
# both starts: ranking cost of cash: 1 to 5
(both_cash_vote1 = nrow(both_unique_id[both_unique_id$assess_cost_cash == 1, ]))
(both_cash_vote2 = nrow(both_unique_id[both_unique_id$assess_cost_cash == 2, ]))
(both_cash_vote3 = nrow(both_unique_id[both_unique_id$assess_cost_cash == 3, ]))
(both_cash_vote4 = nrow(both_unique_id[both_unique_id$assess_cost_cash == 4, ]))
(both_cash_vote5 = nrow(both_unique_id[both_unique_id$assess_cost_cash == 5, ]))
# no_cc starts: ranking cost of cash: 1 to 5
(no_cc_cash_vote1 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_cash == 1, ]))
(no_cc_cash_vote2 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_cash == 2, ]))
(no_cc_cash_vote3 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_cash == 3, ]))
(no_cc_cash_vote4 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_cash == 4, ]))
(no_cc_cash_vote5 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_cash == 5, ]))
# no_dc starts: ranking cost of cash: 1 to 5
(no_dc_cash_vote1 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_cash == 1, ]))
(no_dc_cash_vote2 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_cash == 2, ]))
(no_dc_cash_vote3 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_cash == 3, ]))
(no_dc_cash_vote4 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_cash == 4, ]))
(no_dc_cash_vote5 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_cash == 5, ]))
# none_banked starts: ranking cost of cash: 1 to 5
(none_banked_cash_vote1 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_cash == 1, ]))
(none_banked_cash_vote2 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_cash == 2, ]))
(none_banked_cash_vote3 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_cash == 3, ]))
(none_banked_cash_vote4 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_cash == 4, ]))
(none_banked_cash_vote5 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_cash == 5, ]))
# none unbanked starts: ranking cost of cash: 1 to 5
(none_unb_cash_vote1 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_cash == 1, ]))
(none_unb_cash_vote2 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_cash == 2, ]))
(none_unb_cash_vote3 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_cash == 3, ]))
(none_unb_cash_vote4 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_cash == 4, ]))
(none_unb_cash_vote5 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_cash == 5, ]))
##
# All starts: ranking cost of check: 1 to 5
(all_check_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_check == 1, ]))
(all_check_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_check == 2, ]))
(all_check_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_check == 3, ]))
(all_check_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_check == 4, ]))
(all_check_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_check == 5, ]))
# both starts: ranking cost of check: 1 to 5
(both_check_vote1 = nrow(both_unique_id[both_unique_id$assess_cost_check == 1, ]))
(both_check_vote2 = nrow(both_unique_id[both_unique_id$assess_cost_check == 2, ]))
(both_check_vote3 = nrow(both_unique_id[both_unique_id$assess_cost_check == 3, ]))
(both_check_vote4 = nrow(both_unique_id[both_unique_id$assess_cost_check == 4, ]))
(both_check_vote5 = nrow(both_unique_id[both_unique_id$assess_cost_check == 5, ]))
# no_cc starts: ranking cost of check: 1 to 7
(no_cc_check_vote1 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_check == 1, ]))
(no_cc_check_vote2 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_check == 2, ]))
(no_cc_check_vote3 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_check == 3, ]))
(no_cc_check_vote4 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_check == 4, ]))
(no_cc_check_vote5 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_check == 5, ]))
# no_dc starts: ranking cost of check: 1 to 7
(no_dc_check_vote1 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_check == 1, ]))
(no_dc_check_vote2 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_check == 2, ]))
(no_dc_check_vote3 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_check == 3, ]))
(no_dc_check_vote4 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_check == 4, ]))
(no_dc_check_vote5 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_check == 5, ]))
# none_banked starts: ranking cost of check: 1 to 5
(none_banked_check_vote1 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_check == 1, ]))
(none_banked_check_vote2 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_check == 2, ]))
(none_banked_check_vote3 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_check == 3, ]))
(none_banked_check_vote4 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_check == 4, ]))
(none_banked_check_vote5 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_check == 5, ]))
# none unbanked starts: ranking cost of check: 1 to 5
(none_unb_check_vote1 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_check == 1, ]))
(none_unb_check_vote2 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_check == 2, ]))
(none_unb_check_vote3 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_check == 3, ]))
(none_unb_check_vote4 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_check == 4, ]))
(none_unb_check_vote5 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_check == 5, ]))
#
## All starts: ranking cost of credit: 1 to 5
(all_credit_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 1, ]))
(all_credit_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 2, ]))
(all_credit_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 3, ]))
(all_credit_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 4, ]))
(all_credit_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_credit == 5, ]))
# both starts: ranking cost of credit: 1 to 5
(both_credit_vote1 = nrow(both_unique_id[both_unique_id$assess_cost_credit == 1, ]))
(both_credit_vote2 = nrow(both_unique_id[both_unique_id$assess_cost_credit == 2, ]))
(both_credit_vote3 = nrow(both_unique_id[both_unique_id$assess_cost_credit == 3, ]))
(both_credit_vote4 = nrow(both_unique_id[both_unique_id$assess_cost_credit == 4, ]))
(both_credit_vote5 = nrow(both_unique_id[both_unique_id$assess_cost_credit == 5, ]))
# no_cc starts: ranking cost of credit: 1 to 5
(no_cc_credit_vote1 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_credit == 1, ]))
(no_cc_credit_vote2 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_credit == 2, ]))
(no_cc_credit_vote3 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_credit == 3, ]))
(no_cc_credit_vote4 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_credit == 4, ]))
(no_cc_credit_vote5 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_credit == 5, ]))
# no_dc starts: ranking cost of credit: 1 to 5
(no_dc_credit_vote1 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_credit == 1, ]))
(no_dc_credit_vote2 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_credit == 2, ]))
(no_dc_credit_vote3 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_credit == 3, ]))
(no_dc_credit_vote4 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_credit == 4, ]))
(no_dc_credit_vote5 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_credit == 5, ]))
# none_banked starts: ranking cost of credit: 1 to 5
(none_banked_credit_vote1 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_credit == 1, ]))
(none_banked_credit_vote2 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_credit == 2, ]))
(none_banked_credit_vote3 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_credit == 3, ]))
(none_banked_credit_vote4 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_credit == 4, ]))
(none_banked_credit_vote5 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_credit == 5, ]))
# none_unb starts: ranking cost of credit: 1 to 5
(none_unb_credit_vote1 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_credit == 1, ]))
(none_unb_credit_vote2 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_credit == 2, ]))
(none_unb_credit_vote3 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_credit == 3, ]))
(none_unb_credit_vote4 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_credit == 4, ]))
(none_unb_credit_vote5 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_credit == 5, ]))
#
## All starts: ranking cost of debit: 1 to 5
(all_debit_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 1, ]))
(all_debit_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 2, ]))
(all_debit_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 3, ]))
(all_debit_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 4, ]))
(all_debit_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_debit == 5, ]))
# both starts: ranking cost of debit: 1 to 5
(both_debit_vote1 = nrow(both_unique_id[both_unique_id$assess_cost_debit == 1, ]))
(both_debit_vote2 = nrow(both_unique_id[both_unique_id$assess_cost_debit == 2, ]))
(both_debit_vote3 = nrow(both_unique_id[both_unique_id$assess_cost_debit == 3, ]))
(both_debit_vote4 = nrow(both_unique_id[both_unique_id$assess_cost_debit == 4, ]))
(both_debit_vote5 = nrow(both_unique_id[both_unique_id$assess_cost_debit == 5, ]))
# no_cc starts: ranking cost of debit: 1 to 5
(no_cc_debit_vote1 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_debit == 1, ]))
(no_cc_debit_vote2 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_debit == 2, ]))
(no_cc_debit_vote3 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_debit == 3, ]))
(no_cc_debit_vote4 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_debit == 4, ]))
(no_cc_debit_vote5 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_debit == 5, ]))
# no_dc starts: ranking cost of debit: 1 to 5
(no_dc_debit_vote1 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_debit == 1, ]))
(no_dc_debit_vote2 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_debit == 2, ]))
(no_dc_debit_vote3 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_debit == 3, ]))
(no_dc_debit_vote4 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_debit == 4, ]))
(no_dc_debit_vote5 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_debit == 5, ]))
# none_banked starts: ranking cost of debit: 1 to 5
(none_banked_debit_vote1 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_debit == 1, ]))
(none_banked_debit_vote2 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_debit == 2, ]))
(none_banked_debit_vote3 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_debit == 3, ]))
(none_banked_debit_vote4 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_debit == 4, ]))
(none_banked_debit_vote5 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_debit == 5, ]))
# none_unb starts: ranking cost of debit: 1 to 5
(none_unb_debit_vote1 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_debit == 1, ]))
(none_unb_debit_vote2 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_debit == 2, ]))
(none_unb_debit_vote3 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_debit == 3, ]))
(none_unb_debit_vote4 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_debit == 4, ]))
(none_unb_debit_vote5 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_debit == 5, ]))
#
# All starts: ranking cost of prepaid: 1 to 7
(all_prepaid_vote1 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 1, ]))
(all_prepaid_vote2 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 2, ]))
(all_prepaid_vote3 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 3, ]))
(all_prepaid_vote4 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 4, ]))
(all_prepaid_vote5 = nrow(all_unique_id[all_unique_id$assess_cost_prepaid == 5, ]))
# both starts: ranking cost of prepaid: 1 to 7
(both_prepaid_vote1 = nrow(both_unique_id[both_unique_id$assess_cost_prepaid == 1, ]))
(both_prepaid_vote2 = nrow(both_unique_id[both_unique_id$assess_cost_prepaid == 2, ]))
(both_prepaid_vote3 = nrow(both_unique_id[both_unique_id$assess_cost_prepaid == 3, ]))
(both_prepaid_vote4 = nrow(both_unique_id[both_unique_id$assess_cost_prepaid == 4, ]))
(both_prepaid_vote5 = nrow(both_unique_id[both_unique_id$assess_cost_prepaid == 5, ]))
# no_cc starts: ranking cost of prepaid: 1 to 7
(no_cc_prepaid_vote1 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_prepaid == 1, ]))
(no_cc_prepaid_vote2 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_prepaid == 2, ]))
(no_cc_prepaid_vote3 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_prepaid == 3, ]))
(no_cc_prepaid_vote4 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_prepaid == 4, ]))
(no_cc_prepaid_vote5 = nrow(no_cc_unique_id[no_cc_unique_id$assess_cost_prepaid == 5, ]))
# no_dc starts: ranking cost of prepaid: 1 to 7
(no_dc_prepaid_vote1 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_prepaid == 1, ]))
(no_dc_prepaid_vote2 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_prepaid == 2, ]))
(no_dc_prepaid_vote3 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_prepaid == 3, ]))
(no_dc_prepaid_vote4 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_prepaid == 4, ]))
(no_dc_prepaid_vote5 = nrow(no_dc_unique_id[no_dc_unique_id$assess_cost_prepaid == 5, ]))
#
# none_banked starts: ranking cost of prepaid: 1 to 5
(none_banked_prepaid_vote1 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_prepaid == 1, ]))
(none_banked_prepaid_vote2 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_prepaid == 2, ]))
(none_banked_prepaid_vote3 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_prepaid == 3, ]))
(none_banked_prepaid_vote4 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_prepaid == 4, ]))
(none_banked_prepaid_vote5 = nrow(none_banked_unique_id[none_banked_unique_id$assess_cost_prepaid == 5, ]))
# none unbanked starts: ranking cost of prepaid: 1 to 5
(none_unb_prepaid_vote1 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_prepaid == 1, ]))
(none_unb_prepaid_vote2 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_prepaid == 2, ]))
(none_unb_prepaid_vote3 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_prepaid == 3, ]))
(none_unb_prepaid_vote4 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_prepaid == 4, ]))
(none_unb_prepaid_vote5 = nrow(none_unb_unique_id[none_unb_unique_id$assess_cost_prepaid == 5, ]))

##
## start summarizing votes by adoption type
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
# both score of cost by PI
(both_cash_vote = 1*both_cash_vote1 + 2*both_cash_vote2 + 3*both_cash_vote3 + 4*both_cash_vote4 + 5*both_cash_vote5)
(both_check_vote = 1*both_check_vote1 + 2*both_check_vote2 + 3*both_check_vote3 + 4*both_check_vote4 + 5*both_check_vote5)
(both_credit_vote = 1*both_credit_vote1 + 2*both_credit_vote2 + 3*both_credit_vote3 + 4*both_credit_vote4 + 5*both_credit_vote5)
(both_debit_vote = 1*both_debit_vote1 + 2*both_debit_vote2 + 3*both_debit_vote3 + 4*both_debit_vote4 + 5*both_debit_vote5)
(both_prepaid_vote = 1*both_prepaid_vote1 + 2*both_prepaid_vote2 + 3*both_prepaid_vote3 + 4*both_prepaid_vote4 + 5*both_prepaid_vote5)
# both score of cost by PI
(both_cash_vote = 1*both_cash_vote1 + 2*both_cash_vote2 + 3*both_cash_vote3 + 4*both_cash_vote4 + 5*both_cash_vote5)
(both_check_vote = 1*both_check_vote1 + 2*both_check_vote2 + 3*both_check_vote3 + 4*both_check_vote4 + 5*both_check_vote5)
(both_credit_vote = 1*both_credit_vote1 + 2*both_credit_vote2 + 3*both_credit_vote3 + 4*both_credit_vote4 + 5*both_credit_vote5)
(both_debit_vote = 1*both_debit_vote1 + 2*both_debit_vote2 + 3*both_debit_vote3 + 4*both_debit_vote4 + 5*both_debit_vote5)
(both_prepaid_vote = 1*both_prepaid_vote1 + 2*both_prepaid_vote2 + 3*both_prepaid_vote3 + 4*both_prepaid_vote4 + 5*both_prepaid_vote5)
rank(c(both_cash_vote, both_check_vote, both_credit_vote, both_debit_vote, both_prepaid_vote))# Note: Highest rank = lowest cost
(both_vote_vec = c(both_cash_vote, both_check_vote, both_credit_vote, both_debit_vote, both_prepaid_vote))# vector of both votes for PI
(both_vote_score = both_vote_vec/nrow(both_unique_id))
#
# no_cc score of cost by PI
(no_cc_cash_vote = 1*no_cc_cash_vote1 + 2*no_cc_cash_vote2 + 3*no_cc_cash_vote3 + 4*no_cc_cash_vote4 + 5*no_cc_cash_vote5)
(no_cc_check_vote = 1*no_cc_check_vote1 + 2*no_cc_check_vote2 + 3*no_cc_check_vote3 + 4*no_cc_check_vote4 + 5*no_cc_check_vote5)
(no_cc_credit_vote = 1*no_cc_credit_vote1 + 2*no_cc_credit_vote2 + 3*no_cc_credit_vote3 + 4*no_cc_credit_vote4 + 5*no_cc_credit_vote5)
(no_cc_debit_vote = 1*no_cc_debit_vote1 + 2*no_cc_debit_vote2 + 3*no_cc_debit_vote3 + 4*no_cc_debit_vote4 + 5*no_cc_debit_vote5)
(no_cc_prepaid_vote = 1*no_cc_prepaid_vote1 + 2*no_cc_prepaid_vote2 + 3*no_cc_prepaid_vote3 + 4*no_cc_prepaid_vote4 + 5*no_cc_prepaid_vote5)
# no_cc score of cost by PI
(no_cc_cash_vote = 1*no_cc_cash_vote1 + 2*no_cc_cash_vote2 + 3*no_cc_cash_vote3 + 4*no_cc_cash_vote4 + 5*no_cc_cash_vote5)
(no_cc_check_vote = 1*no_cc_check_vote1 + 2*no_cc_check_vote2 + 3*no_cc_check_vote3 + 4*no_cc_check_vote4 + 5*no_cc_check_vote5)
(no_cc_credit_vote = 1*no_cc_credit_vote1 + 2*no_cc_credit_vote2 + 3*no_cc_credit_vote3 + 4*no_cc_credit_vote4 + 5*no_cc_credit_vote5)
(no_cc_debit_vote = 1*no_cc_debit_vote1 + 2*no_cc_debit_vote2 + 3*no_cc_debit_vote3 + 4*no_cc_debit_vote4 + 5*no_cc_debit_vote5)
(no_cc_prepaid_vote = 1*no_cc_prepaid_vote1 + 2*no_cc_prepaid_vote2 + 3*no_cc_prepaid_vote3 + 4*no_cc_prepaid_vote4 + 5*no_cc_prepaid_vote5)
rank(c(no_cc_cash_vote, no_cc_check_vote, no_cc_credit_vote, no_cc_debit_vote, no_cc_prepaid_vote))# Note: Highest rank = lowest cost
(no_cc_vote_vec = c(no_cc_cash_vote, no_cc_check_vote, no_cc_credit_vote, no_cc_debit_vote, no_cc_prepaid_vote))# vector of no_cc votes for PI
(no_cc_vote_score = no_cc_vote_vec/nrow(no_cc_unique_id))
#
# no_dc score of cost by PI
(no_dc_cash_vote = 1*no_dc_cash_vote1 + 2*no_dc_cash_vote2 + 3*no_dc_cash_vote3 + 4*no_dc_cash_vote4 + 5*no_dc_cash_vote5)
(no_dc_check_vote = 1*no_dc_check_vote1 + 2*no_dc_check_vote2 + 3*no_dc_check_vote3 + 4*no_dc_check_vote4 + 5*no_dc_check_vote5)
(no_dc_credit_vote = 1*no_dc_credit_vote1 + 2*no_dc_credit_vote2 + 3*no_dc_credit_vote3 + 4*no_dc_credit_vote4 + 5*no_dc_credit_vote5)
(no_dc_debit_vote = 1*no_dc_debit_vote1 + 2*no_dc_debit_vote2 + 3*no_dc_debit_vote3 + 4*no_dc_debit_vote4 + 5*no_dc_debit_vote5)
(no_dc_prepaid_vote = 1*no_dc_prepaid_vote1 + 2*no_dc_prepaid_vote2 + 3*no_dc_prepaid_vote3 + 4*no_dc_prepaid_vote4 + 5*no_dc_prepaid_vote5)
# no_dc score of cost by PI
(no_dc_cash_vote = 1*no_dc_cash_vote1 + 2*no_dc_cash_vote2 + 3*no_dc_cash_vote3 + 4*no_dc_cash_vote4 + 5*no_dc_cash_vote5)
(no_dc_check_vote = 1*no_dc_check_vote1 + 2*no_dc_check_vote2 + 3*no_dc_check_vote3 + 4*no_dc_check_vote4 + 5*no_dc_check_vote5)
(no_dc_credit_vote = 1*no_dc_credit_vote1 + 2*no_dc_credit_vote2 + 3*no_dc_credit_vote3 + 4*no_dc_credit_vote4 + 5*no_dc_credit_vote5)
(no_dc_debit_vote = 1*no_dc_debit_vote1 + 2*no_dc_debit_vote2 + 3*no_dc_debit_vote3 + 4*no_dc_debit_vote4 + 5*no_dc_debit_vote5)
(no_dc_prepaid_vote = 1*no_dc_prepaid_vote1 + 2*no_dc_prepaid_vote2 + 3*no_dc_prepaid_vote3 + 4*no_dc_prepaid_vote4 + 5*no_dc_prepaid_vote5)
rank(c(no_dc_cash_vote, no_dc_check_vote, no_dc_credit_vote, no_dc_debit_vote, no_dc_prepaid_vote))# Note: Highest rank = lowest cost
(no_dc_vote_vec = c(no_dc_cash_vote, no_dc_check_vote, no_dc_credit_vote, no_dc_debit_vote, no_dc_prepaid_vote))# vector of no_dc votes for PI
(no_dc_vote_score = no_dc_vote_vec/nrow(no_dc_unique_id))
#
# none_banked score of cost by PI
(none_banked_cash_vote = 1*none_banked_cash_vote1 + 2*none_banked_cash_vote2 + 3*none_banked_cash_vote3 + 4*none_banked_cash_vote4 + 5*none_banked_cash_vote5)
(none_banked_check_vote = 1*none_banked_check_vote1 + 2*none_banked_check_vote2 + 3*none_banked_check_vote3 + 4*none_banked_check_vote4 + 5*none_banked_check_vote5)
(none_banked_credit_vote = 1*none_banked_credit_vote1 + 2*none_banked_credit_vote2 + 3*none_banked_credit_vote3 + 4*none_banked_credit_vote4 + 5*none_banked_credit_vote5)
(none_banked_debit_vote = 1*none_banked_debit_vote1 + 2*none_banked_debit_vote2 + 3*none_banked_debit_vote3 + 4*none_banked_debit_vote4 + 5*none_banked_debit_vote5)
(none_banked_prepaid_vote = 1*none_banked_prepaid_vote1 + 2*none_banked_prepaid_vote2 + 3*none_banked_prepaid_vote3 + 4*none_banked_prepaid_vote4 + 5*none_banked_prepaid_vote5)
rank(c(none_banked_cash_vote, none_banked_check_vote, none_banked_credit_vote, none_banked_debit_vote, none_banked_prepaid_vote))# Note: Highest rank = lowest cost
(none_banked_vote_vec = c(none_banked_cash_vote, none_banked_check_vote, none_banked_credit_vote, none_banked_debit_vote, none_banked_prepaid_vote))# vector of none_banked votes for PI
(none_banked_vote_score = none_banked_vote_vec/nrow(none_banked_unique_id))
#
# none unbanked score of cost by PI
(none_unb_cash_vote = 1*none_unb_cash_vote1 + 2*none_unb_cash_vote2 + 3*none_unb_cash_vote3 + 4*none_unb_cash_vote4 + 5*none_unb_cash_vote5)
(none_unb_check_vote = 1*none_unb_check_vote1 + 2*none_unb_check_vote2 + 3*none_unb_check_vote3 + 4*none_unb_check_vote4 + 5*none_unb_check_vote5)
(none_unb_credit_vote = 1*none_unb_credit_vote1 + 2*none_unb_credit_vote2 + 3*none_unb_credit_vote3 + 4*none_unb_credit_vote4 + 5*none_unb_credit_vote5)
(none_unb_debit_vote = 1*none_unb_debit_vote1 + 2*none_unb_debit_vote2 + 3*none_unb_debit_vote3 + 4*none_unb_debit_vote4 + 5*none_unb_debit_vote5)
(none_unb_prepaid_vote = 1*none_unb_prepaid_vote1 + 2*none_unb_prepaid_vote2 + 3*none_unb_prepaid_vote3 + 4*none_unb_prepaid_vote4 + 5*none_unb_prepaid_vote5)
rank(c(none_unb_cash_vote, none_unb_check_vote, none_unb_credit_vote, none_unb_debit_vote, none_unb_prepaid_vote))# Note: Highest rank = lowest cost
(none_unb_vote_vec = c(none_unb_cash_vote, none_unb_check_vote, none_unb_credit_vote, none_unb_debit_vote, none_unb_prepaid_vote))# vector of none_unb votes for PI
(none_unb_vote_score = none_unb_vote_vec/nrow(none_unb_unique_id))
rank(c(none_unb_cash_vote, none_unb_check_vote, none_unb_credit_vote, none_unb_debit_vote, none_unb_prepaid_vote))# Note: Highest rank = lowest cost
(none_unb_vote_vec = c(none_unb_cash_vote, none_unb_check_vote, none_unb_credit_vote, none_unb_debit_vote, none_unb_prepaid_vote))# vector of none_unb votes for PI
(none_unb_vote_score = none_unb_vote_vec/nrow(none_unb_unique_id))
#

## Convert this cost ranking to graphs
# Below ranking by 5 subsamples of respondents (5 = lowest cost)
all_vote_score
both_vote_score
no_cc_vote_score
no_dc_vote_score
none_banked_vote_score
none_unb_vote_score
# Below, reversing the score (1 = lowest score, 5 = highest) revised 190709, line 1003
(all_vote_score_rev = 6- all_vote_score)# skip All in graph
(both_vote_score_rev = 6- both_vote_score)
(no_cc_vote_score_rev = 6- no_cc_vote_score)
(no_dc_vote_score_rev = 6- no_dc_vote_score)
(none_banked_vote_score_rev = 6- none_banked_vote_score)
(none_unb_vote_score_rev = 6- none_unb_vote_score)
#
plot(both_vote_score_rev, pch = 15, col = "darkgreen", cex = 3, axes = F, ylim = c(1.4, 3.6), ylab = "Payment insturment cost ratings", xlab = "")
points(no_cc_vote_score_rev, pch = 16, col = "magenta", cex = 3)
points(no_dc_vote_score_rev, pch = 18, col = "black", cex = 3)
points(none_banked_vote_score_rev, pch = 17, col = "blue", cex = 3)
points(none_unb_vote_score_rev, pch = 17, col = "red", cex = 3)
axis(2, at=seq(1.4, 3.6, 0.2),las=2)
axis(1,at=c(1:5), labels=c("Cash","Check","Credit card", "Debit card", "Prepaid card"))
#axis(4, at=seq(0,100, 10),las=2)
legend(2.5,2.3,c("Both cards","No credit card","No debit card", "None banked", "None unbanked"), col = c("darkgreen", "magenta", "black", "blue", "red"), pch = c(15, 16, 18, 17), pt.cex = 2)# pt.cex increases symbol size only
# End of Figure 4. Starts on Line 819, ends on line 1116

### Appendix A (w/ Table 5: Running demog regressions on the entire diary (not restricted to in-person trans or 6 merchant types) 
# Recall already-defined 4 dataframes according to adoption profile
dim(all_unique_id) # all respondents
names(all_unique_id)
# Removing NAs from adoption profiles
#all_unique_id = all_unique_id %>% filter(!is.na(all_unique_id$cc_adopt) & !is.na(all_unique_id$dc_adopt & !is.na(all_unique_id$bnk_acnt_adopt)) )
#dim(all_unique_id2)
#
# Define new variables according to card adoption profile
all_unique_id$both = 0
#
all_unique_id[all_unique_id$cc_adopt %in% 1 & all_unique_id$dc_adopt %in% 1, ]$both = 1
table(all_unique_id$both)
#
all_unique_id$no_cc = 0
all_unique_id[all_unique_id$cc_adopt %in% 0, ]$no_cc = 1
table(all_unique_id$no_cc)
#
all_unique_id$no_dc = 0
all_unique_id[all_unique_id$dc_adopt %in% 0 , ]$no_dc = 1
table(all_unique_id$no_dc)
#
all_unique_id$none_banked = 0
all_unique_id[all_unique_id$cc_adopt %in% 0 & all_unique_id$dc_adopt %in% 0 & all_unique_id$bnk_acnt_adopt %in% 1, ]$none_banked = 1
table(all_unique_id$none_banked)
#
all_unique_id$none_unb = 0
all_unique_id[all_unique_id$cc_adopt %in% 0 & all_unique_id$dc_adopt %in% 0 & all_unique_id$bnk_acnt_adopt %in% 0, ]$none_unb = 1

all_unique_id[all_unique_id$cc_adopt==0 & all_unique_id$dc_adopt==0  & !is.na(all_unique_id$cc_adopt) & !is.na(all_unique_id$dc_adopt) & !is.na(all_unique_id$bnk_acnt_adopt & all_unique_id$bnk_acnt_adopt==0 ), ]$none_unb = 1
table(all_unique_id$none_unb)
#
all_unique_id$both = as.factor(all_unique_id$both)
all_unique_id$no_cc = as.factor(all_unique_id$no_cc)
all_unique_id$no_dc = as.factor(all_unique_id$no_dc)
all_unique_id$none_banked = as.factor(all_unique_id$none_banked)
all_unique_id$none_unb = as.factor(all_unique_id$none_unb)

# Simplifying education (less levels) w/ a new variable "educ"
names(all_unique_id)
table(all_unique_id$education)
str(all_unique_id$education) 
all_unique_id$education = as.integer(all_unique_id$education) # change from factor to integer for the oprations below
all_unique_id$educ = NA # new variable "education"
all_unique_id$educ[all_unique_id$education <= 9] = "HS_or_less"
all_unique_id$educ[all_unique_id$education == 10] = "Some_college"
all_unique_id$educ[all_unique_id$education >= 11 & all_unique_id$education <= 12] = "Assoc_degree"
all_unique_id$educ[all_unique_id$education == 13 ] = "BA_degree"
all_unique_id$educ[all_unique_id$education >= 14] = "MA_or_higher"
table(all_unique_id$educ)
all_unique_id$educ = factor(all_unique_id$educ, levels = c("HS_or_less", "Some_college", "Assoc_degree", "BA_degree", "MA_or_higher")) #HS or less is ref
levels(all_unique_id$educ)
table(all_unique_id$educ)
#
table(all_unique_id$married)
str(all_unique_id$married)
colnames(all_unique_id)[colnames(all_unique_id)=="married"] = "marital"# rename var
table(all_unique_id$marital)
str(all_unique_id$marital)
#
table(all_unique_id$employed)
colnames(all_unique_id)[colnames(all_unique_id)=="employed"] = "work"# rename var
table(all_unique_id$work)

#Regression begin
names(all_unique_id)
str(all_unique_id[, c("both","no_cc","no_dc","none_banked", "none_unb","work","marital","educ","age","gender","income","hh_size")])
both1 = both~age+log(1+income)+hh_size+work+marital+educ+gender# 
no_cc1 = no_cc~age+log(1+income)+hh_size+work+marital+educ+gender# 
no_dc1 = no_dc~age+log(1+income)+hh_size+work+marital+educ+gender# 
none_banked1 = none_banked~age+log(1+income)+hh_size+work+marital+educ+gender# 
none_unb1 = none_unb~age+log(1+income)+hh_size+work+marital+educ+gender# 
#
both_mfx = logitmfx(both1, atmean = F, data = all_unique_id)
both_mfx
#
no_cc_mfx = logitmfx(no_cc1, atmean = F, data = all_unique_id)
no_cc_mfx
#
no_dc_mfx = logitmfx(no_dc1, atmean = F, data = all_unique_id)
no_dc_mfx
#
none_banked_mfx = logitmfx(none_banked1, atmean = F, data = all_unique_id)
none_banked_mfx# has only 21 obs = 21 resp 
#
none_unb_mfx = logitmfx(none_unb1, atmean = F, data = all_unique_id)
none_unb_mfx
# NOTE: The last 2 regressions produced complete separation, so I switched to log(1+income)

#
# Creating a table combining all 5 regressions
(both_dfdx = as.data.frame(both_mfx$mfxest))
# Below, fix a problem that ***, **, * do not appear => need to add column)
for (i in 1:nrow(both_dfdx)){both_dfdx[i,5]=' '}
for (i in 1:nrow(both_dfdx)){
  if(both_dfdx[i,4]<=0.05){both_dfdx[i,5]='*'}
}
for (i in 1:nrow(both_dfdx)){
  if(both_dfdx[i,4]<=0.01){both_dfdx[i,5]='**'}
}
for (i in 1:nrow(both_dfdx)){
  if(both_dfdx[i,4]<=0.001){both_dfdx[i,5]='***'}
}
both_dfdx # This table has a new column with *, **, and ***
dim(both_dfdx)
names(both_dfdx)
(both_dfdx = subset(both_dfdx, select = c("dF/dx", "V5")))
colnames(both_dfdx)[colnames(both_dfdx)=="V5"] = "Sig."
names(both_dfdx)
#
(no_cc_dfdx = as.data.frame(no_cc_mfx$mfxest))
# Below, fix a problem that ***, **, * do not appear => need to add column)
for (i in 1:nrow(no_cc_dfdx)){no_cc_dfdx[i,5]=' '}
for (i in 1:nrow(no_cc_dfdx)){
  if(no_cc_dfdx[i,4]<=0.05){no_cc_dfdx[i,5]='*'}
}
for (i in 1:nrow(no_cc_dfdx)){
  if(no_cc_dfdx[i,4]<=0.01){no_cc_dfdx[i,5]='**'}
}
for (i in 1:nrow(no_cc_dfdx)){
  if(no_cc_dfdx[i,4]<=0.001){no_cc_dfdx[i,5]='***'}
}
no_cc_dfdx # This table has a new column with *, **, and ***
dim(no_cc_dfdx)
names(no_cc_dfdx)
(no_cc_dfdx = subset(no_cc_dfdx, select = c("dF/dx", "V5")))
colnames(no_cc_dfdx)[colnames(no_cc_dfdx)=="V5"] = "Sig."
names(no_cc_dfdx)
#
(no_dc_dfdx = as.data.frame(no_dc_mfx$mfxest))
# Below, fix a problem that ***, **, * do not appear => need to add column)
for (i in 1:nrow(no_dc_dfdx)){no_dc_dfdx[i,5]=' '}
for (i in 1:nrow(no_dc_dfdx)){
  if(no_dc_dfdx[i,4]<=0.05){no_dc_dfdx[i,5]='*'}
}
for (i in 1:nrow(no_dc_dfdx)){
  if(no_dc_dfdx[i,4]<=0.01){no_dc_dfdx[i,5]='**'}
}
for (i in 1:nrow(no_dc_dfdx)){
  if(no_dc_dfdx[i,4]<=0.001){no_dc_dfdx[i,5]='***'}
}
no_dc_dfdx # This table has a new column with *, **, and ***
dim(no_dc_dfdx)
names(no_dc_dfdx)
(no_dc_dfdx = subset(no_dc_dfdx, select = c("dF/dx", "V5")))
colnames(no_dc_dfdx)[colnames(no_dc_dfdx)=="V5"] = "Sig."
names(no_dc_dfdx)
#
(none_banked_dfdx = as.data.frame(none_banked_mfx$mfxest))
# Below, fix a problem that ***, **, * do not appear => need to add column)
for (i in 1:nrow(none_banked_dfdx)){none_banked_dfdx[i,5]=' '}
for (i in 1:nrow(none_banked_dfdx)){
  if(none_banked_dfdx[i,4]<=0.05){none_banked_dfdx[i,5]='*'}
}
for (i in 1:nrow(none_banked_dfdx)){
  if(none_banked_dfdx[i,4]<=0.01){none_banked_dfdx[i,5]='**'}
}
for (i in 1:nrow(none_banked_dfdx)){
  if(none_banked_dfdx[i,4]<=0.001){none_banked_dfdx[i,5]='***'}
}
none_banked_dfdx # This table has a new column with *, **, and ***
dim(none_banked_dfdx)
names(none_banked_dfdx)
(none_banked_dfdx = subset(none_banked_dfdx, select = c("dF/dx", "V5")))
colnames(none_banked_dfdx)[colnames(none_banked_dfdx)=="V5"] = "Sig."
names(none_banked_dfdx)
#
#
(none_unb_dfdx = as.data.frame(none_unb_mfx$mfxest))
# Below, fix a problem that ***, **, * do not appear => need to add column)
for (i in 1:nrow(none_unb_dfdx)){none_unb_dfdx[i,5]=' '}
for (i in 1:nrow(none_unb_dfdx)){
  if(none_unb_dfdx[i,4]<=0.05){none_unb_dfdx[i,5]='*'}
}
for (i in 1:nrow(none_unb_dfdx)){
  if(none_unb_dfdx[i,4]<=0.01){none_unb_dfdx[i,5]='**'}
}
for (i in 1:nrow(none_unb_dfdx)){
  if(none_unb_dfdx[i,4]<=0.001){none_unb_dfdx[i,5]='***'}
}
none_unb_dfdx # This table has a new column with *, **, and ***
dim(none_unb_dfdx)
names(none_unb_dfdx)
(none_unb_dfdx = subset(none_unb_dfdx, select = c("dF/dx", "V5")))
colnames(none_unb_dfdx)[colnames(none_unb_dfdx)=="V5"] = "Sig."
names(none_unb_dfdx)
#
# merging 5 regression tables
(dfdx_df = cbind(both_dfdx, no_cc_dfdx, no_dc_dfdx, none_banked_dfdx, none_unb_dfdx))
dim(dfdx_df)
xtable(dfdx_df, digits = 5)
# some info
nrow(all_unique_id)# num  obs (resp) in the regressions
nrow(subset(all_unique_id, income == 0))# num  resp with income ==0


# End of Appendix A

### End of cashless code ###
