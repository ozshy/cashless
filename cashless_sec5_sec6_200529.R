# cashless_sec5_sec6_200529.R presenting relative CS change rate to Table 5.  
# cashless_sec5_sec6_200528.R Sec.6, changing mlogit regression from all data to both subsample. Line 471 
# cashless_sec5_sec6_200526.R adding median assessments to a new Table 4 in Sec.5
# cashless_sec5_sec6_200523.R deleting old Table 4 (coefficients are no in the text). Removing squared assessments b/c they yield the same CS. 
# cashless_sec5_sec6_200522.R focusing on med and 2 X med assessement only
# cashless_sec5_sec6_200521.R last version with avg avg^2, med, med^2 assessments
# cashless_sec5_sec6_200520.R typing cashless-57.tex using median and median of squared assessments. 
# cashless_sec5_sec6_200519.R trying avg and med assessments, but running the entire sample for coefficients, and then subsetting for computing CS
# cashless_sec5_sec6_200518.R Going back to mixed effects
# cashless_sec5_sec6_200517b.R introduced avg assess squared. Failed to get regressions for subsets such as unbanked. 
# cashless_sec5_sec6_200426.R Major revision starting cashless-56.tex, trying to use assessments more objectively by taking economy-wide average as given.
# cashless_sec_6_tab_3_191020.R
# cashless_mult_191019.R Removing reflevel from regressions. Not needed when assessments vary with PI. That did not matter anyway, as R ignored reflevel. 
# cashless_mult_191015.R Major changes: Starting Section 6: Line 323 below: 
# 1. Do not set coefficients to zero, when computing v-cash (utility of cash)
# 2. Fixing mistake w.r.t. noneunb (removed "debit" from alt in regression model)
# 3. Remove all regressions b/c it regresses on PI some resp don't have. 
# cashless_mult_190930.R Section 5: Discrete choice random utility analysis. Note: This version cuts the <$50 and <$20 analysis. I also ADDED mixed logit (random coefficients). (see versions of the R-code before 190930 for previous versions of the paper: cashless_43.tex )
# cashless_main_190930.R Moving random utility (multinomial) analysis to a separte R-file

### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
#library(ggplot2)
library(spatstat) # for weighted.median
#library(mfx)
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
library(mlogit)
#library(mnlogit)# Where I replicate the existing mlogit regressions and experiment with individual effects. 
#library(tibble)
setwd("~/Papers/cashless/cashless-coding")# NOTE, your working directory is different. Adjust accordingly!!! 
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

## adding var with respondent's adoption profile: Both_cards, DC_only, CC_only, None_banked, None_unb. Then, removing NAs. 
# Note: Adopt is first used in Figure 2 (not in any Table)
# Note: From cashless_190613b, changed adoption profile to match those in the tables. 
d9 = d8
table(d9$bnk_acnt_adopt)
# num trans by banked and unbanked. 
unbanked_tran = subset(d9, bnk_acnt_adopt == 0)#cotains payments by unbanked only
nrow(unbanked_tran)# trans by unbanked
length(unique(unbanked_tran$uasid))# num of unbanked
length(unique(unbanked_tran$uasid))/length(unique(d9$uasid))# fraction of unbanked respondents
table(unbanked_tran$pi)# some inconsistency => need to redefine unbanked as those who also don't have credit and debit cards. Those who have debit card trans may have mistakenly stated that they don't have a bank account. 
#
# New var adopt with adoption profile of the resp for each payment
d9$adopt[d9$dc_adopt==1 & d9$cc_adopt==1] = "Both_cards"
d9$adopt[d9$cc_adopt==0 & d9$pi != "credit"] = "No_cc"
d9$adopt[d9$dc_adopt==0 & d9$pi != "debit"] = "No_dc"
# d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0] = "None" # split below
d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0  & d9$bnk_acnt_adopt==1 & d9$pi != "credit" & d9$pi != "debit"] = "None_banked"
d9$adopt[d9$dc_adopt==0 & d9$cc_adopt==0  & d9$bnk_acnt_adopt==0 & d9$pi != "credit" & d9$pi != "debit"] = "None_unbanked"
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

# simplifying variables for the regressions
# First do it for all trans. Later, restrict it to no_dc_cc only
inst5 = d10
inst5$paycash = 0# regression left side initialize paid w/ cards
inst5[inst5$pi == "cash", ]$paycash = 1# left side paid w/ cash
head(inst5$paycash, 10)
head(inst5$pi, 10)
table(inst5$paycash)
percent(prop.table(table(inst5$paycash)))# % of trans who pay cash
#
# Simplifying education (less levels) w/ a new variable "educ"
table(inst5$education)
str(inst5$education) 
inst5$education = as.integer(inst5$education) # change from factor to integer for the oprations below
inst5$educ = NA #new variable "educ" to simplfy education
inst5$educ[inst5$education <= 8] = "Elem_or_less"
inst5$educ[inst5$education >= 9 & inst5$education < 12] = "High_school"
inst5$educ[inst5$education >= 12 & inst5$education < 14] = "Assoc_or_college"
inst5$educ[inst5$education >= 14] = "MA_or_higher"
table(inst5$educ)
inst5$educ = factor(inst5$educ, levels = c("Elem_or_less", "High_school", "Assoc_or_college", "MA_or_higher")) #elem or less is ref
levels(inst5$educ)
table(inst5$educ)

### Section 5: Cost assessment by transaction (note: Unlike Figure 4 which plots averages among unique respondents. Here, it is attached to each transaction)
# NOTE on raw SCPC assessments: 1 = lowest rank, 5 = highest rank (including cost)
# For cost 5 (highest rank) means lowest cost!
inst6 = inst5
inst7 = inst6
dim(inst7)
# Reversing reverse) COST rating: 
# Before the change 1=highest cost 5=lowest cost (highest rating)
# After the change 1=lowest cost 5=highest cost (lowest rating). Now consistent with Figure 4
inst7$assess_cost_cash = 6 - inst6$assess_cost_cash
summary(inst7$assess_cost_cash)
inst7$assess_cost_check = 6 - inst6$assess_cost_check
summary(inst7$assess_cost_check)
inst7$assess_cost_debit = 6 - inst6$assess_cost_debit
summary(inst7$assess_cost_debit)
inst7$assess_cost_credit = 6 - inst6$assess_cost_credit
summary(inst7$assess_cost_credit)
inst7$assess_cost_prepaid = 6 - inst6$assess_cost_prepaid
summary(inst7$assess_cost_prepaid)
#
## Start assessment discussion in Section 5, Table 3 started started Line 181 ended Line 326
## NOTE: All the "cost_actual", "security_actual"... analysis is used only for the discussion in Section 5.1 showing that most respondents chose the PI with highest attributes. NOT used in the multinomial random utility model. 
table(inst7$assess_cost_cash)# rate dist of cost of cash
table(inst7$assess_cost_check)
table(inst7$assess_cost_debit)
table(inst7$assess_cost_credit)
table(inst7$assess_cost_prepaid)
#
## Define "cost_actual" which will be the cost assessment of the respondent to the PI actually used (1:5). this is NOT dollar-valued! 
inst7$cost_actual = NA #initialize cost_actual
inst7[inst7$pi == "cash", ]$cost_actual = inst7[inst7$pi == "cash", ]$assess_cost_cash
inst7[inst7$pi == "check", ]$cost_actual = inst7[inst7$pi == "check", ]$assess_cost_check
inst7[inst7$pi == "debit", ]$cost_actual = inst7[inst7$pi == "debit", ]$assess_cost_debit
inst7[inst7$pi == "credit", ]$cost_actual = inst7[inst7$pi == "credit", ]$assess_cost_credit
inst7[inst7$pi == "prepaid", ]$cost_actual = inst7[inst7$pi == "prepaid", ]$assess_cost_prepaid
#
## Define "acceptance_actual" which will be the acceptance assessment of the respondent to the PI actually used (1:5). 5 = highest score (most accepted)
table(inst7$assess_acceptance_cash)
table(inst7$assess_acceptance_check)
table(inst7$assess_acceptance_debit)
table(inst7$assess_acceptance_credit)
table(inst7$assess_acceptance_prepaid)
#
inst7$acceptance_actual = NA #initialize acceptance_actual
inst7[inst7$pi == "cash", ]$acceptance_actual = inst7[inst7$pi == "cash", ]$assess_acceptance_cash
inst7[inst7$pi == "check", ]$acceptance_actual = inst7[inst7$pi == "check", ]$assess_acceptance_check
inst7[inst7$pi == "debit", ]$acceptance_actual = inst7[inst7$pi == "debit", ]$assess_acceptance_debit
inst7[inst7$pi == "credit", ]$acceptance_actual = inst7[inst7$pi == "credit", ]$assess_acceptance_credit
inst7[inst7$pi == "prepaid", ]$acceptance_actual = inst7[inst7$pi == "prepaid", ]$assess_acceptance_prepaid
#
## Define "convenience_actual" which will be the convenience  assessment of the respondent to the PI actually used (1:5). 5 = highest score (most convenient)
table(inst7$assess_convenience_cash)
table(inst7$assess_convenience_check)
table(inst7$assess_convenience_debit)
table(inst7$assess_convenience_credit)
table(inst7$assess_convenience_prepaid)
#
inst7$convenience_actual = NA #initialize _convenience_actual
inst7[inst7$pi == "cash", ]$convenience_actual = inst7[inst7$pi == "cash", ]$assess_convenience_cash
inst7[inst7$pi == "check", ]$convenience_actual = inst7[inst7$pi == "check", ]$assess_convenience_check
inst7[inst7$pi == "debit", ]$convenience_actual = inst7[inst7$pi == "debit", ]$assess_convenience_debit
inst7[inst7$pi == "credit", ]$convenience_actual = inst7[inst7$pi == "credit", ]$assess_convenience_credit
inst7[inst7$pi == "prepaid", ]$convenience_actual = inst7[inst7$pi == "prepaid", ]$assess_convenience_prepaid
#
## Define "security_actual" which will be the security  assessment of the respondent to the PI actually used (1:5) 5=highest score (most secure)
table(inst7$assess_security_cash)
table(inst7$assess_security_check)
table(inst7$assess_security_debit)
table(inst7$assess_security_credit)
table(inst7$assess_security_prepaid)
#
inst7$security_actual = NA #initialize _security_actual
inst7[inst7$pi == "cash", ]$security_actual = inst7[inst7$pi == "cash", ]$assess_security_cash
inst7[inst7$pi == "check", ]$security_actual = inst7[inst7$pi == "check", ]$assess_security_check
inst7[inst7$pi == "debit", ]$security_actual = inst7[inst7$pi == "debit", ]$assess_security_debit
inst7[inst7$pi == "credit", ]$security_actual = inst7[inst7$pi == "credit", ]$assess_security_credit
inst7[inst7$pi == "prepaid", ]$security_actual = inst7[inst7$pi == "prepaid", ]$assess_security_prepaid
#
## Define "record_actual" which will be the record  assessment of the respondent to the PI actually used (1:5) 5 = highest score (easiest to keep records)
table(inst7$assess_record_cash)
table(inst7$assess_record_check)
table(inst7$assess_record_debit)
table(inst7$assess_record_credit)
table(inst7$assess_record_prepaid)
#
inst7$record_actual = NA #initialize _record_actual
inst7[inst7$pi == "cash", ]$record_actual = inst7[inst7$pi == "cash", ]$assess_record_cash
inst7[inst7$pi == "check", ]$record_actual = inst7[inst7$pi == "check", ]$assess_record_check
inst7[inst7$pi == "debit", ]$record_actual = inst7[inst7$pi == "debit", ]$assess_record_debit
inst7[inst7$pi == "credit", ]$record_actual = inst7[inst7$pi == "credit", ]$assess_record_credit
inst7[inst7$pi == "prepaid", ]$record_actual = inst7[inst7$pi == "prepaid", ]$assess_record_prepaid
#
## Define "setup_actual" which will be the setup  assessment of the respondent to the PI actually used (1:5) 5 = highest score (easiest to set up)
table(inst7$assess_setup_cash)
table(inst7$assess_setup_check)
table(inst7$assess_setup_debit)
table(inst7$assess_setup_credit)
table(inst7$assess_setup_prepaid)
#
inst7$setup_actual = NA #initialize _setup_actual
inst7[inst7$pi == "cash", ]$setup_actual = inst7[inst7$pi == "cash", ]$assess_setup_cash
inst7[inst7$pi == "check", ]$setup_actual = inst7[inst7$pi == "check", ]$assess_setup_check
inst7[inst7$pi == "debit", ]$setup_actual = inst7[inst7$pi == "debit", ]$assess_setup_debit
inst7[inst7$pi == "credit", ]$setup_actual = inst7[inst7$pi == "credit", ]$assess_setup_credit
inst7[inst7$pi == "prepaid", ]$setup_actual = inst7[inst7$pi == "prepaid", ]$assess_setup_prepaid
#
names(inst7)
# verify that the assessements were correctly assigned to score for each PI
head(inst7[inst7$pi == "cash", c("uasid","cost_actual", "assess_cost_cash" , "pi", "acceptance_actual", "assess_acceptance_cash")])
head(inst7[inst7$pi == "check", c("uasid","cost_actual", "assess_cost_check", "pi", "acceptance_actual", "assess_acceptance_check")])
head(inst7[inst7$pi == "debit", c("uasid","cost_actual", "assess_cost_debit", "pi", "acceptance_actual", "assess_acceptance_debit")])
head(inst7[inst7$pi == "credit", c("uasid","cost_actual", "assess_cost_credit", "pi", "acceptance_actual", "assess_acceptance_credit")])
head(inst7[inst7$pi == "prepaid", c("uasid","cost_actual", "assess_cost_prepaid", "pi", "acceptance_actual", "assess_acceptance_prepaid")])
#
head(inst7[inst7$pi == "cash", c("uasid","acceptance_actual", "assess_acceptance_cash" , "pi", "convenience_actual", "assess_convenience_cash")])
head(inst7[inst7$pi == "check", c("uasid","acceptance_actual", "assess_acceptance_check", "pi", "convenience_actual", "assess_convenience_check")])
head(inst7[inst7$pi == "debit", c("uasid","acceptance_actual", "assess_acceptance_debit", "pi", "convenience_actual", "assess_convenience_debit")])
head(inst7[inst7$pi == "credit", c("uasid","acceptance_actual", "assess_acceptance_credit", "pi", "convenience_actual", "assess_convenience_credit")])
head(inst7[inst7$pi == "prepaid", c("uasid","acceptance_actual", "assess_acceptance_prepaid", "pi", "convenience_actual", "assess_convenience_prepaid")])
#
head(inst7[inst7$pi == "cash", c("uasid","setup_actual", "assess_setup_cash" , "pi", "record_actual", "assess_record_cash")])
head(inst7[inst7$pi == "check", c("uasid","setup_actual", "assess_setup_check", "pi", "record_actual", "assess_record_check")])
head(inst7[inst7$pi == "debit", c("uasid","setup_actual", "assess_setup_debit", "pi", "record_actual", "assess_record_debit")])
head(inst7[inst7$pi == "credit", c("uasid","setup_actual", "assess_setup_credit", "pi", "record_actual", "assess_record_credit")])
head(inst7[inst7$pi == "prepaid", c("uasid","setup_actual", "assess_setup_prepaid", "pi", "record_actual", "assess_record_prepaid")])
#
table(inst7$cost_actual)# This shows resp who did not choose to pay with the least-costly PI scored the highest
sum(table(inst7$cost_actual))# check sums up to total num trans
table(inst7$security_actual)# This shows resp who did not choose to pay with the PI
sum(table(inst7$security_actual))# check sums up to total num trans
table(inst7$acceptance_actual)#
sum(table(inst7$acceptance_actual))# check sums up to total num trans
table(inst7$convenience_actual)#
sum(table(inst7$convenience_actual))# check sums up to total num trans
table(inst7$setup_actual)#
table(inst7$record_actual)#
#
percent(table(inst7$cost_actual)/sum(table(inst7$cost_actual)))# Nice results, showing that 56.9% paid with the least costly, 20.5% second costly, 15.2% third clostly
percent(table(inst7$acceptance_actual)/sum(table(inst7$acceptance_actual)))# Nice results, 73.75% paid with the PI ranked highest for acceptance
percent(table(inst7$convenience_actual)/sum(table(inst7$convenience_actual)))# Nice results, 64.9% paid with the PI ranked *lowest* for acceptance
percent(table(inst7$security_actual)/sum(table(inst7$security_actual)))#  21% paid with the PI ranked most secured
percent(table(inst7$setup_actual)/sum(table(inst7$setup_actual)))#  45.7% paid with the PI ranked highest for setup
percent(table(inst7$record_actual)/sum(table(inst7$record_actual)))# Nice results, 42% paid with the PI ranked highest for acceptance
#
# Preparding for an xtable for the paper
(cost_actual_vec = as.vector(table(inst7$cost_actual)/sum(table(inst7$cost_actual)))) # converting table col to vec
(cost_security_vec = as.vector(table(inst7$security_actual)/sum(table(inst7$security_actual)))) # converting table col to vec
(cost_convenience_vec = as.vector(table(inst7$convenience_actual)/sum(table(inst7$cost_actual)))) # converting table col to vec
#
(assess.df = rbind(cost_actual_vec, cost_security_vec, cost_convenience_vec))
(colnames(assess.df) = c("1", "2", "3", "4", "5"))
assess.df
dim(assess.df)
str(assess.df)
assess.df2 = as.numeric(assess.df)

Assessment = c("Cost", "Security", "Convenience")
(assess.df3 = data.frame(cbind(Assessment, round(100*assess.df, digits = 2))))
dim(assess.df3)
str(assess.df3)
#
# below, create matrix w\ 1 extra column to indicate number of digits for each row
#(digitm = matrix(c(rep(0,1), rep(2,6)), nrow = 3, ncol = 7, byrow = T))
#
print(xtable(assess.df3, digits = 0), include.rownames = F, hline.after = c(0))
# End of assessment discussion Table 3 in Section 5, started Line 181 ended Line 326

## section 5 continue with new Table 4 (median assessments by PI)

# Need to find avg and med of each assessment by averaging respondents (not by payments). Note: this averaging is over all respondents (not by subgroups)
length(unique(inst7$uasid))# num resp
inst7_resp = inst7[!duplicated(inst7$uasid), ]
dim(inst7_resp)
sum(inst7_resp$weight_1)
#Rescaling weight_1 
inst7_resp$w = nrow(inst7_resp) * inst7_resp$weight_1/sum(inst7_resp$weight_1)
sum(inst7_resp$w)
nrow(inst7_resp)
#
names(inst7_resp)

# median cost assessment by PI weighted
(assess_cost_cash_med = weighted.median(inst7_resp$assess_cost_cash, inst7_resp$w, na.rm = T))
(assess_cost_check_med = weighted.median(inst7_resp$assess_cost_check, inst7_resp$w, na.rm = T))
(assess_cost_credit_med = weighted.median(inst7_resp$assess_cost_credit, inst7_resp$w, na.rm = T))
(assess_cost_debit_med = weighted.median(inst7_resp$assess_cost_debit, inst7_resp$w, na.rm = T))
(assess_cost_prepaid_med = weighted.median(inst7_resp$assess_cost_prepaid, inst7_resp$w, na.rm = T))
# SD cost assessment by PI 
(assess_cost_cash_sd = sd(inst7_resp$assess_cost_cash, na.rm = T))
(assess_cost_check_sd = sd(inst7_resp$assess_cost_check, na.rm = T))
(assess_cost_credit_sd = sd(inst7_resp$assess_cost_credit, na.rm = T))
(assess_cost_debit_sd = sd(inst7_resp$assess_cost_debit, na.rm = T))
(assess_cost_prepaid_sd = sd(inst7_resp$assess_cost_prepaid, na.rm = T))
# median security assessment by PI
(assess_security_cash_med = weighted.median(inst7_resp$assess_security_cash, inst7_resp$w, na.rm = T))
(assess_security_check_med = weighted.median(inst7_resp$assess_security_check, inst7_resp$w, na.rm = T))
(assess_security_credit_med = weighted.median(inst7_resp$assess_security_credit, inst7_resp$w, na.rm = T))
(assess_security_debit_med = weighted.median(inst7_resp$assess_security_debit, inst7_resp$w, na.rm = T))
(assess_security_prepaid_med = weighted.median(inst7_resp$assess_security_prepaid, inst7_resp$w, na.rm = T))
# SD security assessment by PI 
(assess_security_cash_sd = sd(inst7_resp$assess_security_cash, na.rm = T))
(assess_security_check_sd = sd(inst7_resp$assess_security_check, na.rm = T))
(assess_security_credit_sd = sd(inst7_resp$assess_security_credit, na.rm = T))
(assess_security_debit_sd = sd(inst7_resp$assess_security_debit, na.rm = T))
(assess_security_prepaid_sd = sd(inst7_resp$assess_security_prepaid, na.rm = T))
# median convenience assessment by PI
(assess_convenience_cash_med = weighted.median(inst7_resp$assess_convenience_cash, inst7_resp$w, na.rm = T))
(assess_convenience_check_med = weighted.median(inst7_resp$assess_convenience_check, inst7_resp$w, na.rm = T))
(assess_convenience_credit_med = weighted.median(inst7_resp$assess_convenience_credit, inst7_resp$w, na.rm = T))
(assess_convenience_debit_med = weighted.median(inst7_resp$assess_convenience_debit, inst7_resp$w, na.rm = T))
(assess_convenience_prepaid_med = weighted.median(inst7_resp$assess_convenience_prepaid, inst7_resp$w, na.rm = T))
# SD convenience assessment by PI 
(assess_convenience_cash_sd = sd(inst7_resp$assess_convenience_cash, na.rm = T))
(assess_convenience_check_sd = sd(inst7_resp$assess_convenience_check, na.rm = T))
(assess_convenience_credit_sd = sd(inst7_resp$assess_convenience_credit, na.rm = T))
(assess_convenience_debit_sd = sd(inst7_resp$assess_convenience_debit, na.rm = T))
(assess_convenience_prepaid_sd = sd(inst7_resp$assess_convenience_prepaid, na.rm = T))

## start new Table 4 (median assessments)
# first column: 3 rows ALL followed by 3 rows Both
(assessment.vec = c("Cost", "Security", "Convenience"))
(cash.vec = c(assess_cost_cash_med, assess_security_cash_med, assess_convenience_cash_med))
(check.vec = c(assess_cost_check_med, assess_security_check_med, assess_convenience_check_med))
(credit.vec = c(assess_cost_credit_med, assess_security_credit_med, assess_convenience_credit_med))
(debit.vec = c(assess_cost_debit_med, assess_security_debit_med, assess_convenience_debit_med))
(prepaid.vec = c(assess_cost_prepaid_med, assess_security_prepaid_med, assess_convenience_prepaid_med))

#finalizing Table 4
(med_assess.df = data.frame("Assessment" =  assessment.vec, "Cash" = cash.vec, "Check" = check.vec, "Credit" = credit.vec, "Debit" = debit.vec, "Prepaid" = prepaid.vec))
print(xtable(med_assess.df, digits = 0), include.rownames = F, hline.after = c(0,3))
# End of median assessment discussion Table 4 in Section 5


### Section 6: multinomial logit random utility and consumer surplus (CS) estimations (and Table 4)
# new from 200426: restimating utility using avg and med assessments (instead of individual assessmemnts) 
names(inst7)# transactions

# Use the entire data to estimated coefficients for all subgroups of respodents. Get only one set of coefficients for all utility estimates (actually, two: one for med and one for median of squared assessments)
allml =  subset(inst7, select = c(uasid, pi, amnt, income, cc_adopt, dc_adopt, svc_adopt, bnk_acnt_adopt ))
names(allml)
# Remove 244 1.8% check transactions (see cashless_190731.R for all trans)
table(allml$pi)
percent(prop.table(table(allml$pi)))
nrow(allml)
allml =  subset(allml, pi != "check")
dim(allml)
table(allml$pi)
allml$pi = factor(allml$pi, levels = c("cash", "credit", "debit", "prepaid"))
table(allml$pi)
# remove NAs
any(is.na(allml))
dim(allml)
allml = na.omit(allml)
dim(allml)
#
## mlogit on All payments [change to both below)]
dim(allml)
allml2 = na.omit(allml) # 
dim(allml2)
#
# now make it mlogit data (all payments, subset to both only)
names(allml2)
bothml = subset(allml2, cc_adopt==1 & dc_adopt==1)
dim(bothml)

bothml_data = mlogit.data(bothml, choice = "pi", shape = "wide", id.var = "uasid", drop.index = F)
names(bothml_data)
head(bothml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) 
# cost med
bothml_data$cost_med = NA
bothml_data[bothml_data$alt=="cash", ]$cost_med = assess_cost_cash_med
bothml_data[bothml_data$alt=="debit", ]$cost_med = assess_cost_debit_med
bothml_data[bothml_data$alt=="credit", ]$cost_med = assess_cost_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$cost_med = assess_cost_prepaid_med

bothml_data$security_med = NA
bothml_data[bothml_data$alt=="cash", ]$security_med = assess_security_cash_med
bothml_data[bothml_data$alt=="debit", ]$security_med = assess_security_debit_med
bothml_data[bothml_data$alt=="credit", ]$security_med = assess_security_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$security_med = assess_security_prepaid_med
# convenience med
bothml_data$convenience_med = NA
bothml_data[bothml_data$alt=="cash", ]$convenience_med = assess_convenience_cash_med
bothml_data[bothml_data$alt=="debit", ]$convenience_med = assess_convenience_debit_med
bothml_data[bothml_data$alt=="credit", ]$convenience_med = assess_convenience_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$convenience_med = assess_convenience_prepaid_med

# regression model to be estimated (both)
# regression on median assessments
#(bothmodel = pi ~ cost_med_both + security_med_both + convenience_med_both | -1)  # not used
(bothmodel = pi ~ cost_med + security_med + convenience_med | -1)  
both_ml = mlogit(bothmodel, data = bothml_data)
summary(both_ml)
(both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
(both_ml_sd = coef(summary(both_ml))[,2]) # standard errors
(both_ml_sd = as.vector(both_ml_sd)) # standard errors vector
(both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
(both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
(both_ml_sig = as.vector(symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

###Computing CS and bottom Table 4 for med assessments estimations
## Computations of Consumer Surplus (CS) (Both)
# computing utility of cash, credit, debit, and prepaid 
bothml = na.omit(subset(allml, cc_adopt==1 & dc_adopt==1)) # subsample w/ both cards
dim(bothml)
# using all data coefficients median
(both_ml_coef = both_ml_coef)
(bothml$assess_cost_cash = assess_cost_cash_med)
(bothml$assess_security_cash = assess_security_cash_med)
(bothml$assess_convenience_cash = assess_convenience_cash_med)
(bothml$assess_cost_credit = assess_cost_credit_med)
(bothml$assess_security_credit = assess_security_credit_med)
(bothml$assess_convenience_credit = assess_convenience_credit_med)
(bothml$assess_cost_debit = assess_cost_debit_med)
(bothml$assess_security_debit = assess_security_debit_med)
(bothml$assess_convenience_debit = assess_convenience_debit_med)
(bothml$assess_cost_prepaid = assess_cost_prepaid_med)
(bothml$assess_security_prepaid = assess_security_prepaid_med)
(bothml$assess_convenience_prepaid = assess_convenience_prepaid_med)
#
bothml$v_cash = NA # estimated utility from paying cash median
bothml$v_cash = both_ml_coef[1]*bothml$assess_cost_cash + both_ml_coef[2]*bothml$assess_security_cash + both_ml_coef[3]*bothml$assess_convenience_cash 
#
bothml$v_credit = NA # # estimated utility from paying credit median
bothml$v_credit = both_ml_coef[1]*bothml$assess_cost_credit + both_ml_coef[2]*bothml$assess_security_credit + both_ml_coef[3]*bothml$assess_convenience_credit 

bothml$v_debit = NA # estimated utility from debit median
bothml$v_debit = both_ml_coef[1]*bothml$assess_cost_debit + both_ml_coef[2]*bothml$assess_security_debit + both_ml_coef[3]*bothml$assess_convenience_debit 
#
bothml$v_prepaid = NA # estimated utility from paying prepaid median
bothml$v_prepaid = both_ml_coef[1]*bothml$assess_cost_prepaid + both_ml_coef[2]*bothml$assess_security_prepaid + both_ml_coef[3]*bothml$assess_convenience_prepaid 

#
head(bothml[, c("uasid", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 2) # all rows are identical

# Both: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book (median case)
(mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
bothml$cs = mui*log(exp(bothml$v_cash) + 0+ exp(bothml$v_credit) + exp(bothml$v_debit) + exp(bothml$v_prepaid)) # cash is ref pi here
#
# CS *after* cash is phased out
bothml$cs_cashless =  mui*log(0 + exp(bothml$v_credit) + exp(bothml$v_debit) + exp(bothml$v_prepaid))
#
# Difference in CS (cashless - before)
bothml$cs_diff = bothml$cs_cashless - bothml$cs
# rate of change
bothml$cs_rate = bothml$cs_diff/bothml$cs
#
length(bothml$cs)
summary(bothml$cs) # CS before the change
summary(bothml$cs_cashless) # CS after stores become cashless
summary(bothml$cs_diff)
summary(bothml$cs_rate)
(bothml_loss_rate_med = median(-1*bothml$cs_rate, na.rm = T))# all the same
(bothml_loss_rate_avg = mean(-1*bothml$cs_rate, na.rm = T)) # all the same
#

## Computations of Consumer Surplus (CS) (noccyesdc, no cc yes dc)
noccyesdcml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==1 & pi != "credit" ))
dim(noccyesdcml)
# computing utility of cash, check, credit, debit, and prepaid 
#noccyesdcml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
(noccyesdc_ml_coef = both_ml_coef)
(noccyesdcml$assess_cost_cash = assess_cost_cash_med)
(noccyesdcml$assess_security_cash = assess_security_cash_med)
(noccyesdcml$assess_convenience_cash = assess_convenience_cash_med)
(noccyesdcml$assess_cost_credit = assess_cost_credit_med)
(noccyesdcml$assess_security_credit = assess_security_credit_med)
(noccyesdcml$assess_convenience_credit = assess_convenience_credit_med)
(noccyesdcml$assess_cost_debit = assess_cost_debit_med)
(noccyesdcml$assess_security_debit = assess_security_debit_med)
(noccyesdcml$assess_convenience_debit = assess_convenience_debit_med)
(noccyesdcml$assess_cost_prepaid = assess_cost_prepaid_med)
(noccyesdcml$assess_security_prepaid = assess_security_prepaid_med)
(noccyesdcml$assess_convenience_prepaid = assess_convenience_prepaid_med)

noccyesdcml$v_cash = NA # # estimated utility from paying cash
noccyesdcml$v_cash = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_cash + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_cash + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_cash 

noccyesdcml$v_credit = NA # # estimated utility from paying credit
noccyesdcml$v_credit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_credit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_credit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_credit 

noccyesdcml$v_debit = NA # estimated utility from debit 
noccyesdcml$v_debit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_debit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_debit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_debit 
#
noccyesdcml$v_prepaid = NA # estimated utility from paying prepaid
noccyesdcml$v_prepaid = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_prepaid + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_prepaid + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_prepaid 

#
head(noccyesdcml[, c("uasid", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 2) # Credit will not be included in CS below

# No cc Yes dc: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
(mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
noccyesdcml$cs = mui*log(exp(noccyesdcml$v_cash) + 0 + 0 + exp(noccyesdcml$v_debit) + exp(noccyesdcml$v_prepaid)) # cash is ref pi here
#
# CS *after* cash is phased out
noccyesdcml$cs_cashless =  mui*log(0 + 0 + exp(noccyesdcml$v_debit) + exp(noccyesdcml$v_prepaid))
#
# Difference in CS (cashless - before)
noccyesdcml$cs_diff = noccyesdcml$cs_cashless - noccyesdcml$cs
# rate of change
noccyesdcml$cs_rate = noccyesdcml$cs_diff/noccyesdcml$cs
#
length(noccyesdcml$cs)
summary(noccyesdcml$cs) # CS before the change
summary(noccyesdcml$cs_cashless) # CS after stores become cashless
summary(noccyesdcml$cs_diff)
summary(noccyesdcml$cs_rate)
(noccyesdcml_loss_rate_med = median(-1*noccyesdcml$cs_rate, na.rm = T)) # all the same
(noccyesdcml_loss_rate_avg = mean(-1*noccyesdcml$cs_rate, na.rm = T))


## Computations of Consumer Surplus (CS) (None banked)
nonebankedml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==1 & pi != "credit" & pi != "debit"))
dim(nonebankedml)
# computing utility of cash, check, credit, debit, and prepaid 
#nonebankedml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
# using all data coefficients
(nonebankedml_coef = both_ml_coef)
(nonebankedml$assess_cost_cash = assess_cost_cash_med)
(nonebankedml$assess_security_cash = assess_security_cash_med)
(nonebankedml$assess_convenience_cash = assess_convenience_cash_med)
(nonebankedml$assess_cost_credit = assess_cost_credit_med)
(nonebankedml$assess_security_credit = assess_security_credit_med)
(nonebankedml$assess_convenience_credit = assess_convenience_credit_med)
(nonebankedml$assess_cost_debit = assess_cost_debit_med)
(nonebankedml$assess_security_debit = assess_security_debit_med)
(nonebankedml$assess_convenience_debit = assess_convenience_debit_med)
(nonebankedml$assess_cost_prepaid = assess_cost_prepaid_med)
(nonebankedml$assess_security_prepaid = assess_security_prepaid_med)
(nonebankedml$assess_convenience_prepaid = assess_convenience_prepaid_med)

nonebankedml$v_cash = NA # # estimated utility from paying cash
nonebankedml$v_cash = nonebankedml_coef[1]*nonebankedml$assess_cost_cash + nonebankedml_coef[2]*nonebankedml$assess_security_cash + nonebankedml_coef[3]*nonebankedml$assess_convenience_cash 
#
nonebankedml$v_credit = NA # # estimated utility from paying credit
nonebankedml$v_credit = nonebankedml_coef[1]*nonebankedml$assess_cost_credit + nonebankedml_coef[2]*nonebankedml$assess_security_credit + nonebankedml_coef[3]*nonebankedml$assess_convenience_credit 

nonebankedml$v_debit = NA # estimated utility from debit 
nonebankedml$v_debit = nonebankedml_coef[1]*nonebankedml$assess_cost_debit + nonebankedml_coef[2]*nonebankedml$assess_security_debit + nonebankedml_coef[3]*nonebankedml$assess_convenience_debit 
#
nonebankedml$v_prepaid = NA # estimated utility from paying prepaid
nonebankedml$v_prepaid = nonebankedml_coef[1]*nonebankedml$assess_cost_prepaid + nonebankedml_coef[2]*nonebankedml$assess_security_prepaid + nonebankedml_coef[3]*nonebankedml$assess_convenience_prepaid 

#
head(nonebankedml[, c("uasid", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 2) # all rows are the same

# None banked: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
(mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
nonebankedml$cs = mui*log(exp(nonebankedml$v_cash) + 0 + 0 + 0 + exp(nonebankedml$v_prepaid)) # cash is ref pi here
#
# CS *after* cash is phased out
nonebankedml$cs_cashless =  mui*log(0 +0 +0 + exp(nonebankedml$v_prepaid))
#
# Difference in CS (cashless - before)
nonebankedml$cs_diff = nonebankedml$cs_cashless - nonebankedml$cs
# rate of change
nonebankedml$cs_rate = nonebankedml$cs_diff/nonebankedml$cs
#
length(nonebankedml$cs)
summary(nonebankedml$cs) # CS before the change
summary(nonebankedml$cs_cashless) # CS after stores become cashless
summary(nonebankedml$cs_diff)
summary(nonebankedml$cs_rate)
(nonebankedml_loss_rate_med = median(-1*nonebankedml$cs_rate, na.rm = T))# all the same
(nonebankedml_loss_rate_avg = mean(-1*nonebankedml$cs_rate, na.rm = T))


## Computations of Consumer Surplus (CS) (None Unbanked)
noneunbml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==0 & pi != "credit" & pi != "debit"))
dim(noneunbml)
# computing utility of cash, check, credit, debit, and prepaid 
#noneunbml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
(noneunbml_coef = both_ml_coef)
(noneunbml$assess_cost_cash = assess_cost_cash_med)
(noneunbml$assess_security_cash = assess_security_cash_med)
(noneunbml$assess_convenience_cash = assess_convenience_cash_med)
(noneunbml$assess_cost_credit = assess_cost_credit_med)
(noneunbml$assess_security_credit = assess_security_credit_med)
(noneunbml$assess_convenience_credit = assess_convenience_credit_med)
(noneunbml$assess_cost_debit = assess_cost_debit_med)
(noneunbml$assess_security_debit = assess_security_debit_med)
(noneunbml$assess_convenience_debit = assess_convenience_debit_med)
(noneunbml$assess_cost_prepaid = assess_cost_prepaid_med)
(noneunbml$assess_security_prepaid = assess_security_prepaid_med)
(noneunbml$assess_convenience_prepaid = assess_convenience_prepaid_med)

noneunbml$v_cash = NA # # estimated utility from paying cash 
noneunbml$v_cash = noneunbml_coef[1]*noneunbml$assess_cost_cash + noneunbml_coef[2]*noneunbml$assess_security_cash + noneunbml_coef[3]*noneunbml$assess_convenience_cash 
#
noneunbml$v_credit = NA # # estimated utility from paying credit
noneunbml$v_credit = noneunbml_coef[1]*noneunbml$assess_cost_credit + noneunbml_coef[2]*noneunbml$assess_security_credit + noneunbml_coef[3]*noneunbml$assess_convenience_credit 

noneunbml$v_debit = NA # estimated utility from debit 
noneunbml$v_debit = noneunbml_coef[1]*noneunbml$assess_cost_debit + noneunbml_coef[2]*noneunbml$assess_security_debit + noneunbml_coef[3]*noneunbml$assess_convenience_debit 
#
noneunbml$v_prepaid = NA # estimated utility from paying prepaid
noneunbml$v_prepaid = noneunbml_coef[1]*noneunbml$assess_cost_prepaid + noneunbml_coef[2]*noneunbml$assess_security_prepaid + noneunbml_coef[3]*noneunbml$assess_convenience_prepaid 

#
head(noneunbml[, c("uasid", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 2)

# None unbanked: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
(mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
noneunbml$cs = mui*log(exp(noneunbml$v_cash) + 0 + 0 + 0 + exp(noneunbml$v_prepaid)) # cash is ref pi here
#
# CS *after* cash is phased out
noneunbml$cs_cashless =  mui*log(0 +0 +0 + exp(noneunbml$v_prepaid))
#
# Difference in CS (cashless - before)
noneunbml$cs_diff = noneunbml$cs_cashless - noneunbml$cs
# rate of change
noneunbml$cs_rate = noneunbml$cs_diff/noneunbml$cs
#
length(noneunbml$cs)
summary(noneunbml$cs) # CS before the change
summary(noneunbml$cs_cashless) # CS after stores become cashless
summary(noneunbml$cs_diff)
summary(noneunbml$cs_rate)
(noneunbml_loss_rate_med = median(-1*noneunbml$cs_rate, na.rm = T))# all the same
(noneunbml_loss_rate_avg = mean(-1*noneunbml$cs_rate, na.rm = T))

### Start Table 5: change in CS. Squared assessments deleted (same CS, no need)
#(sample.vec = c("Credit_and_debit", "Debit_only", "Unbanked_no_credit/debit", "Credit_and_debit", "Debit_only", "Unbanked_no_credit/debit"))
(sample.vec = c("Credit_and_debit", "Debit_only", "Unbanked_no_credit/debit"))
#
#(payments.vec = c(nrow(bothml), nrow(noccyesdcml), nrow(noneunbml),nrow(bothml), nrow(noccyesdcml), nrow(noneunbml)))
(payments.vec = c(nrow(bothml), nrow(noccyesdcml), nrow(noneunbml)))
#
#(respondents.vec = c(length(unique(bothml$uasid)), length(unique(noccyesdcml$uasid)), length(unique(noneunbml$uasid)), length(unique(bothml$uasid)), length(unique(noccyesdcml$uasid)), length(unique(noneunbml$uasid))))
(respondents.vec = c(length(unique(bothml$uasid)), length(unique(noccyesdcml$uasid)), length(unique(noneunbml$uasid))))
#
#(cs.vec = c(median(bothml$cs), median(noccyesdcml$cs), median(noneunbml$cs), median(bothml$cs2), median(noccyesdcml$cs2), median(noneunbml$cs2)))# Note: all median=mean=1st=2nd... all obs are the same
(cs.vec = c(median(bothml$cs), median(noccyesdcml$cs), median(noneunbml$cs)))# Note: all median=mean=1st=2nd... all obs are the same
#
#(cs_cashless.vec = c(median(bothml$cs_cashless), median(noccyesdcml$cs_cashless), median(noneunbml$cs_cashless), median(bothml$cs_cashless2), median(noccyesdcml$cs_cashless2), median(noneunbml$cs_cashless2)))# Note: all median=mean=1st=2nd... all obs are the same
(cs_cashless.vec = c(median(bothml$cs_cashless), median(noccyesdcml$cs_cashless), median(noneunbml$cs_cashless)))# Note: all median=mean=1st=2nd... all obs are the same

#
#(cs_rate.vec = c(median(bothml$cs_rate), median(noccyesdcml$cs_rate), median(noneunbml$cs_rate), median(bothml$cs_rate2), median(noccyesdcml$cs_rate2), median(noneunbml$cs_rate2)))# Note: all median=mean=1st=2nd... all obs are the same
(cs_rate.vec = c(median(bothml$cs_rate), median(noccyesdcml$cs_rate), median(noneunbml$cs_rate)))# Note: all median=mean=1st=2nd... all obs are the same

# adding relative drop rate (relative to consumers with both)
(relative.vec = c(1, cs_rate.vec[2]/cs_rate.vec[1], cs_rate.vec[3]/cs_rate.vec[1]))

# finalizing Table 5
(cs.df = data.frame("Sample" = sample.vec, "Payments" = payments.vec, "Respondents" = respondents.vec, "CS" = cs.vec, "CS_cashless" = cs_cashless.vec, "CS_change (%)" = 100*cs_rate.vec, "Relative" = relative.vec))
#
dim(cs.df)

# below, create matrix w\ 1 extra column to indicate number of digits for each row. Add 1 column and place 0 at the start of the matrix
(digitm = matrix(c(0,0,0,0,2,2,2,2), nrow = 3, ncol = 7+1, byrow = T))
#
print(xtable(cs.df, digits = digitm), include.rownames = F, hline.after = c(0))

# # for caption of Table 5
nrow(bothml)# num of payments
length(unique(bothml$uasid))
# End of CS drop: Table 4.


########### End of cashless_sec5_sec6.R
### unused code

## repeat assessments for respondents who have both cards
# names(inst7_resp)
# dim(inst7_resp)# num all resp
# inst7_resp_both = subset(inst7_resp, cc_adopt==1 & dc_adopt==1)
# dim(inst7_resp_both)# num resp with both cards
# sum(inst7_resp_both$weight_1)
# #Rescaling weight_1 for sumbample of both
# inst7_resp_both$w = nrow(inst7_resp_both) * inst7_resp_both$weight_1/sum(inst7_resp_both$weight_1)
# sum(inst7_resp_both$w)
# nrow(inst7_resp_both)
# #
# # median cost assessment by PI weighted (both)
# (assess_cost_cash_med_both = weighted.median(inst7_resp_both$assess_cost_cash, inst7_resp_both$w, na.rm = T))
# (assess_cost_check_med_both = weighted.median(inst7_resp_both$assess_cost_check, inst7_resp_both$w, na.rm = T))
# (assess_cost_credit_med_both = weighted.median(inst7_resp_both$assess_cost_credit, inst7_resp_both$w, na.rm = T))
# (assess_cost_debit_med_both = weighted.median(inst7_resp_both$assess_cost_debit, inst7_resp_both$w, na.rm = T))
# (assess_cost_prepaid_med_both = weighted.median(inst7_resp_both$assess_cost_prepaid, inst7_resp_both$w, na.rm = T))
# # SD cost assessment by PI 
# (assess_cost_cash_sd_both = sd(inst7_resp_both$assess_cost_cash, na.rm = T))
# (assess_cost_check_sd_both = sd(inst7_resp_both$assess_cost_check, na.rm = T))
# (assess_cost_credit_sd_both = sd(inst7_resp_both$assess_cost_credit, na.rm = T))
# (assess_cost_debit_sd_both = sd(inst7_resp_both$assess_cost_debit, na.rm = T))
# (assess_cost_prepaid_sd_both = sd(inst7_resp_both$assess_cost_prepaid, na.rm = T))
# # median security assessment by PI
# (assess_security_cash_med_both = weighted.median(inst7_resp_both$assess_security_cash, inst7_resp_both$w, na.rm = T))
# (assess_security_check_med_both = weighted.median(inst7_resp_both$assess_security_check, inst7_resp_both$w, na.rm = T))
# (assess_security_credit_med_both = weighted.median(inst7_resp_both$assess_security_credit, inst7_resp_both$w, na.rm = T))
# (assess_security_debit_med_both = weighted.median(inst7_resp_both$assess_security_debit, inst7_resp_both$w, na.rm = T))
# (assess_security_prepaid_med_both = weighted.median(inst7_resp_both$assess_security_prepaid, inst7_resp_both$w, na.rm = T))
# # SD security assessment by PI 
# (assess_security_cash_sd_both = sd(inst7_resp_both$assess_security_cash, na.rm = T))
# (assess_security_check_sd_both = sd(inst7_resp_both$assess_security_check, na.rm = T))
# (assess_security_credit_sd_both = sd(inst7_resp_both$assess_security_credit, na.rm = T))
# (assess_security_debit_sd_both = sd(inst7_resp_both$assess_security_debit, na.rm = T))
# (assess_security_prepaid_sd_both = sd(inst7_resp_both$assess_security_prepaid, na.rm = T))
# # median convenience assessment by PI
# (assess_convenience_cash_med_both = weighted.median(inst7_resp_both$assess_convenience_cash, inst7_resp_both$w, na.rm = T))
# (assess_convenience_check_med_both = weighted.median(inst7_resp_both$assess_convenience_check, inst7_resp_both$w, na.rm = T))
# (assess_convenience_credit_med_both = weighted.median(inst7_resp_both$assess_convenience_credit, inst7_resp_both$w, na.rm = T))
# (assess_convenience_debit_med_both = weighted.median(inst7_resp_both$assess_convenience_debit, inst7_resp_both$w, na.rm = T))
# (assess_convenience_prepaid_med_both = weighted.median(inst7_resp_both$assess_convenience_prepaid, inst7_resp_both$w, na.rm = T))
# # SD convenience assessment by PI 
# (assess_convenience_cash_sd_both = sd(inst7_resp_both$assess_convenience_cash, na.rm = T))
# (assess_convenience_check_sd_both = sd(inst7_resp_both$assess_convenience_check, na.rm = T))
# (assess_convenience_credit_sd_both = sd(inst7_resp_both$assess_convenience_credit, na.rm = T))
# (assess_convenience_debit_sd_both = sd(inst7_resp_both$assess_convenience_debit, na.rm = T))
# (assess_convenience_prepaid_sd_both = sd(inst7_resp_both$assess_convenience_prepaid, na.rm = T))

# first column: 3 rows ALL followed by 3 rows Both
# (assessment.vec = c("Cost", "Security", "Convenience","Cost", "Security", "Convenience"))
# (cash.vec = c(assess_cost_cash_med, assess_security_cash_med, assess_convenience_cash_med, assess_cost_cash_med_both, assess_security_cash_med_both, assess_convenience_cash_med_both))
# (check.vec = c(assess_cost_check_med, assess_security_check_med, assess_convenience_check_med, assess_cost_check_med_both, assess_security_check_med_both, assess_convenience_check_med_both))
# 
# (credit.vec = c(assess_cost_credit_med, assess_security_credit_med, assess_convenience_credit_med, assess_cost_credit_med_both, assess_security_credit_med_both, assess_convenience_credit_med_both))
# (debit.vec = c(assess_cost_debit_med, assess_security_debit_med, assess_convenience_debit_med, assess_cost_debit_med_both, assess_security_debit_med_both, assess_convenience_debit_med_both))
# (prepaid.vec = c(assess_cost_prepaid_med, assess_security_prepaid_med, assess_convenience_prepaid_med, assess_cost_prepaid_med_both, assess_security_prepaid_med_both, assess_convenience_prepaid_med_both))
# 
# #finalizing Table 4
# (med_assess.df = data.frame("Assessment" =  assessment.vec, "Cash" = cash.vec, "Check" = check.vec, "Credit" = credit.vec, "Debit" = debit.vec, "Prepaid" = prepaid.vec))
# print(xtable(med_assess.df, digits = 0), include.rownames = F, hline.after = c(0,3))
# # End of median assessment discussion Table 4 in Section 5

# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) 
# cost med
# bothml_data$cost_med_both = NA
# bothml_data[bothml_data$alt=="cash", ]$cost_med_both = assess_cost_cash_med_both
# bothml_data[bothml_data$alt=="debit", ]$cost_med_both = assess_cost_debit_med_both
# bothml_data[bothml_data$alt=="credit", ]$cost_med_both = assess_cost_credit_med_both
# bothml_data[bothml_data$alt=="prepaid", ]$cost_med_both = assess_cost_prepaid_med_both
# 
# bothml_data$security_med_both = NA
# bothml_data[bothml_data$alt=="cash", ]$security_med_both = assess_security_cash_med_both
# bothml_data[bothml_data$alt=="debit", ]$security_med_both = assess_security_debit_med_both
# bothml_data[bothml_data$alt=="credit", ]$security_med_both = assess_security_credit_med_both
# bothml_data[bothml_data$alt=="prepaid", ]$security_med_both = assess_security_prepaid_med_both
# # convenience med
# bothml_data$convenience_med_both = NA
# bothml_data[bothml_data$alt=="cash", ]$convenience_med_both = assess_convenience_cash_med_both
# bothml_data[bothml_data$alt=="debit", ]$convenience_med_both = assess_convenience_debit_med_both
# bothml_data[bothml_data$alt=="credit", ]$convenience_med_both = assess_convenience_credit_med_both
# bothml_data[bothml_data$alt=="prepaid", ]$convenience_med_both = assess_convenience_prepaid_med_both
