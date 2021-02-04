# cashless_sec5_sec6_210204.R switching to new id
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
#library(spatstat) # for weighted.median
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
d1 = readRDS("cashless_merged_2017_2018_210204.rds")
objects()
names(d1)
### Data preparations
# NOTE: The paper itself (section-by-section) starts on line xxx

length(unique(d1$id)) # num of unique respondents
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
length(unique(d2$id))# num of respondents (in-person only)
d2 = subset(d2,  type == "expenditure")# restriction expenditures only
dim(d2)# num trans
length(unique(d2$id))# num of respondents (in-person only)
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
length(unique(d3$id))# num respondents
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
length(unique(d5$id))
nrow(d5) - nrow(d4)# num payments lost by restricting PI
length(unique(d5$id)) - length(unique(d4$id)) # num respondents lost by restricting PI
table(d5$pi)
sum(table(d5$pi))# Number of trans 
length(unique(d5$id))# Number of respondents 
# Below, select only 4 used variables from the transaction dataset and xxx variables from the survey (adoption and assessments)
names(d5)
d6 = subset(d5, select = c("id", "weight_1", "weight_2", "amnt", "pi", "merch", "date", "hh_size", "age", "gender", "employed", "married", "education", "income", "year", "bnk_acnt_adopt", "chk_adopt", "cc_adopt", "dc_adopt", "svc_adopt", "assess_cost_cash", "assess_cost_check", "assess_cost_debit", "assess_cost_credit", "assess_cost_prepaid", "assess_acceptance_cash", "assess_acceptance_check", "assess_acceptance_debit", "assess_acceptance_credit", "assess_acceptance_prepaid", "assess_convenience_cash", "assess_convenience_check", "assess_convenience_debit", "assess_convenience_credit", "assess_convenience_prepaid", "assess_security_cash", "assess_security_check", "assess_security_debit", "assess_security_credit", "assess_security_prepaid", "assess_setup_cash", "assess_setup_check", "assess_setup_debit", "assess_setup_credit", "assess_setup_prepaid", "assess_record_cash", "assess_record_check", "assess_record_debit", "assess_record_credit", "assess_record_prepaid"))

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
length(unique(unbanked_tran$id))# num of unbanked
length(unique(unbanked_tran$id))/length(unique(d9$id))# fraction of unbanked respondents
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
d10_unique = d10[!duplicated(d10$id), ] # data set containing each resp only once (not to be used for trans stats, only adoption stats)
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
head(inst7[inst7$pi == "cash", c("cost_actual", "assess_cost_cash" , "pi", "acceptance_actual", "assess_acceptance_cash")])
head(inst7[inst7$pi == "check", c("cost_actual", "assess_cost_check", "pi", "acceptance_actual", "assess_acceptance_check")])
head(inst7[inst7$pi == "debit", c("cost_actual", "assess_cost_debit", "pi", "acceptance_actual", "assess_acceptance_debit")])
head(inst7[inst7$pi == "credit", c("cost_actual", "assess_cost_credit", "pi", "acceptance_actual", "assess_acceptance_credit")])
head(inst7[inst7$pi == "prepaid", c("cost_actual", "assess_cost_prepaid", "pi", "acceptance_actual", "assess_acceptance_prepaid")])
#
head(inst7[inst7$pi == "cash", c("acceptance_actual", "assess_acceptance_cash" , "pi", "convenience_actual", "assess_convenience_cash")])
head(inst7[inst7$pi == "check", c("acceptance_actual", "assess_acceptance_check", "pi", "convenience_actual", "assess_convenience_check")])
head(inst7[inst7$pi == "debit", c("acceptance_actual", "assess_acceptance_debit", "pi", "convenience_actual", "assess_convenience_debit")])
head(inst7[inst7$pi == "credit", c("acceptance_actual", "assess_acceptance_credit", "pi", "convenience_actual", "assess_convenience_credit")])
head(inst7[inst7$pi == "prepaid", c("acceptance_actual", "assess_acceptance_prepaid", "pi", "convenience_actual", "assess_convenience_prepaid")])
#
head(inst7[inst7$pi == "cash", c("setup_actual", "assess_setup_cash" , "pi", "record_actual", "assess_record_cash")])
head(inst7[inst7$pi == "check", c("setup_actual", "assess_setup_check", "pi", "record_actual", "assess_record_check")])
head(inst7[inst7$pi == "debit", c("setup_actual", "assess_setup_debit", "pi", "record_actual", "assess_record_debit")])
head(inst7[inst7$pi == "credit", c("setup_actual", "assess_setup_credit", "pi", "record_actual", "assess_record_credit")])
head(inst7[inst7$pi == "prepaid", c("setup_actual", "assess_setup_prepaid", "pi", "record_actual", "assess_record_prepaid")])
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

### Section 6.1 & 6.2: multinomial logit random utility and consumer surplus (CS) estimations (and Table 4)
## Multinomial regressions with NO random coefficients: starts Line 329 ends Line 800 Mixed logit started thereafter
# Run this part first before mixed logit (uses the same data)
names(inst7)

allml =  subset(inst7, select = c(id, pi, amnt, income, assess_cost_cash, assess_cost_check, assess_cost_credit, assess_cost_debit, assess_cost_prepaid, assess_security_cash, assess_security_check, assess_security_credit, assess_security_debit, assess_security_prepaid, assess_convenience_cash, assess_convenience_check, assess_convenience_credit, assess_convenience_debit, assess_convenience_prepaid, cc_adopt, dc_adopt, svc_adopt, bnk_acnt_adopt )) 
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
## mlogit on All (all data) 
dim(allml)
allml2 = na.omit(allml) # subsample w/ both cards
dim(allml2)
#
# now make it mlogit data (both)
allml2_data = mlogit.data(allml2, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(allml2_data)
head(allml2_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
allml2_data$cost = NA
allml2_data[allml2_data$alt=="cash", ]$cost = allml2_data[allml2_data$alt=="cash", ]$assess_cost_cash
# allml2_data[allml2_data$alt=="check", ]$cost = allml2_data[allml2_data$alt=="check", ]$assess_cost_check
allml2_data[allml2_data$alt=="debit", ]$cost = allml2_data[allml2_data$alt=="debit", ]$assess_cost_debit
allml2_data[allml2_data$alt=="credit", ]$cost = allml2_data[allml2_data$alt=="credit", ]$assess_cost_credit
allml2_data[allml2_data$alt=="prepaid", ]$cost = allml2_data[allml2_data$alt=="prepaid", ]$assess_cost_prepaid
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$security = NA
allml2_data[allml2_data$alt=="cash", ]$security = allml2_data[allml2_data$alt=="cash", ]$assess_security_cash
# allml2_data[allml2_data$alt=="check", ]$security = allml2_data[allml2_data$alt=="check", ]$assess_security_check
allml2_data[allml2_data$alt=="debit", ]$security = allml2_data[allml2_data$alt=="debit", ]$assess_security_debit
allml2_data[allml2_data$alt=="credit", ]$security = allml2_data[allml2_data$alt=="credit", ]$assess_security_credit
allml2_data[allml2_data$alt=="prepaid", ]$security = allml2_data[allml2_data$alt=="prepaid", ]$assess_security_prepaid
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$convenience = NA
allml2_data[allml2_data$alt=="cash", ]$convenience = allml2_data[allml2_data$alt=="cash", ]$assess_convenience_cash
# allml2_data[allml2_data$alt=="check", ]$convenience = allml2_data[allml2_data$alt=="check", ]$assess_convenience_check
allml2_data[allml2_data$alt=="debit", ]$convenience = allml2_data[allml2_data$alt=="debit", ]$assess_convenience_debit
allml2_data[allml2_data$alt=="credit", ]$convenience = allml2_data[allml2_data$alt=="credit", ]$assess_convenience_credit
allml2_data[allml2_data$alt=="prepaid", ]$convenience = allml2_data[allml2_data$alt=="prepaid", ]$assess_convenience_prepaid
#
# model to be estimated (All)
(allml2model = pi ~ cost + security + convenience | -1)  # w/o adoption
all_ml = mlogit(allml2model, data = allml2_data)
summary(all_ml)
(all_ml_coef = as.vector(all_ml$coefficients[1:3])) # cost, security, convenience
(all_ml_sd = coef(summary(all_ml))[,2]) # standard errors
(all_ml_sd = as.vector(all_ml_sd)) # standard errors vector
(all_ml_pvalue = coef(summary(all_ml))[,4]) # extract p-value
(all_ml_pvalue = as.vector(all_ml_pvalue)) # p-values vector
(all_ml_sig = as.vector( symnum(all_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
#

## mlogit on both cards 
dim(allml)
bothml = na.omit(subset(allml, cc_adopt==1 & dc_adopt==1)) # subsample w/ both cards
dim(bothml)
#
# now make it mlogit data (both)
bothml_data = mlogit.data(bothml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(bothml_data)
head(bothml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
bothml_data$cost = NA
bothml_data[bothml_data$alt=="cash", ]$cost = bothml_data[bothml_data$alt=="cash", ]$assess_cost_cash
# bothml_data[bothml_data$alt=="check", ]$cost = bothml_data[bothml_data$alt=="check", ]$assess_cost_check
bothml_data[bothml_data$alt=="debit", ]$cost = bothml_data[bothml_data$alt=="debit", ]$assess_cost_debit
bothml_data[bothml_data$alt=="credit", ]$cost = bothml_data[bothml_data$alt=="credit", ]$assess_cost_credit
bothml_data[bothml_data$alt=="prepaid", ]$cost = bothml_data[bothml_data$alt=="prepaid", ]$assess_cost_prepaid
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$security = NA
bothml_data[bothml_data$alt=="cash", ]$security = bothml_data[bothml_data$alt=="cash", ]$assess_security_cash
# bothml_data[bothml_data$alt=="check", ]$security = bothml_data[bothml_data$alt=="check", ]$assess_security_check
bothml_data[bothml_data$alt=="debit", ]$security = bothml_data[bothml_data$alt=="debit", ]$assess_security_debit
bothml_data[bothml_data$alt=="credit", ]$security = bothml_data[bothml_data$alt=="credit", ]$assess_security_credit
bothml_data[bothml_data$alt=="prepaid", ]$security = bothml_data[bothml_data$alt=="prepaid", ]$assess_security_prepaid
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$convenience = NA
bothml_data[bothml_data$alt=="cash", ]$convenience = bothml_data[bothml_data$alt=="cash", ]$assess_convenience_cash
# bothml_data[bothml_data$alt=="check", ]$convenience = bothml_data[bothml_data$alt=="check", ]$assess_convenience_check
bothml_data[bothml_data$alt=="debit", ]$convenience = bothml_data[bothml_data$alt=="debit", ]$assess_convenience_debit
bothml_data[bothml_data$alt=="credit", ]$convenience = bothml_data[bothml_data$alt=="credit", ]$assess_convenience_credit
bothml_data[bothml_data$alt=="prepaid", ]$convenience = bothml_data[bothml_data$alt=="prepaid", ]$assess_convenience_prepaid
#
# model to be estimated (both)
(bothmlmodel = pi ~ cost + security + convenience | -1)  # w/o adoption
both_ml = mlogit(bothmlmodel, data = bothml_data)
summary(both_ml)
(both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
(both_ml_sd = coef(summary(both_ml))[,2]) # standard errors
(both_ml_sd = as.vector(both_ml_sd)) # standard errors vector
(both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
(both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
(both_ml_sig = as.vector( symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
#

## mlogit on no_cc & yes_dc
dim(allml)
noccyesdcml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==1 & pi != "credit" ))
dim(noccyesdcml)
#
# now make it mlogit data (no cc & yes dc)
noccyesdcml_data = mlogit.data(noccyesdcml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(noccyesdcml_data)
head(noccyesdcml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
noccyesdcml_data$cost = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$assess_cost_cash
# noccyesdcml_data[noccyesdcml_data$alt=="check", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_cost_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$assess_cost_debit
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$assess_cost_credit
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$assess_cost_prepaid
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noccyesdcml_data$security = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$assess_security_cash
# noccyesdcml_data[noccyesdcml_data$alt=="check", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_security_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$assess_security_debit
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$assess_security_credit
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$assess_security_prepaid
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noccyesdcml_data$convenience = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$assess_convenience_cash
#noccyesdcml_data[noccyesdcml_data$alt=="check", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_convenience_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$assess_convenience_debit
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$assess_convenience_credit
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$assess_convenience_prepaid
#
# model to be estimated (noccyesdc)
noccyesdcmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
noccyesdc_ml = mlogit(noccyesdcmlmodel, alt.subset = c("cash", "debit", "prepaid"),  data = noccyesdcml_data)
summary(noccyesdc_ml)
(noccyesdc_ml_coef = as.vector(noccyesdc_ml$coefficients[1:3])) # cost, security, convenience
(noccyesdc_ml_sd = coef(summary(noccyesdc_ml))[,2]) # starndard errors
(noccyesdc_ml_sd = as.vector(noccyesdc_ml_sd)) # standard errors vector
(noccyesdc_ml_pvalue = coef(summary(noccyesdc_ml))[,4]) # extract p-value
(noccyesdc_ml_pvalue = as.vector(noccyesdc_ml_pvalue)) # p-values vector
(noccyesdc_ml_sig = as.vector(symnum(noccyesdc_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## mlogit on none_banked
dim(allml)
nonebankedml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==1 & pi != "credit" & pi != "debit"))
dim(nonebankedml)
#
# now make it mlogit data (no cc & yes dc)
nonebankedml_data = mlogit.data(nonebankedml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(nonebankedml_data)
head(nonebankedml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
nonebankedml_data$cost = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$cost = nonebankedml_data[nonebankedml_data$alt=="cash", ]$assess_cost_cash
# nonebankedml_data[nonebankedml_data$alt=="check", ]$cost = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_cost_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$cost = nonebankedml_data[nonebankedml_data$alt=="debit", ]$assess_cost_debit
nonebankedml_data[nonebankedml_data$alt=="credit", ]$cost = nonebankedml_data[nonebankedml_data$alt=="credit", ]$assess_cost_credit
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$cost = nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$assess_cost_prepaid
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$security = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$security = nonebankedml_data[nonebankedml_data$alt=="cash", ]$assess_security_cash
# nonebankedml_data[nonebankedml_data$alt=="check", ]$security = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_security_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$security = nonebankedml_data[nonebankedml_data$alt=="debit", ]$assess_security_debit
nonebankedml_data[nonebankedml_data$alt=="credit", ]$security = nonebankedml_data[nonebankedml_data$alt=="credit", ]$assess_security_credit
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$security = nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$assess_security_prepaid
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$convenience = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="cash", ]$assess_convenience_cash
#nonebankedml_data[nonebankedml_data$alt=="check", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_convenience_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="debit", ]$assess_convenience_debit
nonebankedml_data[nonebankedml_data$alt=="credit", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="credit", ]$assess_convenience_credit
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$assess_convenience_prepaid
#
# model to be estimated (nonebanked)
nonebankedmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
nonebanked_ml = mlogit(nonebankedmlmodel, alt.subset = c("cash", "debit", "prepaid"),  data = nonebankedml_data)
summary(nonebanked_ml)
(nonebankedml_coef = as.vector(nonebanked_ml$coefficients[1:3])) # cost, security, convenience
(nonebankedml_sd = coef(summary(nonebanked_ml))[,2]) # standard errors
(nonebankedml_sd = as.vector(nonebankedml_sd)) # standard errors vect
(nonebankedml_pvalue = coef(summary(nonebanked_ml))[,4]) # extract p-value
(nonebankedml_pvalue = as.vector(nonebankedml_pvalue)) # p-values vector
(nonebankedml_sig = as.vector(symnum(nonebankedml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector


## mlogit on none_unb (unbanked)
dim(allml)
noneunbml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==0 & pi != "credit" & pi != "debit"))
dim(noneunbml)
#
# now make it mlogit data (no cc & yes dc)
noneunbml_data = mlogit.data(noneunbml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(noneunbml_data)
head(noneunbml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
noneunbml_data$cost = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$cost = noneunbml_data[noneunbml_data$alt=="cash", ]$assess_cost_cash
# noneunbml_data[noneunbml_data$alt=="check", ]$cost = noneunbml_data[noneunbml_data$alt=="check", ]$assess_cost_check
noneunbml_data[noneunbml_data$alt=="debit", ]$cost = noneunbml_data[noneunbml_data$alt=="debit", ]$assess_cost_debit
noneunbml_data[noneunbml_data$alt=="credit", ]$cost = noneunbml_data[noneunbml_data$alt=="credit", ]$assess_cost_credit
noneunbml_data[noneunbml_data$alt=="prepaid", ]$cost = noneunbml_data[noneunbml_data$alt=="prepaid", ]$assess_cost_prepaid
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$security = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$security = noneunbml_data[noneunbml_data$alt=="cash", ]$assess_security_cash
# noneunbml_data[noneunbml_data$alt=="check", ]$security = noneunbml_data[noneunbml_data$alt=="check", ]$assess_security_check
noneunbml_data[noneunbml_data$alt=="debit", ]$security = noneunbml_data[noneunbml_data$alt=="debit", ]$assess_security_debit
noneunbml_data[noneunbml_data$alt=="credit", ]$security = noneunbml_data[noneunbml_data$alt=="credit", ]$assess_security_credit
noneunbml_data[noneunbml_data$alt=="prepaid", ]$security = noneunbml_data[noneunbml_data$alt=="prepaid", ]$assess_security_prepaid
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$convenience = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$convenience = noneunbml_data[noneunbml_data$alt=="cash", ]$assess_convenience_cash
#noneunbml_data[noneunbml_data$alt=="check", ]$convenience = noneunbml_data[noneunbml_data$alt=="check", ]$assess_convenience_check
noneunbml_data[noneunbml_data$alt=="debit", ]$convenience = noneunbml_data[noneunbml_data$alt=="debit", ]$assess_convenience_debit
noneunbml_data[noneunbml_data$alt=="credit", ]$convenience = noneunbml_data[noneunbml_data$alt=="credit", ]$assess_convenience_credit
noneunbml_data[noneunbml_data$alt=="prepaid", ]$convenience = noneunbml_data[noneunbml_data$alt=="prepaid", ]$assess_convenience_prepaid
#
# model to be estimated (none UNbanked)
noneunbmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
noneunb_ml = mlogit(noneunbmlmodel, alt.subset = c("cash",  "prepaid"),  data = noneunbml_data)
summary(noneunb_ml)
(noneunbml_coef = as.vector(noneunb_ml$coefficients[1:3])) # cost, security, convenience
(noneunbml_sd = coef(summary(noneunb_ml))[,2]) # standard errors
(noneunbml_sd = as.vector(noneunbml_sd)) # standard error vector
(noneunbml_pvalue = coef(summary(noneunb_ml))[,4]) # extract p-value
(noneunbml_pvalue = as.vector(noneunbml_pvalue)) # p-values vector
(noneunbml_sig = as.vector(symnum(noneunbml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## Computations of Consumer Surplus (CS) (All)
# computing utility of cash, check, credit, debit, and prepaid 
# allml2$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015.R)
allml2$v_cash = NA # # estimated utility from paying cash
allml2$v_cash = all_ml_coef[1]*allml2$assess_cost_cash + all_ml_coef[2]*allml2$assess_security_cash + all_ml_coef[3]*allml2$assess_convenience_cash 
#
# allml2$v_check = NA # estimated utility from paying check (removed)
# allml2$v_check = all_ml_coef[1]*allml2$assess_cost_check + all_ml_coef[2]*allml2$assess_security_check + all_ml_coef[3]*allml2$assess_convenience_check 
#
allml2$v_credit = NA # # estimated utility from paying credit
allml2$v_credit = all_ml_coef[1]*allml2$assess_cost_credit + all_ml_coef[2]*allml2$assess_security_credit + all_ml_coef[3]*allml2$assess_convenience_credit 

allml2$v_debit = NA # estimated utility from debit 
allml2$v_debit = all_ml_coef[1]*allml2$assess_cost_debit + all_ml_coef[2]*allml2$assess_security_debit + all_ml_coef[3]*allml2$assess_convenience_debit 
#
allml2$v_prepaid = NA # estimated utility from paying prepaid
allml2$v_prepaid = all_ml_coef[1]*allml2$assess_cost_prepaid + all_ml_coef[2]*allml2$assess_security_prepaid + all_ml_coef[3]*allml2$assess_convenience_prepaid 
#
head(allml2[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

# All: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
(mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
allml2$cs = mui*log(exp(allml2$v_cash) + 0+ exp(allml2$v_credit) + exp(allml2$v_debit) + exp(allml2$v_prepaid)) # cash is ref pi here
#
# CS *after* cash is phased out
allml2$cs_cashless =  mui*log(0 + exp(allml2$v_credit) + exp(allml2$v_debit) + exp(allml2$v_prepaid))
#
# Difference in CS (cashless - before)
allml2$cs_diff = allml2$cs_cashless - allml2$cs
# rate of change
allml2$cs_rate = allml2$cs_diff/allml2$cs
#
length(allml2$cs)
summary(allml2$cs) # CS before the change
summary(allml2$cs_cashless) # CS after stores become cashless
summary(allml2$cs_diff)
summary(allml2$cs_rate)
(allml2_loss_rate_med = median(-1*allml2$cs_rate, na.rm = T))
(allml2_loss_rate_avg = mean(-1*allml2$cs_rate, na.rm = T))
(allml2_loss_rate_q25 = quantile(-1*allml2$cs_rate, 0.25, na.rm = T))
(allml2_loss_rate_q75 = quantile(-1*allml2$cs_rate, 0.75, na.rm = T))
(allml2_loss_rate_min = min(-1*allml2$cs_rate, na.rm = T))
(allml2_loss_rate_max = max(-1*allml2$cs_rate, na.rm = T))
(allml2_loss_rate_sd = sd(-1*allml2$cs_rate, na.rm = T))
#head(allml2[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
#

## Computations of Consumer Surplus (CS) (Both)
# computing utility of cash, check, credit, debit, and prepaid 
#bothml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
bothml$v_cash = NA # # estimated utility from paying cash
bothml$v_cash = both_ml_coef[1]*bothml$assess_cost_cash + both_ml_coef[2]*bothml$assess_security_cash + both_ml_coef[3]*bothml$assess_convenience_cash 
#
# bothml$v_check = NA # estimated utility from paying check (removed)
# bothml$v_check = both_ml_coef[1]*bothml$assess_cost_check + both_ml_coef[2]*bothml$assess_security_check + both_ml_coef[3]*bothml$assess_convenience_check 
#
bothml$v_credit = NA # # estimated utility from paying credit
bothml$v_credit = both_ml_coef[1]*bothml$assess_cost_credit + both_ml_coef[2]*bothml$assess_security_credit + both_ml_coef[3]*bothml$assess_convenience_credit 

bothml$v_debit = NA # estimated utility from debit 
bothml$v_debit = both_ml_coef[1]*bothml$assess_cost_debit + both_ml_coef[2]*bothml$assess_security_debit + both_ml_coef[3]*bothml$assess_convenience_debit 
#
bothml$v_prepaid = NA # estimated utility from paying prepaid
bothml$v_prepaid = both_ml_coef[1]*bothml$assess_cost_prepaid + both_ml_coef[2]*bothml$assess_security_prepaid + both_ml_coef[3]*bothml$assess_convenience_prepaid 
#
head(bothml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

# Both: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
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
(bothml_loss_rate_med = median(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_avg = mean(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_q25 = quantile(-1*bothml$cs_rate, 0.25, na.rm = T))
(bothml_loss_rate_q75 = quantile(-1*bothml$cs_rate, 0.75, na.rm = T))
(bothml_loss_rate_min = min(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_max = max(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_sd = sd(-1*bothml$cs_rate, na.rm = T))
#head(bothml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
#

## Computations of Consumer Surplus (CS) (noccyesdc, no cc yes dc)
# computing utility of cash, check, credit, debit, and prepaid 
#noccyesdcml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
noccyesdcml$v_cash = NA # # estimated utility from paying cash
noccyesdcml$v_cash = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_cash + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_cash + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_cash 

#
# noccyesdcml$v_check = NA # estimated utility from paying check (removed)
# noccyesdcml$v_check = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_check + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_check + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_check 
#
noccyesdcml$v_credit = NA # # estimated utility from paying credit
noccyesdcml$v_credit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_credit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_credit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_credit 

noccyesdcml$v_debit = NA # estimated utility from debit 
noccyesdcml$v_debit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_debit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_debit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_debit 
#
noccyesdcml$v_prepaid = NA # estimated utility from paying prepaid
noccyesdcml$v_prepaid = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_prepaid + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_prepaid + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_prepaid 
#
head(noccyesdcml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20) # Credit will not be included in CS below

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
(noccyesdcml_loss_rate_med = median(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_avg = mean(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_q25 = quantile(-1*noccyesdcml$cs_rate, 0.25, na.rm = T))
(noccyesdcml_loss_rate_q75 = quantile(-1*noccyesdcml$cs_rate, 0.75, na.rm = T))
(noccyesdcml_loss_rate_min = min(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_max = max(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_sd = sd(-1*noccyesdcml$cs_rate, na.rm = T))
#head(noccyesdcml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

## Computations of Consumer Surplus (CS) (None banked)
# computing utility of cash, check, credit, debit, and prepaid 
#nonebankedml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
nonebankedml$v_cash = NA # # estimated utility from paying cash
nonebankedml$v_cash = nonebankedml_coef[1]*nonebankedml$assess_cost_cash + nonebankedml_coef[2]*nonebankedml$assess_security_cash + nonebankedml_coef[3]*nonebankedml$assess_convenience_cash 
#
# nonebankedml$v_check = NA # estimated utility from paying check (removed)
# nonebankedml$v_check = nonebankedml_coef[1]*nonebankedml$assess_cost_check + nonebankedml_coef[2]*nonebankedml$assess_security_check + nonebankedml_coef[3]*nonebankedml$assess_convenience_check 
#
nonebankedml$v_credit = NA # # estimated utility from paying credit
nonebankedml$v_credit = nonebankedml_coef[1]*nonebankedml$assess_cost_credit + nonebankedml_coef[2]*nonebankedml$assess_security_credit + nonebankedml_coef[3]*nonebankedml$assess_convenience_credit 

nonebankedml$v_debit = NA # estimated utility from debit 
nonebankedml$v_debit = nonebankedml_coef[1]*nonebankedml$assess_cost_debit + nonebankedml_coef[2]*nonebankedml$assess_security_debit + nonebankedml_coef[3]*nonebankedml$assess_convenience_debit 
#
nonebankedml$v_prepaid = NA # estimated utility from paying prepaid
nonebankedml$v_prepaid = nonebankedml_coef[1]*nonebankedml$assess_cost_prepaid + nonebankedml_coef[2]*nonebankedml$assess_security_prepaid + nonebankedml_coef[3]*nonebankedml$assess_convenience_prepaid 
#
head(nonebankedml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

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
(nonebankedml_loss_rate_med = median(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_avg = mean(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_q25 = quantile(-1*nonebankedml$cs_rate, 0.25, na.rm = T))
(nonebankedml_loss_rate_q75 = quantile(-1*nonebankedml$cs_rate, 0.75, na.rm = T))
(nonebankedml_loss_rate_min = min(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_max = max(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_sd = sd(-1*nonebankedml$cs_rate, na.rm = T))
#head(nonebankedml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

## Computations of Consumer Surplus (CS) (None Unbanked)
# computing utility of cash, check, credit, debit, and prepaid 
#noneunbml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
noneunbml$v_cash = NA # # estimated utility from paying cash
noneunbml$v_cash = noneunbml_coef[1]*noneunbml$assess_cost_cash + noneunbml_coef[2]*noneunbml$assess_security_cash + noneunbml_coef[3]*noneunbml$assess_convenience_cash 
#
# noneunbml$v_check = NA # estimated utility from paying check (removed)
# noneunbml$v_check = noneunbml_coef[1]*noneunbml$assess_cost_check + noneunbml_coef[2]*noneunbml$assess_security_check + noneunbml_coef[3]*noneunbml$assess_convenience_check 
#
noneunbml$v_credit = NA # # estimated utility from paying credit
noneunbml$v_credit = noneunbml_coef[1]*noneunbml$assess_cost_credit + noneunbml_coef[2]*noneunbml$assess_security_credit + noneunbml_coef[3]*noneunbml$assess_convenience_credit 

noneunbml$v_debit = NA # estimated utility from debit 
noneunbml$v_debit = noneunbml_coef[1]*noneunbml$assess_cost_debit + noneunbml_coef[2]*noneunbml$assess_security_debit + noneunbml_coef[3]*noneunbml$assess_convenience_debit 
#
noneunbml$v_prepaid = NA # estimated utility from paying prepaid
noneunbml$v_prepaid = noneunbml_coef[1]*noneunbml$assess_cost_prepaid + noneunbml_coef[2]*noneunbml$assess_security_prepaid + noneunbml_coef[3]*noneunbml$assess_convenience_prepaid 
#
head(noneunbml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

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
(noneunbml_loss_rate_med = median(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_avg = mean(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_q25 = quantile(-1*noneunbml$cs_rate, 0.25, na.rm = T))
(noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
(noneunbml_loss_rate_min = min(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_max = max(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
(noneunbml_loss_rate_sd = sd(-1*noneunbml$cs_rate, na.rm = T))
#head(noneunbml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

# Right columns the Table 4 in paper: CS LOSS table 
(lossinf.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)),  "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "sd_C" = round(c(all_ml_sd[1], both_ml_sd[1], noccyesdc_ml_sd[1], nonebankedml_sd[1], nonebankedml_sd[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "sd_S" = round(c(all_ml_sd[2], both_ml_sd[2], noccyesdc_ml_sd[2], nonebankedml_sd[2], nonebankedml_sd[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "sd_E" = round(c(all_ml_sd[3], both_ml_sd[3], noccyesdc_ml_sd[3], nonebankedml_sd[3], nonebankedml_sd[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "Median" = percent(c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), digits = 1), "Mean" = percent(c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), digits = 1), "Min" = percent(c(allml2_loss_rate_min, bothml_loss_rate_min, noccyesdcml_loss_rate_min, nonebankedml_loss_rate_min, noneunbml_loss_rate_min), digits = 1), "Max" = percent(c(allml2_loss_rate_max, bothml_loss_rate_max, noccyesdcml_loss_rate_max, nonebankedml_loss_rate_max, noneunbml_loss_rate_max), digits = 1) ))

# Above table in 100*fraction format (needed for xtable, x for xtable)
# also q25 and q75 repalce min and max
(lossinfx.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)),  "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "sd_C" = round(c(all_ml_sd[1], both_ml_sd[1], noccyesdc_ml_sd[1], nonebankedml_sd[1], nonebankedml_sd[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "sd_S" = round(c(all_ml_sd[2], both_ml_sd[2], noccyesdc_ml_sd[2], nonebankedml_sd[2], nonebankedml_sd[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "sd_E" = round(c(all_ml_sd[3], both_ml_sd[3], noccyesdc_ml_sd[3], nonebankedml_sd[3], nonebankedml_sd[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "q25" = 100*c(allml2_loss_rate_q25, bothml_loss_rate_q25, noccyesdcml_loss_rate_q25, nonebankedml_loss_rate_q25, noneunbml_loss_rate_q25), "Median" = 100*c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), "Mean" = 100*c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), "q75" = 100*c(allml2_loss_rate_q75, bothml_loss_rate_q75, noccyesdcml_loss_rate_q75, nonebankedml_loss_rate_q75, noneunbml_loss_rate_q75) ))

names(lossinfx.df)# top part of Table 4 (no random effects)
# multinomial and CS w/o random effects starts Line 329 ends Line 800 Mixed logit started thereafter

##############suspended below for cash-56.tex and on (instead try fixed assessments)
### Redo all above with mixed logit (random coefficients) [to be added below the above in Table 3 starts line 802 ends line 1107 (analysis suspended)

# # model to be estimated (All)
# (allml2model = pi ~ cost + security + convenience | -1)  # w/o adoption
# all_ml = mlogit(allml2model, rpar = c(cost = "zbt", security = "t", convenience = "t"),  data = allml2_data)# ln, cn do not work! t=triangular dist, n=normal, , zbt=zero bond triangular
# summary(all_ml)
# (all_ml_coef = as.vector(all_ml$coefficients[1:3])) # cost, security, convenience
# (all_ml_pvalue = coef(summary(all_ml))[,4]) # extract p-value
# (all_ml_pvalue = as.vector(all_ml_pvalue)) # p-values vector
# (all_ml_sig = as.vector( symnum(all_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
# #
# 
# # model to be estimated (both)
# (bothmlmodel = pi ~ cost + security + convenience | -1)  # w/o adoption
# both_ml = mlogit(bothmlmodel, rpar = c(cost = "zbt", security = "t", convenience = "t"),  data = bothml_data)# ln, cn do not work! t=triangular dist, n=normal, zbt=zero bond triangular
# summary(both_ml)
# (both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
# (both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
# (both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
# (both_ml_sig = as.vector( symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
# #
# 
# # model to be estimated (noccyesdc)
# noccyesdcmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
# noccyesdc_ml = mlogit(noccyesdcmlmodel, alt.subset = c("cash", "check", "debit", "prepaid"), rpar = c(cost = "zbt", security = "t", convenience = "t"),  data = noccyesdcml_data)# ln, cn do not work! t=triangular dist, n=normal
# summary(noccyesdc_ml)
# (noccyesdc_ml_coef = as.vector(noccyesdc_ml$coefficients[1:3])) # cost, security, convenience
# (noccyesdc_ml_pvalue = coef(summary(noccyesdc_ml))[,4]) # extract p-value
# (noccyesdc_ml_pvalue = as.vector(noccyesdc_ml_pvalue)) # p-values vector
# (noccyesdc_ml_sig = as.vector(symnum(noccyesdc_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
# 
# # model to be estimated (none banked)
# nonebankedmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
# nonebanked_ml = mlogit(nonebankedmlmodel, alt.subset = c("cash", "check", "debit", "prepaid"), rpar = c(cost = "zbt", security = "t", convenience = "t"),  data = nonebankedml_data)# ln, cn do not work! t=triangular dist, n=normal
# summary(nonebanked_ml)
# (nonebankedml_coef = as.vector(nonebanked_ml$coefficients[1:3])) # cost, security, convenience
# (nonebankedml_pvalue = coef(summary(nonebanked_ml))[,4]) # extract p-value
# (nonebankedml_pvalue = as.vector(nonebankedml_pvalue)) # p-values vector
# (nonebankedml_sig = as.vector(symnum(nonebankedml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
# 
# # model to be estimated (none unbanked)
# noneunbmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
# noneunb_ml = mlogit(noneunbmlmodel, alt.subset = c("cash", "check",  "prepaid"), rpar = c(cost = "zbt", security = "t", convenience = "t"),  data = noneunbml_data)# ln, cn do not work! t=triangular dist, n=normal
# summary(noneunb_ml)
# (noneunbml_coef = as.vector(noneunb_ml$coefficients[1:3])) # cost, security, convenience
# (noneunbml_pvalue = coef(summary(noneunb_ml))[,4]) # extract p-value
# (noneunbml_pvalue = as.vector(noneunbml_pvalue)) # p-values vector
# (noneunbml_sig = as.vector(symnum(noneunbml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
# 
# ## Computations of Consumer Surplus (CS) (All)
# # computing utility of cash, check, credit, debit, and prepaid 
# # allml2$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
# allml2$v_credit = NA # # estimated utility from paying credit
# allml2$v_credit = all_ml_coef[1]*allml2$assess_cost_credit + all_ml_coef[2]*allml2$assess_security_credit + all_ml_coef[3]*allml2$assess_convenience_credit 
# #
# # allml2$v_check = NA # estimated utility from paying check (removed)
# # allml2$v_check = all_ml_coef[1]*allml2$assess_cost_check + all_ml_coef[2]*allml2$assess_security_check + all_ml_coef[3]*allml2$assess_convenience_check 
# #
# allml2$v_credit = NA # # estimated utility from paying credit
# allml2$v_credit = all_ml_coef[1]*allml2$assess_cost_credit + all_ml_coef[2]*allml2$assess_security_credit + all_ml_coef[3]*allml2$assess_convenience_credit 
# 
# allml2$v_debit = NA # estimated utility from debit 
# allml2$v_debit = all_ml_coef[1]*allml2$assess_cost_debit + all_ml_coef[2]*allml2$assess_security_debit + all_ml_coef[3]*allml2$assess_convenience_debit 
# #
# allml2$v_prepaid = NA # estimated utility from paying prepaid
# allml2$v_prepaid = all_ml_coef[1]*allml2$assess_cost_prepaid + all_ml_coef[2]*allml2$assess_security_prepaid + all_ml_coef[3]*allml2$assess_convenience_prepaid 
# #
# head(allml2[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)
# 
# # All: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
# allml2$cs = mui*log(exp(allml2$v_cash) + 0+ exp(allml2$v_credit) + exp(allml2$v_debit) + exp(allml2$v_prepaid)) # cash is ref pi here
# #
# # CS *after* cash is phased out
# allml2$cs_cashless =  mui*log(0 + exp(allml2$v_credit) + exp(allml2$v_debit) + exp(allml2$v_prepaid))
# #
# # Difference in CS (cashless - before)
# allml2$cs_diff = allml2$cs_cashless - allml2$cs
# # rate of change
# allml2$cs_rate = allml2$cs_diff/allml2$cs
# #
# length(allml2$cs)
# summary(allml2$cs) # CS before the change
# summary(allml2$cs_cashless) # CS after stores become cashless
# summary(allml2$cs_diff)
# summary(allml2$cs_rate)
# (allml2_loss_rate_med = median(-1*allml2$cs_rate, na.rm = T))
# (allml2_loss_rate_avg = mean(-1*allml2$cs_rate, na.rm = T))
# (allml2_loss_rate_q25 = quantile(-1*allml2$cs_rate, probs = 0.25, na.rm = T))
# (allml2_loss_rate_q75 = quantile(-1*allml2$cs_rate, probs = 0.75, na.rm = T))
# (allml2_loss_rate_min = min(-1*allml2$cs_rate, na.rm = T))
# (allml2_loss_rate_max = max(-1*allml2$cs_rate, na.rm = T))
# (allml2_loss_rate_sd = sd(-1*allml2$cs_rate, na.rm = T))
# #head(allml2[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
# #
# 
# ## Computations of Consumer Surplus (CS) (Both)
# # computing utility of cash, check, credit, debit, and prepaid 
# bothml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
# bothml$v_cash = NA # # estimated utility from paying cash
# bothml$v_cash = both_ml_coef[1]*bothml$assess_cost_cash + both_ml_coef[2]*bothml$assess_security_cash + both_ml_coef[3]*bothml$assess_convenience_cash 
# #
# # bothml$v_check = NA # estimated utility from paying check (removed)
# # bothml$v_check = both_ml_coef[1]*bothml$assess_cost_check + both_ml_coef[2]*bothml$assess_security_check + both_ml_coef[3]*bothml$assess_convenience_check 
# #
# bothml$v_credit = NA # # estimated utility from paying credit
# bothml$v_credit = both_ml_coef[1]*bothml$assess_cost_credit + both_ml_coef[2]*bothml$assess_security_credit + both_ml_coef[3]*bothml$assess_convenience_credit 
# 
# bothml$v_debit = NA # estimated utility from debit 
# bothml$v_debit = both_ml_coef[1]*bothml$assess_cost_debit + both_ml_coef[2]*bothml$assess_security_debit + both_ml_coef[3]*bothml$assess_convenience_debit 
# #
# bothml$v_prepaid = NA # estimated utility from paying prepaid
# bothml$v_prepaid = both_ml_coef[1]*bothml$assess_cost_prepaid + both_ml_coef[2]*bothml$assess_security_prepaid + both_ml_coef[3]*bothml$assess_convenience_prepaid 
# #
# head(bothml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)
# 
# # Both: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
# bothml$cs = mui*log(exp(bothml$v_cash) + 0+ exp(bothml$v_credit) + exp(bothml$v_debit) + exp(bothml$v_prepaid)) # cash is ref pi here
# #
# # CS *after* cash is phased out
# bothml$cs_cashless =  mui*log(0 + exp(bothml$v_credit) + exp(bothml$v_debit) + exp(bothml$v_prepaid))
# #
# # Difference in CS (cashless - before)
# bothml$cs_diff = bothml$cs_cashless - bothml$cs
# # rate of change
# bothml$cs_rate = bothml$cs_diff/bothml$cs
# #
# length(bothml$cs)
# summary(bothml$cs) # CS before the change
# summary(bothml$cs_cashless) # CS after stores become cashless
# summary(bothml$cs_diff)
# summary(bothml$cs_rate)
# (bothml_loss_rate_med = median(-1*bothml$cs_rate, na.rm = T))
# (bothml_loss_rate_avg = mean(-1*bothml$cs_rate, na.rm = T))
# (bothml_loss_rate_q25 = quantile(-1*bothml$cs_rate, 0.25, na.rm = T))
# (bothml_loss_rate_q75 = quantile(-1*bothml$cs_rate, 0.75, na.rm = T))
# (bothml_loss_rate_min = min(-1*bothml$cs_rate, na.rm = T))
# (bothml_loss_rate_max = max(-1*bothml$cs_rate, na.rm = T))
# (bothml_loss_rate_sd = sd(-1*bothml$cs_rate, na.rm = T))
# #head(bothml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
# #
# 
# ## Computations of Consumer Surplus (CS) (noccyesdc, no cc yes dc)
# # computing utility of cash, check, credit, debit, and prepaid 
# # noccyesdcml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
# noccyesdcml$v_cash = NA # # estimated utility from paying cash
# noccyesdcml$v_cash = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_cash + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_cash + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_cash 
# #
# # noccyesdcml$v_check = NA # estimated utility from paying check (removed)
# # noccyesdcml$v_check = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_check + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_check + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_check 
# #
# noccyesdcml$v_credit = NA # # estimated utility from paying credit
# noccyesdcml$v_credit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_credit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_credit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_credit 
# 
# noccyesdcml$v_debit = NA # estimated utility from debit 
# noccyesdcml$v_debit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_debit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_debit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_debit 
# #
# noccyesdcml$v_prepaid = NA # estimated utility from paying prepaid
# noccyesdcml$v_prepaid = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_prepaid + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_prepaid + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_prepaid 
# #
# head(noccyesdcml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20) # Credit will not be included in CS below
# 
# # No cc Yes dc: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
# noccyesdcml$cs = mui*log(exp(noccyesdcml$v_cash) + 0 + 0 + exp(noccyesdcml$v_debit) + exp(noccyesdcml$v_prepaid)) # cash is ref pi here
# #
# # CS *after* cash is phased out
# noccyesdcml$cs_cashless =  mui*log(0 + 0 + exp(noccyesdcml$v_debit) + exp(noccyesdcml$v_prepaid))
# #
# # Difference in CS (cashless - before)
# noccyesdcml$cs_diff = noccyesdcml$cs_cashless - noccyesdcml$cs
# # rate of change
# noccyesdcml$cs_rate = noccyesdcml$cs_diff/noccyesdcml$cs
# #
# length(noccyesdcml$cs)
# summary(noccyesdcml$cs) # CS before the change
# summary(noccyesdcml$cs_cashless) # CS after stores become cashless
# summary(noccyesdcml$cs_diff)
# summary(noccyesdcml$cs_rate)
# (noccyesdcml_loss_rate_med = median(-1*noccyesdcml$cs_rate, na.rm = T))
# (noccyesdcml_loss_rate_avg = mean(-1*noccyesdcml$cs_rate, na.rm = T))
# (noccyesdcml_loss_rate_q25 = quantile(-1*noccyesdcml$cs_rate, 0.25, na.rm = T))
# (noccyesdcml_loss_rate_q75 = quantile(-1*noccyesdcml$cs_rate, 0.75, na.rm = T))
# (noccyesdcml_loss_rate_min = min(-1*noccyesdcml$cs_rate, na.rm = T))
# (noccyesdcml_loss_rate_max = max(-1*noccyesdcml$cs_rate, na.rm = T))
# (noccyesdcml_loss_rate_sd = sd(-1*noccyesdcml$cs_rate, na.rm = T))
# #head(noccyesdcml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
# 
# ## Computations of Consumer Surplus (CS) (None banked)
# # computing utility of cash, check, credit, debit, and prepaid 
# #nonebankedml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
# nonebankedml$v_cash = NA # # estimated utility from paying cash
# nonebankedml$v_cash = nonebankedml_coef[1]*nonebankedml$assess_cost_cash + nonebankedml_coef[2]*nonebankedml$assess_security_cash + nonebankedml_coef[3]*nonebankedml$assess_convenience_cash 
# #
# # nonebankedml$v_check = NA # estimated utility from paying check (removed)
# # nonebankedml$v_check = nonebankedml_coef[1]*nonebankedml$assess_cost_check + nonebankedml_coef[2]*nonebankedml$assess_security_check + nonebankedml_coef[3]*nonebankedml$assess_convenience_check 
# #
# nonebankedml$v_credit = NA # # estimated utility from paying credit
# nonebankedml$v_credit = nonebankedml_coef[1]*nonebankedml$assess_cost_credit + nonebankedml_coef[2]*nonebankedml$assess_security_credit + nonebankedml_coef[3]*nonebankedml$assess_convenience_credit 
# 
# nonebankedml$v_debit = NA # estimated utility from debit 
# nonebankedml$v_debit = nonebankedml_coef[1]*nonebankedml$assess_cost_debit + nonebankedml_coef[2]*nonebankedml$assess_security_debit + nonebankedml_coef[3]*nonebankedml$assess_convenience_debit 
# #
# nonebankedml$v_prepaid = NA # estimated utility from paying prepaid
# nonebankedml$v_prepaid = nonebankedml_coef[1]*nonebankedml$assess_cost_prepaid + nonebankedml_coef[2]*nonebankedml$assess_security_prepaid + nonebankedml_coef[3]*nonebankedml$assess_convenience_prepaid 
# #
# head(nonebankedml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)
# 
# # None unbanked: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
# nonebankedml$cs = mui*log(exp(nonebankedml$v_cash) + 0 + 0 + 0 + exp(nonebankedml$v_prepaid)) # cash is ref pi here
# #
# # CS *after* cash is phased out
# nonebankedml$cs_cashless =  mui*log(0 +0 +0 + exp(nonebankedml$v_prepaid))
# #
# # Difference in CS (cashless - before)
# nonebankedml$cs_diff = nonebankedml$cs_cashless - nonebankedml$cs
# # rate of change
# nonebankedml$cs_rate = nonebankedml$cs_diff/nonebankedml$cs
# #
# length(nonebankedml$cs)
# summary(nonebankedml$cs) # CS before the change
# summary(nonebankedml$cs_cashless) # CS after stores become cashless
# summary(nonebankedml$cs_diff)
# summary(nonebankedml$cs_rate)
# (nonebankedml_loss_rate_med = median(-1*nonebankedml$cs_rate, na.rm = T))
# (nonebankedml_loss_rate_avg = mean(-1*nonebankedml$cs_rate, na.rm = T))
# (nonebankedml_loss_rate_q25 = quantile(-1*nonebankedml$cs_rate, 0.25, na.rm = T))
# (nonebankedml_loss_rate_q75 = quantile(-1*nonebankedml$cs_rate, 0.75, na.rm = T))
# (nonebankedml_loss_rate_min = min(-1*nonebankedml$cs_rate, na.rm = T))
# (nonebankedml_loss_rate_max = max(-1*nonebankedml$cs_rate, na.rm = T))
# (nonebankedml_loss_rate_sd = sd(-1*nonebankedml$cs_rate, na.rm = T))
# #head(nonebankedml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
# 
# ## Computations of Consumer Surplus (CS) (None Unbanked)
# # computing utility of cash, check, credit, debit, and prepaid 
# #noneunbml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
# noneunbml$v_cash = NA # # estimated utility from paying cash
# noneunbml$v_cash = noneunbml_coef[1]*noneunbml$assess_cost_cash + noneunbml_coef[2]*noneunbml$assess_security_cash + noneunbml_coef[3]*noneunbml$assess_convenience_cash 
# #
# # noneunbml$v_check = NA # estimated utility from paying check (removed)
# # noneunbml$v_check = noneunbml_coef[1]*noneunbml$assess_cost_check + noneunbml_coef[2]*noneunbml$assess_security_check + noneunbml_coef[3]*noneunbml$assess_convenience_check 
# #
# noneunbml$v_credit = NA # # estimated utility from paying credit
# noneunbml$v_credit = noneunbml_coef[1]*noneunbml$assess_cost_credit + noneunbml_coef[2]*noneunbml$assess_security_credit + noneunbml_coef[3]*noneunbml$assess_convenience_credit 
# 
# noneunbml$v_debit = NA # estimated utility from debit 
# noneunbml$v_debit = noneunbml_coef[1]*noneunbml$assess_cost_debit + noneunbml_coef[2]*noneunbml$assess_security_debit + noneunbml_coef[3]*noneunbml$assess_convenience_debit 
# #
# noneunbml$v_prepaid = NA # estimated utility from paying prepaid
# noneunbml$v_prepaid = noneunbml_coef[1]*noneunbml$assess_cost_prepaid + noneunbml_coef[2]*noneunbml$assess_security_prepaid + noneunbml_coef[3]*noneunbml$assess_convenience_prepaid 
# #
# head(noneunbml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)
# 
# # None: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed)
# noneunbml$cs = mui*log(exp(noneunbml$v_cash) + 0 + 0 + 0 + exp(noneunbml$v_prepaid)) # cash is ref pi here
# #
# # CS *after* cash is phased out
# noneunbml$cs_cashless =  mui*log(0 +0 +0 + exp(noneunbml$v_prepaid))
# #
# # Difference in CS (cashless - before)
# noneunbml$cs_diff = noneunbml$cs_cashless - noneunbml$cs
# # rate of change
# noneunbml$cs_rate = noneunbml$cs_diff/noneunbml$cs
# #
# length(noneunbml$cs)
# summary(noneunbml$cs) # CS before the change
# summary(noneunbml$cs_cashless) # CS after stores become cashless
# summary(noneunbml$cs_diff)
# summary(noneunbml$cs_rate)
# (noneunbml_loss_rate_med = median(-1*noneunbml$cs_rate, na.rm = T))
# (noneunbml_loss_rate_avg = mean(-1*noneunbml$cs_rate, na.rm = T))
# (noneunbml_loss_rate_q25 = quantile(-1*noneunbml$cs_rate, 0.25, na.rm = T))
# (noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
# (noneunbml_loss_rate_min = min(-1*noneunbml$cs_rate, na.rm = T))
# (noneunbml_loss_rate_max = max(-1*noneunbml$cs_rate, na.rm = T))
# (noneunbml_loss_rate_sd = sd(-1*noneunbml$cs_rate, na.rm = T))
# #head(noneunbml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
# 
# # Right columns the Table 3 in paper: CS LOSS table  with mixed logit
# (lossinf_mixed.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)), "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "Median" = percent(c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), digits = 1), "Mean" = percent(c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), digits = 1), "Min" = percent(c(allml2_loss_rate_min, bothml_loss_rate_min, noccyesdcml_loss_rate_min, nonebankedml_loss_rate_min, noneunbml_loss_rate_min), digits = 1), "Max" = percent(c(allml2_loss_rate_max, bothml_loss_rate_max, noccyesdcml_loss_rate_max, nonebankedml_loss_rate_max, noneunbml_loss_rate_max), digits = 1) ))
# 
# # Above table in 100*fraction format (needed for xtable, x for xtable) with mixed 
# # contains q25 and q75 to replace min and max
# (lossinfx_mixed.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), 
# "Resp" = c(length(unique(allml2$id)), length(unique(bothml$id)), length(unique(noccyesdcml$id)), length(unique(nonebankedml$id)), length(unique(noneunbml$id))),                 "Payments" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)), "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]),"q25" = 100*c(allml2_loss_rate_q25, bothml_loss_rate_q25, noccyesdcml_loss_rate_q25, nonebankedml_loss_rate_q25, noneunbml_loss_rate_q25), "Median" = 100*c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), "Mean" = 100*c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), "q75" = 100*c(allml2_loss_rate_q75, bothml_loss_rate_q75, noccyesdcml_loss_rate_q75, nonebankedml_loss_rate_q75, noneunbml_loss_rate_q75) ))
# 
# ## Finalizing Table 4: (top: multinomia no random coefficients), bottom: mixed logit)
# # need to combine 2 data frames:
# lossinfx.df
# lossinfx_mixed.df
# 
# (loss.df = rbind(lossinfx.df, lossinfx_mixed.df))
# (loss2.df = loss.df[-c(1,6), ])# Removing ALL
# # (loss2.df = loss.df[-6, ])# remove line 6 which repeats the colnames
# # colnames(loss2.df)
# # dim(loss2.df)
# 
# # below, create matrix w\ 1 extra column to indicate number of digits for each row
# (digitm = matrix(c(rep(0,3), rep(3,6), rep(1,5)), nrow = 8, ncol = 14, byrow = T))
# #
# print(xtable(loss2.df, digits = digitm), include.rownames = F, hline.after = c(0,4))
# Random effects: line 802 ends line 1107 (analysis suspended)

### new from 200426: restimating utility using avg and med assessments (instead of individual assessmemnts) 
names(inst7)# transactions

# Need to find avg and med of each assessment by averaging respondents (not by payments). Note: this averaging is over all respondents (not by subgroups)
length(unique(inst7$id))# num resp
inst7_resp = inst7[!duplicated(inst7$id), ]
dim(inst7_resp)
#
names(inst7_resp)
# avg cost assessment by PI
(assess_cost_cash_avg = mean(inst7_resp$assess_cost_cash, na.rm = T))
(assess_cost_check_avg = mean(inst7_resp$assess_cost_check, na.rm = T))
(assess_cost_credit_avg = mean(inst7_resp$assess_cost_credit, na.rm = T))
(assess_cost_debit_avg = mean(inst7_resp$assess_cost_debit, na.rm = T))
(assess_cost_prepaid_avg = mean(inst7_resp$assess_cost_prepaid, na.rm = T))
# avg squared cost assessment by PI
(assess_cost_cash_avg2 = mean(inst7_resp$assess_cost_cash^2, na.rm = T))
(assess_cost_check_avg2 = mean(inst7_resp$assess_cost_check^2, na.rm = T))
(assess_cost_credit_avg2 = mean(inst7_resp$assess_cost_credit^2, na.rm = T))
(assess_cost_debit_avg2 = mean(inst7_resp$assess_cost_debit^2, na.rm = T))
(assess_cost_prepaid_avg2 = mean(inst7_resp$assess_cost_prepaid^2, na.rm = T))
# median cost assessment by PI
(assess_cost_cash_med = median(inst7_resp$assess_cost_cash, na.rm = T))
(assess_cost_check_med = median(inst7_resp$assess_cost_check, na.rm = T))
(assess_cost_credit_med = median(inst7_resp$assess_cost_credit, na.rm = T))
(assess_cost_debit_med = median(inst7_resp$assess_cost_debit, na.rm = T))
(assess_cost_prepaid_med = median(inst7_resp$assess_cost_prepaid, na.rm = T))
#
# avg security assessment by PI
(assess_security_cash_avg = mean(inst7_resp$assess_security_cash, na.rm = T))
(assess_security_check_avg = mean(inst7_resp$assess_security_check, na.rm = T))
(assess_security_credit_avg = mean(inst7_resp$assess_security_credit, na.rm = T))
(assess_security_debit_avg = mean(inst7_resp$assess_security_debit, na.rm = T))
(assess_security_prepaid_avg = mean(inst7_resp$assess_security_prepaid, na.rm = T))
# avg squared security assessment by PI
(assess_security_cash_avg2 = mean(inst7_resp$assess_security_cash^2, na.rm = T))
(assess_security_check_avg2 = mean(inst7_resp$assess_security_check^2, na.rm = T))
(assess_security_credit_avg2 = mean(inst7_resp$assess_security_credit^2, na.rm = T))
(assess_security_debit_avg2 = mean(inst7_resp$assess_security_debit^2, na.rm = T))
(assess_security_prepaid_avg2 = mean(inst7_resp$assess_security_prepaid^2, na.rm = T))

# median security assessment by PI
(assess_security_cash_med = median(inst7_resp$assess_security_cash, na.rm = T))
(assess_security_check_med = median(inst7_resp$assess_security_check, na.rm = T))
(assess_security_credit_med = median(inst7_resp$assess_security_credit, na.rm = T))
(assess_security_debit_med = median(inst7_resp$assess_security_debit, na.rm = T))
(assess_security_prepaid_med = median(inst7_resp$assess_security_prepaid, na.rm = T))
#
# avg convenience assessment by PI
(assess_convenience_cash_avg = mean(inst7_resp$assess_convenience_cash, na.rm = T))
(assess_convenience_check_avg = mean(inst7_resp$assess_convenience_check, na.rm = T))
(assess_convenience_credit_avg = mean(inst7_resp$assess_convenience_credit, na.rm = T))
(assess_convenience_debit_avg = mean(inst7_resp$assess_convenience_debit, na.rm = T))
(assess_convenience_prepaid_avg = mean(inst7_resp$assess_convenience_prepaid, na.rm = T))
# avg squared convenience assessment by PI
(assess_convenience_cash_avg2 = mean(inst7_resp$assess_convenience_cash^2, na.rm = T))
(assess_convenience_check_avg2 = mean(inst7_resp$assess_convenience_check^2, na.rm = T))
(assess_convenience_credit_avg2 = mean(inst7_resp$assess_convenience_credit^2, na.rm = T))
(assess_convenience_debit_avg2 = mean(inst7_resp$assess_convenience_debit^2, na.rm = T))
(assess_convenience_prepaid_avg2 = mean(inst7_resp$assess_convenience_prepaid^2, na.rm = T))
# median convenience assessment by PI
(assess_convenience_cash_med = median(inst7_resp$assess_convenience_cash, na.rm = T))
(assess_convenience_check_med = median(inst7_resp$assess_convenience_check, na.rm = T))
(assess_convenience_credit_med = median(inst7_resp$assess_convenience_credit, na.rm = T))
(assess_convenience_debit_med = median(inst7_resp$assess_convenience_debit, na.rm = T))
(assess_convenience_prepaid_med = median(inst7_resp$assess_convenience_prepaid, na.rm = T))
#
# avg acceptance assessment by PI
(assess_acceptance_cash_avg = mean(inst7_resp$assess_acceptance_cash, na.rm = T))
(assess_acceptance_check_avg = mean(inst7_resp$assess_acceptance_check, na.rm = T))
(assess_acceptance_credit_avg = mean(inst7_resp$assess_acceptance_credit, na.rm = T))
(assess_acceptance_debit_avg = mean(inst7_resp$assess_acceptance_debit, na.rm = T))
(assess_acceptance_prepaid_avg = mean(inst7_resp$assess_acceptance_prepaid, na.rm = T))
# avg squared acceptance assessment by PI
(assess_acceptance_cash_avg2 = mean(inst7_resp$assess_acceptance_cash^2, na.rm = T))
(assess_acceptance_check_avg2 = mean(inst7_resp$assess_acceptance_check^2, na.rm = T))
(assess_acceptance_credit_avg2 = mean(inst7_resp$assess_acceptance_credit^2, na.rm = T))
(assess_acceptance_debit_avg2 = mean(inst7_resp$assess_acceptance_debit^2, na.rm = T))
(assess_acceptance_prepaid_avg2 = mean(inst7_resp$assess_acceptance_prepaid^2, na.rm = T))
# median acceptance assessment by PI
(assess_acceptance_cash_med = median(inst7_resp$assess_acceptance_cash, na.rm = T))
(assess_acceptance_check_med = median(inst7_resp$assess_acceptance_check, na.rm = T))
(assess_acceptance_credit_med = median(inst7_resp$assess_acceptance_credit, na.rm = T))
(assess_acceptance_debit_med = median(inst7_resp$assess_acceptance_debit, na.rm = T))
(assess_acceptance_prepaid_med = median(inst7_resp$assess_acceptance_prepaid, na.rm = T))
# avg setup assessment by PI
(assess_setup_cash_avg = mean(inst7_resp$assess_setup_cash, na.rm = T))
(assess_setup_check_avg = mean(inst7_resp$assess_setup_check, na.rm = T))
(assess_setup_credit_avg = mean(inst7_resp$assess_setup_credit, na.rm = T))
(assess_setup_debit_avg = mean(inst7_resp$assess_setup_debit, na.rm = T))
(assess_setup_prepaid_avg = mean(inst7_resp$assess_setup_prepaid, na.rm = T))
# avg squared setup assessment by PI
(assess_setup_cash_avg2 = mean(inst7_resp$assess_setup_cash^2, na.rm = T))
(assess_setup_check_avg2 = mean(inst7_resp$assess_setup_check^2, na.rm = T))
(assess_setup_credit_avg2 = mean(inst7_resp$assess_setup_credit^2, na.rm = T))
(assess_setup_debit_avg2 = mean(inst7_resp$assess_setup_debit^2, na.rm = T))
(assess_setup_prepaid_avg2 = mean(inst7_resp$assess_setup_prepaid^2, na.rm = T))
# median setup assessment by PI
(assess_setup_cash_med = median(inst7_resp$assess_setup_cash, na.rm = T))
(assess_setup_check_med = median(inst7_resp$assess_setup_check, na.rm = T))
(assess_setup_credit_med = median(inst7_resp$assess_setup_credit, na.rm = T))
(assess_setup_debit_med = median(inst7_resp$assess_setup_debit, na.rm = T))
(assess_setup_prepaid_med = median(inst7_resp$assess_setup_prepaid, na.rm = T))
# avg record assessment by PI
(assess_record_cash_avg = mean(inst7_resp$assess_record_cash, na.rm = T))
(assess_record_check_avg = mean(inst7_resp$assess_record_check, na.rm = T))
(assess_record_credit_avg = mean(inst7_resp$assess_record_credit, na.rm = T))
(assess_record_debit_avg = mean(inst7_resp$assess_record_debit, na.rm = T))
(assess_record_prepaid_avg = mean(inst7_resp$assess_record_prepaid, na.rm = T))
# avg squared record assessment by PI
(assess_record_cash_avg2 = mean(inst7_resp$assess_record_cash^2, na.rm = T))
(assess_record_check_avg2 = mean(inst7_resp$assess_record_check^2, na.rm = T))
(assess_record_credit_avg2 = mean(inst7_resp$assess_record_credit^2, na.rm = T))
(assess_record_debit_avg2 = mean(inst7_resp$assess_record_debit^2, na.rm = T))
(assess_record_prepaid_avg2 = mean(inst7_resp$assess_record_prepaid^2, na.rm = T))
# median record assessment by PI
(assess_record_cash_med = median(inst7_resp$assess_record_cash, na.rm = T))
(assess_record_check_med = median(inst7_resp$assess_record_check, na.rm = T))
(assess_record_credit_med = median(inst7_resp$assess_record_credit, na.rm = T))
(assess_record_debit_med = median(inst7_resp$assess_record_debit, na.rm = T))
(assess_record_prepaid_med = median(inst7_resp$assess_record_prepaid, na.rm = T))

### From line 333 regression All data but with avg assessements
allml =  subset(inst7, select = c(id, pi, amnt, income, assess_cost_cash, assess_cost_check, assess_cost_credit, assess_cost_debit, assess_cost_prepaid, assess_security_cash, assess_security_check, assess_security_credit, assess_security_debit, assess_security_prepaid, assess_convenience_cash, assess_convenience_check, assess_convenience_credit, assess_convenience_debit, assess_convenience_prepaid, assess_acceptance_cash, assess_acceptance_check, assess_acceptance_credit, assess_acceptance_debit, assess_acceptance_prepaid, assess_setup_cash, assess_setup_check, assess_setup_credit, assess_setup_debit, assess_setup_prepaid, assess_record_cash, assess_record_check, assess_record_credit, assess_record_debit, assess_record_prepaid, cc_adopt, dc_adopt, svc_adopt, bnk_acnt_adopt )) 
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
## mlogit on All payments
dim(allml)
allml2 = na.omit(allml) # subsample w/ both cards
dim(allml2)
#
# now make it mlogit data (all payments)
allml2_data = mlogit.data(allml2, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(allml2_data)
head(allml2_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) ### Now, average cost 
allml2_data$cost = NA
allml2_data[allml2_data$alt=="cash", ]$cost = assess_cost_cash_avg
# allml2_data[allml2_data$alt=="check", ]$cost = allml2_data[allml2_data$alt=="check", ]$assess_cost_check
allml2_data[allml2_data$alt=="debit", ]$cost = assess_cost_debit_avg
allml2_data[allml2_data$alt=="credit", ]$cost = assess_cost_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$cost = assess_cost_prepaid_avg
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI) ### Now, average cost
allml2_data$security = NA
allml2_data[allml2_data$alt=="cash", ]$security = assess_security_cash_avg
# allml2_data[allml2_data$alt=="check", ]$security = allml2_data[allml2_data$alt=="check", ]$assess_security_check
allml2_data[allml2_data$alt=="debit", ]$security = assess_security_debit_avg
allml2_data[allml2_data$alt=="credit", ]$security = assess_security_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$security = assess_security_prepaid_avg
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$convenience = NA
allml2_data[allml2_data$alt=="cash", ]$convenience = assess_convenience_cash_avg
# allml2_data[allml2_data$alt=="check", ]$convenience = allml2_data[allml2_data$alt=="check", ]$assess_convenience_check
allml2_data[allml2_data$alt=="debit", ]$convenience = assess_convenience_debit_avg
allml2_data[allml2_data$alt=="credit", ]$convenience = assess_convenience_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_avg
# add a column "acceptance" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$acceptance = NA
allml2_data[allml2_data$alt=="cash", ]$acceptance = assess_acceptance_cash_avg
# allml2_data[allml2_data$alt=="check", ]$acceptance = allml2_data[allml2_data$alt=="check", ]$assess_acceptance_check
allml2_data[allml2_data$alt=="debit", ]$acceptance = assess_acceptance_debit_avg
allml2_data[allml2_data$alt=="credit", ]$acceptance = assess_acceptance_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$acceptance = assess_acceptance_prepaid_avg
# add a column "setup" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$setup = NA
allml2_data[allml2_data$alt=="cash", ]$setup = assess_setup_cash_avg
# allml2_data[allml2_data$alt=="check", ]$setup = allml2_data[allml2_data$alt=="check", ]$assess_setup_check
allml2_data[allml2_data$alt=="debit", ]$setup = assess_setup_debit_avg
allml2_data[allml2_data$alt=="credit", ]$setup = assess_setup_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$setup = assess_setup_prepaid_avg
# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$record = NA
allml2_data[allml2_data$alt=="cash", ]$record = assess_record_cash_avg
# allml2_data[allml2_data$alt=="check", ]$record = allml2_data[allml2_data$alt=="check", ]$assess_record_check
allml2_data[allml2_data$alt=="debit", ]$record = assess_record_debit_avg
allml2_data[allml2_data$alt=="credit", ]$record = assess_record_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$record = assess_record_prepaid_avg

## avg squared var
allml2_data$cost2 = NA
allml2_data[allml2_data$alt=="cash", ]$cost2 = assess_cost_cash_avg2
# allml2_data[allml2_data$alt=="check", ]$cost = allml2_data[allml2_data$alt=="check", ]$assess_cost_check
allml2_data[allml2_data$alt=="debit", ]$cost2 = assess_cost_debit_avg2
allml2_data[allml2_data$alt=="credit", ]$cost2 = assess_cost_credit_avg2
allml2_data[allml2_data$alt=="prepaid", ]$cost2 = assess_cost_prepaid_avg2
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI) ### Now, average cost squared
allml2_data$security2 = NA
allml2_data[allml2_data$alt=="cash", ]$security = assess_security_cash_avg
# allml2_data[allml2_data$alt=="check", ]$security = allml2_data[allml2_data$alt=="check", ]$assess_security_check
allml2_data[allml2_data$alt=="debit", ]$security = assess_security_debit_avg
allml2_data[allml2_data$alt=="credit", ]$security = assess_security_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$security = assess_security_prepaid_avg
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$convenience = NA
allml2_data[allml2_data$alt=="cash", ]$convenience = assess_convenience_cash_avg
# allml2_data[allml2_data$alt=="check", ]$convenience = allml2_data[allml2_data$alt=="check", ]$assess_convenience_check
allml2_data[allml2_data$alt=="debit", ]$convenience = assess_convenience_debit_avg
allml2_data[allml2_data$alt=="credit", ]$convenience = assess_convenience_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_avg
# add a column "acceptance" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$acceptance = NA
allml2_data[allml2_data$alt=="cash", ]$acceptance = assess_acceptance_cash_avg
# allml2_data[allml2_data$alt=="check", ]$acceptance = allml2_data[allml2_data$alt=="check", ]$assess_acceptance_check
allml2_data[allml2_data$alt=="debit", ]$acceptance = assess_acceptance_debit_avg
allml2_data[allml2_data$alt=="credit", ]$acceptance = assess_acceptance_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$acceptance = assess_acceptance_prepaid_avg
# add a column "setup" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$setup = NA
allml2_data[allml2_data$alt=="cash", ]$setup = assess_setup_cash_avg
# allml2_data[allml2_data$alt=="check", ]$setup = allml2_data[allml2_data$alt=="check", ]$assess_setup_check
allml2_data[allml2_data$alt=="debit", ]$setup = assess_setup_debit_avg
allml2_data[allml2_data$alt=="credit", ]$setup = assess_setup_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$setup = assess_setup_prepaid_avg
# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$record = NA
allml2_data[allml2_data$alt=="cash", ]$record = assess_record_cash_avg
# allml2_data[allml2_data$alt=="check", ]$record = allml2_data[allml2_data$alt=="check", ]$assess_record_check
allml2_data[allml2_data$alt=="debit", ]$record = assess_record_debit_avg
allml2_data[allml2_data$alt=="credit", ]$record = assess_record_credit_avg
allml2_data[allml2_data$alt=="prepaid", ]$record = assess_record_prepaid_avg


#
# model to be estimated (All)
(allml2model = pi ~ cost + security + convenience  | -1)  # adding any 4th avg var (acceptance or setup or record) kills the regression
all_ml = mlogit(allml2model, data = allml2_data)
summary(all_ml)
(all_ml_coef = as.vector(all_ml$coefficients[1:3])) # cost, security, convenience
(all_ml_sd = coef(summary(all_ml))[,2]) # standard errors
(all_ml_sd = as.vector(all_ml_sd)) # standard errors vector
(all_ml_pvalue = coef(summary(all_ml))[,4]) # extract p-value
(all_ml_pvalue = as.vector(all_ml_pvalue)) # p-values vector
(all_ml_sig = as.vector(symnum(all_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
#

# from line 394
## mlogit on both cards using average assessments of all resp
dim(allml)
bothml = na.omit(subset(allml, cc_adopt==1 & dc_adopt==1)) # subsample w/ both cards
dim(bothml)
#
# now make it mlogit data (both)
bothml_data = mlogit.data(bothml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(bothml_data)
head(bothml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
bothml_data$cost = NA
bothml_data[bothml_data$alt=="cash", ]$cost = assess_cost_cash_avg
# bothml_data[bothml_data$alt=="check", ]$cost = bothml_data[bothml_data$alt=="check", ]$assess_cost_check
bothml_data[bothml_data$alt=="debit", ]$cost = assess_cost_debit_avg
bothml_data[bothml_data$alt=="credit", ]$cost = assess_cost_credit_avg
bothml_data[bothml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_avg
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$security = NA
bothml_data[bothml_data$alt=="cash", ]$security = assess_security_cash_avg
# bothml_data[bothml_data$alt=="check", ]$security = bothml_data[bothml_data$alt=="check", ]$assess_security_check
bothml_data[bothml_data$alt=="debit", ]$security = assess_security_debit_avg
bothml_data[bothml_data$alt=="credit", ]$security = assess_security_credit_avg
bothml_data[bothml_data$alt=="prepaid", ]$security = assess_security_prepaid_avg
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$convenience = NA
bothml_data[bothml_data$alt=="cash", ]$convenience = bothml_data[bothml_data$alt=="cash", ]$assess_convenience_cash
# bothml_data[bothml_data$alt=="check", ]$convenience = bothml_data[bothml_data$alt=="check", ]$assess_convenience_check
bothml_data[bothml_data$alt=="debit", ]$convenience = bothml_data[bothml_data$alt=="debit", ]$assess_convenience_debit
bothml_data[bothml_data$alt=="credit", ]$convenience = bothml_data[bothml_data$alt=="credit", ]$assess_convenience_credit
bothml_data[bothml_data$alt=="prepaid", ]$convenience = bothml_data[bothml_data$alt=="prepaid", ]$assess_convenience_prepaid
#
# model to be estimated (both)
(bothmlmodel = pi ~ cost + security + convenience | -1)  # w/o adoption
both_ml = mlogit(bothmlmodel, data = bothml_data)
summary(both_ml)
(both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
(both_ml_sd = coef(summary(all_ml))[,2]) # standard errors
(both_ml_sd = as.vector(all_ml_sd)) # standard errors vector
(both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
(both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
(both_ml_sig = as.vector( symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
#

## from line 438
## mlogit on no_cc & yes_dc
dim(allml)
noccyesdcml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==1 & pi != "credit" ))
dim(noccyesdcml)
#
# now make it mlogit data (no cc & yes dc)
noccyesdcml_data = mlogit.data(noccyesdcml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(noccyesdcml_data)
head(noccyesdcml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
noccyesdcml_data$cost = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$cost = assess_cost_cash_avg
# noccyesdcml_data[noccyesdcml_data$alt=="check", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_cost_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$cost = assess_cost_debit_avg
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$cost = assess_cost_credit_avg
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_avg
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noccyesdcml_data$security = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$security = assess_security_cash_avg
# noccyesdcml_data[noccyesdcml_data$alt=="check", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_security_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$security = assess_security_debit_avg
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$security = assess_security_credit_avg
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$security = assess_security_prepaid_avg
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noccyesdcml_data$convenience = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$convenience = assess_convenience_cash_avg
#noccyesdcml_data[noccyesdcml_data$alt=="check", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_convenience_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$convenience = assess_convenience_debit_avg
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$convenience = assess_convenience_credit_avg
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_avg
#
# model to be estimated (noccyesdc)
noccyesdcmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
noccyesdc_ml = mlogit(noccyesdcmlmodel, alt.subset = c("cash", "debit", "prepaid"),  data = noccyesdcml_data)
summary(noccyesdc_ml)
(noccyesdc_ml_coef = as.vector(noccyesdc_ml$coefficients[1:3])) # cost, security, convenience
(noccyesdc_ml_sd = coef(summary(noccyesdc_ml))[,2]) # starndard errors
(noccyesdc_ml_sd = as.vector(noccyesdc_ml_sd)) # standard errors vector
(noccyesdc_ml_pvalue = coef(summary(noccyesdc_ml))[,4]) # extract p-value
(noccyesdc_ml_pvalue = as.vector(noccyesdc_ml_pvalue)) # p-values vector
(noccyesdc_ml_sig = as.vector(symnum(noccyesdc_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## from line 481
## mlogit on none_banked
dim(allml)
nonebankedml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==1 & pi != "credit" & pi != "debit"))
dim(nonebankedml)
#
# now make it mlogit data (no cc & yes dc)
nonebankedml_data = mlogit.data(nonebankedml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(nonebankedml_data)
head(nonebankedml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
nonebankedml_data$cost = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$cost = assess_cost_cash_avg
# nonebankedml_data[nonebankedml_data$alt=="check", ]$cost = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_cost_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$cost = assess_cost_debit_avg
nonebankedml_data[nonebankedml_data$alt=="credit", ]$cost = assess_cost_credit_avg
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_avg
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$security = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$security = assess_security_cash_avg
# nonebankedml_data[nonebankedml_data$alt=="check", ]$security = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_security_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$security = assess_security_debit_avg
nonebankedml_data[nonebankedml_data$alt=="credit", ]$security = assess_security_credit_avg
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$security = assess_security_prepaid_avg
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$convenience = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$convenience = assess_convenience_cash_avg
#nonebankedml_data[nonebankedml_data$alt=="check", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_convenience_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$convenience = assess_convenience_debit_avg
nonebankedml_data[nonebankedml_data$alt=="credit", ]$convenience = assess_convenience_credit_avg
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_avg
#
# model to be estimated (nonebanked)
nonebankedmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
nonebanked_ml = mlogit(nonebankedmlmodel, alt.subset = c("cash", "debit", "prepaid"),  data = nonebankedml_data)
summary(nonebanked_ml)
(nonebankedml_coef = as.vector(nonebanked_ml$coefficients[1:3])) # cost, security, convenience
(nonebankedml_sd = coef(summary(nonebanked_ml))[,2]) # standard errors
(nonebankedml_sd = as.vector(nonebankedml_sd)) # standard errors vect
(nonebankedml_pvalue = coef(summary(nonebanked_ml))[,4]) # extract p-value
(nonebankedml_pvalue = as.vector(nonebankedml_pvalue)) # p-values vector
(nonebankedml_sig = as.vector(symnum(nonebankedml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## from line 525
## mlogit on none_unb (unbanked)
dim(allml)
noneunbml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==0 & pi != "credit" & pi != "debit"))
dim(noneunbml)
#
# now make it mlogit data (no cc & yes dc)
noneunbml_data = mlogit.data(noneunbml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(noneunbml_data)
head(noneunbml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
noneunbml_data$cost = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$cost = assess_cost_cash_avg
# noneunbml_data[noneunbml_data$alt=="check", ]$cost = noneunbml_data[noneunbml_data$alt=="check", ]$assess_cost_check
noneunbml_data[noneunbml_data$alt=="debit", ]$cost = assess_cost_debit_avg
noneunbml_data[noneunbml_data$alt=="credit", ]$cost = assess_cost_credit_avg
noneunbml_data[noneunbml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_avg
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$security = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$security = assess_security_cash_avg
# noneunbml_data[noneunbml_data$alt=="check", ]$security = noneunbml_data[noneunbml_data$alt=="check", ]$assess_security_check
noneunbml_data[noneunbml_data$alt=="debit", ]$security = assess_security_debit_avg
noneunbml_data[noneunbml_data$alt=="credit", ]$security = assess_security_credit_avg
noneunbml_data[noneunbml_data$alt=="prepaid", ]$security = assess_security_prepaid_avg
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$convenience = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$convenience = assess_convenience_cash_avg
#noneunbml_data[noneunbml_data$alt=="check", ]$convenience = noneunbml_data[noneunbml_data$alt=="check", ]$assess_convenience_check
noneunbml_data[noneunbml_data$alt=="debit", ]$convenience = assess_convenience_debit_avg
noneunbml_data[noneunbml_data$alt=="credit", ]$convenience = assess_convenience_credit_avg
noneunbml_data[noneunbml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_avg
#
# model to be estimated (none UNbanked)
noneunbmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
noneunb_ml = mlogit(noneunbmlmodel, alt.subset = c("cash",  "prepaid"),  data = noneunbml_data)
summary(noneunb_ml)
(noneunbml_coef = as.vector(noneunb_ml$coefficients[1:3])) # cost, security, convenience
(noneunbml_sd = coef(summary(noneunb_ml))[,2]) # standard errors
(noneunbml_sd = as.vector(noneunbml_sd)) # standard error vector
(noneunbml_pvalue = coef(summary(noneunb_ml))[,4]) # extract p-value
(noneunbml_pvalue = as.vector(noneunbml_pvalue)) # p-values vector
(noneunbml_sig = as.vector(symnum(noneunbml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## From line 615 to line 811: Computing CS and bottom Table 4 for avg assessments estimations
## Computations of Consumer Surplus (CS) (Both)
# computing utility of cash, check, credit, debit, and prepaid 
#bothml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
bothml$v_cash = NA # # estimated utility from paying cash
bothml$v_cash = both_ml_coef[1]*bothml$assess_cost_cash + both_ml_coef[2]*bothml$assess_security_cash + both_ml_coef[3]*bothml$assess_convenience_cash 
#
# bothml$v_check = NA # estimated utility from paying check (removed)
# bothml$v_check = both_ml_coef[1]*bothml$assess_cost_check + both_ml_coef[2]*bothml$assess_security_check + both_ml_coef[3]*bothml$assess_convenience_check 
#
bothml$v_credit = NA # # estimated utility from paying credit
bothml$v_credit = both_ml_coef[1]*bothml$assess_cost_credit + both_ml_coef[2]*bothml$assess_security_credit + both_ml_coef[3]*bothml$assess_convenience_credit 

bothml$v_debit = NA # estimated utility from debit 
bothml$v_debit = both_ml_coef[1]*bothml$assess_cost_debit + both_ml_coef[2]*bothml$assess_security_debit + both_ml_coef[3]*bothml$assess_convenience_debit 
#
bothml$v_prepaid = NA # estimated utility from paying prepaid
bothml$v_prepaid = both_ml_coef[1]*bothml$assess_cost_prepaid + both_ml_coef[2]*bothml$assess_security_prepaid + both_ml_coef[3]*bothml$assess_convenience_prepaid 
#
head(bothml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

# Both: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
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
(bothml_loss_rate_med = median(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_avg = mean(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_q25 = quantile(-1*bothml$cs_rate, 0.25, na.rm = T))
(bothml_loss_rate_q75 = quantile(-1*bothml$cs_rate, 0.75, na.rm = T))
(bothml_loss_rate_min = min(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_max = max(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_sd = sd(-1*bothml$cs_rate, na.rm = T))
#head(bothml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
#

## Computations of Consumer Surplus (CS) (noccyesdc, no cc yes dc)
# computing utility of cash, check, credit, debit, and prepaid 
#noccyesdcml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
noccyesdcml$v_cash = NA # # estimated utility from paying cash
noccyesdcml$v_cash = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_cash + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_cash + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_cash 

#
# noccyesdcml$v_check = NA # estimated utility from paying check (removed)
# noccyesdcml$v_check = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_check + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_check + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_check 
#
noccyesdcml$v_credit = NA # # estimated utility from paying credit
noccyesdcml$v_credit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_credit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_credit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_credit 

noccyesdcml$v_debit = NA # estimated utility from debit 
noccyesdcml$v_debit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_debit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_debit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_debit 
#
noccyesdcml$v_prepaid = NA # estimated utility from paying prepaid
noccyesdcml$v_prepaid = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_prepaid + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_prepaid + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_prepaid 
#
head(noccyesdcml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20) # Credit will not be included in CS below

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
(noccyesdcml_loss_rate_med = median(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_avg = mean(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_q25 = quantile(-1*noccyesdcml$cs_rate, 0.25, na.rm = T))
(noccyesdcml_loss_rate_q75 = quantile(-1*noccyesdcml$cs_rate, 0.75, na.rm = T))
(noccyesdcml_loss_rate_min = min(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_max = max(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_sd = sd(-1*noccyesdcml$cs_rate, na.rm = T))
#head(noccyesdcml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

## Computations of Consumer Surplus (CS) (None banked)
# computing utility of cash, check, credit, debit, and prepaid 
#nonebankedml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
nonebankedml$v_cash = NA # # estimated utility from paying cash
nonebankedml$v_cash = nonebankedml_coef[1]*nonebankedml$assess_cost_cash + nonebankedml_coef[2]*nonebankedml$assess_security_cash + nonebankedml_coef[3]*nonebankedml$assess_convenience_cash 
#
# nonebankedml$v_check = NA # estimated utility from paying check (removed)
# nonebankedml$v_check = nonebankedml_coef[1]*nonebankedml$assess_cost_check + nonebankedml_coef[2]*nonebankedml$assess_security_check + nonebankedml_coef[3]*nonebankedml$assess_convenience_check 
#
nonebankedml$v_credit = NA # # estimated utility from paying credit
nonebankedml$v_credit = nonebankedml_coef[1]*nonebankedml$assess_cost_credit + nonebankedml_coef[2]*nonebankedml$assess_security_credit + nonebankedml_coef[3]*nonebankedml$assess_convenience_credit 

nonebankedml$v_debit = NA # estimated utility from debit 
nonebankedml$v_debit = nonebankedml_coef[1]*nonebankedml$assess_cost_debit + nonebankedml_coef[2]*nonebankedml$assess_security_debit + nonebankedml_coef[3]*nonebankedml$assess_convenience_debit 
#
nonebankedml$v_prepaid = NA # estimated utility from paying prepaid
nonebankedml$v_prepaid = nonebankedml_coef[1]*nonebankedml$assess_cost_prepaid + nonebankedml_coef[2]*nonebankedml$assess_security_prepaid + nonebankedml_coef[3]*nonebankedml$assess_convenience_prepaid 
#
head(nonebankedml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

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
(nonebankedml_loss_rate_med = median(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_avg = mean(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_q25 = quantile(-1*nonebankedml$cs_rate, 0.25, na.rm = T))
(nonebankedml_loss_rate_q75 = quantile(-1*nonebankedml$cs_rate, 0.75, na.rm = T))
(nonebankedml_loss_rate_min = min(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_max = max(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_sd = sd(-1*nonebankedml$cs_rate, na.rm = T))
#head(nonebankedml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

## Computations of Consumer Surplus (CS) (None Unbanked)
# computing utility of cash, check, credit, debit, and prepaid 
#noneunbml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
noneunbml$v_cash = NA # # estimated utility from paying cash
noneunbml$v_cash = noneunbml_coef[1]*noneunbml$assess_cost_cash + noneunbml_coef[2]*noneunbml$assess_security_cash + noneunbml_coef[3]*noneunbml$assess_convenience_cash 
#
# noneunbml$v_check = NA # estimated utility from paying check (removed)
# noneunbml$v_check = noneunbml_coef[1]*noneunbml$assess_cost_check + noneunbml_coef[2]*noneunbml$assess_security_check + noneunbml_coef[3]*noneunbml$assess_convenience_check 
#
noneunbml$v_credit = NA # # estimated utility from paying credit
noneunbml$v_credit = noneunbml_coef[1]*noneunbml$assess_cost_credit + noneunbml_coef[2]*noneunbml$assess_security_credit + noneunbml_coef[3]*noneunbml$assess_convenience_credit 

noneunbml$v_debit = NA # estimated utility from debit 
noneunbml$v_debit = noneunbml_coef[1]*noneunbml$assess_cost_debit + noneunbml_coef[2]*noneunbml$assess_security_debit + noneunbml_coef[3]*noneunbml$assess_convenience_debit 
#
noneunbml$v_prepaid = NA # estimated utility from paying prepaid
noneunbml$v_prepaid = noneunbml_coef[1]*noneunbml$assess_cost_prepaid + noneunbml_coef[2]*noneunbml$assess_security_prepaid + noneunbml_coef[3]*noneunbml$assess_convenience_prepaid 
#
head(noneunbml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

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
(noneunbml_loss_rate_med = median(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_avg = mean(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_q25 = quantile(-1*noneunbml$cs_rate, 0.25, na.rm = T))
(noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
(noneunbml_loss_rate_min = min(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_max = max(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
(noneunbml_loss_rate_sd = sd(-1*noneunbml$cs_rate, na.rm = T))
#head(noneunbml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

# Right columns the Table 4 in paper: CS LOSS table 
(lossinf.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)),  "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "sd_C" = round(c(all_ml_sd[1], both_ml_sd[1], noccyesdc_ml_sd[1], nonebankedml_sd[1], nonebankedml_sd[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "sd_S" = round(c(all_ml_sd[2], both_ml_sd[2], noccyesdc_ml_sd[2], nonebankedml_sd[2], nonebankedml_sd[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "sd_E" = round(c(all_ml_sd[3], both_ml_sd[3], noccyesdc_ml_sd[3], nonebankedml_sd[3], nonebankedml_sd[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "Median" = percent(c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), digits = 1), "Mean" = percent(c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), digits = 1), "Min" = percent(c(allml2_loss_rate_min, bothml_loss_rate_min, noccyesdcml_loss_rate_min, nonebankedml_loss_rate_min, noneunbml_loss_rate_min), digits = 1), "Max" = percent(c(allml2_loss_rate_max, bothml_loss_rate_max, noccyesdcml_loss_rate_max, nonebankedml_loss_rate_max, noneunbml_loss_rate_max), digits = 1) ))

# Above table in 100*fraction format (needed for xtable, x for xtable)
# also q25 and q75 repalce min and max
(lossinfx.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)),  "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "sd_C" = round(c(all_ml_sd[1], both_ml_sd[1], noccyesdc_ml_sd[1], nonebankedml_sd[1], nonebankedml_sd[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "sd_S" = round(c(all_ml_sd[2], both_ml_sd[2], noccyesdc_ml_sd[2], nonebankedml_sd[2], nonebankedml_sd[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "sd_E" = round(c(all_ml_sd[3], both_ml_sd[3], noccyesdc_ml_sd[3], nonebankedml_sd[3], nonebankedml_sd[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "q25" = 100*c(allml2_loss_rate_q25, bothml_loss_rate_q25, noccyesdcml_loss_rate_q25, nonebankedml_loss_rate_q25, noneunbml_loss_rate_q25), "Median" = 100*c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), "Mean" = 100*c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), "q75" = 100*c(allml2_loss_rate_q75, bothml_loss_rate_q75, noccyesdcml_loss_rate_q75, nonebankedml_loss_rate_q75, noneunbml_loss_rate_q75) ))

names(lossinfx.df)# top part of Table 4 (no random effects)
# multinomial and CS w/o random effects starts Line 329 ends Line 800 Mixed logit started thereafter

### Redo regressions w.r.t. median assessments (instead of average) redo from line 1120
names(inst7)# transactions

# Need to find avg and med of each assessment by averaging respondents (not by payments). Note: this averaging is over all respondents (not by subgroups)
length(unique(inst7$id))# num resp
inst7_resp = inst7[!duplicated(inst7$id), ]
dim(inst7_resp)
#
names(inst7_resp)
# avg cost assessment by PI
(assess_cost_cash_avg = mean(inst7_resp$assess_cost_cash, na.rm = T))
(assess_cost_check_avg = mean(inst7_resp$assess_cost_check, na.rm = T))
(assess_cost_credit_avg = mean(inst7_resp$assess_cost_credit, na.rm = T))
(assess_cost_debit_avg = mean(inst7_resp$assess_cost_debit, na.rm = T))
(assess_cost_prepaid_avg = mean(inst7_resp$assess_cost_prepaid, na.rm = T))
# median cost assessment by PI
(assess_cost_cash_med = median(inst7_resp$assess_cost_cash, na.rm = T))
(assess_cost_check_med = median(inst7_resp$assess_cost_check, na.rm = T))
(assess_cost_credit_med = median(inst7_resp$assess_cost_credit, na.rm = T))
(assess_cost_debit_med = median(inst7_resp$assess_cost_debit, na.rm = T))
(assess_cost_prepaid_med = median(inst7_resp$assess_cost_prepaid, na.rm = T))
#
# avg security assessment by PI
(assess_security_cash_avg = mean(inst7_resp$assess_security_cash, na.rm = T))
(assess_security_check_avg = mean(inst7_resp$assess_security_check, na.rm = T))
(assess_security_credit_avg = mean(inst7_resp$assess_security_credit, na.rm = T))
(assess_security_debit_avg = mean(inst7_resp$assess_security_debit, na.rm = T))
(assess_security_prepaid_avg = mean(inst7_resp$assess_security_prepaid, na.rm = T))
# median security assessment by PI
(assess_security_cash_med = median(inst7_resp$assess_security_cash, na.rm = T))
(assess_security_check_med = median(inst7_resp$assess_security_check, na.rm = T))
(assess_security_credit_med = median(inst7_resp$assess_security_credit, na.rm = T))
(assess_security_debit_med = median(inst7_resp$assess_security_debit, na.rm = T))
(assess_security_prepaid_med = median(inst7_resp$assess_security_prepaid, na.rm = T))
#
# avg convenience assessment by PI
(assess_convenience_cash_avg = mean(inst7_resp$assess_convenience_cash, na.rm = T))
(assess_convenience_check_avg = mean(inst7_resp$assess_convenience_check, na.rm = T))
(assess_convenience_credit_avg = mean(inst7_resp$assess_convenience_credit, na.rm = T))
(assess_convenience_debit_avg = mean(inst7_resp$assess_convenience_debit, na.rm = T))
(assess_convenience_prepaid_avg = mean(inst7_resp$assess_convenience_prepaid, na.rm = T))
# median convenience assessment by PI
(assess_convenience_cash_med = median(inst7_resp$assess_convenience_cash, na.rm = T))
(assess_convenience_check_med = median(inst7_resp$assess_convenience_check, na.rm = T))
(assess_convenience_credit_med = median(inst7_resp$assess_convenience_credit, na.rm = T))
(assess_convenience_debit_med = median(inst7_resp$assess_convenience_debit, na.rm = T))
(assess_convenience_prepaid_med = median(inst7_resp$assess_convenience_prepaid, na.rm = T))
#
# avg acceptance assessment by PI
(assess_acceptance_cash_avg = mean(inst7_resp$assess_acceptance_cash, na.rm = T))
(assess_acceptance_check_avg = mean(inst7_resp$assess_acceptance_check, na.rm = T))
(assess_acceptance_credit_avg = mean(inst7_resp$assess_acceptance_credit, na.rm = T))
(assess_acceptance_debit_avg = mean(inst7_resp$assess_acceptance_debit, na.rm = T))
(assess_acceptance_prepaid_avg = mean(inst7_resp$assess_acceptance_prepaid, na.rm = T))
# median acceptance assessment by PI
(assess_acceptance_cash_med = median(inst7_resp$assess_acceptance_cash, na.rm = T))
(assess_acceptance_check_med = median(inst7_resp$assess_acceptance_check, na.rm = T))
(assess_acceptance_credit_med = median(inst7_resp$assess_acceptance_credit, na.rm = T))
(assess_acceptance_debit_med = median(inst7_resp$assess_acceptance_debit, na.rm = T))
(assess_acceptance_prepaid_med = median(inst7_resp$assess_acceptance_prepaid, na.rm = T))
#
# avg setup assessment by PI
(assess_setup_cash_avg = mean(inst7_resp$assess_setup_cash, na.rm = T))
(assess_setup_check_avg = mean(inst7_resp$assess_setup_check, na.rm = T))
(assess_setup_credit_avg = mean(inst7_resp$assess_setup_credit, na.rm = T))
(assess_setup_debit_avg = mean(inst7_resp$assess_setup_debit, na.rm = T))
(assess_setup_prepaid_avg = mean(inst7_resp$assess_setup_prepaid, na.rm = T))
# median setup assessment by PI
(assess_setup_cash_med = median(inst7_resp$assess_setup_cash, na.rm = T))
(assess_setup_check_med = median(inst7_resp$assess_setup_check, na.rm = T))
(assess_setup_credit_med = median(inst7_resp$assess_setup_credit, na.rm = T))
(assess_setup_debit_med = median(inst7_resp$assess_setup_debit, na.rm = T))
(assess_setup_prepaid_med = median(inst7_resp$assess_setup_prepaid, na.rm = T))
#
# avg record assessment by PI
(assess_record_cash_avg = mean(inst7_resp$assess_record_cash, na.rm = T))
(assess_record_check_avg = mean(inst7_resp$assess_record_check, na.rm = T))
(assess_record_credit_avg = mean(inst7_resp$assess_record_credit, na.rm = T))
(assess_record_debit_avg = mean(inst7_resp$assess_record_debit, na.rm = T))
(assess_record_prepaid_avg = mean(inst7_resp$assess_record_prepaid, na.rm = T))
# median record assessment by PI
(assess_record_cash_med = median(inst7_resp$assess_record_cash, na.rm = T))
(assess_record_check_med = median(inst7_resp$assess_record_check, na.rm = T))
(assess_record_credit_med = median(inst7_resp$assess_record_credit, na.rm = T))
(assess_record_debit_med = median(inst7_resp$assess_record_debit, na.rm = T))
(assess_record_prepaid_med = median(inst7_resp$assess_record_prepaid, na.rm = T))

# 
### From line 333 regression All data but with med assessements
allml =  subset(inst7, select = c(id, pi, amnt, income, assess_cost_cash, assess_cost_check, assess_cost_credit, assess_cost_debit, assess_cost_prepaid, assess_security_cash, assess_security_check, assess_security_credit, assess_security_debit, assess_security_prepaid, assess_convenience_cash, assess_convenience_check, assess_convenience_credit, assess_convenience_debit, assess_convenience_prepaid, cc_adopt, dc_adopt, svc_adopt, bnk_acnt_adopt )) 
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
## mlogit on All payments
dim(allml)
allml2 = na.omit(allml) # subsample w/ both cards
dim(allml2)
#
# now make it mlogit data (all payments)
allml2_data = mlogit.data(allml2, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(allml2_data)
head(allml2_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) ### Now, Median cost
allml2_data$cost = NA
allml2_data[allml2_data$alt=="cash", ]$cost = assess_cost_cash_med
# allml2_data[allml2_data$alt=="check", ]$cost = allml2_data[allml2_data$alt=="check", ]$assess_cost_check
allml2_data[allml2_data$alt=="debit", ]$cost = assess_cost_debit_med
allml2_data[allml2_data$alt=="credit", ]$cost = assess_cost_credit_med
allml2_data[allml2_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI) ### Now, median
allml2_data$security = NA
allml2_data[allml2_data$alt=="cash", ]$security = assess_security_cash_med
# allml2_data[allml2_data$alt=="check", ]$security = allml2_data[allml2_data$alt=="check", ]$assess_security_check
allml2_data[allml2_data$alt=="debit", ]$security = assess_security_debit_med
allml2_data[allml2_data$alt=="credit", ]$security = assess_security_credit_med
allml2_data[allml2_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
allml2_data$convenience = NA
allml2_data[allml2_data$alt=="cash", ]$convenience = assess_convenience_cash_med
# allml2_data[allml2_data$alt=="check", ]$convenience = allml2_data[allml2_data$alt=="check", ]$assess_convenience_check
allml2_data[allml2_data$alt=="debit", ]$convenience = assess_convenience_debit_med
allml2_data[allml2_data$alt=="credit", ]$convenience = assess_convenience_credit_med
allml2_data[allml2_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
# add a column "acceptance" corresponding to respondent's assessment of each choice (including NOT chosen PI) ### Now, median 
allml2_data$acceptance = NA
allml2_data[allml2_data$alt=="cash", ]$acceptance = assess_acceptance_cash_med
# allml2_data[allml2_data$alt=="check", ]$acceptance = allml2_data[allml2_data$alt=="check", ]$assess_acceptance_check
allml2_data[allml2_data$alt=="debit", ]$acceptance = assess_acceptance_debit_med
allml2_data[allml2_data$alt=="credit", ]$acceptance = assess_acceptance_credit_med
allml2_data[allml2_data$alt=="prepaid", ]$acceptance = assess_acceptance_prepaid_med
# add a column "setup" corresponding to respondent's assessment of each choice (including NOT chosen PI) ### Now, average cost
allml2_data$setup = NA
allml2_data[allml2_data$alt=="cash", ]$setup = assess_setup_cash_med
# allml2_data[allml2_data$alt=="check", ]$setup = allml2_data[allml2_data$alt=="check", ]$assess_setup_check
allml2_data[allml2_data$alt=="debit", ]$setup = assess_setup_debit_med
allml2_data[allml2_data$alt=="credit", ]$setup = assess_setup_credit_med
allml2_data[allml2_data$alt=="prepaid", ]$setup = assess_setup_prepaid_med
# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI) ### Now, average cost
allml2_data$record = NA
allml2_data[allml2_data$alt=="cash", ]$record = assess_record_cash_med
# allml2_data[allml2_data$alt=="check", ]$record = allml2_data[allml2_data$alt=="check", ]$assess_record_check
allml2_data[allml2_data$alt=="debit", ]$record = assess_record_debit_med
allml2_data[allml2_data$alt=="credit", ]$record = assess_record_credit_med
allml2_data[allml2_data$alt=="prepaid", ]$record = assess_record_prepaid_med
#
# model to be estimated (All)
(allml2model = pi ~ cost + security + convenience | -1)  # w/o adoption
all_ml = mlogit(allml2model, data = allml2_data)
summary(all_ml)
(all_ml_coef = as.vector(all_ml$coefficients[1:3])) # cost, security, convenience
(all_ml_sd = coef(summary(all_ml))[,2]) # standard errors
(all_ml_sd = as.vector(all_ml_sd)) # standard errors vector
(all_ml_pvalue = coef(summary(all_ml))[,4]) # extract p-value
(all_ml_pvalue = as.vector(all_ml_pvalue)) # p-values vector
(all_ml_sig = as.vector(symnum(all_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
#

# from line 394
## mlogit on both cards using median assessments of all resp
dim(allml)
bothml = na.omit(subset(allml, cc_adopt==1 & dc_adopt==1)) # subsample w/ both cards
dim(bothml)
#
# now make it mlogit data (both)
bothml_data = mlogit.data(bothml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(bothml_data)
head(bothml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
bothml_data$cost = NA
bothml_data[bothml_data$alt=="cash", ]$cost = assess_cost_cash_med
# bothml_data[bothml_data$alt=="check", ]$cost = bothml_data[bothml_data$alt=="check", ]$assess_cost_check
bothml_data[bothml_data$alt=="debit", ]$cost = assess_cost_debit_med
bothml_data[bothml_data$alt=="credit", ]$cost = assess_cost_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$security = NA
bothml_data[bothml_data$alt=="cash", ]$security = assess_security_cash_med
# bothml_data[bothml_data$alt=="check", ]$security = bothml_data[bothml_data$alt=="check", ]$assess_security_check
bothml_data[bothml_data$alt=="debit", ]$security = assess_security_debit_med
bothml_data[bothml_data$alt=="credit", ]$security = assess_security_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$convenience = NA
bothml_data[bothml_data$alt=="cash", ]$convenience = assess_convenience_cash_med
# bothml_data[bothml_data$alt=="check", ]$convenience = bothml_data[bothml_data$alt=="check", ]$assess_convenience_check
bothml_data[bothml_data$alt=="debit", ]$convenience = assess_convenience_debit_med
bothml_data[bothml_data$alt=="credit", ]$convenience = assess_convenience_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
# add a column "acceptance" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$acceptance = NA
bothml_data[bothml_data$alt=="cash", ]$acceptance = assess_acceptance_cash_med
# bothml_data[bothml_data$alt=="check", ]$acceptance = bothml_data[bothml_data$alt=="check", ]$assess_acceptance_check
bothml_data[bothml_data$alt=="debit", ]$acceptance = assess_acceptance_debit_med
bothml_data[bothml_data$alt=="credit", ]$acceptance = assess_acceptance_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$acceptance = assess_acceptance_prepaid_med# add a column "setup" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$setup = NA
bothml_data[bothml_data$alt=="cash", ]$setup = assess_setup_cash_med
# bothml_data[bothml_data$alt=="check", ]$setup = bothml_data[bothml_data$alt=="check", ]$assess_setup_check
bothml_data[bothml_data$alt=="debit", ]$setup = assess_setup_debit_med
bothml_data[bothml_data$alt=="credit", ]$setup = assess_setup_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$setup = assess_setup_prepaid_med
# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI)
bothml_data$record = NA
bothml_data[bothml_data$alt=="cash", ]$record = assess_record_cash_med
# bothml_data[bothml_data$alt=="check", ]$record = bothml_data[bothml_data$alt=="check", ]$assess_record_check
bothml_data[bothml_data$alt=="debit", ]$record = assess_record_debit_med
bothml_data[bothml_data$alt=="credit", ]$record = assess_record_credit_med
bothml_data[bothml_data$alt=="prepaid", ]$record = assess_record_prepaid_med

#
# model to be estimated (both)
(bothmlmodel = pi ~ cost + security + convenience | -1)  # w/o adoption
both_ml = mlogit(bothmlmodel, data = bothml_data)
summary(both_ml)
(both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
(both_ml_sd = coef(summary(all_ml))[,2]) # standard errors
(both_ml_sd = as.vector(all_ml_sd)) # standard errors vector
(both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
(both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
(both_ml_sig = as.vector( symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
#

## from line 438
## mlogit on no_cc & yes_dc
dim(allml)
noccyesdcml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==1 & pi != "credit" ))
dim(noccyesdcml)
#
# now make it mlogit data (no cc & yes dc)
noccyesdcml_data = mlogit.data(noccyesdcml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(noccyesdcml_data)
head(noccyesdcml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
noccyesdcml_data$cost = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$cost = assess_cost_cash_med
# noccyesdcml_data[noccyesdcml_data$alt=="check", ]$cost = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_cost_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$cost = assess_cost_debit_med
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$cost = assess_cost_credit_med
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noccyesdcml_data$security = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$security = assess_security_cash_med
# noccyesdcml_data[noccyesdcml_data$alt=="check", ]$security = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_security_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$security = assess_security_debit_med
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$security = assess_security_credit_med
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noccyesdcml_data$convenience = NA
noccyesdcml_data[noccyesdcml_data$alt=="cash", ]$convenience = assess_convenience_cash_med
#noccyesdcml_data[noccyesdcml_data$alt=="check", ]$convenience = noccyesdcml_data[noccyesdcml_data$alt=="check", ]$assess_convenience_check
noccyesdcml_data[noccyesdcml_data$alt=="debit", ]$convenience = assess_convenience_debit_med
noccyesdcml_data[noccyesdcml_data$alt=="credit", ]$convenience = assess_convenience_credit_med
noccyesdcml_data[noccyesdcml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
#
# model to be estimated (noccyesdc)
noccyesdcmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
noccyesdc_ml = mlogit(noccyesdcmlmodel, alt.subset = c("cash", "debit", "prepaid"),  data = noccyesdcml_data)
summary(noccyesdc_ml)
(noccyesdc_ml_coef = as.vector(noccyesdc_ml$coefficients[1:3])) # cost, security, convenience
(noccyesdc_ml_sd = coef(summary(noccyesdc_ml))[,2]) # starndard errors
(noccyesdc_ml_sd = as.vector(noccyesdc_ml_sd)) # standard errors vector
(noccyesdc_ml_pvalue = coef(summary(noccyesdc_ml))[,4]) # extract p-value
(noccyesdc_ml_pvalue = as.vector(noccyesdc_ml_pvalue)) # p-values vector
(noccyesdc_ml_sig = as.vector(symnum(noccyesdc_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## from line 481
## mlogit on none_banked
dim(allml)
nonebankedml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==1 & pi != "credit" & pi != "debit"))
dim(nonebankedml)
#
# now make it mlogit data (no cc & yes dc)
nonebankedml_data = mlogit.data(nonebankedml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(nonebankedml_data)
head(nonebankedml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
nonebankedml_data$cost = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$cost = assess_cost_cash_med
# nonebankedml_data[nonebankedml_data$alt=="check", ]$cost = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_cost_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$cost = assess_cost_debit_med
nonebankedml_data[nonebankedml_data$alt=="credit", ]$cost = assess_cost_credit_med
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$security = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$security = assess_security_cash_med
# nonebankedml_data[nonebankedml_data$alt=="check", ]$security = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_security_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$security = assess_security_debit_med
nonebankedml_data[nonebankedml_data$alt=="credit", ]$security = assess_security_credit_med
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$convenience = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$convenience = assess_convenience_cash_med
#nonebankedml_data[nonebankedml_data$alt=="check", ]$convenience = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_convenience_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$convenience = assess_convenience_debit_med
nonebankedml_data[nonebankedml_data$alt=="credit", ]$convenience = assess_convenience_credit_med
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
# add a column "acceptance" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$acceptance = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$acceptance = assess_acceptance_cash_med
# nonebankedml_data[nonebankedml_data$alt=="check", ]$acceptance = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_acceptance_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$acceptance = assess_acceptance_debit_med
nonebankedml_data[nonebankedml_data$alt=="credit", ]$acceptance = assess_acceptance_credit_med
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$acceptance = assess_acceptance_prepaid_med# add a column "setup" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$setup = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$setup = assess_setup_cash_med
# nonebankedml_data[nonebankedml_data$alt=="check", ]$setup = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_setup_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$setup = assess_setup_debit_med
nonebankedml_data[nonebankedml_data$alt=="credit", ]$setup = assess_setup_credit_med
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$setup = assess_setup_prepaid_med# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI)
nonebankedml_data$record = NA
nonebankedml_data[nonebankedml_data$alt=="cash", ]$record = assess_record_cash_med
# nonebankedml_data[nonebankedml_data$alt=="check", ]$record = nonebankedml_data[nonebankedml_data$alt=="check", ]$assess_record_check
nonebankedml_data[nonebankedml_data$alt=="debit", ]$record = assess_record_debit_med
nonebankedml_data[nonebankedml_data$alt=="credit", ]$record = assess_record_credit_med
nonebankedml_data[nonebankedml_data$alt=="prepaid", ]$record = assess_record_prepaid_med
#
# model to be estimated (nonebanked)
nonebankedmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
nonebanked_ml = mlogit(nonebankedmlmodel, alt.subset = c("cash", "debit", "prepaid"),  data = nonebankedml_data)
summary(nonebanked_ml)
(nonebankedml_coef = as.vector(nonebanked_ml$coefficients[1:3])) # cost, security, convenience
(nonebankedml_sd = coef(summary(nonebanked_ml))[,2]) # standard errors
(nonebankedml_sd = as.vector(nonebankedml_sd)) # standard errors vect
(nonebankedml_pvalue = coef(summary(nonebanked_ml))[,4]) # extract p-value
(nonebankedml_pvalue = as.vector(nonebankedml_pvalue)) # p-values vector
(nonebankedml_sig = as.vector(symnum(nonebankedml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## from line 525
## mlogit on none_unb (unbanked)
dim(allml)
noneunbml = na.omit(subset(allml, cc_adopt==0 & dc_adopt==0 & bnk_acnt_adopt==0 & pi != "credit" & pi != "debit"))
dim(noneunbml)
#
# now make it mlogit data (no cc & yes dc)
noneunbml_data = mlogit.data(noneunbml, choice = "pi", shape = "wide", id.var = "id", drop.index = F)
names(noneunbml_data)
head(noneunbml_data)
#
# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
noneunbml_data$cost = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$cost = assess_cost_cash_med
# noneunbml_data[noneunbml_data$alt=="check", ]$cost = noneunbml_data[noneunbml_data$alt=="check", ]$assess_cost_check
noneunbml_data[noneunbml_data$alt=="debit", ]$cost = assess_cost_debit_med
noneunbml_data[noneunbml_data$alt=="credit", ]$cost = assess_cost_credit_med
noneunbml_data[noneunbml_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$security = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$security = assess_security_cash_med
# noneunbml_data[noneunbml_data$alt=="check", ]$security = noneunbml_data[noneunbml_data$alt=="check", ]$assess_security_check
noneunbml_data[noneunbml_data$alt=="debit", ]$security = assess_security_debit_med
noneunbml_data[noneunbml_data$alt=="credit", ]$security = assess_security_credit_med
noneunbml_data[noneunbml_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$convenience = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$convenience = assess_convenience_cash_med
#noneunbml_data[noneunbml_data$alt=="check", ]$convenience = noneunbml_data[noneunbml_data$alt=="check", ]$assess_convenience_check
noneunbml_data[noneunbml_data$alt=="debit", ]$convenience = assess_convenience_debit_med
noneunbml_data[noneunbml_data$alt=="credit", ]$convenience = assess_convenience_credit_med
noneunbml_data[noneunbml_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
# add a column "acceptance" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$acceptance = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$acceptance = assess_acceptance_cash_med
# noneunbml_data[noneunbml_data$alt=="check", ]$acceptance = noneunbml_data[noneunbml_data$alt=="check", ]$assess_acceptance_check
noneunbml_data[noneunbml_data$alt=="debit", ]$acceptance = assess_acceptance_debit_med
noneunbml_data[noneunbml_data$alt=="credit", ]$acceptance = assess_acceptance_credit_med
noneunbml_data[noneunbml_data$alt=="prepaid", ]$acceptance = assess_acceptance_prepaid_med# add a column "setup" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$setup = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$setup = assess_setup_cash_med
# noneunbml_data[noneunbml_data$alt=="check", ]$setup = noneunbml_data[noneunbml_data$alt=="check", ]$assess_setup_check
noneunbml_data[noneunbml_data$alt=="debit", ]$setup = assess_setup_debit_med
noneunbml_data[noneunbml_data$alt=="credit", ]$setup = assess_setup_credit_med
noneunbml_data[noneunbml_data$alt=="prepaid", ]$setup = assess_setup_prepaid_med# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI)
noneunbml_data$record = NA
noneunbml_data[noneunbml_data$alt=="cash", ]$record = assess_record_cash_med
# noneunbml_data[noneunbml_data$alt=="check", ]$record = noneunbml_data[noneunbml_data$alt=="check", ]$assess_record_check
noneunbml_data[noneunbml_data$alt=="debit", ]$record = assess_record_debit_med
noneunbml_data[noneunbml_data$alt=="credit", ]$record = assess_record_credit_med
noneunbml_data[noneunbml_data$alt=="prepaid", ]$record = assess_record_prepaid_med

#
# model to be estimated (none UNbanked)
noneunbmlmodel = mFormula( pi ~ cost + security + convenience | -1 ) # w/o adoption
noneunb_ml = mlogit(noneunbmlmodel, alt.subset = c("cash",  "prepaid"),  data = noneunbml_data)
summary(noneunb_ml)
(noneunbml_coef = as.vector(noneunb_ml$coefficients[1:3])) # cost, security, convenience
(noneunbml_sd = coef(summary(noneunb_ml))[,2]) # standard errors
(noneunbml_sd = as.vector(noneunbml_sd)) # standard error vector
(noneunbml_pvalue = coef(summary(noneunb_ml))[,4]) # extract p-value
(noneunbml_pvalue = as.vector(noneunbml_pvalue)) # p-values vector
(noneunbml_sig = as.vector(symnum(noneunbml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

## From line 615 to line 811: Computing CS and bottom Table 4 for avg assessments estimations
## Computations of Consumer Surplus (CS) (Both)
# computing utility of cash, check, credit, debit, and prepaid 
#bothml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
bothml$v_cash = NA # # estimated utility from paying cash
bothml$v_cash = both_ml_coef[1]*bothml$assess_cost_cash + both_ml_coef[2]*bothml$assess_security_cash + both_ml_coef[3]*bothml$assess_convenience_cash 
#
# bothml$v_check = NA # estimated utility from paying check (removed)
# bothml$v_check = both_ml_coef[1]*bothml$assess_cost_check + both_ml_coef[2]*bothml$assess_security_check + both_ml_coef[3]*bothml$assess_convenience_check 
#
bothml$v_credit = NA # # estimated utility from paying credit
bothml$v_credit = both_ml_coef[1]*bothml$assess_cost_credit + both_ml_coef[2]*bothml$assess_security_credit + both_ml_coef[3]*bothml$assess_convenience_credit 

bothml$v_debit = NA # estimated utility from debit 
bothml$v_debit = both_ml_coef[1]*bothml$assess_cost_debit + both_ml_coef[2]*bothml$assess_security_debit + both_ml_coef[3]*bothml$assess_convenience_debit 
#
bothml$v_prepaid = NA # estimated utility from paying prepaid
bothml$v_prepaid = both_ml_coef[1]*bothml$assess_cost_prepaid + both_ml_coef[2]*bothml$assess_security_prepaid + both_ml_coef[3]*bothml$assess_convenience_prepaid 
#
head(bothml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

# Both: compute CS *before* the transition to cashless, based equation (3.10) on page 55 Train's book 
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
(bothml_loss_rate_med = median(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_avg = mean(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_q25 = quantile(-1*bothml$cs_rate, 0.25, na.rm = T))
(bothml_loss_rate_q75 = quantile(-1*bothml$cs_rate, 0.75, na.rm = T))
(bothml_loss_rate_min = min(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_max = max(-1*bothml$cs_rate, na.rm = T))
(bothml_loss_rate_sd = sd(-1*bothml$cs_rate, na.rm = T))
#head(bothml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])
#

## Computations of Consumer Surplus (CS) (noccyesdc, no cc yes dc)
# computing utility of cash, check, credit, debit, and prepaid 
#noccyesdcml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
noccyesdcml$v_cash = NA # # estimated utility from paying cash
noccyesdcml$v_cash = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_cash + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_cash + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_cash 

#
# noccyesdcml$v_check = NA # estimated utility from paying check (removed)
# noccyesdcml$v_check = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_check + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_check + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_check 
#
noccyesdcml$v_credit = NA # # estimated utility from paying credit
noccyesdcml$v_credit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_credit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_credit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_credit 

noccyesdcml$v_debit = NA # estimated utility from debit 
noccyesdcml$v_debit = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_debit + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_debit + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_debit 
#
noccyesdcml$v_prepaid = NA # estimated utility from paying prepaid
noccyesdcml$v_prepaid = noccyesdc_ml_coef[1]*noccyesdcml$assess_cost_prepaid + noccyesdc_ml_coef[2]*noccyesdcml$assess_security_prepaid + noccyesdc_ml_coef[3]*noccyesdcml$assess_convenience_prepaid 
#
head(noccyesdcml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20) # Credit will not be included in CS below

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
(noccyesdcml_loss_rate_med = median(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_avg = mean(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_q25 = quantile(-1*noccyesdcml$cs_rate, 0.25, na.rm = T))
(noccyesdcml_loss_rate_q75 = quantile(-1*noccyesdcml$cs_rate, 0.75, na.rm = T))
(noccyesdcml_loss_rate_min = min(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_max = max(-1*noccyesdcml$cs_rate, na.rm = T))
(noccyesdcml_loss_rate_sd = sd(-1*noccyesdcml$cs_rate, na.rm = T))
#head(noccyesdcml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

## Computations of Consumer Surplus (CS) (None banked)
# computing utility of cash, check, credit, debit, and prepaid 
#nonebankedml$v_cash = 0 # estimated utility from paying cash = 0 (reference category CHANGED 191015)
nonebankedml$v_cash = NA # # estimated utility from paying cash
nonebankedml$v_cash = nonebankedml_coef[1]*nonebankedml$assess_cost_cash + nonebankedml_coef[2]*nonebankedml$assess_security_cash + nonebankedml_coef[3]*nonebankedml$assess_convenience_cash 
#
# nonebankedml$v_check = NA # estimated utility from paying check (removed)
# nonebankedml$v_check = nonebankedml_coef[1]*nonebankedml$assess_cost_check + nonebankedml_coef[2]*nonebankedml$assess_security_check + nonebankedml_coef[3]*nonebankedml$assess_convenience_check 
#
nonebankedml$v_credit = NA # # estimated utility from paying credit
nonebankedml$v_credit = nonebankedml_coef[1]*nonebankedml$assess_cost_credit + nonebankedml_coef[2]*nonebankedml$assess_security_credit + nonebankedml_coef[3]*nonebankedml$assess_convenience_credit 

nonebankedml$v_debit = NA # estimated utility from debit 
nonebankedml$v_debit = nonebankedml_coef[1]*nonebankedml$assess_cost_debit + nonebankedml_coef[2]*nonebankedml$assess_security_debit + nonebankedml_coef[3]*nonebankedml$assess_convenience_debit 
#
nonebankedml$v_prepaid = NA # estimated utility from paying prepaid
nonebankedml$v_prepaid = nonebankedml_coef[1]*nonebankedml$assess_cost_prepaid + nonebankedml_coef[2]*nonebankedml$assess_security_prepaid + nonebankedml_coef[3]*nonebankedml$assess_convenience_prepaid 
#
head(nonebankedml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

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
(nonebankedml_loss_rate_med = median(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_avg = mean(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_q25 = quantile(-1*nonebankedml$cs_rate, 0.25, na.rm = T))
(nonebankedml_loss_rate_q75 = quantile(-1*nonebankedml$cs_rate, 0.75, na.rm = T))
(nonebankedml_loss_rate_min = min(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_max = max(-1*nonebankedml$cs_rate, na.rm = T))
(nonebankedml_loss_rate_sd = sd(-1*nonebankedml$cs_rate, na.rm = T))
#head(nonebankedml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

## Computations of Consumer Surplus (CS) (None Unbanked)
# computing utility of cash, check, credit, debit, and prepaid 
#noneunbml$v_cash = 0 # estimated utility from paying cash = 0 (reference category, CHANGED 191015)
noneunbml$v_cash = NA # # estimated utility from paying cash
noneunbml$v_cash = noneunbml_coef[1]*noneunbml$assess_cost_cash + noneunbml_coef[2]*noneunbml$assess_security_cash + noneunbml_coef[3]*noneunbml$assess_convenience_cash 
#
# noneunbml$v_check = NA # estimated utility from paying check (removed)
# noneunbml$v_check = noneunbml_coef[1]*noneunbml$assess_cost_check + noneunbml_coef[2]*noneunbml$assess_security_check + noneunbml_coef[3]*noneunbml$assess_convenience_check 
#
noneunbml$v_credit = NA # # estimated utility from paying credit
noneunbml$v_credit = noneunbml_coef[1]*noneunbml$assess_cost_credit + noneunbml_coef[2]*noneunbml$assess_security_credit + noneunbml_coef[3]*noneunbml$assess_convenience_credit 

noneunbml$v_debit = NA # estimated utility from debit 
noneunbml$v_debit = noneunbml_coef[1]*noneunbml$assess_cost_debit + noneunbml_coef[2]*noneunbml$assess_security_debit + noneunbml_coef[3]*noneunbml$assess_convenience_debit 
#
noneunbml$v_prepaid = NA # estimated utility from paying prepaid
noneunbml$v_prepaid = noneunbml_coef[1]*noneunbml$assess_cost_prepaid + noneunbml_coef[2]*noneunbml$assess_security_prepaid + noneunbml_coef[3]*noneunbml$assess_convenience_prepaid 
#
head(noneunbml[, c("id", "pi", "v_cash", "v_credit", "v_debit", "v_prepaid")], 20)

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
(noneunbml_loss_rate_med = median(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_avg = mean(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_q25 = quantile(-1*noneunbml$cs_rate, 0.25, na.rm = T))
(noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
(noneunbml_loss_rate_min = min(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_max = max(-1*noneunbml$cs_rate, na.rm = T))
(noneunbml_loss_rate_q75 = quantile(-1*noneunbml$cs_rate, 0.75, na.rm = T))
(noneunbml_loss_rate_sd = sd(-1*noneunbml$cs_rate, na.rm = T))
#head(noneunbml[, c("pi", "cs", "cs_cashless", "cs_diff", "cs_rate")])

# Right columns the Table 4 in paper: CS LOSS table 
(lossinf.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)),  "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "sd_C" = round(c(all_ml_sd[1], both_ml_sd[1], noccyesdc_ml_sd[1], nonebankedml_sd[1], nonebankedml_sd[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "sd_S" = round(c(all_ml_sd[2], both_ml_sd[2], noccyesdc_ml_sd[2], nonebankedml_sd[2], nonebankedml_sd[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "sd_E" = round(c(all_ml_sd[3], both_ml_sd[3], noccyesdc_ml_sd[3], nonebankedml_sd[3], nonebankedml_sd[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "Median" = percent(c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), digits = 1), "Mean" = percent(c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), digits = 1), "Min" = percent(c(allml2_loss_rate_min, bothml_loss_rate_min, noccyesdcml_loss_rate_min, nonebankedml_loss_rate_min, noneunbml_loss_rate_min), digits = 1), "Max" = percent(c(allml2_loss_rate_max, bothml_loss_rate_max, noccyesdcml_loss_rate_max, nonebankedml_loss_rate_max, noneunbml_loss_rate_max), digits = 1) ))

# Above table in 100*fraction format (needed for xtable, x for xtable)
# also q25 and q75 repalce min and max
(lossinfx.df = data.frame("Sample" = c("All" ,"Both_cards", "No_CC_Yes_DC", "None_banked", "None_unbanked"), "Size" = c(nrow(allml2), nrow(bothml), nrow(noccyesdcml), nrow(nonebankedml), nrow(noneunbml)),  "beta_C" = round(c(all_ml_coef[1], both_ml_coef[1], noccyesdc_ml_coef[1], nonebankedml_coef[1], noneunbml_coef[1]), digits = 3), "sd_C" = round(c(all_ml_sd[1], both_ml_sd[1], noccyesdc_ml_sd[1], nonebankedml_sd[1], nonebankedml_sd[1]), digits = 3), "Sig_C" = c(all_ml_sig[1], both_ml_sig[1], noccyesdc_ml_sig[1], nonebankedml_sig[1], noneunbml_sig[1]), "beta_S" = round(c(all_ml_coef[2], both_ml_coef[2], noccyesdc_ml_coef[2], nonebankedml_coef[2], noneunbml_coef[2]), digits = 3), "sd_S" = round(c(all_ml_sd[2], both_ml_sd[2], noccyesdc_ml_sd[2], nonebankedml_sd[2], nonebankedml_sd[2]), digits = 3), "Sig_S" = c(all_ml_sig[2], both_ml_sig[2], noccyesdc_ml_sig[2], nonebankedml_sig[2], noneunbml_sig[2]), "beta_E" = round(c(all_ml_coef[3], both_ml_coef[3], noccyesdc_ml_coef[3], nonebankedml_coef[3], noneunbml_coef[3]), digits = 3), "sd_E" = round(c(all_ml_sd[3], both_ml_sd[3], noccyesdc_ml_sd[3], nonebankedml_sd[3], nonebankedml_sd[3]), digits = 3), "Sig_E" = c(all_ml_sig[3], both_ml_sig[3], noccyesdc_ml_sig[3], nonebankedml_sig[3], noneunbml_sig[3]), "q25" = 100*c(allml2_loss_rate_q25, bothml_loss_rate_q25, noccyesdcml_loss_rate_q25, nonebankedml_loss_rate_q25, noneunbml_loss_rate_q25), "Median" = 100*c(allml2_loss_rate_med, bothml_loss_rate_med, noccyesdcml_loss_rate_med, nonebankedml_loss_rate_med, noneunbml_loss_rate_med), "Mean" = 100*c(allml2_loss_rate_avg, bothml_loss_rate_avg, noccyesdcml_loss_rate_avg, nonebankedml_loss_rate_avg, noneunbml_loss_rate_avg), "q75" = 100*c(allml2_loss_rate_q75, bothml_loss_rate_q75, noccyesdcml_loss_rate_q75, nonebankedml_loss_rate_q75, noneunbml_loss_rate_q75) ))

names(lossinfx.df)# top part of Table 4 (no random effects)
# multinomial and CS w/o random effects starts Line 329 ends Line 800 Mixed logit started thereafter



########### End of cashless_sec_6_tab_3.R