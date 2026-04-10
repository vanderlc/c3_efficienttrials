## fucntions

library(table1)

## paths
### may need to change
{
a4_cog_path <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/A4_cognition_clean.csv"
cdr_path  <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/cdr.csv"
ptau_path <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/biomarker_pTau217.csv"
adqs_path <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/ADQS.csv"
subjinfo_path <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/SUBJINFO.csv"

cog <- read_csv(a4_cog_path)
cdr <- read_csv(cdr_path)
ptau <- read_csv(ptau_path)
adqs <- read_csv(adqs_path)
subjinfo <- read_csv(subjinfo_path)
}

# checks
{
  summary(cog) # BID
  summary(cdr) # BID
  summary(ptau) # BID
  summary(adqs)
  summary(subjinfo) # BID
  
  subjinfo %>%
    group_by(SUBSTUDY) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  # gorup by VISIT 1
  cog %>%
    group_by(VISIT) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  # group by AVISIT 1
  cdr %>%
    group_by(AVISIT) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  # group by visitCD 1
  adqs %>%
    group_by(VISITCD) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  # stratify SUBSTUDY by A4
  a4_bl <- subjinfo %>%
    group_by(SUBSTUDY, TX)
  # only visit 1
  
  table(a4_bl$TX)
  # merge visit 1 to a4_bl for cog, cdr, and ptau by matching BID into one dataset
  # only cog and cdr have VISIT and AVISIT, so we will filter for visit 1 in those datasets before mergin
  cog_v1 <- cog %>%
    filter(VISIT == 1)
  
  cdr_v1 <- cdr %>%
    filter(AVISIT == 1) %>%
    select(AVISIT, BID, CDSOB, CDGLOBAL)
  
  ptau_v1 <- ptau %>%
    select(BID, ORRESRAW) %>%
    rename(ptau217 = ORRESRAW)
  
  adqs_v1 <- adqs %>%
    filter(VISITCD == "001")
  
  adqs_v1 %>%
    filter(MITTFL== 1)
  unique(adqs_v1$BID) # check for duplicates
  
  # duplicate check
  cog_v1  %>% group_by(BID) %>% summarise(n = n()) %>% filter(n > 1)
  cdr_v1  %>% group_by(BID) %>% summarise(n = n()) %>% filter(n > 1)
  ptau_v1 %>% group_by(BID) %>% summarise(n = n()) %>% filter(n > 1)
  
  
  a4_bl_comb <- a4_bl %>%
    left_join(cog_v1,  by = "BID") %>%
    left_join(cdr_v1,  by = "BID") %>%
    left_join(ptau_v1, by = "BID")
  # ptau is weird will double check with casey
  # without ptau
  a4_bl_comb <- a4_bl %>%
    left_join(cog_v1,  by = "BID") %>%
    left_join(cdr_v1,  by = "BID")
  # remove na TX rows
  a4_bl_comb2 <- a4_bl_comb %>%
    filter(!is.na(TX))
  summary(a4_bl_comb2)
}

# CDR + memory tests + ptau: CDSOB CDGLOBAL C3 ptau217
table(a4_bl$TX)
table(a4_bl_comb$TX) # they match

### Stratification by Treatment
###
# inner C3Mem C3NonMem BPXT  FNMT   OCL   DET   IDN   ONB 
# table 1
# demographics: AGEYR MARITAL   SEX  RACE EDCCNTU ETHNIC CDSOB CDGLOBAL C3 ptau217

table1::table1(~ AGEYR+MARITAL+SEX+EDCCNTU+RACE+ETHNIC++CDSOB+CDGLOBAL+C3| TX, data = a4_bl_comb2)
table1::table1(~C3Mem+C3NonMem+BPXT+FNMT+OCL+DET+IDN+ONB| TX, data = a4_bl_comb2)

####################################################################################
# instead of TX we need CDR PROG

cdr$CDGLOBAL
unique(cdr$VISCODE)
sort( unique(cdr$AVISIT) )

#  CDR progression flag from post-baseline CDR visits
#   "Progressor"     = CDGLOBAL > 0 at ANY visit where AVISIT > 1
#   "Non-Progressor" = CDGLOBAL == 0 at ALL post-baseline visits
{
cdr_prog_flag <- cdr %>%
  filter(SUBSTUDY == "A4") %>%
  mutate(
    BID    = str_trim(as.character(BID)),
    AVISIT = suppressWarnings(as.numeric(AVISIT))
  ) %>%
  filter(AVISIT > 1, !is.na(CDGLOBAL)) %>%       # post-baseline rows only
  group_by(BID) %>%
  summarise(
    CDR_PROG = if_else(
      any(CDGLOBAL > 0, na.rm = TRUE),
      "Progressor",
      "Non-Progressor"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    CDR_PROG = factor(CDR_PROG, levels = c("Non-Progressor", "Progressor"))
  )

# Sanity check
cat("CDR Progression flag coverage:\n")
print(table(cdr_prog_flag$CDR_PROG, useNA = "ifany"))
}
# joining CDR_PROG
{
a4_bl_prog <- a4_bl %>%
  left_join(cog_v1,        by = "BID") %>%
  left_join(cdr_v1,        by = "BID") %>%
  left_join(cdr_prog_flag, by = "BID") %>%
  filter(!is.na(TX))                              # keep same TX-complete rows as before

# Sanity check
cat("\nCDR Progression in final analysis dataset:\n")
print(table(a4_bl_prog$CDR_PROG, useNA = "ifany"))
}
# Table 1 stratified by treatment and  by CDR progression status
# remove NA
a4_bl_prog <- a4_bl_prog %>%
  filter(!is.na(CDR_PROG))
#
table1::table1(
  ~ AGEYR + as.factor(MARITAL) + as.factor(SEX) + EDCCNTU + as.factor(RACE) + as.factor(ETHNIC) + CDSOB + CDGLOBAL + C3 | TX*CDR_PROG,
  data    = a4_bl_prog,
  caption = "Table 1. Baseline Characteristics by CDR Progression Status"
)
length(unique(a4_bl_prog$BID))
# Table 1 Cognitive Subtests

table1::table1(
  ~ C3Mem + C3NonMem + BPXT + FNMT + OCL + DET + IDN + ONB | TX*CDR_PROG,
  data    = a4_bl_prog,
  caption = "Table 2. Baseline Cognitive Subtests by CDR Progression Status"
)
