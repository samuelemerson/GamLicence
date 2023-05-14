library(tidyverse)
library(readxl)
library(RecordLinkage)
library(urltools)
library(assertive.data.uk)
library(openxlsx)

# First we read in all the data, saving sheets as elements of a list.

Licence_Data <- "business-licence-register.xlsx" %>% excel_sheets() %>%
  set_names() %>% map(read_excel,path = "business-licence-register.xlsx")

# We then create a data frame in which to filter out licences, adding columns to
# highlight potential flags if they come up.

GAMSTOP_Data <- Licence_Data$Licences %>%
  add_column(Flag_Remote = rep(FALSE,nrow(Licence_Data$Licences))) %>%
  add_column(Flag_Active = rep(FALSE,nrow(Licence_Data$Licences))) %>%
  add_column(Flag_Account = rep(FALSE,nrow(Licence_Data$Licences)))

# We now create a vector of names similar to or exactly matching the word "Remote"
# (for example "remote").

remote_names <- unique(GAMSTOP_Data$Type)[which(sapply(unique(GAMSTOP_Data$Type),
                                                       FUN = function(u){levenshteinDist(u,"Remote")})<2)]
# Then with our licence data set we keep only licences whose type is remote or
# whose licence number contains the letter R.

GAMSTOP_Data <- GAMSTOP_Data %>%
  filter(Type%in%remote_names | grepl("R",`Licence Number`)) %>%
  mutate(Flag_Remote=(!Type%in%remote_names | !grepl("R",`Licence Number`)))

# We now create a vector of names similar to or exactly matching the word "Active"
# (for example "active").

active_names <- unique(GAMSTOP_Data$Status)[which(sapply(unique(GAMSTOP_Data$Status),
                                                         FUN = function(u){levenshteinDist(u,"Active")})<2)]
# Then with our updated licence data set we keep only licences whose status is
# active or licences with a start date but no end date.

GAMSTOP_Data <- GAMSTOP_Data %>%
  filter(Status%in%active_names | (!is.na(`Start Date`) & is.na(`End Date`))) %>%
  mutate(Flag_Active=(!Status%in%active_names | (is.na(`Start Date`) | !is.na(`End Date`))))

# We now create a vector of names similar to or exactly matching any of the words
# "Casino", "Bingo", "External Lottery Manager",
# "General Betting Standard - Virtual Event",
# "General Betting Standard - Real Event" or "Pool Betting".

activity_names <- unique(GAMSTOP_Data$Activity)[unique(which(outer(unique(GAMSTOP_Data$Activity),
                                                                   c("Casino","Bingo","External Lottery Manager",
                                                                     "General Betting Standard - Virtual Event",
                                                                     "General Betting Standard - Real Event",
                                                                     "Pool Betting"),FUN = levenshteinDist)<2,arr.ind = T)[,1])]
# And then only keep from our updated licence data the licences with activities
# in the vector activity_names.

GAMSTOP_Data <- GAMSTOP_Data %>% filter(Activity%in%activity_names)

# If there are any accounts that don't match their licence number this is flagged
# and stored.

GAMSTOP_Data <- GAMSTOP_Data %>%
  mutate(Flag_Account = `Account Number`!=as.numeric(substr(`Licence Number`,1,6)))

# In order to check domain we first visit the businesses data set and keep only
# those accounts who have the word (or similar) "UK" in their name.

Business_Data <- Licence_Data$Businesses %>%
  filter(grepl("UK | uk | Uk",`Licence Account Name`))

# We then join this data set to our updated licence data set by left_join, which
# for all rows in GAMSTOP_Data, if there is a row in Business_Data with a matching
# Account Number, joins the columns of the Business_Data row (minus Account Number)
# to the GAMSTOP_Data row. If there no matches then NA are displayed and if there
# are multiple matches then any columns are added (this is not a problem here
# as we do not care what the information is, only that there is a match).

GAMSTOP_Data <- left_join(GAMSTOP_Data,Business_Data,by = "Account Number",
                          multiple = "all")

# We then check the trading name data set, keeping only the accounts whose name
# contains the word (or similar) "UK" and whose status is word Active or similar.
# Then we left_join this data set to the updated licence data set. Note only the
# Account Number and Trading Name are retained.

trading_active_names <- unique(Licence_Data$TradingNames$Status)[which(sapply(unique(Licence_Data$TradingNames$Status),
                                                                              FUN = function(u){levenshteinDist(u,"Active")})<2)]

Trading_Data <- Licence_Data$TradingNames %>%
  filter(Status%in%trading_active_names & grepl("UK | uk | Uk",`Trading Name`)) %>%
  select(`Account Number`,`Trading Name`)

GAMSTOP_Data <- left_join(GAMSTOP_Data,Trading_Data,by = "Account Number",
                          multiple = "all")

# For the domain data, we also check for active status combined with whether the
# suffix of the domain name contains the string "uk" (this covers much more than
# the standard co.uk suffix here, as for example gov.uk is equally informative
# domain information). We then left_join as usual.

domain_active_names <- unique(Licence_Data$DomainNames$Status)[which(sapply(unique(Licence_Data$DomainNames$Status),
                                                                            FUN = function(u){levenshteinDist(u,"Active")})<2)]

Domain_Data <- Licence_Data$DomainNames %>%
  filter(Status%in%domain_active_names & grepl("uk",suffix_extract(domain(`Domain Name`))$suffix)) %>%
  select(`Account Number`,`Domain Name`)

GAMSTOP_Data <- left_join(GAMSTOP_Data,Domain_Data,by = "Account Number",
                          multiple="all")

# Having assessed the names of the countries in Licence_Data$Addresses there
# appears to be several which can be classed as being part of the UK. These are
# "Alderney","Great Britain","Guernsey","Isle of Man","Northern Ireland" and
# "United Kingdom". Keeping data whose country is in this selection or whose
# postcode is a uk postcode and the left joining this to the updated licence data
# then gives all our domain checks.

Address_Data <- Licence_Data$Addresses %>%
  filter(Country%in%c("Alderney","Great Britain","Guernsey","Isle of Man",
                      "Northern Ireland","United Kingdom") | is_uk_postcode(Postcode)) %>%
  select(`Account Number`,Country)

GAMSTOP_Data <- left_join(GAMSTOP_Data,Address_Data,by = "Account Number",
                          multiple="all")

# We now remove any data where we found no evidence of a UK domain

GAMSTOP_Data <- GAMSTOP_Data %>% filter(!is.na(`Licence Account Name`) |
                                          !is.na(`Trading Name`) |
                                          !is.na(`Domain Name`) |
                                          !is.na(Country))

# And separate the flagged data from the none flagged data.

GAMSTOP_NoFlag_Data <- GAMSTOP_Data %>% filter(Flag_Remote==F & Flag_Active==F &
                                                 Flag_Account==F)
GAMSTOP_Flag_Data <- GAMSTOP_Data %>% filter(Flag_Remote!=F | Flag_Active!=F |
                                               Flag_Account!=F)

# Finally we extract the unique account numbers and their business names into
# a new data frame (here we assume that every account number has a unique
# business name).

GAMSTOP_Accounts <- Licence_Data$Businesses %>% filter(`Account Number`%in%unique(GAMSTOP_NoFlag_Data$`Account Number`))

# Taking the list of operators, the non flagged licence data and the flagged
# licence data we then export this information as an excel file with 3 sheets.

GAMSTOP <- list("Accounts" = GAMSTOP_Accounts,"Licenses" = GAMSTOP_NoFlag_Data,
                "Flagged Licences" = GAMSTOP_Flag_Data)

write.xlsx(GAMSTOP,file = "GAMSTOP.xlsx")
