#' @name 01_gather_data.R
#' @title GATHER AND PROCESS RAW DATA
#' @author Tim Fraser
#' @description Script to gather and process raw data


# "Measuring Urban Regimes in Japan through Spending Priorities"
# "Replication Code"
# "Timothy Fraser"
# "December 29, 2020"

# This document details replication code necessary to reproduce raw data for indices
# measuring each city in Japan in terms of the prevalence 
# and influence of several types of urban regimes. 
# The urban regime, coined by Clarence Stone in his study of Atlanta,
# describes a constellation of actors and interests which shape its governance.

# Stone and likeminded scholars sorted American cities into four types of urban regimes, 
# including caretaker, developmental, progressive, and opportunist regimes. 
# Over time, Stone himself came to advocate for a 
# more nuanced perspective on political change in cities, 
# arguing that "there is little reason to expect a stable 
# and cohesive governing coalition in today's cities." 
# For this reason, we might now expect considerable variation in urban regimes among cities,
# especially over time.
# Japan has its own adjacent literature on urban politics.

# 0. Setup ################################################

## 0.1 Packages ################################################

library(tidyverse)
library(viridis)

## 0.2 Setup Folders ################################################

# Initialize model/ folder
dir.create("model")
# Initialize table/ folder
dir.create("table")

# 1. Gather Data ################################################

## 1.1 Source Tables ################################################


bind_rows(
  data.frame(
    code = "0000020101",
    table = "A",
    type = "人口・世帯"),
  data.frame(
    code = "0000020102",
    table = "B",
    type = "自然環境"),
  data.frame(
    code = "0000020103",
    table = "C",
    type = "経済基盤"),
  data.frame(
    code = "0000020104",
    table = "D",
    type = "行政基盤"),
  data.frame(
    code = "0000020105",
    table = "E",
    type = "教育"),
  data.frame(
    code = "0000020106",
    table = "F",
    type = "労働"),
  data.frame(
    code = "0000020107",
    table = "G",
    type = "文化・スポーツ"),
  data.frame(
    code = "0000020108",
    table = "H",
    type = "居住"),
  data.frame(
    code = "0000020109",
    table = "I",
    type = "健康・医療"),
  data.frame(
    code = "0000020110",
    table = "J",
    type = "福祉・社会保障"),
  data.frame(
    code = "0000020111",
    table = "K",
    type = "安全")
) %>%
  saveRDS("raw_data/sources.rds")

# View data sources
read_rds("raw_data/sources.rds") 

################################################

## 1.2 Identify Variables ################################################


bind_rows(
  # All
  data.frame(
    code = "A1101",
    type = "Total population (Both sexes)[person]",
    name = "pop",
    group = "pop"),
  data.frame(
    code = "A1231",
    type = "Median Age",
    name = "age_median",
    group = "age"),  
  data.frame(
    code = "A1303",
    type = "Total population age 65 and over [person]",
    name = "age_elder",
    group = "age"),
  
  data.frame(
    code = "A1303",
    type = "Population (0-17, total)[person]",
    name = "age_youth",
    group = "age"),
  
  data.frame(
    code = "A5103", 
    type = "Inmigrants[persons]",
    name = "inmigrants",
    group = "migration"),
  data.frame(
    code = "A5104", 
    type = "Outmigrants[persons]",
    name = "outmigrants",
    group = "migration"),
  
  
  data.frame(
    code = "E9106",
    type = "Population by last school completed (University and graduate school)[persons]",
    name = "full_college",
    group = "college"),
  data.frame(
    code = "E9105",
    type = "Population by last school completed (junior college and college of technology)[persons]",
    name = "junior_college",
    group = "college"),
  
  data.frame(
    code = "F1101",
    type = "Population in labour force [persons]",
    name = "labor_force",
    group = "unemployed"),
  data.frame(
    code = "F1107",
    type = "Number of unemployed persons [persons]",
    name = "unemployed",
    group = "unemployed"),
  
  
  # Geography
  data.frame(
    code = "B1103",
    type = "Total inhabitable area (ha)",
    name = "inhabitable_area",
    group = "area"),
  # Income
  data.frame(
    code = "C120110",
    type = "Taxable income [thousands of yen]", 
    name = "income",
    group = "income"),
  data.frame(
    code = "C120130",
    type = "Taxpayers [persons]",
    name = "taxpayers",
    group = "income"),
  # Employment
  data.frame(
    code = "C2207",
    type = "Number of persons engaged (Economic Census for Business Frame) [persons]",
    name = "employees",
    group = "employment"),
  data.frame(
    code = "C2210",
    type = "number of persons engaged in primary industry [persons]",
    name = "employees_primary",
    group = "employment"),
  data.frame(
    code = "C2211",
    type = "number of persons engaged in secondary industry [persons]",
    name = "employees_secondary",
    group = "employment"),
  data.frame(
    code = "C2212",
    type = "number of persons engaged in secondary industry [persons]",
    name = "employees_tertiary",
    group = "employment"),
  # Economic Output
  data.frame(
    code = "C3101",
    type = "Gross agricultural product (million yen)",
    name = "output_agr",
    group = "output"),
  data.frame(
    code = "C3401",
    type = "value of manufactured goods shipments (million yen)",
    name = "output_manuf",
    group = "output"),
  data.frame(
    code = "C3501",
    type = "Annual sales of commercial goods (wholesale and retail trade) [million yen]",
    name = "output_com",
    group = "output"),
  # Revenue and Expenditures
  data.frame(
    code = "D2202",
    type = "Real term balance of revenue to expenditure (%)", # 実質収支比率（市町村財政）
    name = "rev_to_exp",
    group = "rev"),
  data.frame(
    code = "D3201",
    type = "Total Revenue from all sources (thousand yen)",
    name = "rev",
    group = "rev"),
  data.frame(
    code = "D3203",
    type = "Settlement of total expenditure (municipalities)[thousand yen]",
    name = "total_spending",
    group = "total_spending"),
  data.frame(
    code = "D320113",
    type = "National Disbursements (thousands of yen)",
    name = "rev_nat",
    group = "rev"),
  data.frame(
    code = "D320115",
    type = "Prefectural Disbursements (thousands of yen)",
    name = "rev_pref",
    group = "rev"),
  # Social Welfare Regime 
  data.frame(
    code = "D3203031",
    type = "Social welfare expenditure (municipalities)[thousand yen]",
    name = "welfare",
    group = "welfare"),
  data.frame(
    code = "D3203032",
    type = "Social welfare expenditure for the aged (municipalities)[thousand yen]",
    name ="welfare_aged",
    group = "welfare_aged"),
  data.frame(
    code = "D3203033",
    type = "Welfare expenditure for children (municipalities)[thousand yen]",
    name ="welfare_kids",
    group = "welfare_kids"),
  data.frame(
    code = "D3203034",
    type = "Expenditure for livelihood protection (municipalities)[thousand yen]",
    name ="livelihood_protection",
    group = "livelihood"),
  
  data.frame(
    code = "D320305",
    type = "(Expenditure) Labor expenditures (local public finance, municipalities)[thousand yen]",
    name ="unemployment",
    group = "unemployment"),
  data.frame(
    code = "D3203086",
    type = "(Expenditure) Housing (local public finance, municipalities)[thousand yen]",
    name ="housing",
    group = "housing"),
  data.frame(
    code = "D320309",
    type = "(Expenditure) Fire service (local public finance, municipalities)[thousand yen]",
    name = "fire_service",
    group = "emergency"),
  # Code progressive variables
  data.frame(
    code = "D3203042",
    type = "(Expenditure) Public health services  (local public finance, municipalities)[thousand yen]",
    name = "public_health_services",
    group = "health"),
  data.frame(
    code = "D3203043",
    type = "(Expenditure) Public health centers (local public finance, municipalities)[thousand yen]",
    name = "public_health_centers",
    group = "health"),
  data.frame(
    code = "D3203044",
    type = "(Expenditure)Waste disposal  (local public finance, municipalities)[thousand yen]",
    name = "waste_disposal",
    group = "waste"),
  data.frame(
    code = "D3203085",
    type = "(Expenditure) City planning (local public finance, municipalities)[thousand yen]",
    name = "city_planning",
    group = "planning"),
  data.frame(
    code = "D3203102",
    type = "Education expenditure for elementary schools (municipalities)[thousand yen]",
    name = "edu_elementary",
    group = "education"),
  data.frame(
    code = "D3203103",
    type = "Education expenditure for junior high schools (municipalities)[thousand yen]",
    name = "edu_junior_high",
    group = "education"),
  data.frame(
    code = "D3203104",
    type = "Education expenditure for senior high schools (municipalities)[thousand yen]",
    name = "edu_senior_high",
    group = "education"),
  data.frame(
    code = "D3203105",
    type = "Expenditure for Schools for special needs education (municipalities)[thousand yen]",
    name = "edu_special_needs",
    group = "education"),
  data.frame(
    code = "D3203106",
    type = "Education expenditure for kindergartens (municipalities)[thousand yen]",
    name = "edu_kinder",
    group = "education"),
  data.frame(
    code = "D3203107",
    type = "Social education expenditure (municipalities)[thousand yen]",
    name = "edu_social",
    group = "edu_social"),
  # Expenses for social education facilities established by local governments 
  # by ordinance and under the jurisdiction of the Board of Education
  # and expenses for social education activities conducted by the Board of Education
  # (including physical education / cultural relations and protection of cultural properties)
  # this is cultural affairs facilities, eg. libraries, community centers, etc.
  # lifelong learning
  # museums
  # cultural parks
  # library
  data.frame(
    code = "D3203108",
    type = "(Expenditure) Health and physical education (local public finance, municipalities)[thousand yen]",
    name = "edu_health_phys",
    group = "education"),
  # Developmental
  data.frame(
    code = "D320306",
    type = "Agriculture, forestry and fishery expenditure (municipalities)[thousand yen]",
    name = "agr",
    group = "agr"),
  data.frame(
    code = "D320307",
    type = "Commerce and manufacturing expenditure (municipalities)[thousand yen]",
    name = "com_manuf",
    group = "com_manuf"),
  data.frame(
    code = "D3203082",
    type = "(Expenditure) Roads and bridges  (local public finance, municipalities)[thousand yen]",
    name = "roads_bridges",
    group = "roads_bridges"),
  data.frame(
    code = "D320406",
    type = "(Expenditure) Ordinary construction works (local public finance, municipalities)[thousand yen]",
    name = "construction",
    group = "construction"),
  
  # Confounding Variables: Disasters
  # Let's also separate disaster variables in general, as this is an important
  # intervening variable, not a direct trait of urban regimees
  
  # Disaster Recovery (Specifically, as its own earmarked category)
  data.frame(
    code = "D320311", # Part of D320311 = its own category
    # D320311_災害復旧費（市町村財政）
    type = "Disaster Restoration expenditure (municipalities)[thousand yen]",
    name ="dis_restoration",
    group = "disaster"),
  # Disaster Recovery Expenses, as part of the Social Welfare Budget
  # D3203035_災害救助費（市町村財政）
  data.frame(
    code = "D3203035",# Part of D320303 = 民生費（Social Welfare)
    type = "(Expenditure) Disaster relief costs (local public finance, municipalities)[thousand yen]",
    name = "dis_relief", 
    group = "disaster"),
  # Disaster Recovery Program Administrative Expenses
  # D320407_災害復旧事業費（市町村財政）
  data.frame(
    code = "D320407",
    type = "(Expenditure) Disaster recovery (local public finance, municipalities)[thousand yen]",
    name = "dis_admin",
    group = "disaster")
) %>%
  mutate(regime = case_when(
    name %in% c("welfare", "welfare_aged", "welfare_kids",
                "livelihood_protection", "unemployment", "housing",
                "fire_service") ~ "social_welfare",
    name %in% c("public_health_services", "public_health_centers",
                "waste_disposal", "city_planning",
                "edu_kinder", "edu_elementary",
                "edu_junior_high", "edu_senior_high",
                "edu_special_needs", "edu_social", "edu_health_phys") ~ "middle_class",
    name %in% c("agr", "com_manuf", "roads_bridges", "construction") ~ "developmental",
    TRUE ~ NA_character_)) %>%
  # Classify them into each table based on first letter
  mutate(table = str_sub(code, 1, 1)) %>%
  saveRDS("raw_data/varnames.rds")

## 1.3 E-Stat API Import ################################################

# Let's import indicators on our three types of urban regimes.

### Functions ################################################

library(tidyverse)
library(jpstat)
# Index of available stats
# https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200502&tstat=000001111376&cycle=8&tclass1=000001111379&tclass2val=0

# Set up your own ESTAT Census API ID - it's free
myid <- "f34fbd978b7295e3ad23596e1a34f6d46b3fb502"

# Load in the table of social and demographic categories and their corresponding codes
sources <- read_rds("raw_data/sources.rds")

# Load in the names of the variables we want specifically
varnames <- read_rds("raw_data/varnames.rds")

# These are the tables we will need.
varnames$table %>% unique()


# Save that list with english names,
paste("https://www.e-stat.go.jp/dbview?sid=",
      filter(sources, table == "A")$code, sep = "") %>%
  # Get census meta data
  estat(appId = myid, statsDataId = ., lang = "E") %>%
  # Constrain time range
  activate("time") %>%
  filter(str_detect(name, "20[0-9]{2}")) %>%
  # Clarify Geography
  activate("area")  %>%
  filter(level %in% c(2, 3) | str_detect(name, "東京都")) %>%
  # drop any totals for special ku
  # Drop any merged sites, which contain the prefix "formerly"（旧）
  #filter(!str_detect(name, "特別区部") & !str_detect(name, "（旧）")) %>%
  activate("area") %>% as_tibble() %>% 
  select(muni_code = code, muni = name, level) %>%
  saveRDS("raw_data/muni_ku.rds")

# Now get just the municipalities, removing the extra ku
read_rds("raw_data/muni_ku.rds") %>%
  # Filter to just 市区町村, not 区 (but keeping the tokyo special wards)
  filter(level == 2  | str_detect(muni, "東京都")) %>%
  filter(!str_detect(muni, "特別区部")) %>%
  select(muni_code, muni) %>%
  saveRDS("raw_data/muni_code.rds")  

# Build a converter

# There are 181 ku (not in Tokyo) that we need to build a translator for
# That's six more than the old translator (175, below), because we're including all records 
#read_csv("raw_data/wards_municipalities_conversion.csv") %>% dim()
read_rds("raw_data/muni_ku.rds") %>% 
  select(ward_code = muni_code, ward = muni, level) %>%
  mutate(muni = str_extract(ward, pattern = ".*[-]shi ") %>% 
           str_remove("[(]former[)] ") %>% str_trim(side = "right")) %>%
  filter(level == 3 & !str_detect(ward, "Tokyo"))  %>% 
  left_join(by = c("muni" = "muni"), 
            y = read_rds("raw_data/muni_ku.rds") %>% 
              filter(level == 2) %>% select(muni, muni_code)) %>%
  select(ward_code, ward, muni, muni_code) %>%
  saveRDS("raw_data/wards_municipalities_conversion.rds")





# Now, Get the list of municipalities available, in Japanese
muni <- paste("https://www.e-stat.go.jp/dbview?sid=",
              filter(sources, table == "A")$code, sep = "") %>%
  # Get census meta data
  estat(appId = myid, statsDataId = .) %>%
  # Constrain time range
  activate("time") %>%
  filter(str_detect(name, "20[0-9]{2}")) %>%
  # Clarify Geography
  activate("area")  %>%
  # Filter to just 市区町村, not 区 (but keeping the tokyo special wards)
  filter(level == 2 | str_detect(name, "東京都")) %>%
  # drop any totals for special ku
  filter(!str_detect(name, "特別区部")) %>%
  # Drop any merged sites, which contain the prefix "formerly"（旧）
  #filter(!str_detect(name, "特別区部") & !str_detect(name, "（旧）")) %>%
  activate("area") %>% as_tibble() %>% select(code, name, parentCode)

# Let's write a for-loop to gather results,
# it works for 1741 municipalities over time
get_stats = function(i, mydirectory, mydata){
  print(i)
  
  # Filter to your prefecture's data
  mypref <- mydata %>%
    filter(str_sub(code, 1,2) %>% as.numeric() == i)
  
  # Grab the municipality codes in that prefecture
  somemuni <- muni %>%
    filter(str_sub(code, 1,2) %>% as.numeric() == i)
  
  # Check how many municipalities are there in that prefecture
  
  # Divide that number by 100, then jump up to the next whole digit 
  # to get the number of times we would need to run the algorithm to catch all cases.
  nchunks <- ceiling(nrow(somemuni) / 100)
  
  # Assign each municipality to an equally sized chunk.
  # If under 100, they'll all be in chunk 1. If 101-200, they'll be in 2 chunks; If 201 to 300, they'll be in 3 chunks.
  somemuni <- somemuni %>%
    mutate(chunk = ntile(1:n(), n = nchunks))
  
  # If just 1 chunk
  if(nchunks == 1){
    # Filter to the cases in chunk 1 (all of them)
    mypref %>%
      filter(code %in% filter(somemuni, chunk == 1)$code) %>%
      collect() %>%
      saveRDS(paste(mydirectory, "/", i, ".rds", sep = ""))
  }
  # If 2 chunks
  if(nchunks == 2){
    # Filter to the cases in chunk 1
    h1 <- mypref %>%
      filter(code %in% filter(somemuni, chunk == 1)$code) %>%
      collect()
    # Filter to the cases in chunk 2
    h2 <- mypref %>%
      filter(code %in% filter(somemuni, chunk == 2)$code) %>%
      collect()
    
    # Bind and save together
    bind_rows(h1,h2) %>%
      saveRDS(paste(mydirectory, "/", i, ".rds", sep = ""))
    remove(h1,h2)
  }
  # If 3 chunks
  if(nchunks == 3){
    # Filter to the cases in chunk 1
    h1 <- mypref %>%
      filter(code %in% filter(somemuni, chunk == 1)$code) %>%
      collect()
    # Filter to the cases in chunk 2
    h2 <- mypref %>%
      filter(code %in% filter(somemuni, chunk == 2)$code) %>%
      collect()
    # Filter to the cases in chunk 2
    h3 <- mypref %>%
      filter(code %in% filter(somemuni, chunk == 3)$code) %>%
      collect()
    
    # Bind and save together
    bind_rows(h1,h2,h3) %>%
      saveRDS(paste(mydirectory, "/", i, ".rds", sep = ""))
    remove(h1,h2,h3)
  }
  remove(mypref, somemuni, nchunks)
}


get_table = function(mytable){
  
  # Let's start with 
  fulldata <-  paste("https://www.e-stat.go.jp/dbview?sid=",
                     filter(sources, table == mytable)$code, sep = "") %>%
    # Get census meta data
    estat(appId = myid, statsDataId = .) %>%
    # Clarify Geography
    activate("area") %>%
    filter(code %in% muni$code) %>%
    # Let's get the metadata for the full dataset we want
    # Clarify types of values; filter to observed values
    activate("tab") %>%
    filter(name == "観測値") %>%
    select() %>%
    # Zoom into select variables
    activate("cat01") %>%
    filter(code %in% filter(varnames, table == mytable)$code) %>%
    select(code) %>%
    # Clarify time period, from 2000 to 2018 (2019 not fully available)
    activate("time") %>%
    select(name) %>%
    filter(str_detect(name, "200[0-9]{1}|201[0-8]{1}")) %>%
    # Clarify geographic variables
    activate("area") %>%
    select(code)
  
  # Create a home for these files
  mydir <- tempdir()
  
  c(1:47) %>%
    map(~get_stats(i = ., mydir, mydata = fulldata))
  
  # Then bind together
  dir(mydir, full.names = TRUE, pattern = "[0-9]+.rds") %>%
    map_dfr(~read_rds(.)) %>%
    saveRDS(paste("raw_data/", mytable, ".rds", sep = ""))
  
  # And get rid of temp
  unlink(mydir, recursive = TRUE)
  
}







### Download ################################################

# Do this just one time
get_table(mytable = "A") # get simple demographic variables
get_table(mytable = "B") # get area variables
get_table(mytable = "C") # get socioeconomic variables
get_table(mytable = "D") # get governance variables
get_table(mytable = "E") # get education variables
get_table(mytable = "F") # get labor variables



### Covariates ################################################

# Finally, we're going to download a dataset from a past study, 
# which records key disaster data from 2011 for each municipality affected by the 3/11 distaster. 
# This data is originally sourced from the work of Daniel P. Aldrich, 
# and these variables were used in 
# Tim Fraser's study of renewable energy in Japan 
# We'll query that study's replication data using its DOI,
# which is available [by URL here.](https://doi.org/10.7910/DVN/GBRVWY) 


# Let's load the dataverse package
library("dataverse")
# And tell dataverse where to look
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

# Query that replication dataset via its DOI
get_dataset("doi:10.7910/DVN/GBRVWY")$files %>%
  # Search its files for this specific excel file
  filter(label == "Database_prefectural_analysis_R_relevant_variables.xlsx") %>%
  # acquire the file's own DOI
  with(persistentId) %>%
  # Download data, using the file DOI, and read it in using the read_excel function
  get_dataframe_by_doi(filedoi = ., .f = readxl::read_excel) %>%
  # Then give the variables simple and intuitive names
  select(code = Code, deaths = death11, damages = damaged11, 
         tsunami = coast, exclusion_zone = Fukushima_Exclusion_Zone) %>%
  # and save to file!
  saveRDS("raw_data/disaster_data.rds")


# Next, let's repeat that process and download
# [Tim Fraser's Social Capital Indices](https://doi.org/10.7910/DVN/PBBKBF), from 2000-2017.


# Download data, using the file name, and read it in using the read_csv function
get_dataframe_by_name(filename = "indices_V2_2020_10_28.tab",
                      dataset = "10.7910/DVN/PBBKBF",
                      original = FALSE,
                      .f = read_tsv) %>%
  select(muni_code, year, social_capital, bonding, bridging, linking, vulnerability) %>%
  # and save to file!
  saveRDS("raw_data/sci.rds")

### Voting ################################################

#### Lower House Elections ################################################


# Import main municipality codes about which we seek data

main = read_rds("raw_data/muni_ku.rds") %>%
  select(ward_code = muni_code) %>%
  # In a few cases, we have ward tallies instead of municipalities; 
  # let's get the municipality in there too.
  # Let's join in the correct municipality code for each ward
  left_join(by = c("ward_code" = "ward_code"),
            y = read_rds("raw_data/wards_municipalities_conversion.rds") %>%
              select(ward_code, muni_code)) %>%
  # If the new municipality code is missing, it means it wasn't a ward; fill it in with its original municipality code. 
  mutate(muni_code = if_else(is.na(muni_code), ward_code, muni_code)) %>%
  inner_join(
    # Using this unique identifier as an index to merge by,
    by = c("ward_code" = "muni_code"),
    # Import and join in our unique conversion file
    y = read_csv("raw_data/reed_smith_key_municipalities.csv") %>% 
      select(#muni = Municipality, 
        muni_code = Code,
        kuname_jp = 4,
        kuname = 5) %>%
      mutate(muni_code = str_pad(muni_code, width = 5, side = "left", pad = "0"))) %>%
  select(muni_code, ward_code, kuname_jp, kuname)

# Wowsers! Gains back about ~40 municipalities. WORTH IT.


# First, we acquire party voteshare data.


# Create a data.frame listing which parties were in a government coalition
winning_party = bind_rows(
  data.frame(
    party_en = "DPJ",
    year = 2009:2011),
  data.frame(
    party_en = "JSP",
    year = 1993:1995),
  data.frame(
    party_en = "SDP",
    year = 1996:1999),
  data.frame(
    party_en = "Komeito",
    year = c(2000:2008, 2012:2020)),
  data.frame(
    party_en = "Conservative",
    year = 2000:2002),
  data.frame(
    party_en = "LDP",
    year = c(1958:1992, 1994:2008, 2012:2020)),
  data.frame(
    party_en = "Sakigake",
    year = 1994:1999)) %>%
  mutate(winning_party = 1)


party_vote = read_csv("raw_data/reed_smith_japan_house_of_reps_elections_candidates.csv") %>%
  filter(year >= 2000) %>%
  # Select only key variables
  select(pid, year, kuname, kucode, 
         ku_vote, ku_totvote, ku_electorate, party_en, result) %>%
  filter(!is.na(ku_vote)) %>%
  # Calculate percentage of votes won by candidate out of votes won by all candidates
  mutate(voteshare = ku_vote / ku_totvote, 
         # Calculate turnout out of electorate for their race
         turnout = ku_totvote / ku_electorate) %>%
  arrange(kuname, year) %>%
  # Zoom into just who won the election. We're not going to worry about special cases for now.
  # Just, sheer, who won?
  filter(result == 1) %>%
  # aggregate to party
  group_by(year, kuname, party_en) %>%
  summarize(voteshare = sum(voteshare, na.rm = TRUE),
            turnout = unique(turnout, na.rm = TRUE)) %>%
  ungroup() %>%
  # Joining in data on which parties were in power during which years since 1958
  left_join(by = c("party_en", "year"),
            y = winning_party) %>%
  mutate(winning_party = if_else(is.na(winning_party), 0, 1))

vote_winner = main %>%
  left_join(by = c("kuname"),
            y = party_vote %>% 
              filter(winning_party == 1) %>%
              select(-party_en))

vote_ldp = main %>%
  left_join(by = c("kuname"),
            y = party_vote %>% 
              filter(party_en == "LDP") %>%
              select(-party_en))

vote_komeito = main %>%
  left_join(by = c("kuname"),
            y = party_vote %>% 
              filter(party_en == "Komeito") %>%
              select(-party_en))

vote_turnout = main %>%
  left_join(by = c("kuname"),
            y = party_vote %>%
              group_by(year, kuname) %>%
              summarize(turnout = unique(turnout)) %>%
              ungroup() )



# Third, we merge this all together, and save it as the "vote_timeseries_data.csv",
# which records from 1958-2020 the district level electoral outcomes for Lower House Elections.

# Now create a complete list of municipality codes between 1958 and 2020
expand_grid(muni_code = main$muni_code %>% unique(), 
            year = 1958:2020) %>%
  # Join in winning party voteshare
  left_join(by = c("muni_code", "year"),
            y = vote_winner %>% select(muni_code, year, voteshare_winner = voteshare)) %>%
  # Join in LDP party voteshare
  left_join(by = c("muni_code", "year"),
            y = vote_ldp %>% select(muni_code, year, voteshare_LDP = voteshare)) %>%
  # Join in Komeito party voteshare
  left_join(by = c("muni_code", "year"),
            y = vote_komeito %>% select(muni_code, year, voteshare_komeito = voteshare)) %>%
  # Join in voter turnout
  left_join(by = c("muni_code", "year"),
            y = vote_turnout %>% select(muni_code, year, voter_turnout = turnout)) %>%
  # Sort to get results like Tokyo 2020, Tokyo 2019, Tokyo 2018, Tokyo 2017...
  arrange(muni_code, desc(year)) %>%
  # Export to file
  saveRDS("raw_data/vote_timeseries_data.rds")

# Remove excess data
remove(party_vote, vote_ldp, vote_turnout, vote_winner, main, winning_party)


# Fill in data with most recent year's data
lowerhouse = read_rds("raw_data/vote_timeseries_data.rds") %>%
  # Keep just rows on or after 2000
  filter(year >= 2000) %>%
  arrange(muni_code, desc(year)) %>%
  group_by(muni_code) %>%
  # Fill in
  fill(voteshare_winner:voter_turnout, .direction = "downup") %>%
  # Specify that all this comes from the Lower House election
  rename(voteshare_winner_house = voteshare_winner,
         voteshare_LDP_house = voteshare_LDP,
         voteshare_Komeito_house = voteshare_komeito,
         voter_turnout_house = voter_turnout)  %>%
  mutate(voteshare_LDP_Komeito_house = voteshare_LDP_house + voteshare_Komeito_house)


#### Prefectural Elections Data ################################################

# First, we import Ryota Natori's dataset of elections from 2003 to the present.


pref <- c("raw_data/pref_elections/data2003.xlsx",
          "raw_data/pref_elections/data2007.xlsx",
          "raw_data/pref_elections/data2011.xlsx",
          "raw_data/pref_elections/data2015.xlsx") %>%
  map_dfr(~readxl::read_excel(., col_types = "text") %>%
            select(
              year = `選挙年`,
              pref_code = `都道府県コード`,
              muni_code = `市区町村コード`,
              district = `郡名`,     
              electoral_district = `県選挙区名`,
              muni = `市町村名`,
              eligible_voters = `有権者数`,
              votes_cast = `投票者数`,
              votes_cast_valid = `有効投票数`,
              num_candidates = `候補者数`,
              candidate_votes_here = `得票数`,
              candidate_votes_total = `得票総数`,
              name = `名前`,
              age = `年齢`,
              party = `党派`,
              status = `現新`,
              rank = `順位`)) %>%
  # Code these variables as numeric
  mutate_at(vars(year, eligible_voters, votes_cast, votes_cast_valid,
                 num_candidates, candidate_votes_here, candidate_votes_total, age), 
            as.numeric)  %>%
  # Code these variables as character
  mutate_at(vars(pref_code, muni_code, district, electoral_district, muni,
                 name, party, status), as.character) %>%
  # Turn numeric muni code into a five digit code
  mutate(muni_code = muni_code %>% str_pad(width = 5, side = "left", pad = "0")) %>%
  # Now fix municipality names
  # If the muni has a KU but not SHI, add it
  mutate(muni_name = case_when(
    # If the municipality name has a WARD (区),     # But does not have a CITY (市),
    # Please replace the municipality name with both the district and the muni values
    str_detect(muni, "区") & str_detect(muni, "市", negate = TRUE) ~ paste(district, muni, sep = ""),
    TRUE ~ muni)) %>%
  # Now, some outcomes have been recorded for each municipal ward instead of municipality
  # Let's join in the correct municipality code for each ward
  left_join(by = c("muni_code" = "ward_code"),
            y = read_rds("raw_data/wards_municipalities_conversion.rds") %>%
              select(ward_code, muni_code_new = muni_code)) %>%
  # If the new municipality code is missing, it means it wasn't a ward; fill it in with its original municipality code. 
  mutate(muni_code_new = if_else(is.na(muni_code_new), muni_code, muni_code_new))

# Next, we collect voter turnout from 2000-2018 
# (the most recent year we got census data for.)
# Since election data is only collected every election cycle, 
# we fill in in-between years with the most recent observations for that town.
# In the case of years 2000-2003, we fill this in with the value from 2003.
# This is a caveat to the data collection process, 
# but we assume that voter turnout did not change so substantially 
# in this period to dramatically change patterns.


# Create a complete grid of all municipality codes from 2000-2018
voter_turnout_pref = expand_grid(read_rds("raw_data/muni_code.rds") %>% select(muni_code), 
                                 year = as.numeric(2000:2018)) %>%
  # Join in summarized election data by municipality code and year
  left_join(by = c("muni_code" = "muni_code_new", "year"),
            # Summarize election data
            y = pref %>%
              # Grabbing just the eligible voters and votes cast
              select(year, muni_code_new, muni_code, eligible_voters, votes_cast) %>%
              # Zooming into just distinct observations,
              distinct() %>%
              # For each year and town,
              group_by(year, muni_code_new) %>%
              # Calculating the total votes cast and eligible voters
              # And then calculating the voter turnout based on that
              summarize(votes_cast = sum(votes_cast, na.rm = TRUE),
                        eligible_voters = sum(eligible_voters, na.rm = TRUE)) %>%
              mutate(voter_turnout = votes_cast / eligible_voters)) %>%
  ungroup() %>%
  # Next, order them by town and by year, like Tokyo 2020, Tokyo 2019, Tokyo 2018
  arrange(muni_code, desc(year)) %>%
  group_by(muni_code) %>%
  # Now take the most recent value for each town, fill in missing cases below it, and then fill up too.
  # This means that we can easily catch voter turnout from elections held in different years
  # It also means that we can approximate voter turnout in years not recorded.
  # It also means that we can preserve towns which NEVER reported data, and then impute those missing data points later.
  fill(voter_turnout, .direction = "downup") %>%
  # rename
  rename(voter_turnout_pref = voter_turnout)

#voter_turnout_pref %>% summary()


# Then, we collect votes for the winning party's coalition and for the Liberal Democratic Party, 
# to represent voters' pull on their elected officials. 
# We fill in missing data in prior years in the same way as above.

voteshare_pref = expand_grid(read_rds("raw_data/muni_code.rds") %>% select(muni_code), 
                             year = as.numeric(2000:2018)) %>%
  # Join in summarized election data by municipality code and year
  left_join(by = c("muni_code" = "muni_code_new", "year"),
            y = pref %>%
              # Zoom into the candidates who WON.
              # How much did that town support them?
              filter(rank == 1) %>%
              select(year, muni_code_new, muni_code, votes_cast_valid, candidate_votes_here, party) %>%
              group_by(year, muni_code_new, muni_code) %>%
              # Now let's aggregate from the urban ward level to the municipality level
              group_by(year, muni_code_new, party) %>%
              summarize(votes_cast_valid = sum(votes_cast_valid, na.rm = TRUE),
                        candidate_votes_here = sum(candidate_votes_here, na.rm = TRUE)) %>%
              # Finally, calculate voteshare directly for that candidate here.
              mutate(voteshare = candidate_votes_here / votes_cast_valid) %>%
              # recode party names into English
              mutate(party = party %>% dplyr::recode(
                "自" = "LDP", 
                "公" = "Komeito", 
                "民" = "DPJ")) %>%
              filter(party %in% c("LDP", "Komeito", "DPJ")) %>%
              # Pivot into a wider matrix
              pivot_wider(
                id_cols = c(year, muni_code_new),
                names_from = party,
                values_from = voteshare,
                values_fill = list(voteshare = 0)) %>%
              mutate(LDP_Komeito = LDP + Komeito) %>%
              mutate(winning_party = case_when(
                year >= 2000 & year <= 2008 & LDP_Komeito > 0 ~ LDP_Komeito,
                year >= 2009 & year <= 2011 & DPJ > 0 ~ DPJ,
                year >= 2012 & year <= 2018 & LDP_Komeito > 0 ~ LDP_Komeito,
                TRUE ~ 0))) %>%
  # arrange cities reverse chronologically, like Tokyo 2020, Tokyo 2019, Tokyo 2018...
  arrange(muni_code, desc(year)) %>%
  group_by(muni_code) %>%
  fill(LDP:winning_party, .direction = "downup")  %>%
  # Rename
  rename(voteshare_LDP_pref = LDP,
         voteshare_Komeito_pref = Komeito,
         voteshare_DPJ_pref = DPJ,
         voteshare_winner_pref = winning_party) 


expand_grid(read_rds("raw_data/muni_code.rds") %>% select(muni_code), year = 2000:2018) %>%
  # Join in prefectural voter turnout
  left_join(by = c("muni_code", "year"),
            y = voter_turnout_pref)  %>%
  # Join in prefectural voteshare
  left_join(by = c("muni_code", "year"),
            y = voteshare_pref) %>%
  # Join in Lower House data
  left_join(by = c("muni_code", "year"),
            y = lowerhouse) %>%
  # Infinites mean NA
  mutate_at(vars(voter_turnout_pref:voteshare_LDP_Komeito_house),
            list(~if_else(is.infinite(.), NA_real_, as.numeric(.)))) %>%
  # Fill in any voteshare NA with 0 if it had a vaild turnout cell,
  # Because that just means that they did not support a winning candidate,
  mutate_at(vars(voteshare_LDP_pref, voteshare_Komeito_pref,
                 voteshare_DPJ_pref, voteshare_winner_pref),
            list(~if_else(is.na(.) & !is.na(voter_turnout_pref), 0, .))) %>%
  mutate_at(vars(voteshare_LDP_house, voteshare_Komeito_house,
                 voteshare_LDP_Komeito_house, voteshare_winner_house),
            list(~if_else(is.na(.) & !is.na(voter_turnout_house), 0, .))) %>%
  select(-votes_cast, -eligible_voters) %>%
  arrange(muni_code, desc(year) ) %>%
  # Fill in most recent year with estimate
  group_by(muni_code) %>%
  fill(voter_turnout_pref:voteshare_LDP_Komeito_house, .direction = "downup")   %>%
  ungroup() %>%
  saveRDS("raw_data/voting_data.rds")


# Remove extraneous data
rm(list = ls())



### Mergers ################################################

# Merger Crosswalk, 2000 to 2022
# https://www.e-stat.go.jp/municipalities/cities/absorption-separation-of-municipalities
dat <- read_csv("raw_data/merger_crosswalk.csv", 
                locale = locale(encoding = "SHIFT-JIS",asciify = TRUE))  %>%
  select(code = `標準地域コード`, change = `改正事由`, date = `廃置分合等施行年月日`) %>%
  mutate(date = lubridate::ymd(date)) %>%
  mutate(change = case_when(
    # If a series of towns' numbers were changed to a new number, grab the "changed to a new number' portion
    str_detect(change, "[(][0-9]{5}[)]に編入") ~ str_extract(change, "[(][0-9]{5}[)]に編入") %>% str_extract(., pattern = "[0-9]{5}"),
    # If a single place got name changed to another, grab the new site's code
    str_detect(change, "[(][0-9]{5}[)]に区域変更") ~ str_extract(change, "[(][0-9]{5}[)]に区域変更") %>% str_extract(., pattern = "[0-9]{5}"),
    str_detect(change, "[(][0-9]{5}[)]に郡の区域変更") ~ str_extract(change, "[(][0-9]{5}[)]に郡の区域変更") %>% str_extract(., pattern = "[0-9]{5}"),
    str_detect(change, "[(][0-9]{5}[)]に市制施行") ~ str_extract(change, "[(][0-9]{5}[)]に市制施行") %>% str_extract(., pattern = "[0-9]{5}"),
    str_detect(change, "[(][0-9]{5}[)]に名称変更") ~ str_extract(change, "[(][0-9]{5}[)]に名称変更") %>% str_extract(., pattern = "[0-9]{5}"),
    # If a series of towns were merged and a new city was created, list the code of the new city
    str_detect(change, "[(][0-9]{5}[)]を新設") ~ str_extract(change, "[(][0-9]{5}[)]を新設") %>% str_extract(., pattern = "[0-9]{5}"),
    # Sometimes, towns are merged, but the final town is not a newly made town, just a consolidated one.
    # In this case, it will always be the first town listed.
    # If they mention 新設 and 合併し, grab the first word
    str_detect(change, "を新設") & str_detect(change, "が合併し") ~ 
      str_extract(change, ".*[、]") %>% str_extract(., pattern = "[0-9]{5}"),
    # If their designation was changed, just extract the number - there's only one number listed ever
    str_detect(change, "中核市に移行|特例市に移行") ~ str_extract(change, "[0-9]{5}"),
    TRUE ~ change))  %>%
  mutate(year = str_sub(date, 1,4) %>% as.numeric()) %>% 
  select(code, change, year)


# Find codes where updated codes still include the original
samecode <- dat %>%
  filter(code == change) %>%
  select(code) %>% distinct() %>% unlist()

# Ignore code updates when the updated codes include the original;
# eg. it's probably best to assign Kagoshima's value to Kagoshima, rather than another, even if a merge was involved.

dat %>%
  filter(!code %in% samecode) %>%
  mutate(year = year - 1) %>% 
  mutate(premerge = 1) %>% distinct() %>%
  saveRDS("raw_data/merger_crosswalk_cleaned.rds")




### Inflation ################################################

# Also, let's add in the inflation rate, 
# compared to the previous year, for each year. 
# Many calculators are available online, 
# [like this one.](https://www.inflationtool.com/japanese-yen/2000-to-present-value?amount=1000). 

# Let's record the expected value of 1000 yen from year X in year 2020. 
# Make the rate *per yen*, not per 1000 yen. Each measure is original measured in 1000s of yen. 
# It is useful to us to keep our measures in 1000s of yen,
# lest they become unnecessarily large or detailed, 
# since we don't actually know anything about the 10s or 100s of yen. 
# Since each tally was recorded in a specific year, we must first convert it to 2020 yen,
# where we multiple by the number of yen you'd get in 2020 based on having 1 yen in year X.

list("2000" = 1030.84, "2001" = 1035.01, "2002" = 1047.72, "2003" = 1050.95,
     "2004" = 1055.29, "2005" = 1053.11, "2006" = 1057.47, "2007" = 1054.20,
     "2008" = 1046.65, "2009" = 1042.39, "2010" = 1060.75, "2011" = 1064.06,
     "2012" = 1066.28, "2013" = 1068.85, "2014" = 1050.95, "2015" = 1026.71,
     "2016" = 1025.68, "2017" = 1022.60, "2018" = 1011.49, "2019" = 1008.50) %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "year", 
               values_to = "inflation") %>%
  mutate(year = as.numeric(year),
         inflation = inflation / 1000) %>%
  saveRDS("raw_data/inflation.rds")


### Format ################################################

# Next, we're going to take our indicators and format the dataset. 
# I've done a couple of important things below, as labelled.

# 1. Bind the dataframes into a tidy format of city-year-variables, for easy computation.

# 2. Consolidate some spending indicators into more concise variables.  For example, we gathered many types of education spending; we're just going to consolidate that all into a single lump sum of education spending. (These aggregation categories are shown in the ```group``` variable. Note that for variables that weren't part of a specific urban regime and a specific indicator group (eg. education), we reclassified them to each have their own group, so that they do not get aggregated.)

# 3. Pivot the data into a wide matrix, where each row is a city-year, with indicators as columns.

# 4. Fill in variables available for just some years. While spending data is plentiful, some variables are recorded only every few years. I don't use all the variables below, but it's good to collect them. For missing years, I use ```fill`` to attach the most recent valid response for that municipality, first imputing any available data from the past, and then imputing with the most recent future data. For example: 

# - ```pop```: 2000, 2005, 2015, 2020
# - ```inhabitable_area```: 2000-2018
# - ```age_elder``` 2000, 2010, 2015, 2020
# - ```age_median```: 2010, 2015
# - ```output_manuf```: 2000-2018
# - ```output_com```: 2001, 2003, 2006, 2011, 2013, 2015
# - ```employees```: 2009, 2014
# - ```employees_primary```: 2009, 2014 
# - ```employees_secondary```: 2009, 2014
# - ```employees_tertiary```: 2009, 2014

# 5. Adjust for inflation

# 6. Normalize key variables by population, 
# namely spending variables, to get rates of 1000 yen spent per capita.

# 7. Omit municipalities in the Fukushima exclusion zone (years 2011 onwards); 
# these communities have experienced very different trajectories 
# from the rest of the country, including other disaster hit municipalities.

# 8. Join in disaster data

# * Note: the code mentions a few parsing errors; 
# this refers to some cells that just contain a "-" or a "--" to denote NAs. 
# The ```parse_numeric()``` function appropriately classifies these as NA.

#########################################################################
# 1. Bind indicators ################################################
#########################################################################
bind_rows(
  read_rds("raw_data/A.rds"),
  read_rds("raw_data/B.rds"),
  read_rds("raw_data/C.rds"),
  read_rds("raw_data/D.rds"),
  read_rds("raw_data/E.rds"),
  read_rds("raw_data/F.rds")) %>%
  rename(code = cat01_code, muni_code = area_code, year = time_name, estimate = n) %>%
  mutate(year = parse_number(year)) %>% 
  mutate(estimate = parse_number(estimate)) %>% 
  left_join(by = c("code"), 
            y = read_rds("raw_data/varnames.rds") %>% select(code, name, group, regime)) %>%
  #########################################################################
# 2. Consolidate indicators ################################################
#########################################################################
# Second, let's aggregate other variables,
# so that our index has a small, hopefully similar number of indicators for each concept.
# If it's a regime, give it the requiste groups for aggregation
# If it's not a regime indicator, just give it a unique name so it doesn't aggregate
mutate(group = if_else(is.na(regime), name, group)) %>%
  group_by(year, muni_code, group) %>%
  summarize(value = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  #########################################################################################
# 3. Make a wide dataframe of indicators ################################################
#########################################################################################
pivot_wider(
  id_cols = c(year, muni_code),
  names_from = group,
  values_from = value) %>%
  ######################################################################################################
# 4. Fill in variables available for just some years ################################################
######################################################################################################
# Fill in population with nearest year
arrange(muni_code, desc(year)) %>%
  group_by(muni_code) %>%
  # If it says it has an area of zero, it's full of baloney.
  mutate(inhabitable_area = if_else(inhabitable_area == 0, NA_real_, inhabitable_area)) %>%
  fill(pop, inhabitable_area, age_elder, age_youth, age_median, output_manuf, output_com, 
       employees, employees_primary, employees_secondary, employees_tertiary,
       full_college, junior_college, labor_force, unemployed, inmigrants, outmigrants,
       .direction = "downup") %>%
  ungroup() %>%
  # Join in inflation rate and adjust spending measures for inflation
  left_join(by = "year", y = read_rds("raw_data/inflation.rds")) %>%
  select(year, muni_code, pop, inflation, 
         # indicators
         agr, com_manuf, construction, roads_bridges,
         education, edu_social, health, planning, waste, 
         welfare, welfare_aged, welfare_kids, livelihood, unemployment, emergency, housing,
         # Disaster variables
         contains("dis_"),
         # spending controls
         total_spending, rev, rev_to_exp, rev_nat, rev_pref, 
         # Covariates
         inhabitable_area, age_elder, age_youth, age_median, 
         full_college, junior_college, inmigrants, outmigrants, #output_manuf, output_com, 
         income, labor_force, unemployed, employees, 
         employees_primary, employees_secondary, employees_tertiary) %>%
  #############################################################################
# 5. Adjust for Inflation ################################################
#############################################################################
mutate_at(vars(agr:housing, contains("dis_"), rev, rev_nat, rev_pref, total_spending, income),
          list(~.*inflation)) %>%
  #################################################################################################
# 6. Normalized Key Variables by Population ################################################
#################################################################################################
# Normalize per capita
mutate_at(vars(agr:housing, contains("dis_"), rev, rev_nat, rev_pref, total_spending, income), 
          funs(. / pop)) %>%
  # Calculate percentages
  mutate(
    migration = (inmigrants + outmigrants) / pop, #total migration rate
    age_elder = age_elder / pop,
    adult = pop - age_youth, # adult population (eg. NOT 0-17)
    age_youth = age_youth / pop, # % youth
    college = (full_college + junior_college) / adult, # college educated adults
    full_college = full_college / adult, # % some college 
    junior_college = junior_college / adult, # junior college
    unemployed = unemployed / labor_force,
    employees_primary = employees_primary / employees, 
    employees_secondary = employees_secondary / employees,
    employees_tertiary = employees_tertiary / employees,
    # What percentage of your revenue came from the prefecture of national government?
    # (note, since all rates were normalized by population, population cancels out here, so it's fine)
    rev_external = (rev_pref + rev_nat) / rev) %>%
  # Two municipalities report impossibly high education rates (>100%)
  # Rather than bound the variable at 1, it's better to treat these as random data errors, 
  # and impute them later, since I don't truly believe the education rate could be accurate here. 
  mutate_at(vars(college, full_college, junior_college),
            list(~case_when(muni_code %in% c("01601", "09206") ~ NA_real_, TRUE ~ .))) %>%
  ##################################################################################################
# 7. Omit Fukushima Exclusion Zone municipalities ################################################
#     (not a very good comparison for other municipaliteis) 
#################################################################################################
# Identify Fukushima exclusion zone sites
mutate(fukushima = case_when(
  year >= 2011 & muni_code %in% c(
    "07564", #	Fukushima-ken Iitate-mura 
    "07308", #	Fukushima-ken Kawamata-machi
    "07211", #	Fukushima-ken Tamura-shi
    "07541", #	Fukushima-ken Hirono-machi
    "07542",  # Fukushima-ken Naraha-machi			
    "07543",  # Fukushima-ken Tomioka-machi			
    "07544",  # Fukushima-ken Kawauchi-mura			
    "07545",  # Fukushima-ken Okuma-machi			
    "07546",  # Fukushima-ken Futaba-machi			
    "07547",  # Fukushima-ken Namie-machi			
    "07548") ~ 1,  # Fukushima-ken Katsurao-mura
  TRUE ~ 0)) %>%
  # Filter these out, and any cities with a population of 0. 
  # These are unusual places that should not be compared with the average city.
  filter(fukushima == 0, pop > 0) %>%
  select(-fukushima) %>%
  ##################################################################################################
# 8. Join in 2011 disaster outcome data for relevant cities in or after 2011 ################################################
#################################################################################################
# Join in the following variables
mutate(post = if_else(year >= 2011, 1, 0)) %>%
  left_join(by = c("muni_code" = "code", "post"),
            y = read_rds("raw_data/disaster_data.rds") %>% 
              mutate(post = 1) %>% select(code, post, damages, deaths, tsunami)) %>%
  select(-post) %>%
  mutate_at(vars(damages, deaths, tsunami), list(~if_else(is.na(.), 0, as.numeric(.)))) %>%
  mutate_at(vars(deaths, damages), list(~./pop*100000)) %>%
  ##############################################################################################################################
# 10. Drop former observations in their final year which lack spending data. ################################################
##############################################################################################################################
left_join(by = "muni_code", y = read_rds("raw_data/muni_code.rds")) %>%
  group_by(muni_code) %>%
  # If it's a former municipality in its last year of data,
  # BUT it's missing data points across EACH of 3 different spending types,
  # it's missing data across ALL spending types. 
  # This is because it's their last year when they were reconsolidated.
  # Some cities that reconsolidate have complete data (they probably merged at the end of the year)
  # but these do not, and are not really 'complete' or even 'completable' or 'comparable'
  # observations; they should be dropped to not be duplicative, 
  # since their newer municipality now contains their spending data.
  mutate(cutit = if_else(str_detect(muni, "(former)") & year == max(year) & 
                           # And 
                           is.na(agr) & is.na(welfare) & is.na(health), 1, 0)) %>%
  ungroup() %>%
  filter(cutit == 0) %>%
  select(-cutit) %>%
  ####################################################################################################
# 9. Get post-merger municipality codes ################################################
###################################################################################################
# These are all ~40,000 muni_codes, coded by year
# Using the year of the change,
left_join(by = c("muni_code" = "code", "year"), 
          # Join in the new municipality code,
          # plus an indicator that a merge happened the next year
          y = read_rds("raw_data/merger_crosswalk_cleaned.rds")) %>%
  arrange(muni_code, desc(year)) %>%
  # Later, let's also fill in all preceding values with that new municpality code,
  # So that we can match that municipality to the town.
  ungroup() %>%
  group_by(muni_code) %>%
  fill(change, .direction = "down") %>%
  ungroup() %>%
  mutate(change = if_else(!is.na(change), change, muni_code)) %>%
  ####################################################################################################
# 10. Join voting data ################################################
###################################################################################################
# CONFIRM
left_join(by = c("change", "year"),
          y = read_rds("raw_data/voting_data.rds")) %>%
  ####################################################################################################
# 11. Join social capital data ################################################
###################################################################################################
mutate(yearjoin = if_else(year == 2018, 2017, year)) %>%  
  # Join 2017's value to 2018, since we've just got from 2000 to 2017
  left_join(by = c("change" = "muni_code", "yearjoin" = "year"),
            y = read_rds("raw_data/sci.rds")) %>%
  select(-yearjoin) %>%
  select(-muni) %>%
  saveRDS("raw_data/indicators.rds")


# Finally, let's clear all data to get a clean environment
rm(list = ls())

