#' @name 06_table_1.R
#' @author Tim Fraser

## 4.6 Table 1: Regime Lit
library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(tidyr)


tab <- bind_rows(

  # Developmental
  tibble(
    Regime = c("Developmental"),
    Definition = c("promotes economic growth while preventing economic decline"),
    Examples = c("Atlanta (Stone 1989)"),
    Related = c("Entrepreneurial Regimes (Euchner 1993)
                 \n
                 Public-Private Partnerships (Davies 2017)
                 \n
                 Player (Portz 1990)
                 \n
                 Castle Towns (Funabashi 2006; Hill & Fujita 1993)"),
    `Japanese Examples` = "Tokyo (Saito 2003; Tsukamoto 2012; Sorensen et al. 2010)
    \n
    Kitakyushu (Yeum 2002)
    \n
    Minamata (Funabashi 2006)
    \n
    Kobe Post-1995 (Edgington 2010)"),
  
  # Middle Class
  tibble(
    Regime = c("Middle-Class"),
    Definition = c("promotes egalitarian policies in education, health, environment, and city planning"),
    Examples = c("Santa Cruz (Gendron & Domhoff 2018)"),
    Related = c("Progressive (Stone 1989)
                \n
                'Anti-Regime' (DeLeon 1992)"),
    `Japanese Examples` = "Mitaka (Takao 2006)
                           \n
                           Kyoto (Sugiyama & Takeuchi 2008)
                           \n
                           Iida (Fraser et al. 2020)"),
  # Social Welfare
  tibble(
    Regime = c("Social Welfare"),
    Definition = c("improves conditions for working class, expands social safety net"),
    Examples = c("Early Toronto (Mahon 2007)"),
    Related = c("Opportunist (Stone 1989)
                 \n
                 Activist (Clark 2001)
                 \n
                 Stewardship Regimes (Nissen 1995)"),
    `Japanese Examples` = "Yokohama (Hayashi 2013)
                           \nMikura Ward, Kobe (Yasui 2007)"),
  
  # Caretaker
  tibble(
    Regime = c("Caretaker"),
    Definition = c("maintains status quo, traditional municipal service provision"),
    Examples = c("New Orleans (Whelan et al. 1994)"),
    Related = c("Maintenance/Status Quo (Stone 1989)
                 \n
                 Bystander (Portz 1990)
                 \n
                 Austerity (Davies & Blanco 2017)"),
    `Japanese Examples` = " - ")
)


require(knitr)
require(kableExtra)

viz = function(data){
  kbl(data, booktabs=TRUE,label = NA,
      caption = "\\textbf{Table \\ref{tab:regimelit}: \\label{tab:regimelit}{Typology of Urban Regimes}}") %>%
    kable_styling(full_width = TRUE, latex_options = "striped") %>%
    column_spec(column = 1, width = "2cm")  
}

list(viz = viz, tab = tab) %>%
  saveRDS(file = "table/table_lit.rds")

rm(list = ls())
