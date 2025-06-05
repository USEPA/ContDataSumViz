# Global
#
# Add package from UI and Server
#
# Erik.Leppo@tetratech.com
# 2022-08-17
#~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----

# ## UI----
library(shiny)
library(shinyWidgets)
library(shinyjs) # fails without it
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(plotly)
library(xts)
library(shinycssloaders)
library("shinyjqui")
#
# ## Server----
library("readxl")        # to read excel files
library("writexl")
library("data.table")
library("tidyverse") # fails without dplyr, stringr
library("tibbletime")
library("shinyBS")
library("shinyvalidate")
library("conflicted") # fails without it
library("dataRetrieval")
library("doBy")
library("knitr")
library("htmltools")
library("rmarkdown")
library("highr")
library("survival")
library("shinyFiles")
library("zip")
library("reshape2")
library("ContDataQC")
library("StreamThermal")
library("IHA")
library("daymetr")
library("promises")
library("future")
library("tinytex")
library("padr")
library("openxlsx") #LCN

plan(multisession)

# Functions ----
source("_moved/import_raw_data.R")
source("update_ContDataQC/config.R")
source("update_ContDataQC/CompSiteCDF.updated.R")
source("update_ContDataQC/SumStats.updated.R")
source("update_ContDataQC/ReportMetaData.R")
source("update_ContDataQC/build_summary_updated.R")


source("update_ContDataQC/fun.ConvertDateFormat.R")
source("constants.R")
source("head_tag.R")
source("custom_header.R")
source("custom_footer.R")
source("page_header.R")
source("page_footer.R")

list.files("modules", recursive = TRUE) %>%
purrr::map(~ source(paste0("modules/", .)))


# Other ----
options(shiny.maxRequestSize = 100*1024^2)
