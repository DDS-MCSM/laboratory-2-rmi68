#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Aitor Vivanco - Data Driven Securty                             #
#                                                                              #
#******************************************************************************#

# Si no tenemos el paquete "xml2"
list.of.packages <- c("xml2","stringr", "tidyr", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(xml2)
library(stringr)
library(tidyr)
library(dplyr)

# compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
# cpes_filename <- "cpes.zip"
# download.file(compressed_cpes_url, cpes_filename)
# unzip(zipfile = cpes_filename)
Download <- function(){
  

  if(!file.exists(" cpes.zip ")){
    compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
    cpes_filename <- "cpes.zip"
    download.file(compressed_cpes_url, cpes_filename)
    unzip(zipfile = cpes_filename)
  }
}


GetCPEItems <- function(cpe_xml) {
  
  cpe_nom <- xml2::xml_text(xml2::xml_find_all(cpe_xml, "//*[name()='cpe-item']/@name"))
  cpe_23 <- xml2::xml_text(xml2::xml_find_all(cpe_xml, "//*[cpe-23:cpe23-item]/*/@name"))
  cpe_title <- xml2::xml_text(xml2::xml_find_all(cpe_xml, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]"))
  cpes <- data.frame(cpe_nom, cpe_title, cpe_23, stringsAsFactors = F)
  
  return(cpes)

  # transform the list to data frame

  # return data frame
}


CleanCPEs <- function(cpes){
  col_name <- c("std", "std.v", "parte", "vendedor", "producto",
                 "versión", "update", "edición", "idioma", "sw_edition",
                 "target_sw", "target_hw", "otro")
  
  cpes$cpe_23 <- stringr::str_replace_all(cpes$cpe_23, "\\\\:", ";")
  
  cpes <- tidyr::separate(data = cpes, col = cpe_23, into = col_name, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -std, -std.v)
  
  cpes$vendedor <- as.factor(cpes$vendedor)
  cpes$producto <- as.factor(cpes$producto)
  cpes$idioma <- as.factor(cpes$idioma)
  cpes$sw_edition <- as.factor(cpes$sw_edition)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_hw <- as.factor(cpes$target_hw)

  return(cpes)
}

ParseCPEData <- function() {
  
  cpe.file <- "./official-cpe-dictionary_v2.3.xml"
  
  if(!file.exists(cpe.file)){
    Download()
  }

  # load cpes as xml file
  cpe_xml <- xml2::read_xml(x = cpe.file)

  # get CPEs
  cpes <- GetCPEItems(cpe_xml)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}
