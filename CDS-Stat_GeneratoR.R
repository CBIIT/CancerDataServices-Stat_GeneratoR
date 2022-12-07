#!/usr/bin/env Rscript

#Cancer Data Services - Stat_GeneratoR R v2.0.1


##################
#
# USAGE
#
##################

#This takes a validated indexed CDS template, and/or the newest version of dbGaP submission for the study.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CDS-Stat_GeneratoR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","readr","stringi","readxl","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="A validated and indexed CDS template file (.xlsx, .tsv, .csv)", metavar="character"),
  make_option(c("-c", "--subject_consent"), type="character", default=NULL, 
              help="A dbGaP subject_consent data file (SC_DS)", metavar="character"),
  make_option(c("-a", "--sample_attribute"), type="character", default=NULL, 
              help="A dbGaP sample_attribute data file (SA_DS)", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCDS-Stat_GeneratoR v2.0.1")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$subject_consent)&is.null(opt$sample_attribute)){
  print_help(opt_parser)
  cat("Please supply an input file.\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Null the paths for logic gates later in script
file_path=NULL
subcon_path=NULL
samatt_path=NULL

#Data file pathway
if (!is.null(opt$file)){
  file_path=file_path_as_absolute(opt$file)
}

#Subject_consent file pathway
if (!is.null(opt$subject_consent)){
  subcon_path=file_path_as_absolute(opt$subject_consent)
}

#Sample_attribute file pathway
if (!is.null(opt$sample_attribute)){
  samatt_path=file_path_as_absolute(opt$sample_attribute)
}

#A start message for the user that the validation is underway.
cat("The data file is being validated at this time.\n")


###############
#
# Start write out
#
###############

#Make sure if the file_path is empty, it can pass on some information to setup out files.
file_path_null=FALSE
if (is.null(file_path)){
  file_path_null=TRUE
  if (is.null(subcon_path)){
    file_path=samatt_path
  }else{
    file_path=subcon_path
  }
}

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_Stats",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")

#Read in metadata page/file to check against the expected/required properties. 
#Logic has been setup to accept the original XLSX as well as a TSV or CSV format.
if (file_path_null==FALSE){
  if (ext == "tsv"){
    df=suppressMessages(read_tsv(file = file_path, guess_max = 1000000, col_types = cols(.default = col_character())))
  }else if (ext == "csv"){
    df=suppressMessages(read_csv(file = file_path, guess_max = 1000000, col_types = cols(.default = col_character())))
  }else if (ext == "xlsx"){
    df=suppressMessages(read_xlsx(path = file_path,sheet = "Metadata", guess_max = 1000000, col_types = "text"))
  }else{
    stop("\n\nERROR: Please submit a data file that is in either xlsx, tsv or csv format.\n\n")
  }
}


if (!is.null(subcon_path)){
    df_subcon=suppressMessages(read_tsv(file = subcon_path, guess_max = 1000000, col_types = cols(.default = col_character())))
}

if (!is.null(samatt_path)){
  df_samatt=suppressMessages(read_tsv(file = samatt_path, guess_max = 1000000, col_types = cols(.default = col_character())))
}

cat(paste("This is a validation output for ",file_name,".\n\n",sep = ""))


############
#
# Stat generation
#
############

#Start writing in the outfile.
sink(paste(path,output_file,".txt",sep = ""))

if (file_path_null==FALSE){
  #number of unique participants in the submission
  participant_count=length(unique(df$participant_id))
  
  #number of unique samples in the submission
  sample_count=length(unique(df$sample_id))
  
  #number of unique files in the submission
  file_count=length(unique(df$url))
  
  #file size in Tb in the submission
  file_size=sum(as.numeric(df$file_size),na.rm = T)/1e12
  
  #number of each file type
  file_type_count=count(group_by(df,file_type))
  
  #number of each gender in the submission
  gender_count=count(group_by(unique(select(df, participant_id,gender)), gender))
  
  #number of each race in the submission
  race_count=count(group_by(unique(select(df, participant_id,race)), race))
  
  #number of each ethnicity in the submission
  ethnicity_count=count(group_by(unique(select(df, participant_id,ethnicity)), ethnicity))
  
  #number of each sample type in the submission
  sample_type_count=count(group_by(select(count(group_by(df,sample_id,sample_type)),-n),sample_type))
  
  #number of each library_strategy in the submission
  library_strategy_count=count(group_by(df,library_strategy))
  
  #number of each library_source in the submission
  library_source_count=count(group_by(df,library_source))
  
  #number of each sample_anatomic_site in the submission
  anatomic_site_count=count(group_by(select(count(group_by(df,sample_id,sample_anatomic_site)),-n),sample_anatomic_site))
  
  #number of each primary_diagnosis in the submission
  primary_diagnosis_count=count(group_by(select(count(group_by(df,sample_id,primary_diagnosis)),-n),primary_diagnosis))
  
  #number of each disease_type in the submission
  disease_type_count=count(group_by(select(count(group_by(df,sample_id,disease_type)),-n),disease_type))
  
  
  #########
  #
  # Stats output
  #
  #########
  
  cat("Below is the stat output file for: ",file_name,"\n\n",sep = "")
  
  cat("Number of participants: ",participant_count,"\n",
      "Number of samples: ", sample_count, "\n",
      "Number of Files: ", file_count,"\n",
      "File size (Tb): ",file_size,"\n",sep = "")
  
  cat("\nGender:\n")
  for (x in 1:dim(gender_count)[1]){
    cat("\t",gender_count[x,1][[1]],": ",gender_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nRace:\n")
  for (x in 1:dim(race_count)[1]){
    cat("\t",race_count[x,1][[1]],": ",race_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nEthnicity:\n")
  for (x in 1:dim(ethnicity_count)[1]){
    cat("\t",ethnicity_count[x,1][[1]],": ",ethnicity_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nSample Type:\n")
  for (x in 1:dim(sample_type_count)[1]){
    cat("\t",sample_type_count[x,1][[1]],": ",sample_type_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nLibrary Strategy:\n")
  for (x in 1:dim(library_strategy_count)[1]){
    cat("\t",library_strategy_count[x,1][[1]],": ",library_strategy_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nLibrary Source:\n")
  for (x in 1:dim(library_source_count)[1]){
    cat("\t",library_source_count[x,1][[1]],": ",library_source_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nSample Anatomic Site:\n")
  for (x in 1:dim(anatomic_site_count)[1]){
    cat("\t",anatomic_site_count[x,1][[1]],": ",anatomic_site_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nPrimary Diagnosis:\n")
  for (x in 1:dim(primary_diagnosis_count)[1]){
    cat("\t",primary_diagnosis_count[x,1][[1]],": ",primary_diagnosis_count[x,"n"][[1]],"\n",sep = "")
  }
  
  cat("\nDisease Type:\n")
  for (x in 1:dim(disease_type_count)[1]){
    cat("\t",disease_type_count[x,1][[1]],": ",disease_type_count[x,"n"][[1]],"\n",sep = "")
  }
  
}else{
  cat("\n\nFor indepth stats for a specific submission, please submit the indexed manifest.\n")
}


############
#
# Stat generation
#
############

#Stats for subcon file
if (!is.null(subcon_path)){
  
  cat("\n\nBelow is the stat output file for: ",basename(subcon_path),"\n\n",sep = "")
  
  participant_subcon_count=length(unique(df_subcon$SUBJECT_ID))
  
  gender_subcon_count=count(group_by(unique(df_subcon),SEX))
  gender_subcon_count$SEX[grep(pattern = TRUE, x = gender_subcon_count$SEX %in% "1")]<-"Male"
  gender_subcon_count$SEX[grep(pattern = TRUE, x = gender_subcon_count$SEX %in% "2")]<-"Female"
  
  
#########
#
# Stats output
#
#########
  
  cat("Cumulative number of participants: ",participant_subcon_count,"\n",sep = "")
  
  cat("\nCumulative Gender:\n")
  for (x in 1:dim(gender_subcon_count)[1]){
    cat("\t",gender_subcon_count[x,1][[1]],": ",gender_subcon_count[x,"n"][[1]],"\n",sep = "")
  }
}else{
  cat("\n\nFor cumulative stats of the subjects, please submit the dbGaP subject_consent data set file.\n")
}


############
#
# Stat generation
#
############

#Stats for samatt file
if (!is.null(samatt_path)){
  
  cat("\n\nBelow is the stat output file for: ",basename(samatt_path),"\n\n",sep = "")
  
  sample_samatt_count=length(unique(df_samatt$SAMPLE_ID))
  
  sample_type_samatt_count=count(group_by(unique(df_samatt),SAMPLE_TYPE))


#########
#
# Stats output
#
#########

  cat("Cumulative number of samples: ",sample_samatt_count,"\n",sep = "")
  
  cat("\nCumulative Sample Type:\n")
  for (x in 1:dim(sample_type_samatt_count)[1]){
    cat("\t",sample_type_samatt_count[x,1][[1]],": ",sample_type_samatt_count[x,"n"][[1]],"\n",sep = "")
  }
}else{
  cat("\n\nFor cumulative stats of the samples, please submit the dbGaP sample_attributes data set file.\n")
}


sink()

cat("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")

