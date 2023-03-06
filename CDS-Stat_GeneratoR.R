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
list_of_packages=c("dplyr","readr","stringi","readxl","ggplot2","ggthemes","gridExtra","viridis","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(ggplot2,verbose = F))
suppressMessages(library(ggthemes,verbose = F))
suppressMessages(library(gridExtra,verbose = F))
suppressMessages(library(viridis,verbose = F))
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

#A start message for the user that the stats generation is underway.
cat("The data file stats are being generated at this time.\n")


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

cat(paste("This is a stats output for ",file_name,".\n\n",sep = ""))


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
  file_count=length(unique(df$file_url_in_cds))

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

  cat("\nFile Type:\n")
  for (x in 1:dim(file_type_count)[1]){
    cat("\t",file_type_count[x,1][[1]],": ",file_type_count[x,"n"][[1]],"\n",sep = "")
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

  #close file
  sink()
  
###############
#
# Create Figures for values
#
###############
  
  #Manipulate data frames to create graph-able data sets
  df_gen_stats=tibble('participants'=participant_count,'samples'=sample_count,'files'=file_count)
  df_gen_stats=t(df_gen_stats)
  stat_col=rownames(df_gen_stats)
  df_gen_stats=tibble(Count=df_gen_stats[,1])
  df_gen_stats$Stat=stat_col
  df_gen_stats$Size=NA
  df_gen_stats$Size[grep(pattern = "files",x = df_gen_stats$Stat)]=paste(round(file_size,2), " Tb",sep = "")
  
  #Cleaning of general data frames to include an 'ingest' type and turn NA's into "NA"s
  file_type_count$ingest="File Type"
  file_type_count[is.na(file_type_count)]<-"NA"
  gender_count$ingest="Gender"
  gender_count[is.na(gender_count)]<-"NA"
  race_count$ingest="Race"
  race_count[is.na(race_count)]<-"NA"
  ethnicity_count$ingest="Ethnicity"
  ethnicity_count[is.na(ethnicity_count)]<-"NA"
  sample_type_count$ingest="Sample Type"
  sample_type_count[is.na(sample_type_count)]<-"NA"
  library_strategy_count$ingest="Library Strategy"
  library_strategy_count[is.na(library_strategy_count)]<-"NA"
  library_source_count$ingest="Library Source"
  library_source_count[is.na(library_source_count)]<-"NA"
  anatomic_site_count$ingest="Anatomic Site"
  anatomic_site_count[is.na(anatomic_site_count)]<-"NA"
  primary_diagnosis_count$ingest="Primary Diagnosis"
  primary_diagnosis_count[is.na(primary_diagnosis_count)]<-"NA"
  disease_type_count$ingest="Disease Type"
  disease_type_count[is.na(disease_type_count)]<-"NA"
  
  
  #Fixes for common datasets that have over 10 values
  if (dim(file_type_count)[1]>11){
    file_type_count=
      file_type_count%>%
      arrange(desc(n))
    cutoff_n=file_type_count$n[10]
    other_df=filter(file_type_count, n<cutoff_n)
    other_value=sum(other_df$n)
    file_type_count=filter(file_type_count, n>=cutoff_n)
    file_type_count_add=tibble(file_type="Other",n=other_value,ingest="File Type")
    file_type_count=rbind(file_type_count,file_type_count_add)
  }
  
  if (dim(primary_diagnosis_count)[1]>11){
    primary_diagnosis_count=
      primary_diagnosis_count%>%
      arrange(desc(n))
    cutoff_n=primary_diagnosis_count$n[10]
    other_df=filter(primary_diagnosis_count, n<cutoff_n)
    other_value=sum(other_df$n)
    primary_diagnosis_count=filter(primary_diagnosis_count, n>=cutoff_n)
    primary_diagnosis_count_add=tibble(primary_diagnosis="Other",n=other_value,ingest="Primary Diagnosis")
    primary_diagnosis_count=rbind(primary_diagnosis_count,primary_diagnosis_count_add)
  }
  
  if (dim(disease_type_count)[1]>11){
    disease_type_count=
      disease_type_count%>%
      arrange(desc(n))
    cutoff_n=disease_type_count$n[10]
    other_df=filter(disease_type_count, n<cutoff_n)
    other_value=sum(other_df$n)
    disease_type_count=filter(disease_type_count, n>=cutoff_n)
    disease_type_count_add=tibble(disease_type="Other",n=other_value,ingest="Disease Type")
    disease_type_count=rbind(disease_type_count,disease_type_count_add)
  }
  
  
  
  #Plot general stats and file type information
  
  plot_gen_stats=ggplot(data = df_gen_stats, mapping = aes(x= Stat, y=Count, fill=Stat))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_col(alpha=0.5)+
    geom_text(aes(label= Count, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_label(mapping = aes(label=Size,  vjust=0),alpha=0, size=5, label.size = NA)+
    guides(fill=guide_legend(title = "Statistics"))+
    theme_few()+
    ggtitle("Counts for Submission")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  plot_file_stats=ggplot(data = file_type_count, mapping = aes(x=ingest, y=n, fill=file_type))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(file_type_count$n), y=sum(file_type_count$n)+(sum(file_type_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "File Type"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  #save plot
  suppressWarnings(ggsave(plot = grid.arrange(plot_gen_stats, plot_file_stats, nrow=1) , filename =paste(output_file,"_gen_stats.png",sep = ""),path = path, width = 16, height = 8, units = "in", device = "png"))
  

  #Create plots for gender, race, and ethnicity.

  plot_gender_stats=ggplot(data = gender_count, mapping = aes(x=ingest, y=n, fill=gender))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(gender_count$n), y=sum(gender_count$n)+(sum(gender_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Gender"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot_race_stats=ggplot(data = race_count, mapping = aes(x=ingest, y=n, fill=race))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(race_count$n), y=sum(race_count$n)+(sum(race_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Race"))+
    theme_few()+
    ggtitle("Counts for Submission")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot_ethnicity_stats=ggplot(data = ethnicity_count, mapping = aes(x=ingest, y=n, fill=ethnicity))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(ethnicity_count$n), y=sum(ethnicity_count$n)+(sum(ethnicity_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Ethnicity"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  suppressWarnings(ggsave(plot = grid.arrange(plot_gender_stats, plot_race_stats, plot_ethnicity_stats, nrow=1) , filename =paste(output_file,"_demo_stats.png",sep = ""),path = path, width = 16, height = 8, units = "in", device = "png"))

  
  #Create plots for sample_types, strategy and source.

  plot_type_stats=ggplot(data = sample_type_count, mapping = aes(x=ingest, y=n, fill=sample_type))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(sample_type_count$n), y=sum(sample_type_count$n)+(sum(sample_type_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Sample Type"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot_strategy_stats=ggplot(data = library_strategy_count, mapping = aes(x=ingest, y=n, fill=library_strategy))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(library_strategy_count$n), y=sum(library_strategy_count$n)+(sum(library_strategy_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Library Strategy"))+
    theme_few()+
    ggtitle("Counts for Submission")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot_source_stats=ggplot(data = library_source_count, mapping = aes(x=ingest, y=n, fill=library_source))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(library_source_count$n), y=sum(library_source_count$n)+(sum(library_source_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Library Source"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  suppressWarnings(ggsave(plot = grid.arrange(plot_type_stats, plot_strategy_stats, plot_source_stats, nrow=1) , filename =paste(output_file,"_library_stats.png",sep = ""),path = path, width = 16, height = 8, units = "in", device = "png"))


  #Create plots for anatomic_site, diagnosis, and disease.

  plot_anatomic_stats=ggplot(data = anatomic_site_count, mapping = aes(x=ingest, y=n, fill=sample_anatomic_site))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(anatomic_site_count$n), y=sum(anatomic_site_count$n)+(sum(anatomic_site_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Sample Anatomic Site"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot_diagnosis_stats=ggplot(data = primary_diagnosis_count, mapping = aes(x=ingest, y=n, fill=primary_diagnosis))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(primary_diagnosis_count$n), y=sum(primary_diagnosis_count$n)+(sum(primary_diagnosis_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Primary Diagnosis"))+
    theme_few()+
    ggtitle("Counts for Submission")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot_disease_stats=ggplot(data = disease_type_count, mapping = aes(x=ingest, y=n, fill=disease_type))+
    scale_fill_viridis(discrete=TRUE,option="cividis")+
    geom_bar(alpha=0.5, stat = "identity")+
    geom_text(aes(label = n, fontface="bold"),size=5,
              position = position_stack(vjust=0.5))+
    geom_text(aes(x=ingest, label=sum(disease_type_count$n), y=sum(disease_type_count$n)+(sum(disease_type_count$n)/50)), size=5)+
    guides(fill=guide_legend(title = "Disease Type"))+
    theme_few()+
    ggtitle(" ")+
    labs(y="Count")+
    theme(axis.text=element_text(size=12),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

  suppressWarnings(ggsave(plot = grid.arrange(plot_anatomic_stats, plot_diagnosis_stats, plot_disease_stats, nrow=1) , filename =paste(output_file,"_diagnosis_stats.png",sep = ""),path = path, width = 16, height = 8, units = "in", device = "png"))

  
}else{
  cat("\n\nFor indepth stats for a specific submission, please submit the indexed manifest.\n")
  
  #close file
  sink()
}


############
#
# Stat generation
#
############

#Stats for subcon file
if (!is.null(subcon_path)){
  
  #reopen outfile for writing.
  sink(paste(path,output_file,".txt",sep = ""), append = TRUE)

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
  #close file
  sink()
}else{
  #reopen outfile for writing.
  sink(paste(path,output_file,".txt",sep = ""), append = TRUE)
  
  cat("\n\nFor cumulative stats of the subjects, please submit the dbGaP subject_consent data set file.\n")
  
  #close file
  sink()
}


############
#
# Stat generation
#
############

#Stats for samatt file
if (!is.null(samatt_path)){

  #reopen outfile for writing.
  sink(paste(path,output_file,".txt",sep = ""), append = TRUE)
  
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
  #close file
  sink()
}else{
  #reopen outfile for writing.
  sink(paste(path,output_file,".txt",sep = ""), append = TRUE)
  
  cat("\n\nFor cumulative stats of the samples, please submit the dbGaP sample_attributes data set file.\n")
  
  #close file
  sink()
}


cat("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")

