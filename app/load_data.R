
calls_all = readRDS("calls_all_with_all_info_server.rds")
calls_all=readRDS("calls_all_with_all_info_server_w_fn.rds")
#calls_all = readRDS("calls_all_with_all_info.rds")
d = calls_all
d$SV_sample = d['SV calls file']
d$SV_sample = as.vector(unlist(d$SV_sample))
d$CNV = d['SCNA calls file']
d$CNV = as.vector(unlist(d$CNV))

#names(d)[which(names(d) == "stage_backup")] = "tumor_stage"
#names(d)[3] = "Start coordinate for SV cluster in chr"
#names(d)[4] = "End coordinate for SV cluster in chr"
#names(d)[5] = "Nb. intrachromosomal SVs"
#names(d)[6] = "Total Nb. SVs (intrachr. + interchr.)"
#names(d)[7] = "Nb. SVs in sample"

#print(d$SV_sample)

for (i in 1:ncol(d)){

if (is.numeric(d[,i])){

d[,i] = round(d[,i],digits=3)

}

}

d$histo = as.vector(d$histo)
d$donor_unique_id = as.vector(d$donor_unique_id)

d$chromo_binary="No"
d$chromo_binary[which(d$chromo==1)] = "Yes"

d$type_chromo = as.vector(d$type_chromo)
d$type_chromo[which(d$type_chromo == "NA")] = "No chromothripsis"

###
d$donor_diagnosis_icd10 <- NULL
d$first_therapy_type <- NULL
d$first_therapy_response <- NULL
d$tobacco_smoking_history_indicator <- NULL
d$tobacco_smoking_intensity <- NULL
d$alcohol_history <- NULL
d$alcohol_history_intensity <- NULL
d$response <- NULL
d$radiation <- NULL
d$vital <- NULL
d$donor_tumour_staging_system_at_diagnosis <- NULL
d$stage_backup <- NULL
d$metastasis <- NULL

d$donor_sex         <- NULL
d$donor_vital_status      <- NULL
#d$donor_age_at_diagnosis    <- NULL
d$tumor_stage <- NULL
d$donor_survival_time        <- NULL
d$donor_interval_of_last_followup <- NULL
d$donor_diagnosis_icd10 <- NULL
d$first_therapy_type <- NULL
d$first_therapy_response <- NULL
d$tobacco_smoking_history_indicator <- NULL
d$tobacco_smoking_intensity <- NULL
d$alcohol_history <- NULL
d$alcohol_history_intensity <- NULL
d$response <- NULL
d$radiation <- NULL
d$vital <- NULL
d$donor_tumour_staging_system_at_diagnosis <- NULL
d$stage_backup <- NULL
d$metastasis <- NULL



# clinical data
clinical = readRDS("clinical_data_march.rds")
clinical$percentage_cellularity = NULL
clinical$level_of_cellularity = NULL
clinical$tcga_expert_re.review = NULL
clinical$response <- NULL
clinical$event <- NULL
clinical$code <- NULL
clinical$tissue <- NULL
clinical$percentage_cellularity <- NULL
clinical$project_code <- NULL
clinical$donor_wgs_included_excluded = NULL
names(clinical)[1]="donor_unique_id"


print("comment" %in% names(d))
