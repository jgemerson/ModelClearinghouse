# Jojo Emerson
# Gates: Open-Source Model Clearinghouse
# October 3, 2017
# data cleaning for clearinghouse data:
#   - import excel file - should eventually be connected to SQL
#   - fixes missing data from initial entries
#   - insert titles and OSF links
#   - create concatenated primary author variable
#   - creates string variables populated by option name for filters: disease, intervention, type, sponsor
#   - creates concatenated display variables for filters: disease, intervention, type, sponsor
#   - concatenate "other text" onto end of filters that include other options

library(readxl)
library(stringr)

#import data
#setwd("G:/CEVR/Projects/Gates Phase 2 - Global Health/03. Tasks/05 - Clearinghouse/06. Database/UI/Clearinghouse")
models_raw<-read_excel("Data/02 04 2019 Update.xlsx", col_names = TRUE)

#DATA CLEANING#
#fix missings
#missing country
models_raw$model_country[models_raw$modeID == "2018-1005"]<-"United States of America (USA)"
models_raw$model_country[models_raw$modeID == "2018-1004"]<-"Multiple countries"
#missing date
models_raw$model_lastupdate[models_raw$modeID == "2018-1015"]<-"January/1/2017"
models_raw$model_lastupdate[models_raw$modeID == "2018-1010"]<-"December/18/2014"
#clean up
#wrong disease
models_raw$disease_NTDmalaria[models_raw$modeID == "2018-1014"]<-0
models_raw$disease_maternal[models_raw$modeID == "2018-1014"]<-1
models_raw$disease_respiratory[models_raw$modeID == "2018-1021"]<-1
models_raw$disease_otherNCD[models_raw$modeID == "2018-1021"]<-0


#standardize softwares
models_raw$model_software[1]<-"Excel, Stata"
models_raw$model_software[2]<-"R"
models_raw$model_software[3]<-"SAS"
models_raw$model_software[4]<-"Stata"
models_raw$model_software[5]<-"TreeAge"
models_raw$model_software[6]<-"C++"
models_raw$model_software[7]<-"C#"
models_raw$model_software[8]<-"C#"
models_raw$model_software[11]<-"C#"

###INSERT TITLES HERE####
#format:   models_raw$title[models_raw$modeID == "INSERT MODEL ID"]<-"INSERT FULL TITLE HERE (NO SPECIAL CHARACTERS)"
colnames(models_raw)[11]<-"ProjectTitle"
models_raw$title<-models_raw$ProjectTitle
models_raw$title[models_raw$modeID == "2018-1012"]<-"Cost-effectiveness of government ART program in Zambia"
models_raw$title[models_raw$modeID == "2018-1014"]<-"Community mobilisation through womens groups and quality improvement in health facilities in Malawi"
models_raw$title[models_raw$modeID == "2018-1013" ]<-"Mass drug administration strategies for schistosomiasis and soil-transmitted helminthiasis in Cote d'Ivoire"
models_raw$title[models_raw$modeID == "2018-1015"]<- "Alcohol Use Disorder"
models_raw$title[models_raw$modeID == "2018-1010" ]<- "Preventing mother to child transmission of HIV ART in Tanzania"
models_raw$title[models_raw$modeID == "2018-1008" ]<- "Bovine Spongiform Encephelopathy"
models_raw$title[models_raw$modeID == "2018-1005"]<- "Long-term impacts of alternative treatments for Hodgkin Lymphoma"
models_raw$title[models_raw$modeID == "2018-1004"]<- "Cost-effectiveness of nutrition-sensitive food programs in India, Nigeria and Ethiopia"
models_raw$title[models_raw$modeID == "2018-1017"]<- "Cost-effectiveness model for hepatitis C screening and treatment in Egypt"
models_raw$title[models_raw$modeID == "2018-1019"]<- "Cost-Effectiveness of Treatments for Open-Angle Glaucoma"
models_raw$title[models_raw$modeID == "2018-1018"]<- "Cost-Effectiveness of Interventions for Diabetic Macular Edema"

###INSERT OSF LINK HERE####
#format:   models_raw$link[models_raw$modeID == "INSERT MODEL ID"]<-"INSERT OSF LINK"
models_raw$link<-""
models_raw$link[models_raw$modeID == "2018-1012"]<-"https://osf.io/xmnvy/"
models_raw$link[models_raw$modeID == "2018-1014"]<-"https://osf.io/8zc5s/"
models_raw$link[models_raw$modeID == "2018-1013" ]<-"https://osf.io/pcf3y/"
models_raw$link[models_raw$modeID == "2018-1015"]<-"https://osf.io/jvayu/"
models_raw$link[models_raw$modeID == "2018-1010" ]<-"https://osf.io/nyjtg/"
models_raw$link[models_raw$modeID == "2018-1008" ]<-"https://osf.io/v9ghe/"
models_raw$link[models_raw$modeID == "2018-1005"]<-"https://osf.io/ke8np/"
models_raw$link[models_raw$modeID == "2018-1004"]<-"https://osf.io/hy543/"
models_raw$link[models_raw$modeID == "2018-1017"]<-"https://osf.io/nqvh7/"
models_raw$link[models_raw$modeID == "2018-1019"]<-"https://osf.io/593wa/"
models_raw$link[models_raw$modeID == "2018-1018"]<-"https://osf.io/6xwv4/"
models_raw$link[models_raw$modeID == "2018-1020"]<-"https://osf.io/38fjm/"
models_raw$link[models_raw$modeID == "2018-1021"]<-"https://osf.io/hwt62/"
models_raw$link[models_raw$modeID == "2019-1000"]<-"https://osf.io/3srh4/"
models_raw$url<-""
models_raw$url <- paste0("<a href='",models_raw$link,"' target='_blank'>",models_raw$link,"</a>")

#create primary author var
models_raw$PrimaryAuthor<-paste(models_raw$author1_first, models_raw$author1_last, sep = " ")

##DISEASE##
#create disease string variable
suppressWarnings({
models_raw$disease_s_cardiovascular[models_raw$disease_cardiovascular == 1]<-"Cardiovascular"
models_raw$disease_s_maternal[models_raw$disease_maternal == 1]<-"Maternal disorders"
models_raw$disease_s_HIVTB[models_raw$disease_HIVTB == 1]<-"HIV/AIDs and Tuberculosis"
models_raw$disease_s_mentalbehavioral[models_raw$disease_mentalbehavioral == 1]<-"Mental and behavioral disorders"
models_raw$disease_s_commoninfectious[models_raw$disease_commoninfectious == 1]<-"Common infectious diseases: diarrhea, lower respiratory infections, meningitis"
models_raw$disease_s_diabetes[models_raw$disease_diabetes == 1]<-"Diabetes, urogenital, blood, and endocrine disorders"
models_raw$disease_s_otherNCD[models_raw$disease_otherNCD == 1]<-"Other communicable, maternal, neonatal, and nutritional disorders"
models_raw$disease_s_nutritional[models_raw$disease_nutritional == 1]<-"Nutritional deficiencies"
models_raw$disease_s_neurological[models_raw$disease_neurological == 1]<-"Neurological disorders"
models_raw$disease_s_injury[models_raw$disease_injury == 1]<-"Injury: unintentional or transport"
models_raw$disease_s_neonatal[models_raw$disease_neonatal == 1]<-"Neonatal disorders"
models_raw$disease_s_digestive[models_raw$disease_digestive == 1]<-"Digestive diseases"
models_raw$disease_s_neoplasms[models_raw$disease_neoplasms == 1]<-"Neoplasms/cancer"
models_raw$disease_s_respiratory[models_raw$disease_respiratory == 1]<-"Chronic respiratory disease"
models_raw$disease_s_violence[models_raw$disease_violence == 1]<-"Violence: self harm or interpersonal"
models_raw$disease_s_musculoskeletal[models_raw$disease_musculoskeletal == 1]<-"Musculoskeletal disorders"
models_raw$disease_s_othercommunicable[models_raw$disease_othercommunicable == 1]<-"Other non-communicable disease"
#create multiple diseases variable, and fill in "not reported" for missings
models_raw$disease_multiple<- models_raw$disease_cardiovascular + models_raw$disease_maternal + models_raw$disease_HIVTB + models_raw$disease_mentalbehavioral + 
  models_raw$disease_commoninfectious + models_raw$disease_diabetes + models_raw$disease_otherNCD + models_raw$disease_nutritional +
  models_raw$disease_neurological + models_raw$disease_injury + models_raw$disease_neonatal + models_raw$disease_digestive + models_raw$disease_neoplasms + 
  models_raw$disease_respiratory + models_raw$disease_violence + models_raw$disease_musculoskeletal + models_raw$disease_musculoskeletal + models_raw$disease_othercommunicable
models_raw$disease_s_notreported[models_raw$disease_multiple==0]<-"Not reported"
})
#create disease display variable of all non-NA values concatenated
models_raw$disease_display<-paste(models_raw$disease_s_cardiovascular , models_raw$disease_s_maternal , models_raw$disease_s_HIVTB , models_raw$disease_s_mentalbehavioral , 
                                  models_raw$disease_s_commoninfectious , models_raw$disease_s_diabetes , models_raw$disease_s_otherNCD , models_raw$disease_s_nutritional ,
                                  models_raw$disease_s_neurological , models_raw$disease_s_injury , models_raw$disease_s_neonatal , models_raw$disease_s_digestive , models_raw$disease_s_neoplasms , 
                                  models_raw$disease_s_respiratory , models_raw$disease_s_violence , models_raw$disease_s_musculoskeletal , models_raw$disease_s_musculoskeletal , models_raw$disease_s_othercommunicable,
                                  models_raw$disease_s_notreported, sep = ", ")
models_raw$disease_display<-gsub("NA, ","",models_raw$disease_display)
models_raw$disease_display<-gsub(", NA","",models_raw$disease_display)



##INTERVENTION##
#create intervention string variable
suppressWarnings({
models_raw$intervention_s_caredelivery[models_raw$intervention_caredelivery == 1]<-"Care delivery"
models_raw$intervention_s_diagnostic[models_raw$intervention_diagnostic == 1]<-"Diagnostic"
models_raw$intervention_s_edubehavior[models_raw$intervention_edubehavior == 1]<-"Health education and behavior"
models_raw$intervention_s_immunization[models_raw$intervention_immunization == 1]<-"Immunization"
models_raw$intervention_s_meddevice[models_raw$intervention_meddevice == 1]<-"Medical device"
models_raw$intervention_s_medprocedure[models_raw$intervention_medprocedure == 1]<-"Medical procedure"
models_raw$intervention_s_pharma[models_raw$intervention_pharma == 1]<-"Pharmaceutical"
models_raw$intervention_s_screening[models_raw$intervention_screening == 1]<-"Screening"
models_raw$intervention_s_surgical[models_raw$intervention_surgical == 1]<-"Surgical"
models_raw$intervention_s_nutrition[models_raw$intervention_nutrition == 1]<-"Nutrition"
models_raw$intervention_s_maternalneonatal[models_raw$intervention_maternalneonatal == 1]<-"Maternal/neonatal"
models_raw$intervention_s_environmental[models_raw$intervention_environmental == 1]<-"Environmental"
models_raw$intervention_s_legislation[models_raw$intervention_legislation == 1]<-"Legislation"
models_raw$intervention_s_none[models_raw$intervention_none == 1]<-"None"
models_raw$intervention_s_other[models_raw$intervention_other == 1]<-"Other"
#create multiple diseases variable, and fill in "not reported" for missings
models_raw$intervention_multiple<-models_raw$intervention_caredelivery+models_raw$intervention_diagnostic+models_raw$intervention_edubehavior+models_raw$intervention_immunization+
                                  models_raw$intervention_meddevice+models_raw$intervention_medprocedure+models_raw$intervention_pharma+models_raw$intervention_screening+
                                  models_raw$intervention_surgical+models_raw$intervention_nutrition+models_raw$intervention_maternalneonatal+models_raw$intervention_environmental+
                                  models_raw$intervention_legislation+models_raw$intervention_none+models_raw$intervention_other
models_raw$intervention_s_notreported[models_raw$intervention_multiple == 0]<-"Not reported"
})
#create disease display variable of all non-NA values concatenated
models_raw$intervention_display<-paste(models_raw$intervention_s_caredelivery,models_raw$intervention_s_diagnostic,models_raw$intervention_s_edubehavior,models_raw$intervention_s_immunization,
                                       models_raw$intervention_s_meddevice,models_raw$intervention_s_medprocedure,models_raw$intervention_s_pharma,models_raw$intervention_s_screening,
                                       models_raw$intervention_s_surgical,models_raw$intervention_s_nutrition,models_raw$intervention_s_maternalneonatal,models_raw$intervention_s_environmental,
                                       models_raw$intervention_s_legislation,models_raw$intervention_s_none,models_raw$intervention_s_other, sep = ", ")
models_raw$intervention_display<-gsub("NA, ","",models_raw$intervention_display)
models_raw$intervention_display<-gsub(", NA","",models_raw$intervention_display)
models_raw$intervention_display<-ifelse(models_raw$intervention_other == 1, paste(models_raw$intervention_s_other, "(", models_raw$intervention_othertext,")"), models_raw$intervention_display)



##MODEL TYPE##
#create model type string variables
suppressWarnings({
  models_raw$type_s_tree[models_raw$type_tree == 1]<-"Decision tree"
  models_raw$type_s_markov[models_raw$type_markov == 1]<-"Markov/Transition model"
  models_raw$type_s_discreteevent[models_raw$type_discreteevent == 1]<-"Discrete event"
  models_raw$type_s_agentbased[models_raw$type_agentbased == 1]<-"Agent-based"
  models_raw$type_s_microsimulation[models_raw$type_microsimulation == 1]<-"Microsimulation"
  models_raw$type_s_other[models_raw$type_other == 1]<-"Other"
#create multiple types variable, and fill in "not reported" for missings
models_raw$multiple_types<-models_raw$type_tree+models_raw$type_markov+models_raw$type_markov+models_raw$type_discreteevent+models_raw$type_agentbased+models_raw$type_microsimulation+models_raw$type_other
models_raw$type_s_notreported[models_raw$multiple_types == 0]<-"Not reported"
})
#create model type display variable of all non-NA values concatenated
models_raw$type_display<-paste(models_raw$type_s_tree, models_raw$type_s_markov, models_raw$type_s_discreteevent,
                               models_raw$type_s_agentbased, models_raw$type_s_microsimulation, models_raw$type_s_other, models_raw$type_s_notreported, sep = ", ")
models_raw$type_display<-ifelse(models_raw$type_other == 1, paste(models_raw$type_display, "(", models_raw$type_othertext,")"), models_raw$type_display)
models_raw$type_display<-gsub("NA, ","",models_raw$type_display)
models_raw$type_display<-gsub(", NA","",models_raw$type_display)


##SPONSOR##
#create model sponsor string variable
suppressWarnings({
models_raw$sponsor_s_government[models_raw$sponsor_government == 1]<-"Government"
models_raw$sponsor_s_intergovernmental[models_raw$sponsor_intergovernmental == 1]<-"Intergovernmental Org"
models_raw$sponsor_s_foundation[models_raw$sponsor_foundation == 1]<-"Foundation"
models_raw$sponsor_s_pharmadevice[models_raw$sponsor_pharmadevice == 1]<-"Pharma/Medical Device Co"
models_raw$sponsor_s_healthcare[models_raw$sponsor_healthcare == 1]<-"Healthcare Org"
models_raw$sponsor_s_academic[models_raw$sponsor_academic == 1]<-"Academic"
models_raw$sponsor_s_profmemborg[models_raw$sponsor_profmemborg == 1]<-"Professional Membership Org"
models_raw$sponsor_s_other[models_raw$sponsor_other == 1]<-"Other"
#create multiple sponsor variable, and fill in "not reported" for missings
models_raw$multiple_sponsors<-models_raw$sponsor_government+models_raw$sponsor_intergovernmental+models_raw$sponsor_foundation+models_raw$sponsor_pharmadevice+models_raw$sponsor_healthcare+models_raw$sponsor_academic+models_raw$sponsor_profmemborg+models_raw$sponsor_other
models_raw$sponsor_s_notreported[models_raw$multiple_sponsors == 0]<-"Not reported"
})
#create model sponsor display variable of all non-NA values concatenated
models_raw$sponsor_display<-paste(models_raw$sponsor_s_government,models_raw$sponsor_s_intergovernmental,models_raw$sponsor_s_foundation,models_raw$sponsor_s_pharmadevice,models_raw$sponsor_s_healthcare,models_raw$sponsor_s_academic,models_raw$sponsor_s_profmemborg,models_raw$sponsor_s_other,models_raw$sponsor_s_notreported, sep = ", ")
models_raw$sponsor_display<-ifelse(models_raw$sponsor_other == 1, paste(models_raw$sponsor_s_other, "(", models_raw$sponsor_othertext,")"), models_raw$sponsor_display)
models_raw$sponsor_display<-gsub("NA, ","",models_raw$sponsor_display)
models_raw$sponsor_display<-gsub(", NA","",models_raw$sponsor_display)

##YEAR##
#create year variable
models_raw$year<-str_sub(models_raw$model_lastupdate, start= -4)
models_raw$year[models_raw$author1_last == "Kang"]<-"2019"