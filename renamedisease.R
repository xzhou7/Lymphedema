library(stringr)


setwd("~/Box/XinZhouFiles/Projects/ZXP3_Lymphedema/03.Final/remodelfigure1/")
prevalance <- read.csv("./PrevalanceMatrix.csv")
prevalance <- prevalance[rowSums(prevalance[2:5]) != 0,]

colnames(prevalance) <- c("Disease", "SL", "LV", "LP", "NL")
prevalance$abbr <- prevalance$Disease
prevalance$Disease[prevalance$abbr == "PS"] <- "Pulmonary.Stenosis"
prevalance$Disease[prevalance$abbr == "PR"] <- "Pulmonic.Regurgitation"
prevalance$Disease[prevalance$abbr == "PE"] <- "Pulmonary.Embolism"
prevalance$Disease[prevalance$abbr == "Pulm.Fibrosis"] <- "Pulmonary.Fibrosis"
prevalance$Disease[prevalance$abbr == "X2o.PAH"] <- "Secondary.Pulmonary.Arterial.Hypertension"
prevalance$Disease[prevalance$abbr == "TR"] <- "Tricuspid.Regurgitation"
prevalance$Disease[prevalance$abbr == "AF"] <- "Atrial.Fibrillation"
prevalance$Disease[prevalance$abbr == "CHF"] <- "Congestive.Heart.Failure"
prevalance$Disease[prevalance$abbr == "CVI"] <- "Chronic.Venous.Insufficiency"
prevalance$Disease[prevalance$abbr == "AR"] <- "Aortic.Regurgitation"
prevalance$Disease[prevalance$abbr == "DVT"] <- "Deep.Vein.Thrombosis"
prevalance$Disease[prevalance$abbr == "AS"] <- "Aortic.Stenosis"
prevalance$Disease[prevalance$abbr == "MR"] <- "Mitral.Valve.Regurgitation"
prevalance$Disease[prevalance$abbr == "CKD"] <- "Chronic.Kidney.Disease"
prevalance$Disease[prevalance$abbr == "CAD"] <- "Coronary.Artery.Disease"
prevalance$Disease[prevalance$abbr == "DM"] <- "Diabetes.Mellitus"
prevalance$Disease[prevalance$abbr == "PAD"] <- "Peripheral.Artery.Disease"
prevalance$Disease[prevalance$abbr == "HTN"] <- "Hypertension"
prevalance$Disease[prevalance$abbr == "X1o.PAH"] <- "Primary.Pulmonary.Arterial.Hypertension"
prevalance$Disease[prevalance$abbr == "MS"] <- "Mitral.Stenosis"
prevalance$Disease[prevalance$abbr == "CF"] <- "Cystic.Fibrosis"
prevalance$Disease[prevalance$abbr == "OSA"] <- "Obstructive.Sleep.Apnea"
prevalance$Disease[prevalance$abbr == "RA"] <- "Rheumatoid.Arthritis"
prevalance$Disease[prevalance$abbr == "PID"] <- "Pelvic.Inflammatory.Disease "
prevalance$Disease[prevalance$abbr == "DCM"] <- "Dilated.Cardiomyopathy"
prevalance$Disease[prevalance$abbr == "IBS"] <- "Irritable.Bowel.Syndrome"
prevalance$Disease[prevalance$abbr == "UTI"] <- "Urinary.Tract.Infection"
prevalance$Disease[prevalance$abbr == "MVP"] <- "Mitral.Valve.Prolapse"
prevalance$Disease[prevalance$abbr == "RHD"] <- "Rheumatic.Heart.Disease"
prevalance$Disease[prevalance$abbr == "RCM"] <- "Restrictive.Cardiomyopathy"
prevalance$Disease[prevalance$abbr == "GAD"] <- "Generalized.Anxiety.Disorder"
prevalance$Disease[prevalance$abbr == "BPPV"] <- "Benign.Paroxysmal.Positional.Vertigo"
prevalance$Disease[prevalance$abbr == "AoAneur"] <- "Aortic.Aneurysm"
prevalance$Disease[prevalance$abbr == "HCM"] <- "Hypertrophic.Cardiomyopathy"
prevalance$Disease[prevalance$abbr == "MDD"] <- "Major.Depressive.Disorder"
prevalance$Disease[prevalance$abbr == "PCOS"] <- "Polycystic.Ovary.Syndrome"
prevalance$Disease[prevalance$abbr == "BPH"] <- "Benign.Prostatic.Hyperplasia"
prevalance$Disease[prevalance$abbr == "CRVO"] <- "Central.Retinal.Vein.Occlusion"
prevalance$Disease[prevalance$abbr == "SLE"] <- "Systemic.Lupus.Erythematosus"
prevalance$Disease[prevalance$abbr == "TIA"] <- "Transient.Ischemic.Attack"
prevalance$Disease[prevalance$abbr == "TIA.Stroke"] <- "Transient.Ischemic.Attack.Stroke"
prevalance$Disease[prevalance$abbr == "GERD"] <- "Gastroesophageal.Reflux.Disease"
prevalance$Disease[prevalance$abbr == "COPD"] <- "Chronic.Obstructive.Pulmonary.Disease"
prevalance$Disease[prevalance$abbr == "PCOS"] <- "Polycystic.Ovary.Syndrome"

write.csv(file = "./Renamed_Prevalance.csv",prevalance)




