import pandas as pd

asymmetry_pre_vs_post = pd.read_csv("Processed_data/Asymmetry_Pre_vs_Post.txt", sep="\t")

subj_id = asymmetry_pre_vs_post[['SUBJID']]


demographie = pd.read_excel("Processed_data/Asymmetry_DeepBrainStimulation.xlsx", sheet_name="DEMOGRAPHIE ", dtype="str")

demographie = pd.merge(subj_id, demographie, on='SUBJID', how='inner')

demographie = demographie[['SUBJID', 'SEXE', 'DDN', 'D_1ER_SYMPT']]


dates_de_visites = pd.read_excel("Processed_data/Asymmetry_DeepBrainStimulation.xlsx", sheet_name="DATES_DE_VISITES ", dtype="str")

dates_de_visites = dates_de_visites[['SUBJID', 'D_CHIR']]

patient_data_to_pair = pd.merge(dates_de_visites, demographie, on='SUBJID', how='inner')

patient_data_to_pair['D_CHIR'] = patient_data_to_pair['D_CHIR'].str.extract(r'(\d{4})').astype(float)
patient_data_to_pair['DDN'] = patient_data_to_pair['DDN'].str.extract(r'(\d{4})').astype(float)
patient_data_to_pair['D_1ER_SYMPT'] = pd.to_numeric(patient_data_to_pair['D_1ER_SYMPT'], errors='coerce')
patient_data_to_pair['AGESURGERY'] = patient_data_to_pair['D_CHIR'] - patient_data_to_pair['DDN']
patient_data_to_pair['DISDURSURGERY'] = patient_data_to_pair['D_CHIR'] - patient_data_to_pair['D_1ER_SYMPT']

patient_data_to_pair = patient_data_to_pair[['SUBJID', 'SEXE', 'AGESURGERY', 'DISDURSURGERY']]

patient_data_to_pair.isna().sum()

patient_data_to_pair.dropna(inplace=True)

patient_data_to_pair.to_csv("Processed_data/patientDataToPair.csv", index=False)

#TODO

groups_ad_clean = pd.read_excel("Processed_data/Groups_AC_clean.xlsx",  dtype="str")


# Merge with patients with MRI, pair them
# Each case -> control