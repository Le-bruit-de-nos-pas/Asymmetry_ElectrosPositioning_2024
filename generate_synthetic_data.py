import pandas as pd
import numpy as np

# Function to generate synthetic data

def generate_synthetic_data(num_patients_per_group=50):
    np.random.seed(42)

    # Generate patient IDs
    patient_ids_group1 = range(1, num_patients_per_group + 1)
    patient_ids_group2 = range(num_patients_per_group + 1, 2 * num_patients_per_group + 1)

    # Generate random age, gender, and disease duration for each group
    age_group1 = np.random.randint(18, 70, size=num_patients_per_group)
    age_group2 = np.random.randint(18, 70, size=num_patients_per_group)

    gender_group1 = np.random.choice(['Male', 'Female'], size=num_patients_per_group)
    gender_group2 = np.random.choice(['Male', 'Female'], size=num_patients_per_group)

    disease_duration_group1 = np.random.randint(1, 20, size=num_patients_per_group)
    disease_duration_group2 = np.random.randint(1, 20, size=num_patients_per_group)

    # Create DataFrames for each group
    group1_data = pd.DataFrame({
        'PatientID': patient_ids_group1,
        'Group': 'Group1',
        'Age': age_group1,
        'Gender': gender_group1,
        'DiseaseDuration': disease_duration_group1
    })

    group2_data = pd.DataFrame({
        'PatientID': patient_ids_group2,
        'Group': 'Group2',
        'Age': age_group2,
        'Gender': gender_group2,
        'DiseaseDuration': disease_duration_group2
    })

    return group1_data, group2_data




# Example usage:
group1_data, group2_data = generate_synthetic_data()


print("Synthetic Data:")
print(group1_data.head())
print(group2_data.head())



# Export synthetic data to text files for each group
group1_data.to_csv('Processed_data/group1_data.txt', index=False, sep='\t')
group2_data.to_csv('Processed_data/group2_data.txt', index=False, sep='\t')
