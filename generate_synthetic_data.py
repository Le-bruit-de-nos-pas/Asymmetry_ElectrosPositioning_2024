import pandas as pd
import numpy as np
import pair_patients

# Function to generate synthetic data
def generate_synthetic_data(num_patients_per_group=50):
    np.random.seed(42)

    # Generate patient IDs
    patient_ids = range(1, 2 * num_patients_per_group + 1)

    # Generate random group assignments
    groups = np.random.choice(['Group1', 'Group2'], size=num_patients_per_group * 2)

    # Generate random age, gender, and disease duration
    age = np.random.randint(18, 70, size=num_patients_per_group * 2)
    gender = np.random.choice(['Male', 'Female'], size=num_patients_per_group * 2)
    disease_duration = np.random.randint(1, 20, size=num_patients_per_group * 2)

    # Create DataFrame
    data = pd.DataFrame({
        'PatientID': patient_ids,
        'Group': groups,
        'Age': age,
        'Gender': gender,
        'DiseaseDuration': disease_duration
    })

    return data

# Generate synthetic data
synthetic_data = generate_synthetic_data()


synthetic_data
# Display the synthetic data
print("Synthetic Data:")
print(synthetic_data.head())

# Use the pair_patients function
paired_data = pair_patients(synthetic_data, 'Group', ['Age', 'Gender', 'DiseaseDuration'], age_range=5, duration_range=5)

# Display the paired data
print("\nPaired Data:")
print(paired_data.head())

paired_data