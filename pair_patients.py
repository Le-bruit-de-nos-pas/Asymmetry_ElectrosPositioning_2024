import pandas as pd



def pair_patients_with_controls(base_group_data, other_group_data, continuous_cols, age_range=5, duration_range=5):
    paired_data = pd.DataFrame()

    for index, base_row in base_group_data.iterrows():
        # Filter potential control candidates from the other group based on gender and age range
        potential_controls = other_group_data[
            (other_group_data['Gender'] == base_row['Gender']) &
            (other_group_data['Age'].between(base_row['Age'] - age_range, base_row['Age'] + age_range))
        ]

        # Further filter candidates based on disease duration range
        potential_controls = potential_controls[
            (potential_controls['DiseaseDuration'].between(
                base_row['DiseaseDuration'] - duration_range,
                base_row['DiseaseDuration'] + duration_range
            ))
        ]

        # Concatenate the PatientIDs of potential controls into a single string
        control_ids = ', '.join(map(str, potential_controls['PatientID'].tolist()))

        # Add the concatenated control IDs to the base row
        base_row['Control_PatientIDs'] = control_ids

        # Append the updated base row to the paired_data DataFrame
        paired_data = pd.concat([paired_data, base_row.to_frame().transpose()], ignore_index=True)

    return paired_data


# Pair patients from group 1 with potential controls from group 2
paired_data_group1 = pair_patients_with_controls(group1_data, group2_data, ['Age', 'Gender', 'DiseaseDuration'], age_range=5, duration_range=5)



paired_data_group1.to_csv('Processed_data/paired_data_group1.csv', index=False)

# Pair patients from group 2 with potential controls from group 1
# paired_data_group2 = pair_patients_with_controls(group2_data, group1_data, ['Age', 'Gender', 'DiseaseDuration'], age_range=5, duration_range=5)


