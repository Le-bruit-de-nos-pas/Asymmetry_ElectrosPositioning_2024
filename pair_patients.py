import pandas as pd

def pair_patients(data, group_col, continuous_cols, age_range=5, duration_range=5):
    paired_data = pd.DataFrame()

    pair_id_counter = 1  # Counter for pair IDs

    for group_value in data[group_col].unique():
        group_data = data[data[group_col] == group_value]

        for index, case_row in group_data.iterrows():
            # Filter potential control candidates based on gender and age range
            potential_controls = data[
                (data[group_col] != group_value) &
                (data['Gender'] == case_row['Gender']) &
                (data['Age'].between(case_row['Age'] - age_range, case_row['Age'] + age_range))
            ]

            # Further filter candidates based on disease duration range
            potential_controls = potential_controls[
                (potential_controls['DiseaseDuration'].between(
                    case_row['DiseaseDuration'] - duration_range,
                    case_row['DiseaseDuration'] + duration_range
                ))
            ]

            # Check if there are potential control candidates
            if not potential_controls.empty:
                # Select the first available control (can be modified based on your preference)
                control_row = potential_controls.iloc[0]

                # Add case and control to the paired_data DataFrame with pair IDs
                paired_data = pd.concat([
                    paired_data,
                    pd.DataFrame({
                        'PairID': pair_id_counter,
                        'Case_PatientID': case_row['PatientID'],
                        'Control_PatientID': control_row['PatientID'],
                        **case_row[continuous_cols].to_dict(),
                        **control_row[continuous_cols].to_dict()
                    }, index=[0])
                ], ignore_index=True)

                # Drop the selected control from the original data
                data = data.drop(control_row.name)

                # Increment pair ID counter
                pair_id_counter += 1

    return paired_data

# Example usage:
# paired_data = pair_patients(patient_data, 'Group', ['Age', 'Gender', 'DiseaseDuration'], age_range=5, duration_range=5)
