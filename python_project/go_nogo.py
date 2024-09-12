# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
#%%
import glob
import os
import re
import pandas as pd
import numpy as np
from datetime import datetime
import pyreadstat
from typing import Dict, Optional, Tuple

#%%
date_format = "%Y-%m-%d_%Hh%M.%S.%f" # Definicja formatu daty w pliku
limit_rows_in_file = {'min': 0, 'max': 116}

column_sets = {
    'resp_trial.keys': {"resp_trial_Prac1.keys", "resp_trial_Test1.keys", "key_resp_Test2.keys"},
    'resp_trial.corr': {"resp_trial_Prac1.corr", "resp_trial_Test1.corr", "key_resp_Test2.corr"},
    'resp_trial.rt': {"resp_trial_Prac1.rt", "resp_trial_Test1.rt", "key_resp_Test2.rt"},
    "trial.thisRepN": {"prac_trials.thisRepN", "test1trial.thisRepN", "test2trial.thisRepN"},
    "trial.thisTrialN": {"prac_trials.thisTrialN", "test1trial.thisTrialN", "test2trial.thisTrialN"},
    "trial.thisN": {"prac_trials.thisN", "test1trial.thisN", "test2trial.thisN"},
    "trial.thisIndex": {"prac_trials.thisIndex", "test1trial.thisIndex", "test2trial.thisIndex"},
    "trial.ran": {"prac_trials.ran", "test1trial.ran", "test2trial.ran"},
    'stimulus': {"stimulus_prac1", "stimulus_test1", "stimulus_test2"},
    'corrAns': {"corrAns_prac1", "corrAns_test1", "corrAns_test2"}
}

trial_names = {
    "PRAC1": (0, 14),
    "TEST1": (15, 64),
    "TEST2": (65, 114)
    }


#%%
def extract_data_from_csv(file_path):
    """
    Extracts data from a CSV file and loads it into a Pandas DataFrame.

    Parameters:
    - file_path (str): The path to the CSV file.

    Returns:
    - DataFrame: A Pandas DataFrame containing the data from the CSV file.
    - None: If any error occurs during the file reading process.
    """
    try:
        # Check if the file exists
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"File '{file_path}' not found.")

        # Load the CSV file into a DataFrame
        df = pd.read_csv(file_path)
        
        # Check if the DataFrame is empty
        if df.empty:
            print(f"The file {file_path} is empty.")
            return None

        return df

    except FileNotFoundError as fnf_error:
        print(f"Could not find the file {file_path}: {fnf_error}")
    except pd.errors.EmptyDataError:
        print(f"The file {file_path} is empty or has an incorrect format.")
    except pd.errors.ParserError:
        print(f"The file {file_path} contains syntax errors.")
    except Exception as e:
        print(f"Could not open the file {file_path} due to an error: {e}")
    
    return None



def filter_dataframe_by_row_count(df, min_rows=0, max_rows=None):
    """
    Filters the DataFrame based on the number of rows.

    Parameters:
    - df (pd.DataFrame): The DataFrame to be filtered.
    - min_rows (int): Minimum number of rows required in the DataFrame. Default is 0.
    - max_rows (int or None): Maximum number of rows allowed in the DataFrame. Default is None, which means no upper limit.

    Returns:
    - pd.DataFrame: The filtered DataFrame. If the DataFrame does not meet the criteria, an empty DataFrame is returned.
    """
    # Check if the DataFrame is None or not, empty, has a 'shape' attribute
    if (df is None) | (df.empty) | (not hasattr(df, 'shape')):
        return None
    
    # Check if the DataFrame meets the row count criteria
    row_count = df.shape[0]
    
    if row_count >= min_rows and (max_rows is None or row_count <= max_rows):
        return df
    else:
        # Return an empty DataFrame with the same columns if it does not meet the criteria
        return pd.DataFrame(columns=df.columns)
    


def extract_metadata_row(df: pd.DataFrame, date_format: str = "%Y-%m-%d %H:%M:%S") -> pd.DataFrame:
    """
    Extract metadata from the DataFrame and return it as a single-row DataFrame.

    Parameters:
    - df (pd.DataFrame): The DataFrame from which metadata will be extracted.
    - date_format (str): The format of the date string in the DataFrame. Default is "%Y-%m-%d %H:%M:%S".

    Returns:
    - pd.DataFrame: A DataFrame with a single row containing the metadata.
    """
    if df is not None and not df.empty:
        
        # Extract participant ID
        participant_id = df['participant'].values[0] if 'participant' in df.columns else None
        
        # Extract and format date
        date_str = df['date'].values[0] if 'date' in df.columns else None
        date = None
        formatted_date = None
        if date_str:
            try:
                date = datetime.strptime(date_str, date_format)
                formatted_date = date.strftime("%d/%m/%Y %H:%M:%S")
            except ValueError:
                formatted_date = "Invalid date format"
        
        # Gather metadata
        metadata = {
            'ID': participant_id.upper() if participant_id else None,  # Upewnienie się, że jest w uppercase
            'date': formatted_date,
        }
        
        # Convert metadata to DataFrame
        metadata_df = pd.DataFrame([metadata])
        
        return metadata_df
    else:
        # Return an empty DataFrame if the input DataFrame is None or empty
        return pd.DataFrame(columns=['ID', 'date'])

def preprocess_and_convert_columns(df: pd.DataFrame) -> pd.DataFrame:
    """
    Preprocess the DataFrame by converting specified columns to appropriate data types and handling empty values.
    
    Parameters:
    - df (pd.DataFrame): The DataFrame to preprocess.

    Returns:
    - pd.DataFrame: The DataFrame with preprocessed columns.
    """
    
    # Ensure the required columns exist
    required_columns = ['resp_trial.keys', 'resp_trial.corr', 'stimulus', 'corrAns', 'resp_trial.corr', 
                        'resp_trial.rt', 'trial.thisRepN', 'trial.thisTrialN', 
                        'trial.thisN', 'trial.thisIndex', 'trial.ran']
    
    if not all(col in df.columns for col in required_columns):
        missing_cols = [col for col in required_columns if col not in df.columns]
        raise ValueError(f"DataFrame is missing the following columns: {missing_cols}")
    
    for column in df.columns:
        # Check if the column is of object type (typically string columns)
        if df[column].dtype == 'object':
            # Strip leading and trailing whitespace for string columns
            df[column] = df[column].astype(str).str.strip()
    
    # Convert 'resp_trial.keys' column to string and replace empty values with 'none'
    df['resp_trial.keys'] = df['resp_trial.keys'].astype(str).replace('', 'none')
    
    # Convert other specified columns to integer and float types
    int_columns = ['resp_trial.corr', 'trial.thisRepN', 'trial.thisTrialN', 'trial.thisN', 
                   'trial.thisIndex', 'trial.ran']
    float_columns = ['resp_trial.rt']
    
    # Convert columns to integer
    for col in int_columns:
        df[col] = pd.to_numeric(df[col], errors='coerce', downcast='integer')
    
    # Convert columns to float
    for col in float_columns:
        df[col] = pd.to_numeric(df[col], errors='coerce', downcast='float')
    
    return df
    
def parse_log_data(df: pd.DataFrame, column_sets: Dict[str, tuple], separator: str = '', trial_names: Optional[Dict[str, Tuple[int, int]]] = None) -> pd.DataFrame:
    """
    Merge specified columns into single columns in the DataFrame, handling missing columns by filling with NaN,
    and optionally create a 'trial_name' column based on provided trial names and their corresponding row ranges.

    Parameters:
    - df (pd.DataFrame): The DataFrame containing the columns to be merged.
    - column_sets (dict): A dictionary where keys are new column names and values are sets of existing column names to merge.
    - separator (str): A string separator to use between concatenated values.
    - trial_names (dict, optional): A dictionary where keys are trial names and values are tuples representing row ranges.

    Returns:
    - pd.DataFrame: DataFrame with new merged columns, optionally including a 'trial_name' column.
    """
    # Initialize trial_name_column if trial_names are provided
    if trial_names:
        trial_name_column = [None] * len(df)  # Initialize the trial name column

        # Assign trial names to the specified row ranges
        for trial_name, (start_row, end_row) in trial_names.items():
            if 0 <= start_row < len(df) and 0 <= end_row < len(df):
                trial_name_column[start_row:end_row + 1] = [trial_name] * (end_row - start_row + 1)
            else:
                print(f"Warning: Row range ({start_row}, {end_row}) for trial '{trial_name}' is out of bounds.")

    # Iterate over each column set to merge
    for new_column, columns_to_merge in column_sets.items():
        existing_columns = list(columns_to_merge.intersection(df.columns))
        
        if not existing_columns:
            print(f"No columns found to merge for {new_column}")
            df[new_column] = pd.NA
            continue
        
        # Concatenate values and handle missing data
        df[new_column] = df[existing_columns].fillna('').astype(str).agg(separator.join, axis=1)

    # Add the trial names column if trial_names are provided
    if trial_names:
        df['trial_name'] = pd.Series(trial_name_column, dtype=str)

    # Create a list of columns to keep, ensuring they exist in the DataFrame
    columns_to_keep = [col for col in column_sets.keys() if col in df.columns]
    if trial_names and 'trial_name' in df.columns:
        columns_to_keep.append('trial_name')
    
    
    df = preprocess_and_convert_columns(df)   


    # Return only the specified columns
    return df[columns_to_keep]

def calculate_parameters(df: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate performance metrics based on the response and correct answer columns in the DataFrame.
    Metrics are grouped by the 'trial_name' column and stored in separate columns for each group in a new DataFrame.

    Parameters:
    - df (pd.DataFrame): The DataFrame containing the columns 'resp_trial.keys', 'corrAns', and 'trial_name'.

    Returns:
    - pd.DataFrame: A new DataFrame with columns for performance metrics aggregated by 'trial_name'.
    """
    
    # Ensure the required columns are in the DataFrame
    required_columns = ['resp_trial.keys', 'corrAns', 'trial_name']
    if not all(col in df.columns for col in required_columns):
        raise ValueError(f"DataFrame must contain the following columns: {required_columns}")

    # Convert columns to string and replace empty values with 'none'
    df['resp_trial.keys'] = df['resp_trial.keys'].astype(str).replace('', 'none')
    df['corrAns'] = df['corrAns'].astype(str).replace('', 'none')

    # Initialize a dictionary to store metrics
    metrics = {}

    # Calculate metrics for each trial_name
    for trial in df['trial_name'].unique():
        # Filter data for the current trial
        trial_data = df[df['trial_name'] == trial].copy()  # use .copy() to avoid SettingWithCopyWarning

        # Check if the entire 'resp_trial.keys' column is 'none' and set to np.nan
        if (trial_data['resp_trial.keys'] == 'none').all():
            df.loc[df['trial_name'] == trial, 'resp_trial.keys'] = np.nan
            df.loc[df['trial_name'] == trial, 'misses'] = np.nan  # Set 'misses' to NaN for this trial
        else:
            # Calculate metrics only if not all 'resp_trial.keys' are 'none'
            df.loc[df['trial_name'] == trial, 'hits'] = np.where(
                (trial_data['resp_trial.keys'] == 'space') & (trial_data['corrAns'] == 'space'), 1, 0
            )
            df.loc[df['trial_name'] == trial, 'misses'] = np.where(
                (trial_data['resp_trial.keys'] == 'none') & (trial_data['corrAns'] == 'space'), 1, 0
            )
            df.loc[df['trial_name'] == trial, 'false_alarms'] = np.where(
                (trial_data['resp_trial.keys'] == 'space') & (trial_data['corrAns'] == 'none'), 1, 0
            )
            df.loc[df['trial_name'] == trial, 'correct_rejections'] = np.where(
                (trial_data['resp_trial.keys'] == 'none') & (trial_data['corrAns'] == 'none'), 1, 0
            )

        # Aggregate metrics for the current trial
        metrics[f'hits_{trial}'] = df[df['trial_name'] == trial]['hits'].sum()
        metrics[f'misses_{trial}'] = df[df['trial_name'] == trial]['misses'].sum()
        metrics[f'false_alarms_{trial}'] = df[df['trial_name'] == trial]['false_alarms'].sum()
        metrics[f'correct_rejections_{trial}'] = df[df['trial_name'] == trial]['correct_rejections'].sum()
    
    # Calculate total metrics excluding 'PRAC1'
    total_data = df[df['trial_name'] != 'PRAC1']
    metrics['hits_total'] = total_data['hits'].sum()
    metrics['misses_total'] = total_data['misses'].sum()
    metrics['false_alarms_total'] = total_data['false_alarms'].sum()
    metrics['correct_rejections_total'] = total_data['correct_rejections'].sum()

    # Convert the metrics dictionary to a DataFrame with one row
    metrics_df = pd.DataFrame([metrics])

    return metrics_df


def save_results_to_sav(df: pd.DataFrame, output_dir: str = 'out'):
    """
    Zapisuje podany DataFrame do pliku .sav w określonym katalogu wyjściowym z bieżącą datą i czasem w nazwie pliku.

    Parametry:
    - df (pd.DataFrame): DataFrame zawierający wyniki do zapisania.
    - output_dir (str): Katalog wyjściowy, w którym plik zostanie zapisany. Domyślnie 'go_nogo_out'.
    """

    # Sprawdzenie, czy katalog istnieje, a jeśli nie, to jego stworzenie
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Katalog '{output_dir}' został utworzony.")
    else:
        print(f"Katalog '{output_dir}' już istnieje.")

    # Tworzenie nazwy pliku z bieżącą datą i czasem
    current_time = datetime.now().strftime("%Y%m%d_%H%M%S")
    file_name = f"go_nogo_{current_time}.sav"
    file_path = os.path.join(output_dir, file_name)

    # Zapis DataFrame do pliku SAV
    pyreadstat.write_sav(df, file_path)

    print(f"Wyniki zostały zapisane w pliku: {file_path}")
#%%
file_paths = glob.glob('Go_NoGo_OPUS_20240805/data/**.csv')

#%%
# Wyrażenie regularne do filtrowania plików
pattern = re.compile(r"WWA[0-9]|PUM[0-9]{2}[KB]|WRO[0-9]", re.IGNORECASE)

# Filtracja plików według wyrażenia regularnego
filtered_file_paths = [path for path in file_paths if pattern.search(path)]


#%%



#%%
all_results = pd.DataFrame()

for file_path in filtered_file_paths:
    log = extract_data_from_csv(file_path)
    log = filter_dataframe_by_row_count(log, limit_rows_in_file['min'], limit_rows_in_file['max'])

    if log is not None and not log.empty:
        # Usunięcie wierszy większych niż limit 'max'-1
        log = log.iloc[limit_rows_in_file['min']:limit_rows_in_file['max']-1]
        # Usunięcie pustych kolumn
        log = log.dropna(axis=1, how='all')
        
     
        metadata_df = extract_metadata_row(log, date_format)
        parsed_df = parse_log_data(log, column_sets, trial_names=trial_names)
        metrics_df = calculate_parameters(parsed_df)

        result_df = pd.concat([metadata_df, metrics_df], axis=1)
        all_results = pd.concat([all_results, result_df], axis=0)
        
#%%
# Finding duplicates in column 'participant_id'
duplicates = all_results[all_results.duplicated(subset='ID', keep=False)]

#%%ZAPIS
save_results_to_sav(all_results, output_dir='go_nogo_out')

