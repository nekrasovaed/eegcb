import os
import pandas as pd
from collections import defaultdict

def get_data_for_split(X_name, target, split):
    # target in {'cognitive_bias', 'match_mismatch'}
    base = f'datasets_splitted_{X_name}_{target}'

    # features (shared by tasks)
    X_train = pd.read_csv(f'{base}/X_train.csv', index_col=0)
    X_test  = pd.read_csv(f'{base}/X_test.csv',  index_col=0)

    X_length_normalised_train = pd.read_csv(f'{base}/X_length_normalised_train.csv', index_col=0)
    X_length_normalised_test  = pd.read_csv(f'{base}/X_length_normalised_test.csv',  index_col=0)

    X_participants_normalised_train = pd.read_csv(f'{base}/X_participants_normalised_train.csv', index_col=0)
    X_participants_normalised_test  = pd.read_csv(f'{base}/X_participants_normalised_test.csv',  index_col=0)

    X_length_participants_normalised_train = pd.read_csv(f'{base}/X_length_participants_normalised_train.csv', index_col=0)
    X_length_participants_normalised_test  = pd.read_csv(f'{base}/X_length_participants_normalised_test.csv',  index_col=0)

    stimuli_features_train = pd.read_csv(f'{base}/stimul_features_train.csv', index_col=0)
    stimuli_features_test  = pd.read_csv(f'{base}/stimul_features_test.csv',  index_col=0)

    # participant metadata per trial (if present)
    participant_features_train = None
    participant_features_test = None
    if os.path.exists(f'{base}/participant_features_train.csv') and os.path.exists(f'{base}/participant_features_test.csv'):
        participant_features_train = pd.read_csv(f'{base}/participant_features_train.csv', index_col=0)
        participant_features_test  = pd.read_csv(f'{base}/participant_features_test.csv',  index_col=0)

    # optional extras (CB only)
    stim_train_p = f'{base}/X_stimulus_normalised_train.csv'
    stim_test_p  = f'{base}/X_stimulus_normalised_test.csv'
    len_stim_train_p = f'{base}/X_length_stimulus_normalised_train.csv'
    len_stim_test_p  = f'{base}/X_length_stimulus_normalised_test.csv'
    part_test_byavg_p = f'{base}/X_participants_normalised_test_bytrainavg.csv'
    len_part_test_byavg_p = f'{base}/X_length_participants_normalised_test_bytrainavg.csv'

    # targets (per split)
    y_train = pd.read_csv(f'{base}/y_{split}_train.csv', index_col=0).iloc[:,0]
    y_test  = pd.read_csv(f'{base}/y_{split}_test.csv',  index_col=0).iloc[:,0]
    y_reg_train = pd.read_csv(f'{base}/y_reg_train.csv', index_col=0).iloc[:,0]
    y_reg_test  = pd.read_csv(f'{base}/y_reg_test.csv',  index_col=0).iloc[:,0]
    y_answer_train = pd.read_csv(f'{base}/y_answer_train.csv', index_col=0).iloc[:,0]
    y_answer_test  = pd.read_csv(f'{base}/y_answer_test.csv',  index_col=0).iloc[:,0]

    data_features = defaultdict(dict)
    data_features['all_features'] = {'X_train': X_train, 'X_test': X_test}
    data_features['length_normalised_features'] = {'X_train': X_length_normalised_train, 'X_test': X_length_normalised_test}
    data_features['participants_normalised_features'] = {'X_train': X_participants_normalised_train, 'X_test': X_participants_normalised_test}
    data_features['length_participants_normalised'] = {'X_train': X_length_participants_normalised_train, 'X_test': X_length_participants_normalised_test}
    data_features['stimuli_features'] = {'X_train': stimuli_features_train, 'X_test': stimuli_features_test}
    if participant_features_train is not None and participant_features_test is not None:
        data_features['participant_features'] = {'X_train': participant_features_train, 'X_test': participant_features_test}

    # extras if present (mainly for cognitive_bias)
    if os.path.exists(stim_train_p) and os.path.exists(stim_test_p):
        data_features['stimulus_normalised_features'] = {
            'X_train': pd.read_csv(stim_train_p, index_col=0),
            'X_test':  pd.read_csv(stim_test_p,  index_col=0),
        }
    if os.path.exists(len_stim_train_p) and os.path.exists(len_stim_test_p):
        data_features['length_stimulus_normalised_features'] = {
            'X_train': pd.read_csv(len_stim_train_p, index_col=0),
            'X_test':  pd.read_csv(len_stim_test_p,  index_col=0),
        }
    if os.path.exists(part_test_byavg_p):
        data_features['participants_normalised_test_bytrainavg'] = {
            'X_train': X_participants_normalised_train,  # same as above
            'X_test':  pd.read_csv(part_test_byavg_p, index_col=0),
        }
    if os.path.exists(len_part_test_byavg_p):
        data_features['length_participants_normalised_test_bytrainavg'] = {
            'X_train': X_length_participants_normalised_train,
            'X_test':  pd.read_csv(len_part_test_byavg_p, index_col=0),
        }

    target_variables = defaultdict(dict)
    target_variables['cb'] = {'y_train': y_train, 'y_test': y_test}
    target_variables['regression'] = {'y_train': y_reg_train, 'y_test': y_reg_test}
    target_variables['answer'] = {'y_train': y_answer_train, 'y_test': y_answer_test}

    groups = [i.split('_')[0] for i in X_train.index]
    return data_features, target_variables, groups
