# -*- coding: utf-8 -*-
"""parser6.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1nWneVuFLe2ac_Nu0TMzdrwSp4m2ILqdS
"""

import re
import pandas as pd
import os
from io import StringIO

def paste(x):
    if 200 >= x >=0:
        return 1
    if 400 >= x > 200:
        return 2
    if 2000 >= x > 400:
        return 3
    return None

# Путь к директории
directory = 'event_lists'
# Получение списка файлов
files = os.listdir(directory)

end_directory = 'transform_event_lists'
os.makedirs(end_directory, exist_ok=True)

header_skip = 24

def parser(file, end_directory, header_skip):
    dfile = os.path.join(directory, file)
    with open(dfile, 'r') as h_file:
        headers = [next(h_file) for _ in range(header_skip)]

    data_file = pd.read_csv(dfile,  sep='\t', skiprows=header_skip, names=['item', 'bepoch', 'ecode', 'label', 'onset', 'diff', 'dura', 'b_flags', 'a_flags', 'enable', 'bin'], header=None)

    rn = -1

    for i in range(len(data_file)):
        string = data_file.iloc[i, 3]
        if 'tar' in string:  # Шестой столбец имеет индекс 5
            match_du = re.search(r'dr(-?\d+(\.\d+)?)', string)
            if match_du:
                du = float(match_du.group(1))
                data_file.iloc[i, 6] = str(float(data_file.iloc[i, 6]) - du/1000)

                value_after_dr = paste(du)
                if value_after_dr==None:
                    if du > 2000:
                        print(f'FILE: {dfile}\n\tError: Duration value is greater than 2000ms: {du}\n\tString: [{header_skip+i}] {string}')
                        return
                    if du < 0:
                        print(f'FILE: {dfile}\n\tWarning: Duration value is negative: {du}\n\tString: [about {header_skip+i}] {string}\n\tSolution: Value will be process as absolute')
                        value_after_dr = paste(abs(du))

                modified_string = re.sub(r'(cbv\d+)(rn)', r'\1du' + str(value_after_dr) + r'\2', string)
                data_file.iloc[i, 3] = modified_string
            else:
                print(f'FILE: {dfile}\n\tDuration label is not unformatted into a number in a string: {string}\n\tSolution: The file will not be processed')
                return

            match_rn = re.search(r'rn(\d)', string)
            if match_rn:
                found_rn = int(match_rn.group(1))
                if found_rn != rn:
                    data_file.iloc[i, 3] = data_file.iloc[i, 3].replace('tar', 'ftar')
                    rn = found_rn

    buffer = StringIO()
    data_file.to_csv(buffer, index=False, header=None, sep='\t')
    buffer.seek(0)
    modified_data = buffer.getvalue().split('\n')

    with open(os.path.join(end_directory, f'modified_{file}'), 'w') as file:
        file.writelines(headers + ['\n'] + modified_data)

for file in files:
    parser(file, end_directory, header_skip)