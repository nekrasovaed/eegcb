# -*- coding: utf-8 -*-
"""parser4.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1Jf9xGWkusYtnr7aA6hgOzjOvti531gPI
"""

import pandas as pd
import os
dict_to = { '1' : 1, #хорошо
            '2' : 1,
            '3' : 1,
            '4' : 1,
            '5' : 0, #плохо
            '6' : 0,
            '7' : 0,
            '8' : 0,
            '9' : 0,
            '10' : 0,
            '11' : 0,
            '12' : 0,
            '13' : 1,
            '14' : 1,
            '15' : 1,
            '16' : 1
}
dict_cat = {'Вакцинация' : 2,  'Плохо' : 0, 'Хорошо' : 1}

# Отношение к вакцинации
file_path = 'Отношение.txt'
attitude_vac_dict = {}

# Чтение текстового файла
with open(file_path, 'r', encoding='utf-8') as file:
    for line in file:
        line = line.strip()
        if line:  # проверяем, что строка не пустая
            parts = line.split(' ')
            if len(parts) == 2:
                id, feature = parts
                feature = 1 if feature=='positive' else 0
            else:
                id = parts[0]
                feature = None
            attitude_vac_dict[id] = feature
ids_with_none = [id for id, value in attitude_vac_dict.items() if value is None]
print("Список участников без отношения к вакцинации:", ids_with_none)
print(attitude_vac_dict)

base_dir = 'participants'
folders = [f for f in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, f))]

for folder in folders:
    if folder.startswith('EEGCB'):
        id = folder[5:]  # Получаем id после 'EEGCB'

        if id in ids_with_none:
            print(f'{id} пропущен потому что не имеет отчётности об отношении к вакцинации в файле Отношения.txt')
            continue

        out_file = os.path.join(f'{id}_modified.csv')
        if os.path.exists(out_file):
            print(f'{id} файл уже существует!')
            continue

        IAT_file = os.path.join('МеткиIAT', f'{id}_IAT.csv')
        vac_folder = os.path.join(base_dir, folder, 'Vac_2')
        vac_files = os.listdir(vac_folder)[0]
        vac_file = os.path.join(vac_folder, vac_files)

        if os.path.exists(IAT_file) and os.path.exists(vac_file):
            iat_file = pd.read_csv(IAT_file, header=None)
            vac_file = pd.read_excel(vac_file, sheet_name=0, skiprows=8)
            replacement_index = 0
            for i in range(len(iat_file)):
                if 'btar' in iat_file.iloc[i, 5]:  # Шестой столбец имеет индекс 5
                    try:
                        cat_data = vac_file.iloc[replacement_index, 4]
                        cat = dict_cat[cat_data]
                        block_num = vac_file.iloc[replacement_index, 0]
                    except:
                        print(f'Файл в папке Vac_2 сломан, id: {id}')
                        break

                    cb_flag = dict_to[str(block_num)] == attitude_vac_dict[id]
                    cb = 1 if cb_flag else 2
                    cbv_flag = attitude_vac_dict[id]
                    cbv = 2 if cbv_flag else 1

                    replacement = f'cb{cb}cbv{cbv}cat{cat}'
                    iat_file.iloc[i, 5] = iat_file.iloc[i, 5].replace('btar', f'btar{replacement}')
                    replacement_index += 1
        else:
            print('Не существует IAT файла в МеткиIAT или vac файла в папке Vac_2')
        print(f'{id} успешно сделан!')

    # Сохранение изменений в новый CSV файл
    iat_file.to_csv(out_file, header=None,  index=False)
    break