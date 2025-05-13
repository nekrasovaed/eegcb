import csv
import os
participants = ["EEGCB11006", "EEGCB11008", "EEGCB11010", "EEGCB12003", "EEGCB12007",
                "EEGCB12009", "EEGCB12011", "EEGCB12012", "EEGCB11013", "EEGCB11014",
                "EEGCB11016", "EEGCB12018", "EEGCB12017", "EEGCB12015"]

for par in participants:
  name_dir = os.listdir("C:/Users/Sofya Popova/Desktop/participants/" + par + "/Vac_3")[0]
  print(name_dir)
  with open(f"C:/Users/Sofya Popova/Desktop/participants/{par}/Vac_3/{name_dir}/BaseReport.csv", "r") as basereport:
    wnew = open(f"C:/Users/Sofya Popova/Desktop/participants/{par}/Vac_3/{name_dir}/Main/FD_Screen/FixationList_new.csv", "w")
    baserepR = csv.reader(basereport)
    listLines = []
    SentIndexes = []
    name = ""
    trialnum = 0
    sentindex= 0
    flag = False
    for row in baserepR:
      if row == []:
        continue
      row_num = row[0].split(",")
      if flag:
        sentindex = row[0]
        SentIndexes.append(sentindex)
      if row_num[0] == 'Row_num':
        flag = True
    for num_id, Sent_ID in enumerate(SentIndexes):
      fix_list_name = "FixationList_" + str(num_id + 1)
      with open(f"C:/Users/Sofya Popova/Desktop/participants/{par}/Vac_3/{name_dir}/Main/FD_Screen/FixationList/{fix_list_name}.csv", "r") as fix_list_file:
        cur_fix_list = csv.reader(fix_list_file)
        for id_line, line in enumerate(cur_fix_list):
          if id_line == 0 and num_id == 0:
            line.extend(["TrialID", "RowNumber", "Name", "\n"])
          elif id_line == 0:
            continue
          else:
            line.extend([str(num_id + 1), "Main" + str(Sent_ID), par, "\n"])
          wnew.write(','.join(line))

  wnew.close()
