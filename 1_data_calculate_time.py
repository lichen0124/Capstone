import re
import pandas as pd

df = pd.read_csv('/Users/apple/Documents/UCDSmurfit/0_Capstone/data/data0709.csv')

###### part 1: test with one row #######
#s = df['events'][0]
#pattern = re.compile(r"'(\w+?)'")
#keys = pattern.findall(s)

#t = df['times'][0]
#pattern = re.compile(r"(\d+)")
#times = pattern.findall(t)

#d = dict(zip(keys,times))
#print(d)

##### part 2: code applied to all data
def process_events(events: str):
    pattern = re.compile(r"'(\w+?)'")
    return pattern.findall(events)

def process_times(times: str):
    pattern = re.compile(r"(\d+)")
    t = pattern.findall(times)
    return list(map(int, t))

def process_record(record: pd.Series):
    keys = process_events(record['events'])
    times = process_times(record['times'])
    d = dict(zip(keys, times))
    while d['START'] == 0:
        try:
            return d.get('FINISH', 0) - d['TRANSFER']
        except KeyError:
            return d.get('FINISH', 0) - d['START']

#print(process_record(df.iloc[0]))

df['duration_s'] = df.apply(process_record, axis = 1)
df['duration_m'] = df.apply(process_record, axis = 1) // 60

df.to_csv("data_duration.csv", index = False)
