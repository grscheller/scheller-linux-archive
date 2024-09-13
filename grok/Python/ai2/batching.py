#!/usr/bin/python

# Define the dataset
data = list(range(1, 31))

# Define the batch size
batch_size = 5

cnt = 0
idx = 0
batches: list[list[int]] = []
batches.append([])
for num in data:
    cnt += 1
    batches[idx].append(num)
    if cnt % batch_size == 0:
        idx += 1
        batches.append([])
for ii in range(0, len(batches)):
    for jj in batches[ii]:
        print('{} '.format(jj), end='')
    print()

