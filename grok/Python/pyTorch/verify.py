#!/usr/bin/env python

import torch

x = torch.rand(5,3)
print("Randon 5 by 3 \"tensor\":")
print(x)

if torch.cuda.is_available():
    print("\nCUDA is available.\n")
else:
    print("\nCUDA is not available.\n")

