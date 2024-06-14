# Copyright 2024 Geoffrey R. Scheller
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# This module contains software derived from Udacity® exercises.
# Udacity® (https://www.udacity.com/)
#

from __future__ import annotations

from typing import List
import math

__all__ = ['Distribution']

class Distribution():
    """Base Class for calculating and visualizing probability distributions."""

    def __init__(self, mu: float=0.0, sigma: float=1.0):
        self.sample = True
        self.mean = mu
        self.stdev = sigma
        self.data: List[float] = []

    def read_data_file(self, file_name: str, sample: bool=True) -> None:
        """Method to read in data from a text file. The text file should have
        one number (float) per line. The numbers are stored in the data attribute.
        After reading in the file, the mean and standard deviation are calculated.
        """

        # Read in the data from the file given
        data_list: List[float] = []
        with open(file_name) as file:
            line = file.readline()
            while line:
                data_list.append(int(line))
                line = file.readline()

        # Update Gaussian object
        self.data = data_list

    def calculate_mean(self) -> float:
        """From the data set, calculate & return the mean."""

        mu = self.mean
        n = len(self.data)
        if n > 0:
            mu = sum(self.data)/n
            self.mean = mu

        return mu

    def calculate_stdev(self, sample: bool=True) -> float:
        """Set & return a standard deviation calculated from the data set.

        * If sample is True, calculate a sample standard deviation. 
        * If sample is False, calculate a population standard deviation. 

        """

        n = len(self.data)
        mu = self.calculate_mean()

        if sample:
            # sample standard deviation
            if n > 1:
                self.stdev = math.sqrt(sum(((x - mu)**2 for x in self.data))/(n-1))
        else:
            # population standard deviation
            if n > 0:
                self.stdev = math.sqrt(sum(((x - mu)**2 for x in self.data))/n)

        return self.stdev
