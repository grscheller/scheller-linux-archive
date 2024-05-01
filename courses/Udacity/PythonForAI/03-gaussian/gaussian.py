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

"""Module for the Gaussian class - derived from Udacity exercise template."""

from __future__ import annotations

from typing import List, Tuple
import math
import matplotlib.pyplot as plt

__all__ = ['Gaussian']

class Gaussian():
    """ Class for calculating and visualizing Gaussian distributions."""

    def __init__(self, mu: float=0.0, sigma: float=1.0):
        self.c = 1.0 / math.sqrt(2*math.pi)
        self.sample = True
        self.mean = mu
        self.stdev = sigma
        self.data: List[float] = []

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
        self.calculate_stdev(sample)

    def plot_histogram(self) -> None:
        """Produce a histogram of the data using the matplotlib pyplot library."""

        fig, axis = plt.subplots()
        axis.hist(self.data)
        axis.set_title('Histogram of Data')
        axis.set_ylabel('Data')
        axis.set_ylabel('Count')
        plt.show()

    def pdf(self, x: float):
        """Gaussian probability density function for this Gaussian object."""

        exp = math.exp
        sqrt = math.sqrt
        c = self.c

        mu = self.mean
        sigma = self.stdev

        return (c/sigma)*exp(-0.5*((x - mu)/sigma)**2)

    def plot_histogram_pdf(self, n_spaces: int = 50) -> Tuple[List[float], List[float]]:
        """Method to plot the normalized histogram of the data and a plot of the
        probability density function along the same range

        Args:
            n_spaces (int): number of data points

        Returns:
            list: x values for the pdf plot
            list: y values for the pdf plot

        """

        min_xs = min(self.data)
        max_xs = max(self.data)

        # calculates the interval between x values
        interval = 1.0 * (max_xs - min_xs) / n_spaces

        xs: List[float] = []
        ys: List[float] = []

        # calculate the x values to visualize
        for i in range(n_spaces):
            x = min_xs + interval*i
            xs.append(x)
            ys.append(self.pdf(x))

        # make the plots
        fig, axes = plt.subplots(2,sharex=True)
        fig.subplots_adjust(hspace=.5)
        axes[0].hist(self.data, density=True)
        axes[0].set_title('Normed Histogram of Data')
        axes[0].set_ylabel('Density')

        axes[1].plot(xs, ys)
        axes[1].set_title('Normal Distribution for \n Sample Mean and Sample Standard Deviation')
        axes[0].set_ylabel('Density')
        plt.show()

        return xs, ys
