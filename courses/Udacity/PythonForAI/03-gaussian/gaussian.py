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

from typing import Callable, Iterator, Tuple
import math
import matplotlib.pyplot as plt

__all__ = ['Gaussian']

class Gaussian():
    """ Gaussian distribution class for calculating and visualizing
    a Gaussian distribution.

    Attributes:
        mean (float) representing the mean value of the distribution
        stdev (float) representing the standard deviation of the distribution
        data_list (list of floats) a list of floats extracted from the data file
    """
    def __init__(self, mu=0, sigma=1):
        self.c = 1.0 / math.sqrt(2*math.pi)
        self.mean = mu
        self.stdev = sigma
        self.data = []

    def calculate_mean(self):
        """Method to calculate the mean of the data set.

        Args:
            None

        Returns:
            float: mean of the data set
        """
        mu = self.mean
        n = len(self.data)
        if n > 0:
            mu = sum(self.data)/n
            self.mean = mu
        return mu

    def calculate_stdev(self, sample=True):

        """Method to calculate the standard deviation of the data set.

        Args:
            sample (bool): whether the data represents a sample or population

        Returns:
            float: standard deviation of the data set
        """

        sigma = self.stdev
        n = len(self.data)
        mu = self.calculate_mean()

        if sample:
            # sample standard deviation
            if n > 1:
                sigma = math.sqrt(sum(((x - mu)**2 for x in self.data))/(n-1))
                self.stdev = sigma
        else:
            # population standard deviation
            if n > 0:
                sigma = math.sqrt(sum(((x - mu)**2 for x in self.data))/n)
                self.stdev = sigma

        return sigma

    def read_data_file(self, file_name, sample=True):

        """Method to read in data from a txt file. The txt file should have
        one number (float) per line. The numbers are stored in the data attribute.
        After reading in the file, the mean and standard deviation are calculated

        Args:
            file_name (string): name of a file to read from

        Returns:
            None
        """

        # Read in the data from the file given
        data_list = []
        with open(file_name) as file:
            line = file.readline()
            while line:
                data_list.append(int(line))
                line = file.readline()

        # Update gaussian object
        self.data = data_list
        self.calculate_stdev(sample)

    def plot_histogram(self):
        """Method to output a histogram of the instance variable data using
        matplotlib pyplot library.

        Args:
            None

        Returns:
            None
        """

        # make the plot
        fig, axis = plt.subplots()
        axis.hist(self.data)
        axis.set_title('Histogram of Data')
        axis.set_ylabel('Data')
        axis.set_ylabel('Count')
        plt.show()

    def pdf(self, x):
        """Probability density function calculator for the gaussian distribution.

        Args:
            x (float): point for calculating the probability density function


        Returns:
            float: probability density function output
        """

        exp = math.exp
        sqrt = math.sqrt
        c = self.c

        mu = self.mean
        sigma = self.stdev

        return (c/sigma)*exp(-0.5*((x - mu)/sigma)**2)

    def plot_histogram_pdf(self, n_spaces = 50):

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

        xs = []
        ys = []

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
