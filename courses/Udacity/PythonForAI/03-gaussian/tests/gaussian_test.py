# Using pytest for testing
#
# WARNING: Plotting tests depend on user input!!!
#          This is considered bad practice except for
#          single user/maintainer projects.
# 
#          - pytest must be run with the -s option
#          - test data hard coded, run tests from project root
#
# Example: $ pytest -s gaussian_test.py
#

import sys
from ai.gaussian import Gaussian

gauss = Gaussian(25, 2)
euler = Gaussian()

def plot_viewable(plot_name: str) -> bool:
    print(f'\nDid the {plot_name} plot display correctly? ([y] or n) ', end='')
    ans = input()
    return True if ans != 'n' else False

class Test_Gaussian:

    def test_initialization(self) -> None: 
        assert gauss.mean == 25
        assert gauss.stdev == 2

    def test_pdf(self) -> None:
        assert round(gauss.pdf(25), 5) == 0.19947

    def test_mean_calculation(self) -> None:
        euler.read_data_file('data/numbers.txt')
        assert euler.mean == sum(euler.data)/float(len(euler.data))

    def test_stdev_calculation(self) -> None:
        euler.read_data_file('data/numbers.txt', True)
        assert round(euler.calculate_stdev(), 2) == 92.87
        euler.read_data_file('data/numbers.txt', False)
        assert round(euler.calculate_stdev(sample = False), 2) == 88.55

    def test_plot_histogram(self) -> None:
        euler.plot_histogram()
        if plot_viewable('histogram'):
            assert True
        else:
            assert False

    def test_plot_histogram_pdf(self) -> None:
        euler.plot_histogram_pdf()
        if plot_viewable('histogram_pdf'):
            assert True
        else:
            assert False
