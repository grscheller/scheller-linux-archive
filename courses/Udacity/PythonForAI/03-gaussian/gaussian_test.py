# Using pdoc3 for testing

from gaussian import Gaussian

gauss = Gaussian(25, 2)
euler = Gaussian()

class Test_Gaussian_simple:

    def test_initialization(self) -> None: 
        assert gauss.mean == 25
        assert gauss.stdev == 2

    def test_pdf(self) -> None:
        assert round(gauss.pdf(25), 5) == 0.19947

    def test_mean_calculation(self) -> None:
        euler.read_data_file('numbers.txt')
        assert euler.mean == sum(euler.data)/float(len(euler.data))

    def test_stdev_calculation(self) -> None:
        euler.read_data_file('numbers.txt', True)
        assert round(euler.calculate_stdev(), 2) == 92.87
        euler.read_data_file('numbers.txt', False)
        assert round(euler.calculate_stdev(sample = False), 2) == 88.55
                
