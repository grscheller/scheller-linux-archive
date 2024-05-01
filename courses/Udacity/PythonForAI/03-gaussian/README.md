# Gaussian module

Gaussian class based on an exercise from Udacity's Python for AI course.

## Factoids

* do not intend to push this project to PyPI
  * might incorporate it into my grscheller.boring-math project
* using Python 3.12.2 instead of the Python 3.6.3 used by Udacity
  * 3.6.3 was version of Python out when PyTorch was released in 2016
* using `__future__` statement to import annotations
  * for use by Python std library typing module 
  * annotations targeted for Python 3.13.X

## Testing

Pytest plotting tests depend on user input!!!

This is considered bad practice except for single user/single maintainer
projects. For this to work, pytest must be run with the -s option.

Example: `$ pytest -s gaussian_test.py`

## License Summary

Apache v2.0 License

See [LICENSE](LICENSE) for license details
and [NOTICE](NOTICE) for copyright details.
