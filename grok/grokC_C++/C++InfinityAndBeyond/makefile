CXX=g++
CPPFLAGS=-Wall -std=c++2a
BINARIES=hw

default: binaries

binaries: $(BINARIES)

hw: hw.cpp
	$(CXX) $(CPPFLAGS) hw.cpp -o hw

clean:
	rm -f $(BINARIES)
