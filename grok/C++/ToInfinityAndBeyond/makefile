CXX=g++
CPPFLAGS=-Wall
BINARIES=hw crtp

default: binaries

binaries: $(BINARIES)

hw: hw.cpp
	$(CXX) $(CPPFLAGS) -std=gnu++11 -pthread hw.cpp -o hw

crtp: crtp.cpp
	$(CXX) $(CPPFLAGS) -std=gnu++11 crtp.cpp -o crtp

clean:
	rm -f $(BINARIES)
