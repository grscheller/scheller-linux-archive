CXX=g++
CPPFLAGS=-Wall -std=gnu++11
BINARIES=hw crtp

default: binaries

binaries: $(BINARIES)

# guessing where -ptrhead goes
hw: hw.cpp
	$(CXX) $(CPPFLAGS) -pthread hw.cpp -o hw

crtp: crtp.cpp
	$(CXX) $(CPPFLAGS) -pthread crtp.cpp -o crtp

clean:
	rm -f $(BINARIES)
