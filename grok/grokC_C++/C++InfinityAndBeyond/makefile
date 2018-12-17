CXX=g++
CPPFLAGS=-Wall
BINARIES=hw

default: binaries

binaries: $(BINARIES)

# guessing where -ptrhead goes
hw: hw.cpp
	$(CXX) $(CPPFLAGS) -pthread hw.cpp -o hw

clean:
	rm -f $(BINARIES)
