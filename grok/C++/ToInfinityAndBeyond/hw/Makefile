CXX := g++
BINARIES := hw

binaries: $(BINARIES)

hw: hw.cpp
	$(CXX) -Wall -std=gnu++11 -pthread $< -o $@

clean:
	rm -f $(BINARIES)
