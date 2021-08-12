CXX := clang++
BINARIES := crtp

binaries: $(BINARIES)

crtp: counter.cpp
	$(CXX) -Wall -std=gnu++11 $< -o $@

clean:
	rm -f $(BINARIES)
