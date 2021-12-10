CXX := clang++
BINARIES := crtp

binaries: $(BINARIES)

crtp: counter.cpp
	$(CXX) -Wall -std=gnu++20 $< -o $@

clean:
	rm -f $(BINARIES)
