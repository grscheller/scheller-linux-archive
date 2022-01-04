CXX := clang++
BINARIES := spanit

binaries: $(BINARIES)

spanit: spanit.cpp
	$(CXX) -Wall -std=gnu++20 $< -o $@

clean:
	rm -f $(BINARIES)
