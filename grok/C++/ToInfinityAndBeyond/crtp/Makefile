CXX := clang++
BINARIES := crtp

binaries: $(BINARIES)

crtp: crtp.cpp counter.h
	$(CXX) -Wall -std=gnu++20 $< -o $@

clean:
	rm -f $(BINARIES)
