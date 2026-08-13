TARGET := bin/MathParser
SRC := src/Main.hs
BUILD := build

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	mkdir -p $(dir $@) $(BUILD)
	ghc -O2 -outputdir $(BUILD) -o $@ $(SRC)

clean:
	rm -rf $(BUILD) $(TARGET)
