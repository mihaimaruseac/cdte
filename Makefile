.PHONY: all clean run

TARGET = cdte

SOURCES = $(shell find . -type f -name '*.hs')
HIFILES = $(patsubst %.hs, %.hi, $(SOURCES))
OFILES = $(patsubst %.hs, %.o, $(SOURCES))

all: $(TARGET)

$(TARGET): $(SOURCES)
	@ghc --make Main.hs -o $(TARGET)

run: $(TARGET)
	./$(TARGET)

clean:
	@$(RM) $(TARGET) $(HIFILES) $(OFILES)

