BMP_FILES := $(wildcard *.bmp)
PNG_FILES := $(BMP_FILES:.bmp=.png)

all: $(PNG_FILES)

%.png: %.bmp
	convert $<  $@
