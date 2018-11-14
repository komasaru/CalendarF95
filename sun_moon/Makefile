FC = gfortran
CFLAGS = -c -O
TARGET = sun_moon
OBJS = const.o time.o delta_t.o calc.o sun_moon.o
MODS = const.mod time.mod delta_t.mod calc.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

