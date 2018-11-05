FC = gfortran
CFLAGS = -c -O
TARGET = conv_coord
OBJS = const.o matrix.o trigonometric.o coord.o conv_coord.o
MODS = const.mod matrix.mod trigonometric.mod coord.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

