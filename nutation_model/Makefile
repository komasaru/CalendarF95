FC = gfortran
CFLAGS = -c -O
TARGET = nutation_model
OBJS = const.o time.o fundamental_argument.o nutation.o nutation_model.o
MODS = const.mod time.mod fundamental_argument.mod nutation.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

