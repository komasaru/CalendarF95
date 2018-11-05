FC = gfortran
CFLAGS = -c -O
TARGET = apparent_sun_moon_jpl
OBJS = const.o time.o eph_jpl.o matrix.o trigonometric.o coord.o \
       fundamental_argument.o nutation.o eph_bpn.o apos.o \
       apparent_sun_moon_jpl.o
MODS = const.mod time.mod eph_jpl.mod matrix.mod trigonometric.mod coord.mod \
       fundamental_argument.mod nutation.mod eph_bpn.mod apos.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

