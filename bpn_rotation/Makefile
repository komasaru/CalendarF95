FC = gfortran
CFLAGS = -c -O
TARGET = bpn_rotation
OBJS = const.o time.o matrix.o fundamental_argument.o nutation.o eph_bpn.o bpn_rotation.o
MODS = const.mod time.mod matrix.mod fundamental_argument.mod nutation.mod eph_bpn.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

