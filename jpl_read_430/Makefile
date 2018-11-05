FC = gfortran
CFLAGS = -c -O
TARGET = jpl_read_430
OBJS = const.o time.o eph_jpl.o jpl_read_430.o
MODS = const.mod time.mod eph_jpl.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

