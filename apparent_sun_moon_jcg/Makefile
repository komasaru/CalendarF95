FC = gfortran
CFLAGS = -c -O
TARGET = apparent_sun_moon_jcg
OBJS = const.o time.o apparent_sun_moon_jcg.o
MODS = const.mod time.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

