FC = gfortran
CFLAGS = -c -O
TARGET = apparent_sun_moon_jcg
OBJS = apparent_sun_moon_jcg.o

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS)

