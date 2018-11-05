FC = gfortran
CFLAGS = -c -O
TARGET = conv_time
OBJS = const.o time.o delta_t.o conv_time.o
MODS = const.mod time.mod delta_t.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

