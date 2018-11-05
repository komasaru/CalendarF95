FC = gfortran
CFLAGS = -c -O
TARGET = greenwich_time
OBJS = const.o angle.o time.o delta_t.o precession.o \
			 fundamental_argument.o nutation.o matrix.o rotation_fw.o \
			 cip_cio.o greenwich.o greenwich_time.o
MODS = const.mod angle.mod time.mod delta_t.mod precession.mod \
			 fundamental_argument.mod nutation.mod matrix.mod rotation_fw.mod \
			 cip_cio.mod greenwich.mod green_wich.mod

.SUFFIXES: .o .f95
.PHONY: all clean

all:	$(TARGET)

.f95.o:
	$(FC) $(CFLAGS) $<

$(TARGET):	$(OBJS)
	$(FC) -o $@ $(OBJS)

clean:
	@rm -f $(TARGET) $(OBJS) $(MODS)

