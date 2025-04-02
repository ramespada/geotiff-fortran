.SUFFIXES: .o .f90

FC  =gfortran
LIBS= -L/usr/lib/x86_64-linux-gnu -lnetcdf -lnetcdff -lm
INC = -I/usr/include
DEBUG=-g -fbacktrace
FCFLAGS=-O0 -ffree-line-length-none -Wall ${DEBUG}##-Wunused #
LDFLAGS=

OBJS=geotiff.o test.o
EXE =a.out

$(EXE): $(OBJS)
	$(FC) $(LDFLAGS) $(OBJS) $(LIBS) -o $(EXE)
%.o: %.f90
	$(FC) -c $(FCFLAGS) $(INC) $< -o $@
clean:
	rm -f *.o *.mod # a.out
