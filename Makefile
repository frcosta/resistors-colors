FC=gfortran
CC=gcc
FFLAGS=-O3 -Wall -Wextra -std=f2008
SRC = res_cores.f90
OBJ = $(SRC:.f90=.o)

OBJ_LINK = $(OBJ) getch.o

all: res_cores.exe

res_cores.exe: $(OBJ_LINK)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

clean:
	del *.o
	del res_cores.exe