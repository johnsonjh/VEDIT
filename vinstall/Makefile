CC = cc
CFLAGS = -O
CORE = image.o table.o port.o

all: instcore vcfg vintmod

instcore: instcore.o image.o table.o port.o
	$(CC) $(CFLAGS) -o $@ instcore.o image.o table.o port.o

vcfg: vcfg.o image.o table.o inifile.o port.o
	$(CC) $(CFLAGS) -o $@ vcfg.o image.o table.o inifile.o port.o

vintmod: vintmod.o inifile.o table.o port.o
	$(CC) $(CFLAGS) -o $@ vintmod.o inifile.o table.o port.o

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f instcore vcfg vintmod ./*.o

instcore.o vcfg.o vintmod.o image.o table.o inifile.o port.o: port.h
instcore.o vcfg.o image.o: image.h
instcore.o vcfg.o vintmod.o table.o: table.h
vcfg.o vintmod.o inifile.o: inifile.h
