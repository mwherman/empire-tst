@echo c4zvv MT file.c4 file.endf file.zvd
@echo %1 %2 %3 %4
c4dat.exe %1 %2 tmp.dat
datzvd.exe tmp.dat c4.zvd
endzvd.exe %1 %3 end.zvd
@echo #!zvview.exe >%4
type c4.zvd >>%4
type end.zvd >>%4
zvv_w32.exe %4
