*LPARAMETERS 
LPARAMETERS nMes,nAnio,cMes
local crutabas, cmaestro, cnomina, cdesc
local nmonto, nmonto1, liquido, liquidot, l, clin, busca, nnum, nrfc, nnom, tipo, empleado
STORE 0 TO nmonto, nmonto1, liquido, liquidot, l
   crutalis= "Listados\"
    nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
    nAnio= alltrim(Str(nAnio))
    crutabas= "RESPALDOS\NOMI" + nAnio + "\"
    cdesc= "Alfabe" + nAnio + nMes
    cmaestro= "Maes" + nAnio + nMes
    if (!file(crutabas + cmaestro + ".Dbf"))
*    	MESSAGEBOX(crutabas + cmaestro + ".Dbf")
       messagebox("RESPALDO NO ENCONTRADO: " + cMes + " DE " + nAnio,'Aviso')
       CLOSE TABLES ALL 
       RETURN
    ENDIF
	USE &crutabas&cmaestro ALIAS maestro IN 0
	SELECT maestro
	INDEX on Jpp+rfc TO temp\&cmaestro
*	INDEX on Jpp+STR(num,6) TO temp\&cmaestro


   tipo = Space(3)
   mes= Space(10)
   annio= 0
   select maestro
   set filter to superviven="S"
   goto top
   cprin=crutalis+cdesc+'.txt'
   set printer to &cprin
   set printer on
   set device to printer
   clin= 99
   empleado= 0
   nPag=0
   do while (!EOF())
      tipo= jpp
      nrfc= rfc
      nnum= num
      nnom= nombre
      nele= nomelec
      if (clin > 60)
      	 nPag=nPag+1
         @  1,  1 say ;
            "LISTADO ALFABETICO DE JUBILADOS, PENSIONADOS Y PENSIONISTAS"
         @  2,  1 say ;
            "CORRESPONDIENTE AL MES DE "+cMes+" DEL AÑO "+nAnio+SPACE(20)+'Pag.'+ALLTRIM(STR(nPag))
         @  3,  1 say Replicate("-", 80)
         @  4,  1 say "NUM    R.F.C.               NOMBRE                        NOMINA ELECTRONICA"
         @  5,  1 say Replicate("-", 80)
         do case
         case tipo = 'JUP'
            @  7,  1 say "TIPO DE NOMINA: JUBILADO CENTRO"
         case tipo = "JUF"
            @  7,  1 say "TIPO DE NOMINA: JUBILADO FORANEO"
         case tipo = 'PDP'
            @  7,  1 say "TIPO DE NOMINA: PENSIONADO CENTRO"
         case tipo = "PDF"
            @  7,  1 say "TIPO DE NOMINA: PENSIONADO FORANEO"
         case tipo = 'PTP'
            @  7,  1 say "TIPO DE NOMINA: PENSIONISTA CENTRO"
         case tipo = "PTF"
            @  7,  1 say "TIPO DE NOMINA: PENSIONISTA FORANEO"
         case tipo = "PEA"
            @  7,  1 say "TIPO DE NOMINA: PENSION ALIMENTICIA CENTRO"
         endcase
         clin= 9
      endif

      @ clin,  1 say nnum picture "999999"
      @ clin, 10 say nrfc picture "@!"
      @ clin, 30 say nnom picture "@!"
      @ clin, 75 say nele picture "@!"
      clin=clin+1
      empleado=empleado+1
      skip 
      if (tipo != jpp)
         clin=clin+1
         @ clin,  1 say " TOTAL DE EMPLEADOS: " + Str(empleado, 6)
         empleado= 0
         clin= 99
      endif
   enddo
   @ 0,0 Say ''
   eject
   set printer off
   set printer to 
   set device to screen
   return
