*LPARAMETERS 
LPARAMETERS nMes,nAnio,cMes
local crutabas, cmaestro, cnomina, cdesc, aStruc, nTmp1, nTmp2
local nmonto, nmonto1, liquido, liquidot, l, clin, busca, nnum, nrfc, nnom, tipo, empleado
STORE 0 TO nmonto, nmonto1, liquido, liquidot, l
   crutalis= "Listados\"
    nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
    nAnio= alltrim(Str(nAnio))
    crutabas= "RESPALDOS\NOMI" + nAnio + "\"
    cdesc= "BANORTE" + nAnio + nMes
    cmaestro= "Maes" + nAnio + nMes
    cnomina= "Nomi" + nAnio + nMes
    if (!file(crutabas + cmaestro + ".Dbf") .OR. !file(crutabas + cnomina + ".Dbf"))
       messagebox("RESPALDO NO ENCONTRADO: " + cMes + " DE " + nAnio,'Aviso')
       CLOSE TABLES ALL 
       RETURN
    ENDIF
		CLOSE TABLES all
		aStruc= '(JPP C( 3, 0), NUM N( 6, 0), NOMBRE C(40, 0), NETO N( 12, 2), BANCO C(3,0), TCTA C(2,0), CUENTA C(25,0), RFC C(13,0))'
		nTmp1='Listados\BANORTE'
		CREATE TABLE &nTmp1 FREE &aStruc
*		CLOSE TABLES all
*		USE &nTmp ALIAS alias liquido

*		CLOSE TABLES all
*		aStruc= '(JPP   C( 3, 0), NUM N( 6, 0),NOMBRE   C(40, 0), LIQUIDO N( 12, 2))'
		nTmp2='Listados\BANAMEX'
		CREATE TABLE &nTmp2 FREE &aStruc
		CLOSE TABLES all
		USE &nTmp1 ALIAS liquido IN 0
		USE &nTmp2 ALIAS liquido2 IN 0
		
	USE &crutabas&cnomina ALIAS nominew IN 0
	USE &crutabas&cmaestro ALIAS maestro IN 0
	SELECT maestro
	INDEX on Jpp+Str(Num,6) TO temp\&cmaestro
	SELECT nominew
	INDEX on jpp+STR(numjpp,6)+STR(clave,3) TO temp\&cnomina


   tipo = Space(3)
   mes= Space(10)
   annio= 0
   select maestro
   set filter to NOMELEC="S" .AND. superviven="S" .AND. "BANORTE"$BANCO
   goto top
   cprin=crutalis+'liquido.txt'
   set printer to &cprin
   set printer on
   set device to printer
*   cprin=crutalis+cdesc+'.txt'
   clin= 99
   empleado= 0
   nPag=0
   do while (!EOF())
      tipo= jpp
      if (clin > 60)
      	 nPag=nPag+1
         @  1,  1 say ;
            "LISTADO ALFABETICO DE JUBILADOS, PENSIONADOS Y PENSIONISTAS"
         @  2,  1 say ;
            "CON LIQUIDO BANCO BANORTE CORRESPONDIENTE AL MES DE " + ;
            cMes + " DEL AÑO " + nAnio+'  Pag.'+ALLTRIM(STR(nPag))
         @  3,  1 say Replicate("-", 80)
         @  4,  1 say "NUM    R.F.C.               NOMBRE                              LIQUIDO"
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
      busca= jpp + Str(num, 6)
      nrfc= rfc
      nnum= num
      nnom= nombre
      select NOMINEW
      seek busca
      if (Found())
         do while (busca = jpp + Str(numjpp, 6) .AND. !EOF())
            if (clave < 61)
               nmonto= nmonto + monto
            ELSE
            	if (clave > 61)
               		nmonto1= nmonto1 + monto
               	endif
            endif
            skip 
         enddo
      ENDIF
      liquido= nmonto - nmonto1
      liquidot= liquidot + liquido

      SELECT liquido
      APPEND BLANK
      REPLACE jpp WITH maestro.jpp, NUM WITH nNum, Nombre WITH nNom, Neto WITH liquido, ;
      		BANCO WITH '072', TCTA WITH '01', CUENTA WITH MAESTRO.CUENTABANC, RFC WITH MAESTRO.RFC
       
      select MAESTRO
      l= Transform(liquidot, "999,999,999.99")
      @ clin,  1 say nnum picture "999999"
      @ clin, 10 say nrfc picture "@!"
      @ clin, 30 say nnom picture "@!"
      @ clin, 70 say liquido picture "9,999,999.99"
      clin=clin+1
      empleado=empleado+1
      liquido= 0
      nmonto= 0
      nmonto1= 0
      skip 
      if (tipo != jpp)
         clin=clin+1
         @ clin,  1 say " TOTAL DE EMPLEADOS: " + Str(empleado, 6) + SPACE(24) + l
         liquidot= 0
         empleado= 0
         clin= 99
      endif
   enddo
   @0,0 Say ''
   eject
   set printer off
   set printer to 
   set device to screen
   banamex(nMes,nAnio,cMes)
   return

FUNCTION banamex(nMes,nAnio,cMes)
local nmonto, nmonto1, liquido, liquidot, l, clin, busca, nnum, nrfc, nnom, tipo, empleado, cban
LOCAL aStruc, nTmp
   tipo = Space(3)
   STORE 0 TO nmonto, nmonto1, liquido, liquidot, l
*   cban='Listados\BANAMEX'+nAnio+nMes+'.txt'
   cban='Listados\Liquido2.txt'
   select maestro
   set filter to NOMELEC="S" .AND. superviven="S" .AND. "BANAMEX"$BANCO
   goto top
   set printer to &cban
   set printer on
   set device to printer
   clin= 99
   empleado= 0
   nPag=0
   do while (!EOF())
      tipo= jpp
      if (clin > 60)
      	 nPag=nPag+1
         @  1,  1 say ;
            "LISTADO ALFABETICO DE JUBILADOS, PENSIONADOS Y PENSIONISTAS"
         @  2,  1 say ;
            "CON LIQUIDO BANCO BANAMEX CORRESPONDIENTE AL MES DE " + ;
            cMes + " DEL AÑO " + nAnio+'  Pag.'+ALLTRIM(STR(nPag))
         @  3,  1 say Replicate("-", 80)
         @  4,  1 say ;
            "NUM    R.F.C.               NOMBRE                              LIQUIDO"
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
      busca= jpp + Str(num, 6)
      nrfc= rfc
      nnum= num
      nnom= nombre
      select NOMINEW
      seek busca
      if (Found())
         do while (busca = jpp + Str(numjpp, 6) .AND. !EOF())
            if (clave < 61)
               nmonto= nmonto + monto
            ELSE
            	if (clave > 61)
               		nmonto1= nmonto1 + monto
               	Endif
            endif
            skip 
         enddo
      endif
      liquido= nmonto - nmonto1
      liquidot= liquidot + liquido

      SELECT liquido2
      APPEND BLANK
*      REPLACE jpp WITH maestro.jpp, NUM WITH nNum, RFC WITH nRfc, Nombre WITH nNom, Neto WITH liquido
      REPLACE jpp WITH maestro.jpp, NUM WITH nNum, Nombre WITH nNom, Neto WITH liquido, ;
      		BANCO WITH '072', TCTA WITH '01', CUENTA WITH MAESTRO.CUENTABANC, RFC WITH MAESTRO.RFC

      select MAESTRO
      l= Transform(liquidot, "999,999,999.99")
      @ clin,  1 say nnum picture "999999"
      @ clin, 10 say nrfc picture "@!"
      @ clin, 30 say nnom picture "@!"
      @ clin, 70 say liquido picture "9,999,999.99"
      clin=clin+1
      empleado=empleado+1
      liquido= 0
      nmonto= 0
      nmonto1= 0
      skip 
      if (tipo != jpp)
         clin=clin+1
         @ clin,  1 say " TOTAL DE EMPLEADOS: " + Str(empleado, 6) + SPACE(24) + l
         liquidot= 0
         empleado= 0
         clin= 99
      endif
   ENDDO
   @0,0 Say ''
   eject
   set printer off
   set printer to 
   set device to screen
   SELECT liquido
   COPY TO listados\BANORTE TYPE xls
   SELECT liquido2
	&& GENERA LAYOUT DE BANAMEX
   SUM neto TO nTotal
   COUNT TO nDepo
   SET PRINTER TO LISTADOS\BANAMEX.TXT
   set printer on
   SET DEVICE TO PRINT

   SET DATE ANSI
   	@ 0,0 SAY '1000089909706'+STRTRAN(dtoc(DATE()+1),'.','')+PADL('1',4,'0')+;
   				LEFT('OFICINA DE PENSIONES DEL ESTADO DE OAXACA',36)+PADR('JUBILADOS',20,' ')+;
   					'15D01'
   	@ PROW()+1,0 SAY '21001'+PADL(ALLTRIM(STRTRAN(STR(nTotal,18,2),'.','')),18,'0')+'01'+PADL('44110110143',20,'0')+;
   				PADL(ALLTRIM(STR(nDepo,6)),6,'0')
	GO TOP 
   DO WHILE !EOF()
	   	@ PROW()+1,0 SAY '3000101001'+PADL(ALLTRIM(STRTRAN(STR(Neto,18,2),'.','')),18,'0')+'03'+PADL(ALLTRIM(Cuenta),20,'0')+;
	   				PADR(ALLTRIM(STR(Num,6)),16,' ')+PADR(ALLTRIM(nombre),55,' ')+PADR(ALLTRIM(rfc),35,' ')
	   	@ PROW(),PCOL() say SPACE(35+35+35)+'000000'
	   	@ PROW(),PCOL() say SPACE(14+8+80+50)
	   	SKIP 
   ENDDO
   	@ PROW()+1,0 SAY '4001'+PADL(ALLTRIM(STR(nDepo,6)),6,'0')+PADL(ALLTRIM(STRTRAN(STR(nTotal,18,2),'.','')),18,'0')+'000001'+;
   				PADL(ALLTRIM(STRTRAN(STR(nTotal,18,2),'.','')),18,'0')
   				
   SET DATE BRITISH
   set printer off
   SET PRINTER TO
   SET DEVICE TO SCREEN
   
   COPY TO listados\BANAMEX TYPE xls
   CLOSE TABLES All
*   DELETE FILE listados\BANORTE.DBF
*   DELETE FILE listados\BANAMEX.DBF
return
