LPARAMETERS nMes,nAnio,cMes,CveQuin,IndNom,TipoP
local crutabas, cmaestro, cnomina, cdesc, cjpp
   crutades= "Listados\"
    nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
    nAnio= alltrim(Str(nAnio))
    crutabas= "RESPALDOS\NOMI" + nAnio + "\"
    cdesc= "J480" + nAnio + nMes
    cmaestro= "Maes" + nAnio + nMes
    cnomina= "Nomi" + nAnio + nMes
	nTmp=crutades+cdesc
    if (!file(crutabas + cmaestro + ".Dbf") .OR. !file(crutabas + cnomina + ".Dbf"))
       messagebox("RESPALDO NO ENCONTRADO: " + cMes + " DE " + nAnio,'Aviso')
       CLOSE TABLES ALL 
       RETURN
    ENDIF
    if file(crutades + cdesc + ".Dbf")
       if MESSAGEBOX("ALERTA!!!, EL ARCHIVO YA FUE GENERADO"+CHR(13)+CHR(13)+'DESEA REEMPLAZARLO...',1+32,'Aviso')=2
	       CLOSE TABLES ALL 
	       RETURN
       endif
    endif

CLOSE TABLES all
CREATE TABLE &nTmp FREE ;
		(NOMBRE C(40),RFC C(13),PROYECTO C(17),;
		 CVEDESC N(3),FOLIO N(5),NUMDESC N(3),;
		 TOTDESC N(3),IMPORTE N(11, 2),REGISTRO N(10))

*!*		USE data\tablas\perded ORDER clave IN 0
	USE &crutabas&cnomina ALIAS &cnomina IN 0
	USE &crutabas&cmaestro ALIAS &cmaestro IN 0
	SELECT(cmaestro)
	INDEX on Jpp+Str(Num,6) TO temp\&cmaestro
	SET FILTER TO superviven='S'
	SELECT(cnomina)
	INDEX on jpp+STR(numjpp,6)+STR(clave,3) TO temp\&cnomina
      set relation to Jpp+Str(NumJpp,6) into (cmaestro)
      set filter to ( Clave== 227 .OR. Clave== 226  .OR.  Clave== 221 )
      goto top
      do while (!EOF())
         if !EOF(CMAESTRO) &&(!(cmaestro)->(EOF()))
            select (cdesc)
            append blank
            replace nombre with &cmaestro->NOMBRE
            replace rfc with &cmaestro->RFC
            replace proyecto with &cmaestro->jpp+ALLTRIM(STR(&cmaestro->num)) &&cmaestro->PROYECTO
            replace cvedesc with &cnomina->CLAVE
            replace folio with &cnomina->FOLIO
            replace numdesc with &cnomina->PAGO4
            replace totdesc with &cnomina->PAGOT
            replace importe with &cnomina->MONTO
            replace registro with &cnomina->CHEQUE
         endif
         select (cnomina)
		Skip
      enddo
ImprimeRHumanos(nMes,nAnio,cMes)
DO progs\baseampleada WITH VAL(nAnio),VAL(nMes),CveQuin,IndNom,TipoP
Return
********************************

FUNCTION ImprimeRHumanos(nMes,nAnio,cMes)
local nano, nquin, cprin,;
      cdesc, cjpp, descri, nclave, ntot, nemp, ntotjp, ;
      nempjp
crutades= "Listados\"
nano= Val(right(Str(Year(Date())), 2))
nquin= 0
nano= nAnio
nquin= nMes
WAIT Windows 'GENERANDO LISTADO: '+cMes NOWAIT 
CLOSE TABLES ALL 
   cdesc= "J480" + nano + nquin
   USE &crutades&cdesc IN 0
*   cprin=crutades+cdesc+'.txt'
   cprin=crutades+'Recursos.txt'
   set printer to &cprin
   set printer on
   set device to printer
   nclave= 0
   ntot= 0
   nemp= 0
   ntotjp= 0
   nempjp= 0
   nlin= 99
   npag= 0
   nlon= 96
   select (cdesc)
   index on cvedesc to temp\cvedesc
   GO top
   do while (!EOF())
      nclave= cvedesc
      nlin= 99
      do while (nclave == cvedesc)
      	DO CASE 
         	case (nclave == 221)
            	descri= " DIRECTOS"
         	case (nclave == 226)
            	descri= "LINEA BLANCA"
         	case (nclave == 227)
            		descri= "PAQUETE DE MATERIALES"
		ENDCASE 
         if (nlin > 60)
            npag=npag+1
            @  1,  0 say "SECRETARIA DE ADMINISTRACION"
            @  1, 70 say "OFICINA DE PENSIONES"
            @  2,  0 say "LISTADO DE PRESTAMOS, "
            @  2, 23 say descri
            @  4,  0 say "FECHA: " + DToC(Date())
            @  4, 75 say "PAG: " + Str(npag, 4)
            @  5,  0 say Replicate("=", nlon)
            @  6,  4 say ;
               " R.F.C.       NOMBRE                             PROYECTO           NUM/DE  FOLIO   IMPORTE"
            @  7,  0 say Replicate("=", nlon)
            nlin= 9
         endif
         @ nlin,  0 say rfc
         @ nlin, 13 say nombre
         @ nlin, 54 say proyecto
         @ nlin, 71 say numdesc
         @ nlin, 75 say totdesc
         @ nlin, 79 say folio
         @ nlin, 84 say importe
         nlin=nlin+1
         nempjp=nempjp+1
         ntotjp= ntotjp + importe
         skip
      enddo
      nemp= nemp + nempjp
      ntot= ntot + ntotjp
      nempjp= 0
      ntotjp= 0
      if (ntot > 0)
         nlin= nlin + 2
         @ nlin, 10 say Replicate("=", 5)
         @ nlin, 75 say Replicate("=", 13)
         nlin=nlin+1
         @ nlin,  0 say "EMPLEADOS:" + Str(nemp, 5)
         @ nlin, 69 say "TOTAL:" + Transform(ntot, "99,999,999.99")
      endif
      nemp= 0
      ntot= 0
   enddo
   set printer off
   set printer to 
   set device to screen
*	WAIT windows TO clear
	CLOSE TABLES All
   RETURN