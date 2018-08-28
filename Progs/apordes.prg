LPARAMETERS nMes,nAnio,cMes
local crutabas, cmaestro, cnomina, cdesc, cjpp
   crutades= "Listados\"
    nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
    nAnio= alltrim(Str(nAnio))
    crutabas= "RESPALDOS\NOMI" + nAnio + "\"
    cdesc= "D980" + nAnio + nMes
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
      	(NOMBRE C(40),RFC C(12),;
         PROYECTO C(11),CVEDESC N(3),;
         FOLIO N(5),NUMDESC N(3),;
         TOTDESC N(3),IMPORTE N(9,2),;
         TIPODESC C(1),DESDE D(8),;
         HASTA D(8))
	USE data\tablas\perded ORDER clave IN 0
	USE &crutabas&cnomina ALIAS &cnomina IN 0
	USE &crutabas&cmaestro ALIAS &cmaestro IN 0
	SELECT(cmaestro)
    set filter to superviven="S"
	INDEX on Jpp+Str(Num,6) TO temp\&cmaestro
	SELECT(cnomina)
	INDEX on jpp+STR(numjpp,6)+STR(clave,3) TO temp\&cnomina

      set relation to Jpp+Str(NumJpp,6) into (cmaestro)
      set filter to Clave== 205 .OR. Clave== 206  .OR. Clave== 207 .OR.  Clave== 229
      goto top
      do while (!EOF())
         if !EOF(cmaestro)
            select (cdesc)
            append blank
            replace nombre with &cmaestro->NOMBRE
            replace rfc with &cmaestro->RFC
            replace proyecto with &cmaestro->JPP+ALLTRIM(STR(&cmaestro->NUM)) &&cmaestro->PROYECTO
            replace cvedesc with &cnomina->CLAVE
            replace numdesc with &cnomina->PAGO4
            replace totdesc with &cnomina->PAGOT
            replace importe with &cnomina->MONTO
            replace tipodesc with &cnomina->TIPOPAGO
            replace folio with &cnomina->FOLIO
            replace desde with &cnomina->FECHAINI
            replace hasta with &cnomina->FECHAFIN
         endif
         select (cnomina)
         Skip
      enddo
      select (cnomina)
      index on Str(Clave,3)+ Jpp+ Str(NumJpp,6) to temp\DNomiCve
      set relation to STR(Clave,3) into PerDed, Jpp+Str(NumJpp,6) into (cmaestro)
*	   cprin=crutades+cdesc+'.txt'
	   cprin=crutades+'LDeduc.txt'
      set printer to &cprin
      set printer on
      set device to printer
      for j= 1 to 2
         nlin= 99
         npag= 0
         nlon= 91
         for i= 1 to 2
            select (cnomina)
            if (i == 1)
               if (j == 1)
                  set filter to (Jpp== "JUP" .OR. Jpp== 'PDP' .OR. ;
                     Jpp== 'PTP') .AND. &cmaestro .NomElec== "N" .AND. ;
                     &cmaestro .SUPERVIVEN="S"
               else
                  set filter to (Jpp== "JUP" .OR. Jpp== 'PDP' .OR. ;
                     Jpp== 'PTP') .AND. &cmaestro .NomElec<> "N".AND. ;
                     &cmaestro .SUPERVIVEN="S"
               endif
            ELSE
            	if (j == 1)
    	           set filter to (Jpp== "JUF" .OR. Jpp== "PDF" .OR. ;
        	          Jpp== "PTF") .AND. &cmaestro .NomElec== "N" .AND. ;
            	      &cmaestro .SUPERVIVEN="S"
	            else
         	       set filter to (Jpp== "JUF" .OR. Jpp== "PDF" .OR. ;
            	      Jpp== "PTF") .AND. &cmaestro .NomElec<> "N" .AND. ;
                	  &cmaestro .SUPERVIVEN="S"
               	endif
            endif
            goto top
            locate for clave >= 50
            do while (!EOF())
               select (cnomina)
               nclave= clave
               ntotcve= 0
               nempcve= 0
               nlin= 99
               do while (nclave == clave)
                  cjpp= jpp
                  ntotjpp= 0
                  nempjpp= 0
                  do while (cjpp == jpp .AND. nclave == clave)
                     if (nlin > 60)
                        npag=npag+1
                        @  1,  0 say "SECRETARIA DE ADMINISTRACION"
                        @  1, nlon - 20 say "OFICINA DE PENSIONES"
                        @  2,  0 say padc("LISTADO DE DEDUCCIONES, " ;
                           + iif(j == 1, "NOMINA NORMAL", ;
                           "NOMINA ELECTRONICA") + ", DE LA CLAVE: " ;
                           + alltrim(Str(nclave)) + [ "] + ;
                           alltrim(perded->descri) + ["], nlon)
                        @  3,  0 say padc("EN EL MES DE " + ;
                           cMes + " DE " + LTrim(nAnio) + ".", ;
                           nlon)
                        @  4,  0 say "FECHA: " + DToC(Date())
                        @  4, nlon - 10 say "PAG: " + Str(npag, 4)
                        @  5,  0 say Replicate("=", nlon)
                        @  6,  4 say ;
                           "T/N NUM.   R.F.C.       NOMBRE                               NUM/DE   FOLIO    IMPORTE"
                        @  7,  0 say Replicate("=", nlon)
                        nlin= 8
                     endif
                     @ nlin,  0 say jpp
                     @ nlin,  4 say numjpp
                     @ nlin, 11 say &cmaestro->RFC
                     @ nlin, 25 say &cmaestro->NOMBRE
                     @ nlin, 65 say iif(pagot > 0, ALLTRIM(Str(pago4)) + "/" + ALLTRIM(Str(pagot)), " ")
                     @ nlin, 73 say Transform(folio, iif(folio > 0, "999999", " "))
                     @ nlin, 81 say monto picture "99,999.99"
                     nlin=nlin+1
                     nempjpp=nempjpp+1
                     ntotjpp= ntotjpp + monto
                     Skip
                  enddo
                  if (ntotjpp > 0)
                     nempcve= nempcve + nempjpp
                     ntotcve= ntotcve + ntotjpp
                     @ nlin, 10 say Replicate("-", 5)
                     @ nlin, 75 say Replicate("-", 13)
                     nlin=nlin+1
                     @ nlin,  0 say "EMPLEADOS:" + Str(nempjpp, 5)
                     @ nlin, 58 say "SUB-TOTAL " + cjpp + "-->:" + ;
                        Transform(ntotjpp, "99,999,999.99")
                     nlin= nlin + 1
                  endif
               enddo
               if (ntotcve > 0)
                  nlin= nlin + 1
                  @ nlin, 10 say Replicate("=", 5)
                  @ nlin, 75 say Replicate("=", 13)
                  nlin=nlin+1
                  @ nlin,  0 say "EMPLEADOS:" + Str(nempcve, 5)
                  @ nlin, 69 say "TOTAL:" + Transform(ntotcve, "99,999,999.99")
               endif
            enddo
         next
      NEXT
      @ 0,0 Say ''
      set printer off
      set printer to 
      set device to screen
      CLOSE TABLES All
	progesp(nMes,nAnio,cMes)
   RETURN
   
FUNCTION progesp(nMes,nAnio,cMes)
*!*	   local ppan, ppan2, nano, nquin, crutabas, cmaestro, cnomina, ;
*!*	      cfondo, cdesc, cjpp, nnum
*!*	   ccolor:= SetColor()
*!*	   set color to Gr+/B
*!*	   close databases
*!*	   crutabas:= "\SISTEMAS\MENUJUB\"
*!*	   crutades:= "descuen\"
*!*	   nano:= Val(right(Str(Year(Date())), 2))
*!*	   nquin:= 0
*!*	   ppan:= SaveScreen(7, 2, 10, 40)
*!*	   cuadro(7, 2, 10, 40)
*!*	   do while (.T.)
*!*	      nano:= iif(ISCHARACTER(nano), Val(nano), nano)
*!*	      nquin:= iif(ISCHARACTER(nquin), Val(nquin), nquin)
*!*	      @  7,  4 say "Programas Especiales"
*!*	      @  8,  4 say "A¥O A GENERAR: " get nAno picture "9999" valid ;
*!*	         nano > 0
*!*	      @  9,  4 say "Y MES        : " get nQuin picture "99"
*!*	      read
*!*	      if (LastKey() == K_ESC)
*!*	         exit
*!*	      endif
*!*	      if (preguntar(" ¨ COMENZAR ? ") = "N")
*!*	         loop
*!*	      endif
*!*	      nquin:= iif(nquin < 10, "0" + Str(nquin, 1), Str(nquin, 2))
*!*	      nano:= right(alltrim(Str(nano)), 2)
*!*	      crutabas:= "\SISTEMAS\MENUJUB\NOMI" + nano + "\"
*!*	      cdesc:= "J490" + nano + nquin
*!*	      cmaestro:= "Maes" + nano + nquin
*!*	      cnomina:= "Nomi" + nano + nquin
*!*	      if (!file(crutabas + cmaestro + ".Dbf") .OR. !file(crutabas + ;
*!*	            cnomina + ".Dbf"))
*!*	         alerta("      PRIMERO REALIZAR EL RESPALDO DEL MES " + ;
*!*	            nquin + " DE " + nano + "      ")
*!*	         loop
*!*	      endif
*!*	      if (file(crutades + cdesc + ".Dbf"))
*!*	         if ;
*!*	               (preguntar("ALERTA!!!, EL ARCHIVO YA FUE GENERADO, ¨ REEMPLAZAR ?") ;
*!*	               == "N")
*!*	            loop
*!*	         endif
*!*	      endif
*!*	      ppan2:= SaveScreen(11, 5, 13, 75)
*!*	      cuadro(11, 5, 13, 75, "R")
*!*	      @ 12,  6 say padc("ESPERE UN MOMENTO, GENERANDO DESCUENTOS ", ;
*!*	         68) color "W*/R"
local crutabas, cmaestro, cnomina, cdesc, cjpp
   crutades= "Listados\"
*    nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
*    nAnio= alltrim(STR(nAnio))
    crutabas= "RESPALDOS\NOMI" + nAnio + "\"
    cdesc= "J490" + nAnio + nMes
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
      		(NOMBRE C(50), RFC C(12), ;
         	FOLIO C(10), NUMDESC N(3), ;
         	TOTDESC N(3), IMPORTE N(7, 2), ;
         	PROYECTO C(18), DEPEND C(50), ;
         	CVEDESC N(3))
*!*	      dbcreate(crutades + cdesc + ".Dbf", aestruc)
*!*	      dbUseArea(.T., Nil, crutades + cdesc)
*!*	      dbUseArea(.T., Nil, crutabas + cmaestro)
	USE &crutabas&cnomina ALIAS &cnomina IN 0
	USE &crutabas&cmaestro ALIAS &cmaestro IN 0
	SELECT(cmaestro)
	INDEX on Jpp+Str(Num,6) TO temp\&cmaestro
	SELECT(cnomina)
	INDEX on jpp+STR(numjpp,6)+STR(clave,3) TO temp\&cnomina

*	SELECT(cmaestro)
*      index on Jpp+ Str(Num,6) to (cmaestro)
*      dbUseArea(.T., Nil, crutabas + cnomina)
*!*	      set filter to Clave== 51 .OR. Clave== 34 .OR. Clave== 1  .OR. Clave== 98 .OR. Clave== 98
*!*	      copy to TmpSdos all
*!*	      dbUseArea(.T., Nil, "TmpSdos")
*!*	      index on Jpp+ Str(NumJpp,6) to TmpSdos
      select (cnomina)
*!*	      if (.T.)
*!*	         set relation to
*!*	      endif
*!*	      if (.T.)
*!*	         set relation to
*!*	      endif
      set relation to Jpp+Str(NumJpp,6) into (cmaestro)   &&, Jpp+Str(NumJpp,6) into TmpSdos
*      set filter to Clave== 207 .OR. Clave== 219  .OR.  Clave== 229
      set filter to Clave== 51 .OR. Clave== 34 .OR. Clave== 1  .OR. Clave== 98 .OR. Clave== 98
      goto top
      do while (!EOF())
         if !EOF(cmaestro)
            select (cdesc)
            append blank
            replace nombre with &cmaestro->NOMBRE
            replace rfc with &cmaestro->RFC
            replace proyecto with ""
            replace cvedesc with &cnomina->CLAVE
            replace numdesc with &cnomina->PAGO4
            replace totdesc with &cnomina->PAGOT
            replace importe with &cnomina->MONTO
            replace folio with Str(&cnomina->FOLIO)
            replace depend with "JUBILADOS"
         endif
         select (cnomina)
         Skip
      enddo
*!*	      close databases
*!*	      dbUseArea(.T., Nil, crutades + cdesc)
*!*	      if (LastRec() <= 0)
*!*	         close
*!*	         erase (cdesc + ".DBF")
*!*	      endif
*!*	      close databases
*!*	      erase (cmaestro + ".Ntx")
*!*	      erase TmpSdos.Dbf
*!*	      erase TmpSdos.Ntx
*!*	      RestScreen(11, 5, 13, 75, ppan2)
*      exit
*   enddo
*!*	   alert("INSERTE UN DISCO EN LA UNIDAD A:,  PARA COPIAR LA INFORMACION PARA PROG. ESPECIALES")
*!*	   ndesc:= crutades + cdesc + ".DBF"
*!*	   adesc:= cdesc + ".DBF"
*!*	   copy file (ndesc) to "A:&ADESC"
*!*	   RestScreen(7, 2, 10, 40, ppan)
CLOSE TABLES all
Return

