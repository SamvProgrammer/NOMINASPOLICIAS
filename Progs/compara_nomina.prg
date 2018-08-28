LPARAMETERS ntipo, cperiodo,nMes,nAnio
Set Procedure To Progs\RutNomina ADDITIVE
local crutabas, cmaestro, cnomina, crutades,ntmp
nAnio=IIF(nMes=1,nAnio-1,nAnio)
nMes=IIF(nMes=1,12,nMes-1)

crutades= "Nominas\"
nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
nAnio= alltrim(Str(nAnio))
crutabas= "RESPALDOS\NOMI" + nAnio + "\NOMI"+nMes+"\"+UPPER(fnMes(VAL(nMes)))+"\"

cmaestro= crutabas +"Maestro"
cnomina= crutabas +"Nominew"

if (!file(cmaestro + ".Dbf") .OR. !file(cnomina + ".Dbf"))
   messagebox("RESPALDO NO ENCONTRADO: " + cmaestro + ".Dbf",'Aviso')
   CLOSE TABLES ALL 
   RETURN
ENDIF
nTemp='temp\_'+RIGHT(SYS(2015),5)

public aperdedn1, aperdedr1, aperdedn2, aperdedr2
DECLARE aperdedn1[300, 6]
DECLARE aperdedr1[300, 6]
DECLARE aperdedn2[300, 6]
DECLARE aperdedr2[300, 6]

CLOSE TABLES all
USE data\tablas\firmas IN 0
USE data\tablas\perded ORDER clave IN 0
USE data\tablas\maestro order nomina ALIAS maes_actu IN 0
USE data\tablas\nominew ORDER nomina ALIAS nomi_actu IN 0
USE ( cmaestro ) ALIAS maes_ante IN 0
USE ( cnomina ) ALIAS nomi_ante IN 0
SELECT maes_ante
INDEX on jpp+STR(num,6) TO (nTemp) +'1'
SELECT nomi_ante
INDEX on jpp+STR(numjpp,6) TO (nTemp) +'2'

*RETURN

LOCAL nlin, nsuma1, nsuma2, nsuma3, nsuma4
LOCAL nempl, nemp2, cnomina, ctipo, carch, cctipo, k, i, j

nlin= 99
STORE 0 TO nsuma1,nsuma2,nsuma3,nsuma4,nempl,nemp2,k,i
STORE '' TO cnomina,ctipo,carch
j= 1
      
   DECLARE atipos[10], aarchs[10,1], actipos[10]

   atipos[1]= 'JUP'
   aarchs[1,1]= "COM_NOMJUB.TXT"  
   actipos[1]= "JUBILADOS"
   atipos[2]= 'PDP'
   aarchs[2,1]= "COM_NOMPDO.TXT"
   actipos[2]= "PENSIONADOS"
   atipos[3]= 'PTP'
   aarchs[3,1]= "COM_NOMPTA.TXT"
   actipos[3]= "PENSIONISTAS"
   atipos[4]= "JUF"
   aarchs[4,1]= "COM_NOMJUF.TXT"
   actipos[4]= "JUBILADOS FORANEOS"
   atipos[5]= "PDF"
   aarchs[5,1]= "COM_NOMPDF.TXT"
   actipos[5]= "PENSIONADOS FORANEOS"
   atipos[6]= "PTF"
   aarchs[6,1]= "COM_NOMPTF.TXT"
   actipos[6]= "PENSIONISTAS FORANEOS"
   atipos[7]= "PEA"
   aarchs[7,1]= "COM_NOMPEA.TXT"
   actipos[7]= "PENSION ALIMENTICIA"
   atipos[8]= "PEF"
   aarchs[8,1]= "COM_NOMPEF.TXT"
   actipos[8]= "PENSION ALIMENTICIA FORANEO"
   select PerDed
   goto top
   do while (!EOF())
      if (clave > 300)
         exit
      ENDIF
      IF clave>0
	      aperdedn1[clave,2]= clave
	      aperdedn1[clave,3]= descri
	      aperdedr1[clave,2]= clave
	      aperdedr1[clave,3]= descri
	      aperdedn2[clave,2]= clave
	      aperdedn2[clave,3]= descri
	      aperdedr2[clave,2]= clave
	      aperdedr2[clave,3]= descri
      ENDIF 
      skip 
   enddo
   if (ntipo == 9)
      j= 8
      ntipo= 1
   endif
   for k= 1 to j
         for i= 1 to 300
            aperdedn1[i,1]= 0
            aperdedr1[i,1]= 0
            aperdedn1[i,6]= ''
            aperdedr1[i,6]= ''
            aperdedn2[i,1]= 0
            aperdedr2[i,1]= 0
            aperdedn2[i,6]= ''
            aperdedr2[i,6]= ''
         next
         cctipo= actipos[ntipo]
         ctipo= atipos[ntipo]
         carch= 'nominas\'+aarchs[ntipo,1]
         && Obtiene informaciòn de la nomina actual
         select maes_actu
         set filter to Left(Jpp, 2) = Left(cTipo, 2) .AND. superviven="S"
         goto top
         do while (!EOF())
            tjpp= jpp
            tnum= num
            select nomi_actu
            seek tjpp + Str(tnum, 6)
            if (Found())
               nempl=nempl+1
               do while (tjpp == jpp .AND. tnum == numjpp)
                  if (clave < 60)
                     nsuma1= nsuma1 + monto
                  else
                     nsuma2= nsuma2 + monto
                  endif
                  if (tipopago == "N")
                     aperdedn1[clave,1]= aperdedn1[clave,1] + monto
                     aperdedn1[clave,6]= leyen
                  else
                     aperdedr1[clave,1]= aperdedr1[clave,1] + monto
                     aperdedr1[clave,6]= leyen
                  endif
                  SKIP
               enddo
            endif
            select Maes_Actu
            skip 
         ENDDO
         && Obtiene informaciòn de la nomina anterior
         select maes_ante
         set filter to Left(Jpp, 2) = Left(cTipo, 2) .AND. superviven="S"
         goto top
         do while (!EOF())
            tjpp= jpp
            tnum= num
            select nomi_ante
            seek tjpp + Str(tnum, 6)
            if (Found())
               nemp2=nemp2+1
               do while (tjpp == jpp .AND. tnum == numjpp)
                  if (clave < 60)
                     nsuma3= nsuma3 + monto
                  else
                     nsuma4= nsuma4 + monto
                  endif
                  if (tipopago == "N")
                     aperdedn2[clave,1]= aperdedn2[clave,1] + monto
                     aperdedn2[clave,6]= leyen
                  else
                     aperdedr2[clave,1]= aperdedr2[clave,1] + monto
                     aperdedr2[clave,6]= leyen
                  endif
                  SKIP
               enddo
            endif
            select Maes_Ante
            skip 
         enddo

         set printer to (carch)
         set printer on
         set device to printer
         set scoreboard off
*!*			 @  0,  0 SAY chr(27)+'!'+Chr(0)
*!*			 @  0,  0 SAY chr(16)

         @  1,  0 say "SECRETARIA DE ADMINISTRACION"
         @  1, 93 say "OFICINA DE PENSIONES"
         @  2,  0 say "COMPARATIVO DE NOMINA " + iif(K == 1, ;
            "", "ELECTRONICA") + " PARA EL PAGO A " + cctipo + " " + ;
            cperiodo
         @  2, 96 say "FECHA: " + DTOC(DATE())
         @  3,  0 say Replicate("=", 113)
         @  4,  0 say ;
            "                          PERCEPCIONES          NOMINA                            DEDUCCIONES "+SPACE(10)+'NOMIMA'
         @  5,  0 say ;
            "CVE. DESCRIPCION          "+PADR(UPPER(fnMes(VAL(nMes))),10)+"            IMPORTE CVE. DESCRIPCION          "+PADR(UPPER(fnMes(VAL(nMes))),10)+"            IMPORTE"
         @  6,  0 say Replicate("=", 113)
         nlin= 8
         for i= 1 to 300
            if (aperdedn1[i,1] > 0) OR (aperdedn2[i,1] > 0)
               if (aperdedn1[i,2] <= 60)
                  @ nlin,  0 say aperdedn2[i,2] picture "999"
                  @ nlin,  4 say aperdedn2[i,3]
                  @ nlin, 21 say aperdedn2[i,1] picture "999,999,999.99"
                  @ nlin, 36 say LEFT(aperdedn2[i,6],4)
                  @ nlin, 41 say aperdedn1[i,1] picture "999,999,999.99"
               else
                  @ nlin, 57 say aperdedn2[i,2] picture "999"
                  @ nlin, 61 say aperdedn2[i,3]
                  @ nlin, 77 say aperdedn2[i,1] picture "999,999,999.99"
                  @ nlin, 92 say LEFT(aperdedn2[i,6],4)
                  @ nlin, 97 say aperdedn1[i,1] picture "999,999,999.99"
               endif
               nlin=nlin+1
            endif
            if (aperdedr1[i,1] > 0) OR aperdedr2[i,1] > 0
               if (aperdedr1[i,2] <= 60)
                  @ nlin,  0 say aperdedr2[i,2] picture "999"
                  @ nlin,  4 say aperdedr2[i,3]
                  @ nlin, 21 say aperdedr2[i,1] picture "999,999,999.99"
                  @ nlin, 36 say LEFT(aperdedr2[i,6],4)
                  @ nlin, 41 say aperdedr1[i,1] picture "999,999,999.99"
               else
                  @ nlin, 57 say aperdedr2[i,2] picture "999"
                  @ nlin, 61 say aperdedr2[i,3]
                  @ nlin, 77 say aperdedr2[i,1] picture "999,999,999.99"
                  @ nlin, 92 say LEFT(aperdedr2[i,6],4)
                  @ nlin, 97 say aperdedr1[i,1] picture "999,999,999.99"
               endif
               nlin=nlin+1
            endif
         next
         @ nlin, 21 say Replicate("-", 14)
         @ nlin, 41 say Replicate("-", 14)
         @ nlin, 77 say Replicate("-", 14)
*         @ nlin, 139 say Replicate("_", 13)
         @ nlin, 97 say Replicate("-", 14)
         nlin=nlin+1
*         @ nlin,  0 say "TOTALES------>>"
         @ nlin, 0 say "PERCEPCIONES:        " + Transform(nsuma3, ;
            "999,999,999.99")
         @ nlin, 41 say nSuma1 picture "999,999,999.99"
         
         @ nlin, 63 say "DEDUCCIONES:  " + Transform(nsuma4, ;
            "999,999,999.99")
         @ nlin, 97 say nSuma2 picture "999,999,999.99"
          nlin= nlin + 2
         @ nlin,  0 say "TOTAL EMPLEADOS: " + Str(nemp2, 7) +'         NOMINA ACTUAL: '+Str(nempl, 7)
         @ nlin, 67 say "LIQUIDO:  " + Transform(nsuma3 - nsuma4, ;
            "999,999,999.99")
         @ nlin, 97 say nSuma1-nSuma2 picture "999,999,999.99"
*!*	        nlin= nlin + 5
*!*	         @ nlin,  0 say ;
*!*	            "LA PRESENTE NOMINA AMPARA LA CANTIDAD DE: $ " + ;
*!*	            Transform(nsuma1 - nsuma2, "99,999,999.99") + " " + ;
*!*	            FnConver(nsuma1 - nsuma2)
*!*	         nlin= nlin + 6
*!*	         @ nlin, 25 say ;
*!*	            "           ELABORO                                     REVISO                                AUTORIZO"
*!*	         nlin= nlin + 3
*!*	         @ nlin, 25 say ;
*!*	            "________________________________         __________________________________      _______________________________"
*!*	         nlin= nlin + 1
*!*	         @ nlin, 25 say firmas->elaboro + " " + firmas->reviso + " " ;
*!*	            + firmas->autorizo
*!*	         nlin=nlin+1
         @ nlin,  0 say " "
         eject
         set printer off
         set printer to 
         set device to screen
*         set margin to
         nlin= 99
         nsuma1= 0
         nsuma2= 0
         nsuma3= 0
         nsuma4= 0
         nempl= 0
         nemp2= 0
	     ntipo= ntipo + 1
   next
   CLOSE TABLES all
   return

