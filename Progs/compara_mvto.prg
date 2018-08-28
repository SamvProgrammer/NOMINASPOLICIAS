LPARAMETERS ntipo, cperiodo,nMes,nAnio
Set Procedure To Progs\RutNomina ADDITIVE
local crutabas, cmaestro, cnomina, crutades, nTemp, nTot1, nTot2, nTot3, nTot4, nTot5, nTot6, nTot7, nTot8, nTot9, nTot10
LOCAL aStruc
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

local aperdedn1, aperdedr1, aperdedn2, aperdedr2

DECLARE aperdedn1[300, 7]
DECLARE aperdedr1[300, 7]
DECLARE aperdedn2[300, 7]
DECLARE aperdedr2[300, 7]

aStruc= '(NOMINA C( 30, 0), PERDED   N( 1,  0), CLAVE N( 3, 0), DESCRIP C(15, 0), MES_ANT  C( 50, 0),'+;
		' DMES   C( 15, 0), IMP_ANT  N( 12, 2), ALTAS N(12, 2), BAJAS   N(12, 2), IMP_NOMI N( 12, 1))'

CREATE TABLE &nTemp FREE &aStruc
CLOSE TABLES all
USE &nTemp ALIAS NOMIACTUAL

*CLOSE TABLES all
USE data\tablas\firmas IN 0
USE data\tablas\perded ORDER clave IN 0
USE data\tablas\maestro order nomina ALIAS maes_actu IN 0
USE data\tablas\nominew ORDER nomina ALIAS nomi_actu IN 0
USE ( cmaestro ) ALIAS maes_ante IN 0
USE ( cnomina ) ALIAS nomi_ante IN 0
SELECT Maes_Ante
INDEX on jpp+STR(num,6) TO (nTemp) +'1'
SELECT Nomi_Ante
INDEX on jpp+STR(numjpp,6)+STR(clave,3)+STR(secuen,1)+tipopago+STR(monto,12,2) TO (nTemp) +'2'
SELECT Nomi_Actu
INDEX on jpp+STR(numjpp,6)+STR(clave,3)+STR(secuen,1)+tipopago+STR(monto,12,2) TO (nTemp) +'3'

LOCAL nlin, nsuma1, nsuma2, nsuma3, nsuma4
LOCAL nempl, nemp2, cnomina, ctipo, carch, cctipo, k, i, j

nlin= 99
STORE 0 TO nsuma1,nsuma2,nsuma3,nsuma4,nempl,nemp2,k,i, nTot1, nTot2, nTot3, nTot4, nTot5, nTot6, nTot7, nTot8, nTot9, nTot10
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
            aperdedn1[i,7]= 0
            aperdedr1[i,7]= 0
            
            aperdedn2[i,1]= 0
            aperdedr2[i,1]= 0
            aperdedn2[i,6]= ''
            aperdedr2[i,6]= ''
            aperdedn2[i,7]= 0
            aperdedr2[i,7]= 0
         next
         cctipo= actipos[ntipo]
         ctipo= atipos[ntipo]
         carch= 'nominas\'+aarchs[ntipo,1]

         && Obtiene informaciòn de la nomina actual
         select Maes_Ante
         set filter to Left(Jpp, 2) = Left(cTipo, 2) .AND. superviven="S"
         select Maes_Actu
         set filter to Left(Jpp, 2) = Left(cTipo, 2) .AND. superviven="S"
         goto top
         do while (!EOF())
            tjpp= jpp
            tnum= num
            select nomi_actu
            seek tjpp + Str(tnum, 6)
            if (Found())
               nempl=nempl+1
               do while (tjpp == jpp .AND. tnum == numjpp) AND !EOF()
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

         && Obtiene informaciòn de las ALTAS
         SELECT Nomi_Ante
         SET RELATION TO jpp+STR(numjpp,6) INTO Maes_Ante
         SET FILTER TO !EOF('Maes_Ante')

         SELECT Maes_Actu
         goto top
         do while (!EOF())
            tjpp= jpp
            tnum= num
	            select Nomi_Actu
	            seek tjpp + Str(tnum, 6)
				IF FOUND()
	               do while (tjpp == jpp .AND. tnum == numjpp) AND !EOF()
	               	  SELECT Nomi_Ante
	               	  seek tjpp + Str(tnum, 6) + STR(Nomi_Actu.Clave,3) + STR(Nomi_Actu.Secuen,1)+Nomi_Actu.tipopago+STR(Nomi_Actu.MONTO,12,2)
	               	  IF !FOUND()
		                  if (Nomi_Actu.tipopago == "N")
		                     aperdedn1[Nomi_Actu.clave,7]= aperdedn1[Nomi_Actu.clave,7] + Nomi_Actu.monto
		                  else
		                     aperdedr1[Nomi_Actu.clave,7]= aperdedr1[Nomi_Actu.clave,7] + Nomi_Actu.monto
		                  ENDIF
		              ENDIF 
		              SELECT Nomi_Actu
	                  SKIP
	               ENDDO
	           endif
            select Maes_Actu
            skip 
         ENDDO
		 SELECT Nomi_Ante
		 SET FILTER TO 
		 SET RELATION TO 

         && Obtiene informaciòn de la nomina anterior
         select Maes_Ante
         set filter to Left(Jpp, 2) = Left(cTipo, 2) .AND. superviven="S"
         goto top
         do while (!EOF())
            tjpp= jpp
            tnum= num
            select Nomi_ante
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

         && Obtiene informaciòn de las BAJAS
         SELECT Nomi_Actu
         SET RELATION TO jpp+STR(numjpp,6) INTO Maes_Actu
         SET FILTER TO !EOF('Maes_Actu')
         select Maes_Ante
         goto top
         do while (!EOF())
            tjpp= jpp
            tnum= num
            select Nomi_Ante
            seek tjpp + Str(tnum, 6)
			IF FOUND()
               do while (tjpp == jpp .AND. tnum == numjpp) AND !EOF()
               	  SELECT Nomi_Actu
               	  seek tjpp + Str(tnum, 6) + STR(Nomi_Ante.Clave,3) + STR(Nomi_Ante.Secuen,1)+Nomi_Ante.tipopago+STR(Nomi_Ante.MONTO,12,2)
               	  IF !FOUND()
	                  if (Nomi_Ante.tipopago == "N")
	                     aperdedn2[Nomi_Ante.clave,7]= aperdedn2[Nomi_Ante.clave,7] + Nomi_Ante.monto
	                  else
	                     aperdedr2[Nomi_Ante.clave,7]= aperdedr2[Nomi_Ante.clave,7] + Nomi_Ante.monto
	                  ENDIF
                  ENDIF
               	  SELECT Nomi_Ante
                  SKIP
               ENDDO
           ENDIF
           select Maes_Ante
           skip 
         ENDDO
		 SELECT Nomi_Actu
		 SET FILTER TO 
		 SET RELATION TO 
         set printer to (carch)
         set printer on
         set device to printer
         set scoreboard off
		 f_Encabe(k,cctipo,nMes,cperiodo)
         @  8,  0 SAY 'P E R C E P C I O N E S'
         nlin= 10
         for i= 1 to 300
            if (aperdedn1[i,1] > 0) OR (aperdedn2[i,1] > 0)
               if (aperdedn1[i,2] <= 60)
                  @ nlin,  0 say aperdedn2[i,2] picture "999"
                  @ nlin,  4 say aperdedn2[i,3]
                  @ nlin, 21 say aperdedn2[i,1] picture "999,999,999.99"
                  @ nlin, 36 say LEFT(aperdedn2[i,6],4)
                  IF aperdedn2[i,1]-aperdedn1[i,7]>0
                  	 @ nlin, 41 say aperdedn1[i,7] picture "999,999,999.99"
                  	 @ nlin, 56 say aperdedn2[i,7] picture "999,999,999.99"
                  ELSE
                  	 @ nlin, 41 say aperdedn1[i,7]-aperdedn2[i,1] picture "999,999,999.99"
                  	 @ nlin, 56 say aperdedn2[i,7]-aperdedn2[i,1] picture "999,999,999.99"
                  ENDIF 
                  @ nlin, 71 Say (aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7] picture "999,999,999.99"
                  @ nlin, 86 say aperdedn1[i,1] picture "999,999,999.99"
                  @ nlin,101 Say ((aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7])-aperdedn1[i,1] picture "999,999,999.99"
                  IF aperdedn2[i,1]-aperdedn1[i,7]>0
				     nTot1=nTot1+aperdedn1[i,7]
				     nTot2=nTot2+aperdedn2[i,7]
				  ELSE
				     nTot1=nTot1+aperdedn1[i,7]-aperdedn2[i,1]
				     nTot2=nTot2+aperdedn2[i,7]-aperdedn2[i,1]
				  ENDIF
				    nTot3=nTot3+(aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7]
				    nTot4=nTot4+aperdedn1[i,1]
				    nTot5=nTot5+((aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7])-aperdedn1[i,1]

	                nlin=nlin+1
               endif
            ENDIF
            if (aperdedr1[i,1] > 0) OR aperdedr2[i,1] > 0
               if (aperdedr1[i,2] <= 60)
                  @ nlin,  0 say aperdedr2[i,2] picture "999"
                  @ nlin,  4 say aperdedr2[i,3]
                  @ nlin, 21 say aperdedr2[i,1] picture "999,999,999.99"
                  @ nlin, 36 say LEFT(aperdedr2[i,6],4)            	 
                  IF aperdedr2[i,1]-aperdedr1[i,7]>0
                  	 @ nlin, 41 say aperdedr1[i,7] picture "999,999,999.99"
                  	 @ nlin, 56 say aperdedr2[i,7] picture "999,999,999.99"
                  ELSE
                  	 @ nlin, 41 say aperdedr1[i,7]-aperdedr2[i,1] picture "999,999,999.99"
                  	 @ nlin, 56 say aperdedr2[i,7]-aperdedr2[i,1] picture "999,999,999.99"
                  ENDIF
                  @ nlin, 71 say (aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7] picture "999,999,999.99"
                  @ nlin, 86 say aperdedr1[i,1] picture "999,999,999.99"
                  @ nlin,101 say ((aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7])-aperdedr1[i,1] picture "999,999,999.99"
                  IF aperdedr2[i,1]-aperdedr1[i,7]>0
				     nTot1=nTot1+aperdedr1[i,7]
				     nTot2=nTot2+aperdedr2[i,7]
				  ELSE
				     nTot1=nTot1+aperdedr1[i,7]-aperdedr2[i,1]
				     nTot2=nTot2+aperdedr2[i,7]-aperdedr2[i,1]
				  ENDIF
				    nTot3=nTot3+(aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7]
				    nTot4=nTot4+aperdedr1[i,1]
				    nTot5=nTot5+((aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7])-aperdedr1[i,1]

	                nlin=nlin+1
               endif
            endif
         NEXT
         @ nlin, 21 say Replicate("-", 14)
         @ nlin, 41 say Replicate("-", 14)
         @ nlin, 56 say Replicate("-", 14)
         @ nlin, 71 say Replicate("-", 14)
         @ nlin, 86 say Replicate("-", 14)
         @ nlin,101 say Replicate("-", 14)
         nlin=nlin+1
         @ nlin, 0 say "TOTALES: "
         @ nlin, 21 say Transform(nsuma3,"999,999,999.99")
         @ nlin, 41 say Transform(nTot1,"999,999,999.99")
         @ nlin, 56 say Transform(nTot2,"999,999,999.99")
         @ nlin, 71 say Transform(nTot3,"999,999,999.99")
         @ nlin, 86 say Transform(nTot4,"999,999,999.99")
         @ nlin,101 say Transform(nTot5,"999,999,999.99")
         nlin=nlin+2

         @ nlin, 0 say 'D E D U C C I O N E S'
         nlin=nlin+2
         STORE 0 TO nTot6, nTot7, nTot8, nTot9, nTot10

         for i= 1 to 300
            if (aperdedn1[i,1] > 0) OR (aperdedn2[i,1] > 0)
               if (aperdedn1[i,2] > 60)
                  @ nlin,  0 say aperdedn2[i,2] picture "999"
                  @ nlin,  4 say aperdedn2[i,3]
                  @ nlin, 21 say aperdedn2[i,1] picture "999,999,999.99"
                  @ nlin, 36 say LEFT(aperdedn2[i,6],4)
                  @ nlin, 41 say aperdedn1[i,7] picture "999,999,999.99"
                  @ nlin, 56 say aperdedn2[i,7] picture "999,999,999.99"
                  @ nlin, 71 Say (aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7] picture "999,999,999.99"
                  @ nlin, 86 say aperdedn1[i,1] picture "999,999,999.99"
                  @ nlin,101 Say ((aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7])-aperdedn1[i,1] picture "999,999,999.99"
				    nTot6 =nTot6+aperdedn1[i,7]
				    nTot7 =nTot7+aperdedn2[i,7]
				    nTot8 =nTot8+(aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7]
				    nTot9 =nTot9+aperdedn1[i,1]
				    nTot10=nTot10+((aperdedn2[i,1]+aperdedn1[i,7])-aperdedn2[i,7])-aperdedn1[i,1]

	                nlin=nlin+1
					IF nlin>56
						 f_Encabe(k,cctipo,nMes,cperiodo)
				         @  8,  0 SAY 'D E D U C C I O N E S'
				         nlin= 10
					endif
               endif
            ENDIF
            if (aperdedr1[i,1] > 0) OR aperdedr2[i,1] > 0
               if (aperdedr1[i,2] > 60)
                  @ nlin,  0 say aperdedr2[i,2] picture "999"
                  @ nlin,  4 say aperdedr2[i,3]
                  @ nlin, 21 say aperdedr2[i,1] picture "999,999,999.99"
                  @ nlin, 36 say LEFT(aperdedr2[i,6],4)
                  @ nlin, 41 say aperdedr1[i,7] picture "999,999,999.99"
                  @ nlin, 56 say aperdedr2[i,7] picture "999,999,999.99"
                  @ nlin, 71 say (aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7] picture "999,999,999.99"
                  @ nlin, 86 say aperdedr1[i,1] picture "999,999,999.99"
                  @ nlin,101 say ((aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7])-aperdedr1[i,1] picture "999,999,999.99"
				    nTot6 =nTot6+aperdedr1[i,7]
				    nTot7 =nTot7+aperdedr2[i,7]
				    nTot8 =nTot8+(aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7]
				    nTot9 =nTot9+aperdedr1[i,1]
				    nTot10=nTot10+((aperdedr2[i,1]+aperdedr1[i,7])-aperdedr2[i,7])-aperdedr1[i,1]

	                nlin=nlin+1
					IF nlin>56
						 f_Encabe(k,cctipo,nMes,cperiodo)
				         @  8,  0 SAY 'D E D U C C I O N E S'
				         nlin= 10
					endif
               endif
            endif
         next

         @ nlin, 21 say Replicate("-", 14)
         @ nlin, 41 say Replicate("-", 14)
         @ nlin, 56 say Replicate("-", 14)
         @ nlin, 71 say Replicate("-", 14)
         @ nlin, 86 say Replicate("-", 14)
         @ nlin,101 say Replicate("-", 14)
         nlin=nlin+1
         
         @ nlin, 0 say "TOTALES: "
         @ nlin, 21 say Transform(nsuma4,"999,999,999.99")
         @ nlin, 41 say Transform(nTot6,"999,999,999.99")
         @ nlin, 56 say Transform(nTot7,"999,999,999.99")
         @ nlin, 71 say Transform(nTot8,"999,999,999.99")
         @ nlin, 86 say Transform(nTot9,"999,999,999.99")
         @ nlin,101 say Transform(nTot10,"999,999,999.99")

          nlin= nlin + 2
         @ nlin,  0 say "DIFERENCIA: "
         @ nlin, 21 say Transform(nsuma3 - nsuma4,"999,999,999.99")
         @ nlin, 41 say Transform(nTot1-nTot6,"999,999,999.99")
         @ nlin, 56 say Transform(nTot2-nTot7,"999,999,999.99")
         @ nlin, 71 say Transform(nTot3-nTot8,"999,999,999.99")
         @ nlin, 86 say Transform(nTot4-nTot9,"999,999,999.99")
         @ nlin,101 say Transform(nTot5-nTot10,"999,999,999.99")
         @ nlin,  0 say " "
         eject
         set printer off
         set printer to 
         set device to screen
         nlin= 99
         STORE 0 TO nsuma1, nsuma2, nsuma3, nsuma4, nempl, nemp2
         STORE 0 TO nTot1, nTot2, nTot3, nTot4, nTot5
         STORE 0 TO nTot6, nTot7, nTot8, nTot9, nTot10
  	     ntipo= ntipo + 1
   next
   CLOSE TABLES all
   RETURN
  
FUNCTION F_Encabe(k,cctipo,nMes,cperiodo)
         @  1,  0 say "SECRETARIA DE ADMINISTRACION"
         @  1, 93 say "OFICINA DE PENSIONES"
         @  2,  0 say "COMPARATIVO DE NOMINA " + iif(K == 1, ;
            "", "ELECTRONICA") + " PARA EL PAGO A " + cctipo + " " + ;
            cperiodo
         @  2, 96 say "FECHA: " + DTOC(DATE())
         @  3,  0 say Replicate("=", 115)
         @  4,  0 say ;
            "                          NOMINA"
         @  5,  0 say ;
            "CVE. DESCRIPCION          "+PADR(UPPER(fnMes(VAL(nMes))),10)+"            ALTAS           BAJAS         TOTAL        NOMINA       DIFERENCIA"
         @  6,  0 say Replicate("=", 115)
RETURN