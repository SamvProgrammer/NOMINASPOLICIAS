LPARAMETERS nMes,nAnio,nEmp,nTipo

local ppan, ppan2, opc, opc2, nnomina, nmes, nano, ccolor, cfile, aopc, NomiTxt
public aperdedn, aperdedr
DECLARE aperdedn[300, 6]
DECLARE aperdedr[300, 6]
Local aMes
Dimension aMes(12)
aMes(1) ='Enero'
aMes(2) ='Febrero'
aMes(3) ='Marzo'
aMes(4) ='Abril'
aMes(5) ='Mayo'
aMes(6) ='Junio'
aMes(7) ='Julio'
aMes(8) ='Agosto'
aMes(9) ='Septiembre'
aMes(10)='Octubre'
aMes(11)='Noviembre'
aMes(12)='Diciembre'

*CLOSE TABLES all
*USE data\tablas\firmas IN 0
*!*	USE data\tablas\perded ORDER clave IN 0
*!*	USE data\tablas\nominew ORDER nomina IN 0
*!*	USE data\tablas\maestro order nomina IN 0
IF !USED('perded')
	USE data\tablas\perded ORDER clave IN 0
ENDIF 
IF !USED('nominas')
	USE data\tablas\nominas ORDER ruta IN 0
endif
*USE data\tablas\nominew ORDER nomina ALIAS NomiNew IN 0
*!*	IF !USED('maestro')
*!*		USE data\tablas\maestro order nomina IN 0
*!*	ENDIF 
   
LOCAL nlin, nval, ncon, lcancela, ntot1, ntot2, nsuma1, nsuma2
LOCAL nresta, nfin, nempl, ctipo, carch, cctipo, k, i, j, npag
LOCAL cancelar, ppan, ppan2, cleyen

nlin= 99
ncon= 1
lcancela= .F.
STORE 0 TO nval,ntot1,ntot2,nsuma1,nsuma2,nresta,nempl,k,i
STORE '' TO ctipo,carch
j= 1
npag= 1
cancelar= .F.
      
   DECLARE atipos[10], aarchs[10,2], actipos[10], afirmas[10], afirmas[10]
   DECLARE adeduc[50], apercep[50]
   FOR xy=1 TO 50
	   adeduc[xy]=''
	   apercep[xy]=''
   ENDFOR 

*!*	   if (ntipo == 10)
*!*	      return Nil
*!*	   endif
   atipos[1]= 'JUP'
   aarchs[1,1]= "NOMJUB.TXT"  
   aarchs[1,2]= "NOMJUB.ELE"
   actipos[1]= "JUBILADOS"
   atipos[2]= 'PDP'
   aarchs[2,1]= "NOMPDO.TXT"
   aarchs[2,2]= "NOMPDO.ELE"
   actipos[2]= "PENSIONADOS"
   atipos[3]= 'PTP'
   aarchs[3,1]= "NOMPTA.TXT"
   aarchs[3,2]= "NOMPTA.ELE"
   actipos[3]= "PENSIONISTAS"
   atipos[4]= "JUF"
   aarchs[4,1]= "NOMJUF.TXT"
   aarchs[4,2]= "NOMJUF.ELE"
   actipos[4]= "JUBILADOS FORANEOS"
   atipos[5]= "PDF"
   aarchs[5,1]= "NOMPDF.TXT"
   aarchs[5,2]= "NOMPDF.ELE"
   actipos[5]= "PENSIONADOS FORANEOS"
   atipos[6]= "PTF"
   aarchs[6,1]= "NOMPTF.TXT"
   aarchs[6,2]= "NOMPTF.ELE"
   actipos[6]= "PENSIONISTAS FORANEOS"
   atipos[7]= "PEA"
   aarchs[7,1]= "NOMPEA.TXT"
   aarchs[7,2]= "NOMPEA.ELE"
   actipos[7]= "PENSION ALIMENTICIA"
   atipos[8]= "PEF"
   aarchs[8,1]= "NOMPEF.TXT"
   aarchs[8,2]= "NOMPEF.ELE"
   actipos[8]= "PENSION ALIMENTICIA FORANEO"
   select PerDed
   goto top
   do while (!EOF())
      if (clave > 300)
         exit
      ENDIF
      IF clave>0
	      aperdedn[clave,2]= clave
	      aperdedn[clave,3]= descri
	      aperdedr[clave,2]= clave
	      aperdedr[clave,3]= descri
      ENDIF 
      skip 
   enddo
*   if (ntipo == 9)
*      j= 8
*      ntipo= 1
*   endif
*   for k= 1 to j
*      for l= 1 to 2
         for i= 1 to 300
            aperdedn[i,1]= 0
            aperdedr[i,1]= 0
         next
 *        cctipo= actipos[ntipo]
*         ctipo= atipos[ntipo]
*         carch= 'nominas\'+aarchs[ntipo,l]
		 NomiTxt='Temp\_Nom'+RIGHT(SYS(2015),5)+'.txt'
*		 Usa_Indice='Temp\'+nTemp
*         set margin to 0
         set printer to (NomiTxt)
         set printer on
         set device to printer
         set scoreboard off
         SELECT Nominas
         LOCATE FOR nAnio$ruta AND nMes$Ruta
         DO WHILE !EOF()
	            nRuta=ALLTRIM(Ruta)
				cPeriodo=UPPER(aMes( VAL(SUBSTR(nRuta,24,2)) ))+ ' DE '+SUBSTR(nRuta,15,4)

				IF FILE(nRuta+'\nominew.dbf')
					IF USED('MaesHisto')
						SELECT MaesHisto
						use
					endif
					IF USED('Nominew')
						SELECT nominew
						use
					endif
					Usa_Indice='Temp\_Nom'+RIGHT(SYS(2015),5)
					USE (nRuta+'\nominew.dbf') ALIAS Nominew IN 0
					SELECT Nominew
					INDEX on jpp+STR(numjpp,6)+STR(clave,3)+STR(secuen,1) to &usa_Indice

					Usa_Indice='Temp\_Nom'+RIGHT(SYS(2015),5)
					USE (nRuta+'\maestro.dbf') ALIAS MaesHisto IN 0
					SELECT MaesHisto
					INDEX on jpp+STR(num,6) to &usa_Indice
		         	SEEK nTipo+nEmp
					IF FOUND() AND superviven='S'
			            tjpp= jpp
			            tnum= num
			            crfc= rfc
			            cnombre= nombre
			            ccateg= categ
			            cleyen= leyen
											SELECT nominew 
								            seek tjpp + Str(tnum, 6)
								            if (Found())
									            if (nlin > 60)
									               @  1,  0 say "SECRETARIA DE ADMINISTRACION"
									               @  1, 115 say "OFICINA DE PENSIONES"
									               @  2,  0 say 'HISTORIAL DE NOMINA'&&"NOMINA " + iif(l == 1, "", ;
									                  &&"ELECTRONICA") + " PARA EL PAGO A " + cctipo + " " ;
									                  &&+ cperiodo
									               @  2, 127 say "PAG: " + Str(npag, 3)
									               @  3,  0 say Replicate("-", 135)
									               @  4,  0 say ;
									                  "R.F.C.       N O M B R E                             CATEG.NIV.//CVE.EMPL."
									               @  5, 55 say ;
									                  "PERCEPCIONES                                          DEDUCCIONES        LIQUIDO"
									               @  6,  0 say Replicate("-", 135)
									               nlin= 7
									               npag=npag+1
									            endif
								               nempl=nempl+1
								               @ nlin,  0 say cPeriodo
								               nLin=nLin+1
								               @ nlin,  0 say crfc
								               @ nlin, 13 say cnombre
								               @ nlin, 53 say alltrim(ccateg) + "//" + jpp + ;
								                  alltrim(Str(tnum, 6))
								               if (!Empty(cleyen))
								                  nlin=nlin+1
								                  @ nlin, 13 say cleyen
								               endif
								               nlin=nlin+1
								               nReg=1
								               do while (tjpp == jpp .AND. tnum == numjpp) AND !EOF()
								               	  xy=1
								                  if (clave < 60) OR clave=109
								                  	 FOR xy=1 TO 50
								                  	 	IF EMPTY(apercep[xy])
								                  	 		exit
														ENDIF 
								                  	 ENDFOR 
								                     apercep[xy]=iif(!Empty(leyen), iif(tipopago ;
								                        == "N", padl(Trim(leyen), 27), ;
								                        padl(Trim(leyen), 27)), Replicate(" ", 27)) ;
								                        + " " + iif(pagot > 0, Str(pago4, 3) + "/" + ;
								                        padr(alltrim(Str(pagot, 3)), 3), ;
								                        Replicate(" ", 7)) + " " + Str(clave, 3) + ;
								                        " " + padr(alltrim(aperdedn[clave,3]), 15) ;
								                        + " " + Transform(monto, "999,999.99")
								                     ntot1= ntot1 + monto
								                     nsuma1= nsuma1 + monto
								                  else
								                  	 FOR xy=1 TO 50
								                  	 	IF EMPTY(adeduc[xy])
								                  	 		exit
														ENDIF 
								                  	 ENDFOR 
								                     adeduc[xy]=Str(clave, 3) + " " + ;
								                        aperdedn[clave,3] + " " + iif(pagot > 0, ;
								                        Str(pago4, 3) + "/" + ;
								                        padr(alltrim(Str(pagot, 3)), 3), ;
								                        Replicate(" ", 7)) + " " + Transform(monto, ;
								                        "999,999.99") + " " + Trim(leyen)
								                     ntot2= ntot2 + monto
								                     nsuma2= nsuma2 + monto
								                  endif
								                  if (tipopago == "N")
								                     aperdedn[clave,1]= aperdedn[clave,1] + monto
								                     aperdedn[clave,6]= leyen
								                  else
								                     aperdedr[clave,1]= aperdedr[clave,1] + monto
								                     aperdedr[clave,6]= leyen
								                  endif
								                  SKIP
								               enddo
								               for ncon= 1 to 50
								               		IF !EMPTY(apercep[ncon]) OR !EMPTY(adeduc[ncon])
										                  @ nlin,  0 say apercep[ncon]
										                  @ nlin, 82 say adeduc[ncon]
										                  nlin=nlin+1
										                  if (nlin > 60)
										                     @  1,  0 say "SECRETARIA DE ADMINISTRACION"
										                     @  1, 115 say "OFICINA DE PENSIONES"
										                     @  2,  0 say 'HISTORIAL DE NOMINA'&&"NOMINA " + iif(l == 1, "", ;
										                        &&"ELECTRONICA") + " PARA EL PAGO A " + cctipo ;
										                        &&+ " " + cperiodo
										                     @  2, 127 say "PAG: " + Str(npag, 3)
										                     @  3,  0 say Replicate("-", 135)
										                     @  4,  0 say ;
										                        "R.F.C.       N O M B R E                             CATEGORIA//CVE.EMPL."
										                     @  5, 55 say ;
										                        "PERCEPCIONES                                          DEDUCCIONES        LIQUIDO"
										                     @  6,  0 say Replicate("-", 135)
										                     nlin= 8
										                     npag=npag+1
										                  ENDIF
										             ENDIF
								               next
								               @ nlin, 54 say Replicate("-", 12)
								               @ nlin, 108 say Replicate("-", 12)
								               @ nlin, 123 say Replicate("_", 12)
								               nlin=nlin+1
								               @ nlin, 54 say ntot1 picture "9,999,999.99"
								               @ nlin, 108 say ntot2 picture "9,999,999.99"
								               nresta= ntot1 - ntot2
								               @ nlin, 123 say nresta picture "9,999,999.99"
*								               @ nlin, 145 say "______________"
								               nlin= nlin + 2
*!*									               if (nresta < 0)
*!*									                  set printer off
*!*									                  set printer to 
*!*									                  set device to screen
*!*									                  set margin to
*!*									*                  alerta("!!! La persona: " + tjpp + Str(tnum) + ;
*!*									*                     " Tiene liquido menor de 0!!!")
*!*									                  lcancela= .T.
*!*									                  exit
*!*									               ENDIF
								               FOR xy=1 TO 50
												   adeduc[xy]=''
												   apercep[xy]=''
											   ENDFOR 
								*!*	               apercep= {}
								*!*	               adeduc= {}
								               ntot1= 0
								               ntot2= 0
								            ENDIF
					ENDIF  
				ENDIF 
       	     	select nominas
            	skip 
	     ENDDO
*!*	         nlin= nlin + 5
*!*	         if (nlin > 38)
*!*	            @  1,  0 say "SECRETARIA DE ADMINISTRACION"
*!*	            @  1, 140 say "OFICINA DE PENSIONES"
*!*	            @  2,  0 say 'HISTORIAL DE NOMINA'&&"NOMINA " + iif(l == 1, "", "ELECTRONICA") ;
*!*	               &&+ " PARA EL PAGO A " + cctipo + " " + cperiodo
*!*	            @  2, 152 say "PAG: " + Str(npag, 3)
*!*	            @  3,  0 say Replicate("-", 160)
*!*	            @  4,  0 say ;
*!*	               "R.F.C.       N O M B R E                             CATEGORIA//CVE.EMPL."
*!*	            @  5, 55 say ;
*!*	               "PERCEPCIONES                                          DEDUCCIONES             LIQUIDO     F I R M A"
*!*	            @  6,  0 say Replicate("-", 160)
*!*	            nlin= 8
*!*	            npag=npag+1
*!*	         endif
*!*	         @ nlin,  0 say "TOTALES------>>"
*!*	         @ nlin, 51 say "PERCEPCIONES:" + Transform(nsuma1, ;
*!*	            "999,999,999.99")
*!*	         @ nlin, 91 say "DEDUCCIONES:" + Transform(nsuma2, ;
*!*	            "999,999,999.99")
*!*	         @ nlin, 118 say "LIQUIDO:" + Transform(nsuma1 - nsuma2, ;
*!*	            "999,999,999.99")
         @  1,  0 say "SECRETARIA DE ADMINISTRACION"
         @  1, 115 say "OFICINA DE PENSIONES"
         @  2,  0 say "RESUMEN CONTABLE DE LA NOMINA " &&+ iif(l == 1, ;
            &&"", "ELECTRONICA") + " PARA EL PAGO A " + cctipo + " " + ;
            &&cperiodo
         @  2, 127 say "PAG: " + Str(npag, 3)
         @  3,  0 say Replicate("=", 135)
         @  4,  0 say ;
            "                               PERCEPCIONES                                                                       PERCEPCIONES "
         @  5,  0 say ;
            "CVE. DESCRIPCION               IMPORTE                                             CVE. DESCRIPCION               IMPORTE       LIQUIDO"
         @  6,  0 say Replicate("=", 135)
         nlin= 8
         for i= 1 to 300
            if (aperdedn[i,1] > 0)
               if (aperdedn[i,2] <= 60) OR aperdedn[i,2]=109
                  @ nlin,  0 say aperdedn[i,2] picture "999"
                  @ nlin,  5 say aperdedn[i,3]
                  @ nlin, 31 say aperdedn[i,1] picture ;
                     "999,999,999.99"
                  @ nlin, 49 say aperdedn[i,6]
               else
                  @ nlin, 82 say aperdedn[i,2] picture "999"
                  @ nlin, 86 say aperdedn[i,3]
                  @ nlin, 106 say aperdedn[i,1] picture ;
                     "999,999,999.99"
                  @ nlin, 121 say aperdedn[i,6]
               endif
               nlin=nlin+1
            endif
            if (aperdedr[i,1] > 0)
               if (aperdedr[i,2] <= 60) OR aperdedr[i,2]=109
                  @ nlin,  0 say aperdedr[i,2] picture "999"
                  @ nlin,  5 say aperdedr[i,3]
                  @ nlin, 31 say aperdedr[i,1] picture "999,999,999.99"
                  @ nlin, 49 say aperdedr[i,6]
               else
                  @ nlin, 82 say aperdedr[i,2] picture "999"
                  @ nlin, 86 say aperdedr[i,3]
                  @ nlin, 106 say aperdedr[i,1] picture "999,999,999.99"
                  @ nlin, 123 say aperdedr[i,6]
               endif
               nlin=nlin+1
            endif
            npag=npag+1
         next
         @ nlin, 31 say Replicate("-", 14)
         @ nlin, 106 say Replicate("-", 14)
         @ nlin, 121 say Replicate("_", 14)
         nlin=nlin+1
         @ nlin,  0 say "TOTALES------>>"
         @ nlin, 18 say "PERCEPCIONES: " + Transform(nsuma1, ;
            "99,999,999.99")
         @ nlin, 94 say "DEDUCCIONES: " + Transform(nsuma2, ;
            "99,999,999.99")
         @ nlin, 121 say Transform(nsuma1 - nsuma2, ;
            "99,999,999.99")
         nlin= nlin + 2
         @ nlin,  0 say "TOTAL NOMINAS: " + Str(nempl, 7)
         nlin= nlin + 5
         @ nlin,  0 say ;
            "LA PRESENTE NOMINA AMPARA LA CANTIDAD DE: $ " + ;
            Transform(nsuma1 - nsuma2, "99,999,999.99") + " " + ;
            FnConver(nsuma1 - nsuma2)
*!*	         nlin= nlin + 6
*!*	         @ nlin, 25 say ;
*!*	            "           ELABORO                                     REVISO                                AUTORIZO"
*!*	         nlin= nlin + 3
*!*	         @ nlin, 25 say ;
*!*	            "________________________________         __________________________________      _______________________________"
*!*	         nlin= nlin + 1
*!*	         @ nlin, 25 say firmas->elaboro + " " + firmas->reviso + " " ;
*!*	           + firmas->autorizo
*!*	         nlin=nlin+1
         @ 0,  0 say " "
*         eject
         set printer off
         set printer to 
         set device to screen
*  			txtpaso=Usa_Indice+'.txt'
*/			COPY TO &txtpaso DELIMITER WITH ""
		IF FILE(NomiTxt)
*			!notepad &NomiTxt
			DO FORM Forms\frmlpt.scx WITH NomiTxt
		ELSE
			MESSAGEBOX('No se Genero ningun archivo...')
		endif

*         set margin to
*!*	         nlin= 99
*!*	         nsuma1= 0
*!*	         nsuma2= 0
*!*	         npag= 1
*!*	         nempl= 0
*      next
*!*	      ntipo= ntipo + 1
*!*	      if (lcancela)
*!*	         exit
*!*	      endif
*   next
   select Maestro
*   set filter to
*   resume()
*   RestScreen(11, 5, 13, 75, ppan2)
*   return

