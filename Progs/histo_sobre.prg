LPARAMETERS nMes,nAnio,nEmp,nTipo
local ppan, aempleado, cjpp, nnumjpp, crfc, nSobres
local cnombre, apercep, adeduc, ntotaltot, ntotalsob, asobres, nTemp ,nSel
DECLARE adeduc[50,2], apercep[50,2]
PUBLIC ARRAY amonedas[15,2]
nSobres=''
*CLOSE TABLES all
nSel=Select()
nTemp='_Sob'+RIGHT(SYS(2015),5)
Usa_Indice='Temp\'+nTemp
CREATE TABLE &Usa_Indice FREE (CAMPO C(200)) 

*!*	Usa_Tabla='Temp\_Nom'+RIGHT(SYS(2015),5)
*!*	USE data\tablas\nominew ORDER nomina ALIAS NomiNew IN 0
*!*	SELECT nominew
*!*	COPY STRUCTURE TO &Usa_Tabla

*CLOSE TABLES all
SELECT(nTemp)
USE
IF USED('Sobres')
	SELECT sobres
	use
endif
USE &Usa_Indice ALIAS Sobres
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
SELECT sobres
IF !EMPTY(nEmp)
	APPEND BLANK
	APPEND BLANK
Endif

*select NomiNew
*set relation to STR(Clave,3) into PerDed
*!*	   dfechaini= CTOD('01/03/2016')
*!*	   dfechafin= CTOD('31/03/2016')
       DECLARE asobres[3, 5]
      asobres[1,1]=" (Jpp== 'JUP' .Or. Jpp== 'PDP' .Or. Jpp== 'PTP' ) "
      asobres[1,2]="Sobres\SobJUB.Txt"
      asobres[1,3]=0
      asobres[1,4]=0
      asobres[1,5]="JUBILADOS CENTRO"

      asobres[2,1]=" (Jpp== 'JUF' .Or. Jpp== 'PDF' .Or. Jpp== 'PTF') .And. NomElec=='S' "
      asobres[2,2]="Sobres\SobJUF.Txt"
      asobres[2,3]=0
      asobres[2,4]=0
      asobres[2,5]="JUBILADOS FORANEOS"

      asobres[3,1]=" Jpp== 'PEA' "
      asobres[3,2]="Sobres\SobPEA.Txt"
      asobres[3,3]=0
      asobres[3,4]=0
      asobres[3,5]="PENSION ALIMENTICIA"

*      for k= 1 to IIF(EMPTY(nEmp),3,1)    &&&3 &&Len(asobres)
*         cfiltro= asobres[k,1]
*         carchivo= asobres[k,2]
         nlin= 0
         ntotaltot= 0
         ntotalsob= 0
*!*	         select MaesHisto
*!*	         IF !EMPTY(nEmp)
*!*				 SET FILTER TO JPP=nTipo AND INLIST(NUM,&nEmp)
*!*	*			 SET FILTER TO JPP='JUF' AND INLIST(NUM,&nEmp)
*!*	*			 SET FILTER TO &cfiltro AND INLIST(NUM,&nEmp)
*!*			 ELSE
*!*		         SET FILTER TO &cfiltro
*!*			 endif
*!*	         goto top
		 nTantos=0
*         do while (!EOF())
            SELECT Nominas
            LOCATE FOR nAnio$ruta AND nMes$Ruta
            DO WHILE !EOF()
			            nRuta=ALLTRIM(Ruta)
						dfechaini= CTOD('01/'+SUBSTR(nRuta,24,2)+'/'+SUBSTR(nRuta,15,4))
						IF MONTH(dfechaini)=12
		*					xFechaFin=CTOD('31/'+STR(nMes)+'/'+STR(nAnio))
							dfechafin=CTOD('31/'+SUBSTR(nRuta,24,2)+'/'+SUBSTR(nRuta,15,4))
						ELSE
							nMes=MONTH(dfechaini)+1
							FechaFin=CTOD('01/'+STR(nMes)+'/'+STR(YEAR(dfechaini),4))
							dFechaFin=FechaFin-1
						ENDIF 

						IF FILE(nRuta+'\nominew.dbf')
							IF USED('MaesHisto')
								SELECT MaesHisto
								use
							endif
							IF USED('Nominew')
								SELECT nominew
								use
							endif
							Usa_Indice='Temp\_Sob'+RIGHT(SYS(2015),5)
							USE (nRuta+'\nominew.dbf') ALIAS Nominew IN 0
							SELECT nominew 
							INDEX on jpp+STR(numjpp,6)+STR(clave,3)+STR(secuen,1) to &usa_Indice
							set relation to STR(Clave,3) into PerDed
							
							Usa_Indice='Temp\_Sob'+RIGHT(SYS(2015),5)
							USE (nRuta+'\maestro.dbf') ALIAS MaesHisto IN 0
							SELECT MaesHisto
							INDEX on jpp+STR(num,6) to &usa_Indice
				         	SEEK nTipo+nEmp
							IF FOUND() AND superviven='S'
						         	nSobres=nSobres+CHR(13)+STR(num,10)+' '+nombre
						            cjpp= jpp
						            nnumjpp= num
						            cnombre= nombre
						            crfc= rfc
						            ctipoemp1= LEFT(MaesHisto.categ,20)
						            ctipoemp2= LEFT(MaesHisto.categ,20)

								    FOR xy=1 TO 50
									   adeduc[xy,1]=''
									   adeduc[xy,2]=''
									   apercep[xy,1]=''
									   apercep[xy,2]=''
								    ENDFOR 

						            select NomiNew
						            if !Seek(cjpp + Str(nnumjpp,6))
						               select NOMINAS
						               Skip
						               loop
						            endif
						            ndeduc= 0
						            npercep= 0
						            ntotalsob=ntotalsob+1
						            do while (cjpp == jpp .AND. nnumjpp == numjpp) AND !EOF()
						               if (monto > 0)
					 	              	  xy=1
					 	                 if (clave < 60)
					    	              	 FOR xy=1 TO 50
					        	          	 	IF EMPTY(apercep[xy,1])
					            	      	 		exit
												ENDIF 
					                  		 ENDFOR 
						                     npercep= npercep + monto
						                     apercep[xy,1]=Str(clave, 3) + " " + perded.descri
					    	                 apercep[xy,2]=monto
					        	          else
					            	      	 FOR xy=1 TO 50
					                	  	 	IF EMPTY(adeduc[xy,1])
					                  		 		exit
												ENDIF 
						                  	 ENDFOR 
					    	                 ndeduc= ndeduc + monto
					        	             adeduc[xy,1]="!" + Str(clave, 3) + " " + ;
					            	            perded.descri + " " + iif(pago1 == 0, "    ", ;
					                	        Str(pago1, 3) + "->") + iif(pago2 == 0, "    ", ;
					                    	    Str(pago2, 3) + "->") + iif(pago3 == 0, "    ", ;
						                        Str(pago3, 3) + "->") + iif(pago4 == 0, "   ", ;
					    	                    Str(pago4, 3) + "/") + iif(pagot == 0, "    ", ;
					        	                Str(pagot, 3))
					            	         adeduc[xy,2]=monto
					                	  endif
						               endif
						               Skip
						            ENDDO
						            nmayor=0
						            FOR  i= 1 to 50
						            	IF !EMPTY(apercep[i,1]) or !EMPTY(adeduc[i,1])
							            	nmayor=i
						            	endif
									ENDFOR
						*			nSel=SELECT()
									SELECT Sobres
									IF npercep>0
							            for i= 1 to nmayor
							               if (i == 1 .OR. mod(i, 10) == 0)
							                  if (i > 1)
							                     nlin= nlin + 10
										         FOR xxx=1 TO 10
										         	APPEND blank
										         ENDFOR 
							                  ENDIF
							                  IF nTantos=11
							                  	 nlin=1
							                  	 nTantos=0
							                  ENDIF
											  nTantos=nTantos+1
											  APPEND BLANK
											  replace campo WITH SPACE(6)+STR(nnumjpp,6)+' '+cnombre+' '+ctipoemp1+SPACE(1)+crfc+SPACE(34)+STR(nnumjpp,6)+' '+cnombre + " " + TRIM(ctipoemp1) + " " + crfc
							                  APPEND BLANK
							                  replace campo WITH SPACE(8)+"EVITE SUSPENSION DE PAGO FIRME SUPERVIVENCIA. ART. 99 FRACC. V LEY DE PENSIONES"
							                  nlin= nlin + 2
							                  APPEND BLANK
							                  replace campo WITH SPACE(25)+ctipoemp2+SPACE(25)+"PAGO DEL "+formfecha(dfechaini)+" AL "+formfecha(dfechafin)+SPACE(68)+;
							                  "PAGO DEL " + formfecha(dfechaini)+" AL "+formfecha(dfechafin)
							*!*						dfechaini=CTOD('01/01/2016')
							*!*						dfechafin=CTOD('30/04/2016')
							*!*	                  replace campo WITH SPACE(25)+ctipoemp2+SPACE(18)+"PAGO RETRO. DEL "+formfecha(dfechaini)+" AL "+formfecha(dfechafin)+SPACE(61)+;
							*!*	                  "PAGO RETRO. DEL " + formfecha(dfechaini)+" AL "+formfecha(dfechafin)

							                  nlin= nlin + 3
							                  APPEND BLANK
							                  APPEND BLANK
							               ENDIF
							               APPEND BLANK   
							               replace campo WITH SPACE(25)+;
							               					IIF(EMPTY(apercep[i,1]),SPACE(19),apercep[i,1])+' '+;
							               					IIF(EMPTY(adeduc[i,1]),SPACE(40),adeduc[i,1])+;
							               					IIF(EMPTY(apercep[i,1]),SPACE(11),TRANSFORM(apercep[i,2],'99999999.99'))+SPACE(3)+;
							               					TRANSFORM(adeduc[i,2],'99999999.99')
							               if (i == 1)
							               	  replace campo WITH RTRIM(campo)+IIF(EMPTY(adeduc[i,1]),SPACE(38),SPACE(24))+"PERCEPCIONES:..........."+SPACE(25)+TRANSFORM(npercep,' 999,999.99')
							               endif
							               if (i == 2)
							               	  replace campo WITH RTRIM(campo)+IIF(EMPTY(adeduc[i,1]),SPACE(38),SPACE(24))+"DEDUCCIONES:............"+SPACE(25)+TRANSFORM(ndeduc,' 999,999.99')
							               endif
							               nlin=nlin+1
							            next
							            nunidad= Val(right(alltrim(Str(nmayor)), 1)) + ;
							               (Len(alltrim(Str(nmayor))) - 1)
							            nlin= nlin + (10 - nunidad)
							            nBl=10 - nunidad
							            FOR xxx=0 TO nBl
							            	APPEND blank
							            ENDFOR 
										REPLACE campo WITH SPACE(85)+TRANSFORM(npercep,' 999,999.99')+'   '+TRANSFORM(ndeduc,' 999,999.99')+SPACE(75)+;
														TRANSFORM(npercep - ndeduc,'999,999.99')
										
							            nlin= nlin + 2
							            desglose(npercep - ndeduc)
							          	APPEND blank
							           	APPEND blank
										REPLACE campo WITH SPACE(5)+TRANSFORM(amonedas[1,2],'999')+;
																   TRANSFORM(amonedas[2,2],'999')+;
																   TRANSFORM(amonedas[3,2],'999')+' '+;
																   TRANSFORM(amonedas[4,2],'999')+;
																   TRANSFORM(amonedas[5,2],'999')+;
																   TRANSFORM(amonedas[6,2],'999')+' '+;
																   TRANSFORM(amonedas[7,2],'999')+;
																   TRANSFORM(amonedas[8,2],'999')+;
																   TRANSFORM(amonedas[9,2],'999')+' '+;
																   TRANSFORM(amonedas[10,2],'999')+;
																   TRANSFORM(amonedas[11,2],'999')

							            replace campo WITH RTRIM(campo)+SPACE(3)+"DIRECCION DE PENSIONES/*/GOBIERNO DEL ESTADO DE OAXACA"+' '+;
							            		TRANSFORM(npercep - ndeduc,' 999,999.99')+SPACE(13)+;
							            		Transform(dfechaini, "E")+'-'+Transform(dfechafin, "E")+' '+;
							            		"DIRECCION DE PENSIONES"+SPACE(17)+TRANSFORM(npercep - ndeduc,' 999,999.99')
							            nlin= nlin + 7
							            ntotaltot= ntotaltot + (npercep - ndeduc)

							            FOR xxx=1 TO 6
							            	APPEND blank
							            ENDFOR
							        ENDIF
						* 			SELECT(nSel)
*								ENDIF 
							ENDIF 
						ENDIF 
			            select Nominas
			            SKIP
			ENDDO 
	         select MaesHisto
	         set filter to
	         desglose(ntotaltot)
	         nlin= nlin + 5
	         SELECT Sobres
	         FOR xxx=1 TO 5
	           	APPEND blank
	         ENDFOR 
	         replace campo WITH SPACE(39)+Str(amonedas[1,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[1,1])), 5) + " = " + ;
	            Transform(amonedas[1,1] * amonedas[1,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[9,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[9,1],5,1)), 5) + " = " + ;
	            Transform(amonedas[9,1] * amonedas[9,2], ;
	            " 99,999,999.99")
	            
	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[2,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[2,1])), 5) + " = " + ;
	            Transform(amonedas[2,1] * amonedas[2,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[10,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[10,1],5,1)), 5) + " = " + ;
	            Transform(amonedas[10,1] * amonedas[10,2], ;
	            " 99,999,999.99")
	            
	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[3,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[3,1])), 5) + " = " + ;
	            Transform(amonedas[3,1] * amonedas[3,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[11,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[11,1],5,2)), 5) + " = " + ;
	            Transform(amonedas[11,1] * amonedas[11,2], ;
	            " 99,999,999.99")

	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[4,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[4,1])), 5) + " = " + ;
	            Transform(amonedas[4,1] * amonedas[4,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[12,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[12,1],5,2)), 5) + " = " + ;
	            Transform(amonedas[12,1] * amonedas[12,2], ;
	            " 99,999,999.99")

	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[5,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[5,1])), 5) + " = " + ;
	            Transform(amonedas[5,1] * amonedas[5,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[13,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[13,1],5,2)), 5) + " = " + ;
	            Transform(amonedas[13,1] * amonedas[13,2], ;
	            " 99,999,999.99")

	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[6,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[6,1])), 5) + " = " + ;
	            Transform(amonedas[6,1] * amonedas[6,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[14,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[14,1],5,3)), 5) + " = " + ;
	            Transform(amonedas[14,1] * amonedas[14,2], ;
	            " 99,999,999.99")

	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[7,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[7,1])), 5) + " = " + ;
	            Transform(amonedas[7,1] * amonedas[7,2], ;
	            " 99,999,999.99")+SPACE(4)+Str(amonedas[15,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[15,1],5,3)), 5) + " = " + ;
	            Transform(amonedas[15,1] * amonedas[15,2], ;
	            " 99,999,999.99")

	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(39)+Str(amonedas[8,2], 8) + " x " + ;
	            padr(alltrim(Str(amonedas[8,1],5,1)), 5) + " = " + ;
	            Transform(amonedas[8,1] * amonedas[8,2], ;
	            " 99,999,999.99")+SPACE(9)+"----------------------------"

	         nlin=nlin+1

	         APPEND BLANK
	         replace campo WITH SPACE(80)+" T O T A L    = " + Transform(ntotaltot, "99,999,999.99")

	         nlin= nlin + 16
*	     Endif
         asobres[1,3]= ntotalsob
         asobres[1,4]= ntotaltot
		 SELECT sobres
         IF EMPTY(nEmp)
	         nArch=asobres[k,2]
			 COPY TO (nArch) DELIMITER WITH ""
			 ZAP
		 Endif
*         SELECT(nSel)
*      next
*!*	   select MaesHisto
*!*	   set relation to
*!*	   select NomiNew
*!*	   set relation to
   SET DATE BRITISH
   SELECT sobres
   IF !EMPTY(nEmp)
*		If MessageBox('SE IMPRIMIRAN LOS SOBRES DE NOMINA DE: '+chr(13)+nSobres+chr(13)+chr(13)+'Desea Continuar...????',1+32,'')=1
*			COPY TO lpt2 DELIMITER WITH ""
			txtpaso=Usa_Indice+'.TXT'
			COPY TO &txtpaso DELIMITER WITH ""
			! /N notepad &Usa_Indice
*		ENDIF
   endif
IF USED('sobres')
	SELECT sobres
	use
ENDIF
SELECT(nSel)
RETURN
