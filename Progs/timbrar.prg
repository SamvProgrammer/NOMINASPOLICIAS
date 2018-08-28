LPARAMETERS CQUIN,CCONC,FECHD,INICIO,FINAL1,DI,QI,nNomi
Local pPan, pPan2, nAno, nQuin, cRutaBas, cMaestro, cNomina, cFondo, cDesc, cJpp, nNum, TRel, CtaBan, DBanco, cTim
*cruta1="Timbrado\"
 
DIMENSION PERDED(300,6),PERDEC(300,6),atipos[10], aarchs[10], actipos[10]

   atipos[1]= 'JUP'
   aarchs[1]= "NOMJUB.TXT"  
   actipos[1]= "JUBILADOS"
   atipos[2]= 'PDP'
   aarchs[2]= "NOMPDO.TXT"
   actipos[2]= "PENSIONADOS"
   atipos[3]= 'PTP'
   aarchs[3]= "NOMPTA.TXT"
   actipos[3]= "PENSIONISTAS"
   atipos[4]= "JUF"
   aarchs[4]= "NOMJUF.TXT"
   actipos[4]= "JUBILADOS FORANEOS"
   atipos[5]= "PDF"
   aarchs[5]= "NOMPDF.TXT"
   actipos[5]= "PENSIONADOS FORANEOS"
   atipos[6]= "PTF"
   aarchs[6]= "NOMPTF.TXT"
   actipos[6]= "PENSIONISTAS FORANEOS"
   atipos[7]= "PEA"
   aarchs[7]= "NOMPEA.TXT"
   actipos[7]= "PENSION ALIMENTICIA"
   atipos[8]= "PEF"
   aarchs[8]= "NOMPEF.TXT"
   actipos[8]= "PENSION ALIMENTICIA FORANEO"

   ctipo= atipos[nNomi]
   && CENTRO
   cTim=IIF(nNomi=1,'J','')
   cTim=IIF(nNomi=2,'P',cTim)
   && FORANEOS
   cTim=IIF(nNomi=4,'JF',cTim)
   cTim=IIF(nNomi=5,'PF',cTim)
  
NQUI=1

*carchi= "Timbrado\"+atipos[nNomi]+right(str(year(FECHD)),4)+IIF(QI<10,'0','')+ALLTRIM(STR(QI,2)) + ".txt"

carchi= "Timbrado\"+right(str(year(INICIO)),4)+IIF(QI<10,'0','')+ALLTRIM(STR(QI,2)) +IIF(QI<10,'0','')+ALLTRIM(STR(QI,2)) + cTim +".txt"

	CLOSE TABLES all
	USE data\tablas\maestro order nomina IN 0
	USE data\tablas\nominew ORDER nomina ALIAS NomiNew IN 0
	USE data\tablas\perded ORDER clave IN 0
	CREATE TABLE Temp\Timbrado FREE (CAMPO C(250))

  ANNO=YEAR(DATE())
  nMESS=MONTH(DATE())
  DIAA=DAY(DATE())

  MES1=nMESS &&IIF(nMESS<10,'0','')+ALLTRIM(STR(nMESS,2))
  DIA1=DIAA  &&IIF( DIAA<10,'0','')+ALLTRIM(STR( DIAA,2))

  FOR I= 1 TO 300
     PERDED[I,1]=0
     PERDEC[I,1]=0
     PERDED[I,4]=0
     PERDEC[I,4]=0
  NEXT
  
  clin= 0

*!*	  set device to printer
*!*	  set printer to (carchi)
*!*	  set printer on

    SELECT 1
*	 if (l == 1)
*	    set filter to Jpp = cTipo .AND. NomElec<> "S" .AND. superviven="S"
*	    set filter to Jpp = cTipo .AND. superviven="S" AND !EMPTY(curp) AND !EMPTY(rfc_homo) AND LEN(ALLTRIM(curp))=18
*!*		    set filter to LEFT(Jpp,2) = LEFT(cTipo,2) .AND. superviven="S" AND !EMPTY(curp) AND LEN(ALLTRIM(curp))=18 AND LEN(ALLTRIM(RFC))=13
	    set filter to Jpp = cTipo .AND. superviven="S" AND !EMPTY(curp) AND LEN(ALLTRIM(curp))=18 AND LEN(ALLTRIM(RFC))=13
*	 else
*	    set filter to Left(Jpp, 2) = Left(cTipo, 2) .AND. ;
*	       NomElec== "S" .AND. superviven="S"
*	 ENDIF
	  GO TOP 
*      DbGoTop()
      DO WHILE(!EOF())
&&&&&&&&&&&&&&&&&&&&&&&& FOR XXX=1 TO 5

        DEDUC=0
        PERCE=0
        P=0
        D=0
        ISR=0.00
        SUELDO=0.00
        SUELDO1=0.00
		CONTRA=actipos[nNomi]
*!*	        IF JPP == "DIB"
*!*	           CONTRA= ALLTRIM("BASE")
*!*	        ENDIF
*!*	        IF JPP == "COC"
*!*	           CONTRA= ALLTRIM("CONT_CONFIANZA")
*!*	        ENDIF
*!*	        IF JPP == "NOC"
*!*	           CONTRA= ALLTRIM("NOM_CONFIANZA")
*!*	        ENDIF
*!*	        IF JPP == "MMS"
*!*	           CONTRA= ALLTRIM("MANDOS MEDIOS")
*!*	        ENDIF

        idem1= ALLTRIM(JPP)+ALLTRIM(STR(NUM))
        nomem= ALLTRIM(STRTRAN(NOMBRE,'.',' '))
        nomem= ALLTRIM(STRTRAN(nomem,'  ',' '))
        num1=  ALLTRIM(STR(NUM))
*        rfc1=  ALLTRIM(RFC_HOMO)
        rfc1=  ALLTRIM(RFC)
        curp1= alltrim(curp)  &&"No hay campo CURP "&& alltrim(curp)
        imss1= alltrim(imss)  &&"No hay campo IMSS "&& alltrim(imss)
        txtOFICINA= ALLTRIM(STRTRAN(CATEG,'.',' ')) &&"No hay campo OFICINA"&& ALLTRIM(OFICINA)
        FEAN=YEAR(FCHING)&&PEN)
        FEME=MONTH(FCHING)&&PEN)
        FEDI=DAY(FCHING)&&PEN)
        CATEGO= ALLTRIM(CATEG)
        CJPP= JPP
        NnUMJPP= NUM
        TRel=IIF(TipoRel='BASE','S','N')
        CtaBan=ALLTRIM(RIGHT(ALLTRIM(cuentabanc),18))
        DBanco=Banco
       iF FEME<10
         FEME1="0"+ ALLTRIM(STR(FEME))
       ELSE
         FEME1=STR(FEME)
       ENDIF
       IF FEDI<10
         FEDI1="0"+ ALLTRIM(STR(FEDI))
       ELSE
         FEDI1=STR(FEDI)
       ENDIF
        select 2
        SEEK (CJPP+STR(NNUMJPP,6))
		IF FOUND()
           DO While cJpp== Jpp .And. nNumJpp== NumJpp .AND. !EOF()

               CVE=CLAVE

               IF CLAVE = 201
                  ISR=MONTO
               ENDIF

               IF CLAVE = 1 .OR. CLAVE = 2 .OR. CLAVE = 55 .OR. CLAVE = 40 .OR. CLAVE = 70
                  SUELDO=MONTO
*                  SUELDO1= SUELDO / 15
                  SUELDO1= SUELDO / 30
               ENDIF
                
               If Clave < 200 .and. Clave<>69  .and. Clave<>82 .and. Clave<>83 .and. Clave<>85
                 P=P+1
                 PERCE=PERCE + Monto
                 PERDED[P,1]=MONTO
                 PERDED[P,2]=CLAVE

                 SELECT 3
				 SEEK STR(cVE,3)
				 IF FOUND()
                     PERDED[P,4]= SAT  &&CLAVE &&SAT
                     PERDED[P,3]= STRTRAN(DESCRI,'.',' ')
				 ENDIF 
               Else
                 If Clave=69 .or. Clave=82 .or. Clave=83 .or. Clave=85 .or. clave>201
                   D=D+1
                   Deduc=DEDUC + Monto
                   PERDEC[D,1]=MONTO
                   PERDEC[D,2]=CLAVE
                 *  PERDEC[D,3]=DESCRI
                   SELECT 3
				   SEEK STR(cVE,3)
				   IF FOUND()
                        PERDEC[D,4]= SAT &&CLAVE &&SAT
	                    IF nominew.pagot>0
		                    PERDEC[D,3]= ALLTRIM(STRTRAN(DESCRI,'.',' '))+' '+ALLTRIM(STR(nominew.pago1+nominew.pago2+nominew.pago3+nominew.pago4))+' DE '+ALLTRIM(STR(nominew.pagot))
	                    else
	                        PERDEC[D,3]= STRTRAN(DESCRI,'.',' ')
	                    ENDIF
                   ENDIF 
                 endif
               EndIf
               SELECT 2
               Skip
           EndDo
        ENDIF
        SELECT timbrado
        APPEND BLANK 
        replace campo WITH "ECOMPROBANTE|dcto(C5),serie(C25),nominaId(C12),nominaDes(C120),TipoNomina(C1),"+;
        				   "fecha(DT),formaDePago(C60),tipoDeComprobante(C12),Moneda(C12)"
		APPEND BLANK
*!*			REPLACE campo WITH cTipo+Iif(cTipo$'JUB,JUF',"|J|","|P|")+ALLTRIM(CQUIN)+"|"+ALLTRIM(CCONC)+;
*!*							   "|O|"+ALLTRIM(STR(ANNO))+"-"+IIF(MES1<10,'0','')+ALLTRIM(STR(MES1))+"-"+IIF(DIA1<10,'0','')+ALLTRIM(STR(DIA1))+"T"+TIME()+"|"+;
*!*							   "En una sola exhibición|EGRESO|MXN|"
		REPLACE campo WITH Maestro.Jpp+Iif(cTipo$'JUB,JUF',"|J|","|P|")+ALLTRIM(CQUIN)+"|"+ALLTRIM(CCONC)+;
						   "|O|"+ALLTRIM(STR(ANNO))+"-"+IIF(MES1<10,'0','')+ALLTRIM(STR(MES1))+"-"+IIF(DIA1<10,'0','')+ALLTRIM(STR(DIA1))+"T"+TIME()+"|"+;
						   "En una sola exhibición|EGRESO|MXN|"
        
		APPEND BLANK
		REPLACE campo WITH "ECOMPROBANTE|empleadoId(C15),Nombre(C60),NEmpleado(C15),RFC(C13),Calle(C60),"+;
							"NExt(C20),NInt(C20),Colonia(C60),Localidad(C60),Municipio(C60),Estado(C60),"+;
							"Pais(C60),CP(C5),CURP(C18),TRegimenId(C3),NSSocial(C15),Sindicalizado(C1),RfcPatronOrigen(C13)"

		APPEND BLANK
		REPLACE campo WITH idem1+"|"+nomem+"|"+num1+"|"+rfc1+"|ABASOLO|504||CENTRO|OAXACA DE JUAREZ|"+;
							"OAXACA DE JUAREZ|OAXACA|MEXICO|68000|"+curp1+"|12|"+imss1+;
							"|"+TRel+'||'

		APPEND BLANK 
		REPLACE campo WITH "ECOMPROBANTE|FTributariaRFC(C13),Depto(C60),CBancaria(C18),Banco(C3),"+;
							"FIRLaboral(D),Puesto(C60),TContrato(C3),TJornada(C3),PPago(C3),"+;
							"SBase(N12.2),RPuesto(C12),SDIntegrado(N12.2),FechaPago(D)"
&& "+CtaBan+"

		APPEND BLANK
		REPLACE campo WITH "OPE631216S18|"+txtOFICINA+"|0000000000000000|072|"+;
							ALLTRIM(STR(FEAN))+"-"+ALLTRIM(FEME1)+"-"+ALLTRIM(FEDI1)+"|"+;
							CATEGO+"|10|01|05|"+;
							ALLTRIM(STR(SUELDO,12,2))+"|4|"+ALLTRIM(STR(SUELDO1,12,2))+"|"+;
							ALLTRIM(STR(YEAR(FECHD)))+"-"+IIF(MONTH(FECHD) < 10,'0','')+ALLTRIM(STR(month(FECHD)))+"-"+;
							IIF(DAY(FECHD) < 10,'0','')+ALLTRIM(str(day(FECHD)))+"|"

		APPEND BLANK
		REPLACE campo WITH "ECOMPROBANTE|FechaInicialPago(D),FechaFinalPago(D),NDPagados(N10.3),Antiguedad(N4,0),"+;
						   "EntidadSNCF(C1),OrigenRecurso(C2),MontoRecursoPropio(N12.2),ClaveEntFed(C3)"

		APPEND BLANK 
		REPLACE campo WITH ALLTRIM(STR(YEAR(INICIO)))+"-"+IIF(MONTH(INICIO) < 10,'0','')+ALLTRIM(STR(month(INICIO)))+"-"+;
							IIF(DAY(INICIO) < 10,'0','')+alltrim(STR(DAY(INICIO)))+"|"+ALLTRIM(STR(YEAR(final1)))+"-"+;
							IIF(MONTH(final1) < 10,'0','')+alltrim(STR(month(final1)))+"-"+;
							IIF(DAY(final1) < 10,'0','')+alltrim(STR(DAY(final1)))+"|"+ALLTRIM(STR(DI))+ALLTRIM(".000")+;
							"|0|S|IP|"+ALLTRIM(STR(SUELDO,12,2))+'|OAX|'
		
	***CONTINUARIA....
		
		APPEND BLANK
		REPLACE campo WITH "DCONCEPTO|Cantidad(N12.2),unidad(C15),descripcion(C60),valorUnitario(N12.2)"
		
		APPEND BLANK
*		REPLACE campo WITH IIF(nNomi=1 .or. nNomi=4,'3','4')+"|SERVICIO|PAGO DE NOMINA|"+ALLTRIM(STR(PERCE,12,2))+"|"
*		REPLACE campo WITH "10|JUB|PAGO DE NOMINA|"+ALLTRIM(STR(PERCE,12,2))+"|"
*		REPLACE campo WITH "10|JUB|Pago de nómina|"+ALLTRIM(STR(PERCE,12,2))+"|"
		REPLACE campo WITH "1|ACT|Pago de nómina|"+ALLTRIM(STR(PERCE,12,2))+"|"
		
*!*			APPEND BLANK
*!*			REPLACE campo WITH "DRETENCION|impuesto(C5),importe(N12.2)"

*!*	          IF ISR <> 0
*!*	             APPEND BLANK
*!*	             REPLACE campo WITH "ISR|"+ALLTRIM(STR(ISR,12,2))+"|"
*!*	*!*	          ELSE
*!*	*!*				 APPEND BLANK
*!*	*!*	             REPLACE campo WITH "ISR|0.00|"
*!*	          ENDIF
*!*	        
		APPEND BLANK 
		REPLACE campo WITH "DCPERCEPCION|TipoPercepcion(C5),Clave(C15),Concepto(C60),ImporteGravado(N12.2),ImporteExento(N12.2)"

*!*			APPEND BLANK 
*!*			REPLACE campo WITH "040|000|INGRESOS PROPIOS|0.00|0.00|"
 
          FOR I= 1 TO P
          	APPEND BLANK
          	
            IF PERDED[I,2] = 1 .OR. PERDED[I,2] = 2 .OR. PERDED[I,2] = 3 
            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;
            						ALLTRIM("00" + alltrim(str(PERDED[I,2])))+"|"+;
            						ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
            ELSE
	            IF PERDED[I,2] = 40 .OR. PERDED[I,2] = 41 .OR. PERDED[I,2] = 45
	            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;         
										ALLTRIM("0" + alltrim(str(PERDED[I,2])))+"|"+;
										ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
	            ELSE
		            IF PERDED[I,2] = 55 .OR. PERDED[I,2] = 56 .OR. PERDED[I,2] = 60
		            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;
		            						ALLTRIM("0" + alltrim(str(PERDED[I,2])))+"|"+;
		            						ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
		            ELSE
			            IF PERDED[I,2] = 70 .OR. PERDED[I,2] = 71 .OR. PERDED[I,2] = 75
			            	REPLACE campo WITH RIGHT("00" + ALLTRIM(STR(PERDED[I,4])),3)+"|"+;
			            						ALLTRIM("0" + alltrim(str(PERDED[I,2])))+"|"+;
			            						ALLTRIM(PERDED[I,3])+"|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|0.00|"
			            ELSE
							 REPLACE campo WITH IIF(perded[I,4]<10,'00','0')+alltrim(str(PERDED[I,4]))+"|"+;
							  					IIF(perded[i,2]<10,'00',IIF(perded[i,2]<100,'0',''))+ALLTRIM(str(PERDED[I,2]))+"|"+;
							  					ALLTRIM(PERDED[I,3])+"|0.00|"+ALLTRIM(STR(PERDED[I,1],12,2))+"|"
		            	ENDIF
           			ENDIF
            	ENDIF
           	ENDIF
           	IF (PERDED[I,4]=39 .or. PERDED[I,4]=44) && AND !JPRET
				APPEND BLANK
				REPLACE CAMPO WITH "CPJPRETIRO|TotalUnaExhibicion(N12.2),TotalParcialidad(N12.2),MontoDiario(N12.2),IngresoAcumulable(N12.2),IngresoNoAcumulable(N12.2)"
				APPEND BLANK
				REPLACE CAMPO WITH ALLTRIM(STR(PERDED[I,1],12,2))+'|0|0|0.00|0.00|'
           	ENDIF
          NEXT

*!*			APPEND BLANK
*!*			REPLACE CAMPO WITH "CPINDEMNIZA|TotalPagado(N12.2),NumAniosServicio(N12.2),UltimoSueldoMensOrd(N12.2),IngresoAcumulable(N12.2),IngresoNoAcumulable(N12.2)"
*!*			APPEND BLANK
*!*			REPLACE CAMPO WITH "CPATITULOS|ValorMercado(n16.6),PrecioAlOtorgarse(N16.6)"
*!*			APPEND BLANK
*!*			REPLACE campo WITH "DCHEXTRA|TipoHoras(C7),Dias(N4),HorasExtras(N4),ImportePagado(N12.2)"

		APPEND BLANK
		REPLACE campo WITH "DCDEDUCCION|TipoDeduccion(C3),Clave(C15),Concepto(C60),Importe(N12.2)"

		FOR J= 1 TO D
          		APPEND BLANK 
          		REPLACE campo WITH RIGHT('00'+alltrim(str(PERDEC[J,4])),3)+"|"+RIGHT('00'+ALLTRIM(STR(PERDEC[J,2])),3)+"|"+;
          							ALLTRIM(PERDEC[J,3])+"|"+ALLTRIM(STR(PERDEC[J,1],12,2))+"|"
		NEXT
		APPEND BLANK
		REPLACE campo WITH "COTROSTPAGOS|TipoOtroPago(C3),Clave(C15),Concepto(C120),Importe(N12.2),SubsidioCausado(N12.2),"+;
							"SaldoAFavor(N12.2),Anio(N4.0),RemanenteSalFav(N12.2)"
		APPEND BLANK
		REPLACE campo WITH "CSCONTRATO|RFCLabora(C13),PorcentajeTiempo(N9.3)"
		APPEND BLANK
		REPLACE campo WITH "DCINCAPACIDAD|TipoIncapacidad(C5),DiasIncapacidad(N4),Importe(N12.2),Descripcion(C60)"
    	select 1
    	SKIP
&&&&&&&&&&&&&&&&&&&&&&&& ENDFOR
&&&&&&&&&&&&&&&&&&&&&&&& EXIT
     ENDDO
*   ENDIF
	SELECT TIMBRADO
	REPLACE campo WITH STRTRAN(campo,'Ñ','N') ALL
	REPLACE campo WITH STRTRAN(campo,'Ð','N') ALL
	REPLACE campo WITH STRTRAN(campo,'¥','N') ALL
*	REPLACE campo WITH STRTRAN(campo,'.','') ALL
	REPLACE campo WITH STRTRAN(campo,';','N') ALL
	REPLACE campo WITH STRTRAN(campo,'  ',' ') ALL
	COPY TO &carchi DELIMITER WITH ""

*!*	   set printer to
*!*	   set printer off
*!*	   set device to screen
*!*	   RestScreen(03, 02, 12, 75, pPan)
*!*	   
Return
