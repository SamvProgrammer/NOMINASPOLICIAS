*!*	Function Folio(nAccion)
*!*	Local cAlias, nFol
*!*		cAlias = Select()
*!*		Use BdGastos!Folios In 0 Shared
*!*		Select Folios
*!*		Do while !rlock()
*!*		Enddo
*!*		if nAccion='+'
*!*			nFol = Gastos+1
*!*			Replace Gastos With nFol
*!*		Else
*!*			nFol = Captura-1
*!*			Replace Captura With nFol
*!*		Endif
*!*		Unlock
*!*		Use
*!*		Select(cAlias)
*!*	Return nFol

Function Folio( nPar , nAccion )
Local cAlias, nFol
	cAlias = Select()
	Use Data\Tablas\Folios Order 1 In 0 Shared
	if nAccion='+'
		nFol = Iif( Seek(nPar,'Folios',1), Folios.Folio+1, 0 )
	Else
		nFol = Iif( Seek(nPar,'Folios',1), Folios.Folio-1, 0 )
	Endif
	Update Folios Set Folio = nFol Where Clave == nPar
	Use In Folios
	Select(cAlias)
Return nFol

*!*	Function Crypt(nTexto,nMod)
*!*	Local nMacro,x,nClave
*!*	nMacro="Chr(Asc(Substr(nTexto,x,1))"+nMod+")"
*!*	Store ''to nClave
*!*	If !Empty(nTexto)
*!*		For x=1 to len(Alltrim(nTexto))
*!*			nClave=nClave+&nMacro
*!*		EndFor
*!*	Endif
*!*	Return nClave

Function Crypt(Cadena) 
Local I,Llave,nSalida, Cad 
*Invertimos Cadena 
Cad='' 
Cadena=Alltrim(Cadena) 
For i=Len(Cadena) to 1 Step -1 
	Cad=Cad+Substr(Cadena,I,1) 
EndFor 
Cadena=Cad 
nSalida='' 
Llave = 5 && 5 es mi llave de acceso y no de ser cambiada si se desea que esta misma desencripte la cadena 
&& o bajo ninguna circunstancia se podrán recuperar 
&& las cadenas Dadas con esta llave 
For I=1 to Len(Cadena) 
	Car=Asc(Substr(Cadena,I,1)) 
	Num=Chr(Bitxor(Car,5)) 
	nSalida=nSalida + Num 
Endfor 
Return nSalida

Function F_Acceso(nMenu)
Local nSi
If str(nVerMenu,1)$nMenu
	nSi=.F.
Else
	nSi=.T.
Endif
Return nSi

Function fnConver( _CANTID )
  Local mmil,mill,mile,peso,_ret,mdec,s,_Can1,_Can2
  mdec = (_Cantid-int(_Cantid))*100
  s = Alltrim( Str( Int(_Cantid) ) )
  Do while len(s)<12
     s='0'+s
  Enddo
  mmil = Val( Substr(s, 1,3))
  mill = Val( Substr(s, 4,3))
  mile = Val( Substr(s, 7,3))
  peso = Val( Substr(s,10,3))
  Store '' To _Can1,_Can2
  If mmil#0
     _Can1=_Dam(mmil)+'MIL '
  Endif
  If mill#0
     _Can1=_Can1+iif(mill=1,_Dam(mill)+'MILLON',_Dam(mill)+'MILLONES')
  Endif
  If mile#0
     _Can1= iif(mill>0,_Can1+' '+_Dam(mile) + 'MIL', _Dam(mile)+'MIL')
  Endif
  _Can2= _Dam(peso)
  _can3 = '(' + _can1 + ' ' + TRIM(_can2) + Iif( mill<>0.and. mile=0.and. peso=0, 'DE PESOS ', Iif( _cantid > 1.00.or._Cantid < 1.00, ' PESOS ', ' PESO ')) ;
  + Iif( mdec<10 .and. !mdec=0, '0' + Alltrim(Str(mdec)), Iif(mdec = 0, '00', Iif(mdec > 9, Alltrim(str(mdec)), ''))) + '/100 M.N.)'
  
Return _can3

Function _Dam(_dto)
  Local unid, dece, _cen, extr, S3, cvn,cen,dec,uni,nIni,nFin
  S = alltrim(str(_dto,3,0))
  S3=''

  Dimension Unid(9),Dece(7),_Cen(9),Extr(20)

  Store 'UN'     to Unid(1)
  Store 'DOS'    to Unid(2)
  Store 'TRES'   to Unid(3)
  Store 'CUATRO' to Unid(4)
  Store 'CINCO'  to Unid(5)
  Store 'SEIS'   to Unid(6)
  Store 'SIETE'  to Unid(7)
  Store 'OCHO'   to Unid(8)
  Store 'NUEVE'  to Unid(9)

  Store 'TREINTA'   to dece(1)
  Store 'CUARENTA'  to dece(2)
  Store 'CINCUENTA' to dece(3)
  Store 'SESENTA'   to dece(4)
  Store 'SETENTA'   to dece(5)
  Store 'OCHENTA'   to dece(6)
  Store 'NOVENTA'   to dece(7)

  Store 'CIENTO'        to _Cen(1)
  Store 'DOSCIENTOS'    to _Cen(2)
  Store 'TRESCIENTOS'   to _Cen(3)
  Store 'CUATROCIENTOS' to _Cen(4)
  Store 'QUINIENTOS'    to _Cen(5)
  Store 'SEISCIENTOS'   to _Cen(6)
  Store 'SETECIENTOS'   to _Cen(7)
  Store 'OCHOCIENTOS'   to _Cen(8)
  Store 'NOVECIENTOS'   to _Cen(9)

  Store 'DIEZ'          to Extr(1)
  Store 'ONCE'          to Extr(2)
  Store 'DOCE'          to Extr(3)
  Store 'TRECE'         to Extr(4)
  Store 'CATORCE'       to Extr(5)
  Store 'QUINCE'        to Extr(6)
  Store 'DIECISEIS'     to Extr(7)
  Store 'DIECISIETE'    to Extr(8)
  Store 'DIECIOCHO'     to Extr(9)
  Store 'DIECINUEVE'    to Extr(10)
  Store 'VEINTE'        to Extr(11)
  Store 'VEINTIUN'      to Extr(12)
  Store 'VEINTIDOS'     to Extr(13)
  Store 'VEINTITRES'    to Extr(14)
  Store 'VEINTICUATRO'  to Extr(15)
  Store 'VEINTICINCO'   to Extr(16)
  Store 'VEINTISEIS'    to Extr(17)
  Store 'VEINTISIETE'   to Extr(18)
  Store 'VEINTIOCHO'    to Extr(19)
  Store 'VEINTINUEVE'   to Extr(20)

  Do while len(s)<3
     s='0'+s
  Enddo
  cvn = Val( Substr(s,2,2) )
  cen = Val( Substr(s,1,1) )
  dec = Val( Substr(s,2,1) )
  uni = Val( Substr(s,3,1) )

  s3 = Iif( cen>0, _Cen[cen]+' ', s3)
  s3 = Iif( dec>2, s3 + dece[dec-2]+' ', s3)
  s3 = Iif( dec>2 .and. uni>0,s3 + 'Y ', s3)
  s3 = Iif( cvn>9 .and. cvn<30, s3 + extr[cvn-9]+' ',Iif(uni>0, s3 +unid(uni)+' ', s3))
  s3 = Iif( _dto=100, 'CIEN ', s3)

Return s3

Function fnFecha(nFecha)
Local nMes
Dimension nMes(12)
nMes(1) ='Enero  '
nMes(2) ='Febrero'
nMes(3) ='Marzo  '
nMes(4) ='Abril  '
nMes(5) ='Mayo   '
nMes(6) ='Junio  '
nMes(7) ='Julio  '
nMes(8) ='Agosto '
nMes(9) ='Sep.   '
nMes(10)='Octubre'
nMes(11)='Nov.   '
nMes(12)='Dic.   '
if !Empty(nFecha)
	nFecha=IIf(Day(nFecha)<10,'0','')+Alltrim(Str(Day(nFecha)))+' de '+nMes(Month(nFecha))+Iif(Year(nFecha)<2000,' de ',' de ')+Str(Year(nFecha),4)
Else
	nFecha='  /  /  '	
ENDIF
Return nFecha

FUNCTION fnMes(xMes)
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
RETURN(aMes(xMes))

Function PorDefinir()
MessageBox('Proceso en desarrollo...',0)
Return


********************************
function FORMFECHA(dfecha)
   local ameses, cfecha
   DECLARE ameses[12]
   set date german
   ameses[1]= "ENE"
   ameses[2]= "FEB"
   ameses[3]= "MAR"
   ameses[4]= "ABR"
   ameses[5]= "MAY"
   ameses[6]= "JUN"
   ameses[7]= "JUL"
   ameses[8]= "AGO"
   ameses[9]= "SEP"
   ameses[10]= "OCT"
   ameses[11]= "NOV"
   ameses[12]= "DIC"
   cfecha= Str(Day(dfecha), 2) + "." + ameses[Month(dfecha)] + "." ;
      + right(Str(Year(dfecha)), 2)
return cfecha
********************************
function DESGLOSE(Arg1)
   local Local1, Local2, Local3[15,2], Local4[15]
   Local4[1]= 100
   Local4[2]= 50
   Local4[3]= 20
   Local4[4]= 10
   Local4[5]= 5
   Local4[6]= 2
   Local4[7]= 1
   Local4[8]= 0.5
   Local4[9]= 0.2
   Local4[10]= 0.1
   Local4[11]= 0.05
   Local4[12]= 0.02
   Local4[13]= 0.01
   Local4[14]= 0.005
   Local4[15]= 0.001
   for Local1= 1 to ALEN(local4)
      Local3[Local1,1]= Local4[Local1]
      Local3[Local1,2]= 0
      Local2= alltrim(Str(Arg1 / Local4[Local1], 11, 3))
      Local3[Local1,2]= Val(Left(Local2, rat(".", Local2) - 1))
      Arg1= Round(Arg1 - Local3[Local1,2] * Local4[Local1], 3)
*      Local3[Local1,1]= Local4[Local1]
*!*	      Local3[Local1]= 0
*!*	      Local2= alltrim(Str(Arg1 / Local4[Local1], 11, 3))
*!*	      Local3[Local1]= Val(Left(Local2, rat(".", Local2) - 1))
*!*	      Arg1= Round(Arg1 - Local3[Local1] * Local4[Local1], 3)
   NEXT
   FOR i=1 TO 15
   		amonedas(i,1)=Local3(i,1)
   		amonedas(i,2)=Local3(i,2)
   endfor
*!*	   cancel
*	amonedas=Local3
*   FOR i=1 TO 15
*   		?amonedas(i,2)
*   endfor
return

*****************************************
********* Conectarse a Postgres *********
*****************************************
*!*	Function fConectar(nSi)
*!*	if nSi
*!*		* POAG_MIG
*!*	*!*		cStrConec = "driver={PostgreSQL Unicode};Port=5432"+;
*!*	*!*			             ";Server=172.18.19.106"+;
*!*	*!*			             ";Database=saai"+;
*!*	*!*			             ";Uid=postgres"+;
*!*	*!*			             ";Pwd=admindie"
*!*	*!*		* INFORME2008
*!*		cStrConec = "driver={PostgreSQL ANSI};Port=5432"+;
*!*			             ";Server=192.168.100.101"+;
*!*			             ";Database=sispe"+;
*!*			             ";Uid=sispe"+;
*!*			             ";Pwd=admindie"
*!*	*!*		cStrConec = "driver={PostgreSQL ANSI};Port=5432"+;
*!*	*!*			             ";Server=localhost"+;
*!*	*!*			             ";Database=nomina"+;
*!*	*!*			             ";Uid=postgres"+;
*!*	*!*			             ";Pwd=maestro"
*!*		***Evitar que aparezca  la ventana de login
*!*		 =SQLSETPROP(0,"DispLogin",3)
*!*		*!*	create sql view catreg SQLSTRINGCONNECT(cStrConec)
*!*		*** se realiza la conexion 
*!*		nHandle = SQLSTRINGCONNECT(cStrConec)
*!*	*	?nHandle
*!*	*	=SQLEXEC(nHandle, " Select * from datos.aportaciones where rfc like 'AUMA%' ",'paso')
*!*	*	= SQLDISCONNECT(nHandle)
*!*	*	return lnHandle
*!*	Else
*!*		= SQLDISCONNECT(nHandle)
*!*	Endif
*!*	RETURN

FUNCTION Sobres(dfechaini,dfechafin,nEmp,nTipo)
local ppan, aempleado, cjpp, nnumjpp, crfc, nSobres
local cnombre, apercep, adeduc, ntotaltot, ntotalsob, asobres
DECLARE adeduc[50,2], apercep[50,2]
PUBLIC ARRAY amonedas[15,2]
nSobres=''
CLOSE TABLES all
CREATE TABLE Temp\Sobres FREE (CAMPO C(200)) 
USE data\tablas\perded ORDER clave IN 0
USE data\tablas\nominew ORDER nomina ALIAS NomiNew IN 0
USE data\tablas\maestro order nomina IN 0
SELECT sobres
IF !EMPTY(nEmp)
	APPEND BLANK
	APPEND BLANK
Endif

select NomiNew
set relation to STR(Clave,3) into PerDed
*!*	   dfechaini= CTOD('01/03/2016')
*!*	   dfechafin= CTOD('31/03/2016')
       DECLARE asobres[3, 5]
      asobres[1,1]=" (Jpp== 'JUP' .Or. Jpp== 'PDP' .Or. Jpp== 'PTP' ) "
*      asobres[1,1]=" Jpp== 'JUP' "
      asobres[1,2]="Listados\SobJUB.Txt"
      asobres[1,3]=0
      asobres[1,4]=0
      asobres[1,5]="JUBILADOS CENTRO"

      asobres[2,1]=" (Jpp== 'JUF' .Or. Jpp== 'PDF' .Or. Jpp== 'PTF') .And. NomElec=='S' "
*      asobres[2,1]=" Jpp== 'JUF' .And. NomElec=='S' "
      asobres[2,2]="Listados\SobJUF.Txt"
      asobres[2,3]=0
      asobres[2,4]=0
      asobres[2,5]="JUBILADOS FORANEOS"

      asobres[3,1]=" Jpp== 'PEA' "
      asobres[3,2]="Listados\SobPEA.Txt"
      asobres[3,3]=0
      asobres[3,4]=0
      asobres[3,5]="PENSION ALIMENTICIA"

      for k= 1 to IIF(EMPTY(nEmp),3,1)    &&&3 &&Len(asobres)
         cfiltro= asobres[k,1]
         carchivo= asobres[k,2]
         nlin= 0
         ntotaltot= 0
         ntotalsob= 0
         select Maestro
         IF !EMPTY(nEmp)
			 SET FILTER TO JPP=nTipo AND INLIST(NUM,&nEmp)
*			 SET FILTER TO JPP='JUF' AND INLIST(NUM,&nEmp)
*			 SET FILTER TO &cfiltro AND INLIST(NUM,&nEmp)
		 ELSE
	         SET FILTER TO &cfiltro AND superviven='S'
		 ENDIF
*		 SET FILTER TO NUM=507350
         goto top
		 nTantos=0
         do while (!EOF())
         	nSobres=nSobres+CHR(13)+STR(num,10)+' '+nombre
            cjpp= jpp
            nnumjpp= num
            cnombre= nombre
            crfc= rfc
            ctipoemp1= LEFT(Maestro.categ,20)
            ctipoemp2= LEFT(Maestro.categ,20)
            ndeduc= 0
            npercep= 0
		    FOR xy=1 TO 50
			   adeduc[xy,1]=''
			   adeduc[xy,2]=''
			   apercep[xy,1]=''
			   apercep[xy,2]=''
		    ENDFOR 

            select NomiNew
            if !Seek(cjpp + Str(nnumjpp,6))
               select Maestro
               Skip
               loop
            endif
            ntotalsob=ntotalsob+1
            do while (cjpp == jpp .AND. nnumjpp == numjpp)
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
            enddo
            nmayor=0
            FOR  i= 1 to 50
            	IF !EMPTY(apercep[i,1]) OR !EMPTY(adeduc[i,1])
	            	nmayor=i
            	endif
*!*	            	IF !EMPTY(adeduc[i,1])
*!*		            	nmayor=i
*!*	            	endif
			ENDFOR
			nmayor=IIF(nmayor=1,2,nmayor)
*			WAIT WINDOWS MAESTRO.JPP+' '+STR(NMAYOR)
*			nSel=SELECT()
			SELECT Sobres
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
	               	IF EMPTY(campo)
               	  		replace campo WITH SPACE(134)+"DEDUCCIONES:............"+SPACE(25)+TRANSFORM(ndeduc,' 999,999.99')
					else
               	  		replace campo WITH RTRIM(campo)+IIF(EMPTY(adeduc[i,1]),SPACE(38),SPACE(24))+"DEDUCCIONES:............"+SPACE(25)+TRANSFORM(ndeduc,' 999,999.99')
					ENDIF 
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
* 			SELECT(nSel)
            select Maestro
            SKIP
         enddo
         IF EMPTY(nEmp)
	         select Maestro
	         set filter to
*	         nSel=SELECT()
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
	     Endif
         asobres[k,3]= ntotalsob
         asobres[k,4]= ntotaltot
		 SELECT sobres
         IF EMPTY(nEmp)
	         nArch=asobres[k,2]
			 COPY TO (nArch) DELIMITER WITH ""
			 ZAP
		 Endif
*         SELECT(nSel)
      next
   select Maestro
   set relation to
   select NomiNew
   set relation to
   SET DATE BRITISH
   SELECT sobres
   IF !EMPTY(nEmp)
		If MessageBox('SE IMPRIMIRAN LOS SOBRES DE NOMINA DE: '+chr(13)+nSobres+chr(13)+chr(13)+'Desea Continuar...????',1+32,'')=1
			COPY TO lpt2 DELIMITER WITH ""
*			COPY TO SOBRES.TXT DELIMITER WITH ""
		ENDIF
   endif
RETURN

FUNCTION F_Tablas(nMes,nAnio)
Local Usa_Tabla
CLOSE TABLES ALL 
IF EMPTY(Usa_Indice)
	Usa_Indice='Temp\_'+RIGHT(SYS(2015),5)
endif
	Usa_Tabla='Nomi'+STR(nAnio,4)+'\Maes'+STR(nAnio,4)+IIF(nMes<10,'0','')+TRIM(STR(nMes))+'.Dbf'
	IF !FILE(Usa_Tabla)
		MESSAGEBOX('No Existen respaldos del mes seleccionado...'+CHR(13)+CHR(13)+Usa_Tabla)
		RETURN .F.
	endif
	USE (Usa_Tabla) ALIAS Maestro IN 0
	SELECT Maestro
	INDEX on jpp+STR(num,6) TO (Usa_Indice+'1')
	Usa_Tabla='Nomi'+STR(nAnio,4)+'\Nomi'+STR(nAnio,4)+IIF(nMes<10,'0','')+TRIM(STR(nMes))+'.Dbf'
	IF !FILE(Usa_Tabla)
		MESSAGEBOX('No Existen respaldos del mes seleccionado...'+CHR(13)+CHR(13)+Usa_Tabla)
		RETURN .F.
	endif
	USE (Usa_Tabla) ALIAS Nominew IN 0
	SELECT Nominew
	INDEX on jpp+STR(numjpp,6)+STR(clave,3)+STR(secuen,1) TO (Usa_Indice+'2')
	Usa_Tabla='Nomi'+STR(nAnio,4)+'\Perd'+STR(nAnio,4)+IIF(nMes<10,'0','')+TRIM(STR(nMes))+'.Dbf'
	IF !FILE(Usa_Tabla)
		MESSAGEBOX('No Existen respaldos del mes seleccionado...'+CHR(13)+CHR(13)+Usa_Tabla)
		RETURN .F.
	endif
	USE (Usa_Tabla) ALIAS Perded IN 0
	SELECT Perded
	INDEX on STR(clave,3) TO (Usa_Indice+'3')
RETURN .T.

FUNCTION f_Dir(_Anio)
LOCAL nRuta
nRuta= curdir()+"NOMI" + _Anio + "\"
If !Directory(nRuta)
	MessageBox('No se encuentra el Directorio de Respaldo:'+chr(13)+chr(13)+nRuta,64,'Directorio no existe...')
	RETURN .F.
ELSE
	RETURN .T.
Endif

FUNCTION f_Opciones(_Mes,_Anio)
LOCAL nRuta,nNomina
nRuta= curdir()+"NOMI" + _Anio + "\"
nNomina = "Nomi" + _Anio + _Mes + ".Dbf"
*MESSAGEBOX(nRuta+nNomina)
If File(nRuta+nNomina)
	RETURN .T.
Else
	RETURN .F.
Endif

FUNCTION Incrementar()
CLOSE TABLES all
USE Data\tablas\nominew
IF MESSAGEBOX('ESTA SEGURO DE REALIZAR EL INCREMENTO...',4+32,'Aviso...')=6
		WAIT windows 'INCREMENTANDO...' NOWAIT
       	 select NomiNew
         goto top
         do while (!EOF())
            do case
            case pago1 > 0 .AND. pago2 > 0 .AND. pago3 > 0 .AND. ;
                  pago4 > 0
               replace pago1 with pago1 + 1
               replace pago2 with pago2 + 1
               replace pago3 with pago3 + 1
               replace pago4 with pago4 + 1
            case pago2 > 0 .AND. pago3 > 0 .AND. pago4 > 0
               replace pago2 with pago2 + 1
               replace pago3 with pago3 + 1
               replace pago4 with pago4 + 1
            case pago3 > 0 .AND. pago4 > 0
               replace pago3 with pago3 + 1
               replace pago4 with pago4 + 1
            case pago4 > 0
               replace pago4 with pago4 + 1
            endcase
            if (pago1 > pagot .OR. pago2 > pagot .OR. pago3 > pagot ;
                  .OR. pago4 > pagot)
               delete
            endif
            Skip
         ENDDO
         MESSAGEBOX('INCREMENTO FINALIZADO...',0+16,'Aviso.')
ENDIF 
CLOSE TABLES All
RETURN

FUNCTION fMes(xMes)
Local nMes
Dimension nMes(12)
	nMes(1) ='Enero'
	nMes(2) ='Febrero'
	nMes(3) ='Marzo'
	nMes(4) ='Abril'
	nMes(5) ='Mayo'
	nMes(6) ='Junio'
	nMes(7) ='Julio'
	nMes(8) ='Agosto'
	nMes(9) ='Septiembre'
	nMes(10)='Octubre'
	nMes(11)='Noviembre'
	nMes(12)='Diciembre'
RETURN nMes(xMes)

FUNCTION fHorario(nGrupo)
LOCAL xHora
	DO Case
		CASE nGrupo=1 && BASE NORMAL
			xHora='9:00 - 15:00'
		CASE nGrupo=2 && BASE ESPECIAL
			xHora='7:00 - 13:00'
		CASE nGrupo=3 && BASE ESPECIAL
			xHora='8:00 - 14:00'
		CASE nGrupo=4 && BASE ESPECIAL
			xHora='10:00 - 16:00'
		CASE nGrupo=5 && NORMAL CONTRATO - CONTRATO CONFIANZA
			xHora='8:00 - 16:00'
		CASE nGrupo=6 && NORMAL CONTRATO LIMPIEZA
			xHora='7:00 - 15:00'
		OTHERWISE
			xHora='No Existe'
	ENDCASE
RETURN xHora