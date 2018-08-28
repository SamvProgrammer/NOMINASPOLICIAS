LPARAMETERS nAnio,nMes,CveQuin,IndNom,TipoP
Local pPan,cColor,aClave,nClave,cDescri,cMonto,nLin,nPag,nTotEmp,cPart,nPer,nDed
LOCAL nTotDed,nTotPer, cLin, cJpp, nNumJpp, aStruc, aNoper, nEncon,aNoded
STORE 0 TO NUM1,NUM2,NUM3,NUM4,NUM5

local crutabas, cmaestro, cnomina, cdesc, cjpp
crutades= "Listados\"
nMes= iif(nMes < 10,'0','')+ALLTRIM(Str(nMes,2))
nAnio= alltrim(Str(nAnio))
crutabas= "RESPALDOS\NOMI" + nAnio + "\"
cdesc= "BASE" + nAnio + nMes
cmaestro= "Maes" + nAnio + nMes
cnomina= "Nomi" + nAnio + nMes
nTmp=crutades+cdesc
	
AREA=SPACE(70)
*!*	@9, 4 Say 'CLAVE DE LA QUINCENA:'     Get CveQuin Pict '999999'
*!*	@10,4 Say 'INDICADOR DE NOMINA:'      Get IndNom  Pict '99'
*!*	@11,4 Say 'NORMAL (0) RETRO. (1):'    Get TipoP   Pict '9'
*!*	Read

aStruc= '(CQNACVE   N( 6, 0), CQNAIND   N( 2, 0), PBPNUP    N( 6, 0), PBPNUE    N( 6, 0), SIRFC     C(13, 0),'+;
		' SINOM     C(45, 0), SIDEP     C(20, 0), SICATG    C( 7, 0), SISX      C( 1, 0), TIP_EMP   N( 2, 0),'+;
		' NUMFOLIO  N( 6, 0), UBICA     C( 1, 0), CVE       N( 3, 0), TIPOPAGO  N( 1, 0), PBPFIG    D( 8, 0),'+;
		' PBPIPTO   D( 8, 0), PBPFNOMB  D( 8, 0), PBPSTATUS N( 2, 0), LICDES    D( 8, 0), LICHAS    D( 8, 0),'+;
		' CMOTCVE   N( 3, 0), PBPHIJOS  N( 1, 0), GUADES    D( 8, 0), GUAHAS    D( 8, 0), CURP      C(21, 0),'+;
		' AREAADS   C(70, 0), NOMBANCO  C(20, 0), NUMCUENTA C(12, 0), CLAVE_INTE C(20, 0), CESTNIV   N( 2, 0),'+;
		' CESTGDO   N( 1, 0), QNIOS     N( 1, 0), PBPIMSS   C(11, 0), BGIMSS    N(12, 2), CUOPIMSS  N(13, 2),'+;
		' CUOPRCV   N(13, 2), INCDES    D( 8, 0), INCHAS    D( 8, 0), CUOPINF   N(13, 2), CUOPFPEN  N(13, 2),'+;
		' BGISPT    N(12, 2), PROFESION N( 4, 0), STATPAGO  C(180, 0), AGUIFDES  D( 8, 0), AGUIFHAS  D( 8, 0),'+;
		' DAFDES    D( 8, 0), DAFHAS    D( 8, 0)'

CLOSE TABLES all
USE &crutabas&cnomina ALIAS nominew IN 0
SELECT NOMINEW
INDEX ON CLAVE TO temp\&cnomina UNIQUE
GO TOP
DO WHILE !EOF()
    if clave<69
         aStruc=aStruc+ ', P'+AllTrim(Str(clave)) + ' N( 13, 2 )'
    endif
    if clave>=69
         aStruc=aStruc+ ', D'+AllTrim(Str(clave)) + ' N( 13, 2 )'
    endif
    SKIP 
ENDDO
aStruc=aStruc+')'
*MESSAGEBOX(aStruc)
*RETURN 
CLOSE TABLES ALL
CREATE TABLE &nTmp FREE &aStruc
CLOSE TABLES ALL
USE &nTmp ALIAS Base IN 0
USE &crutabas&cnomina ALIAS nominew IN 0
USE &crutabas&cmaestro ALIAS maestro IN 0
SELECT maestro
INDEX on Jpp+Str(Num,6) TO temp\&cmaestro
SELECT nominew
INDEX on jpp+STR(numjpp,6)+TipoPago TO temp\&cnomina
Select('NomiNew')
set filter to tipopago="N"
GO TOP 
Select('Maestro')
Set Filter To Jpp<> 'PEA' AND SUPERVIVEN='S'
GO TOP 
DO While !Eof()
     cJpp= Jpp
     nNumJpp= Num
     cLlave= Jpp+ Str(Num, 6)
     Select('NomiNew')
     If Seek(cLlave)
*	       cTipoPago= TipoPago
	       NUM=0
	       Do Case
	          Case Maestro->Jpp='JUP' .OR. Maestro->Jpp='JUF'
	             TipEmp=13
	             NUM=NUM1
	             AREA="  "
	          Case Maestro->Jpp='PDP' .OR. MAESTRO->JPP="PDF"
	             TipEmp=14
	             NUM=NUM2
	             AREA=" " 
	          Case Maestro->Jpp='PTP' .OR. MAESTRO->JPP="PTF"
	             TipEmp=15
	             AREA="  "
	             NUM=NUM4
	          Case Maestro->Jpp='MMS'
	             AREA=MAESTRO->DEPTO
	             TipEmp=4
	             NUM=NUM5
	          Case Maestro->Jpp='DCC'
	             TipEmp=5
	             AREA=MAESTRO->DEPTO
	             NUM=5023
	          Case Maestro->Jpp='COC'
	             TipEmp=6
	             Area=MAESTRO->DEPTO 
	             NUM=NUM3
	          OtherWise
	             TipEmp= 0
	             NUM=0
	             AREA=" "
	       EndCase
	       Select('Base')
	       APPEND BLANK 
	       Replace                              ;
	         SiRfc     With  Maestro->Rfc      ,;
	         SiNom     With  Maestro->Nombre   ,;
	         SiDep     With  Maestro->JPP+ALLTRIM(STR(Maestro->NUM)) ,;
	         SiCatg    With  " "   ,;       
	         SisX      With  Maestro->Sexo     ,;
	         Cve       With  470               ,;
	         Tip_Emp   With  TipEmp            ,;
	         AREAADS   WITH  AREA              ,;
	         NUMFOLIO  WITH  NUM               ,;
	         CQNACVE   With  CveQuin           ,;
	         TIPOPAGO  WITH  TIPOP             ,;
	         PBPSTATUS  WITH  11                ,; 
	         UBICA     WITH  '3'               ,;
	         CQNAIND   With  IndNom            

	       Select('NomiNew')
	       DO While cJpp== Jpp .And. NumJpp== nNumJpp AND !EOF()
		         nClave= Clave         
		         nMonto= Monto
		         Select('Base')
		         If nClave >=69         &&Clave de percepcion
		           cTipo= 'D'+AllTrim(Str(nClave))
		         Else
		           cTipo= 'P'+AllTrim(Str(nClave))
		         EndIf
		&&             If nClave<> 67 .Or. (nClave= 67 .And. cPenFin=='F') &&Prestamos directos
		         Replace &cTipo With &cTipo + nMonto
		&&             EndIf
		         Select('NomiNew')
		         Skip
	       EndDo
     EndIf
     Select('Maestro')
     Skip
EndDo

*!*	/*INICIA PARA LAS CLAVES 33 se anula el siguiente bloque porque fue agregado en el proceso de generar el listado

*!*	      Select('Maestro')
*!*	      Set Filter To STATUS=33
*!*	      DbGoTop()

*!*	      While !Eof()

*!*	             NSiRfc     =  Rfc      
*!*	             NSiNom     =  Nombre   
*!*	             NSiDep     =  Proyecto 
*!*	             NSiCatg    =  CveCat   
*!*	             NSisX      =  Sexo     
*!*	             NCve       =  470               
*!*	             NTip_Emp   =  TipEmp            
*!*	             NCURP      =  Curp     
*!*	             NPBPHIJOS  =  Hijos    
*!*	             NCESTNIV   =  Nivest   
*!*	             NCESTGDO   =  Gdoest   
*!*	             NPBPNUE    =  Nue      
*!*	             NPBPNUP    =  Nup      
*!*	             NPBPSTATUS =  Status   
*!*	             NQNIOS     =  Numquin  
*!*	             NPBPFNOMB  =  Fchingnom
*!*	             NPBPFIG    =  Fchinggob
*!*	             NPBPIPTO   =  FPUESTO
*!*	             TIPOPAGO   =  TipoP
*!*	             NPBPIMSS   =  IMSS     
*!*	             CQNACVE    =  CveQuin           
*!*	             CQNAIND    =  IndNom

*!*	           Select('Base')
*!*	           DbAppend()

*!*	           replace                       ;
*!*	             SiRfc     With  NSiRfc     ,;
*!*	             SiNom     With  NSiNom     ,;
*!*	             SiDep     With  NsiDep     ,;
*!*	             SiCatg    With  NSiCatg    ,;
*!*	             SisX      With  NSisx      ,;
*!*	             Cve       With  470        ,;       
*!*	             Tip_Emp   With  NTip_Emp   ,;         
*!*	             CURP      With  NCurp      ,;
*!*	             PBPHIJOS  With  NpbpHijos  ,;  
*!*	             CESTNIV   With  NCestNiv   ,;
*!*	             CESTGDO   With  NcEstGdo    ,;
*!*	             PBPNUE    With  NPbpNue    ,;  
*!*	             PBPNUP    With  NpbpNup    ,;  
*!*	             PBPSTATUS With  NPbpStatus ,;  
*!*	             QNIOS     With  Nqnios     ,;
*!*	             PBPFNOMB  With  NpbpFnomb  ,;
*!*	             PBPFIG    With  Npbpfig    ,;
*!*	             PBPIPTO   WITH  NPBPIPTO   ,;
*!*	             TIPOPAGO  With  TipoP      ,;
*!*	             PBPIMSS   With  NpbpIMSS   ,;  
*!*	             CQNACVE   With  CveQuin    ,;
*!*	             CQNAIND   With  IndNom

*!*	        select ('Maestro')
*!*	        DbSkip()
*!*	      enddo

*!*	TERMINA CLAVE 33*/

&&/*inicio 020*/

Select('NomiNew')
Set filter to tipopago="R"
Select('Maestro')
*Set Filter To Jpp<> 'PEA' 
Set Filter To Jpp<> 'PEA' AND SUPERVIVEN='S'
GO TOP 
Do While !Eof()
     cJpp= Jpp
     nNumJpp= Num
     cLlave= Jpp+ Str(Num, 6)
	 Select('NomiNew')
     If Seek(cLlave)
*	       cTipoPago= TipoPago
	       NUM=0
		   Do Case
	          Case Maestro->Jpp='JUP' .OR. Maestro->Jpp='JUF'
	             TipEmp=13
	             NUM=NUM1
	             AREA=" "
	          Case Maestro->Jpp='PDP' .OR. MAESTRO->JPP="PDF"
	             TipEmp=14
	             NUM=NUM2
	             AREA="  "  

	          Case Maestro->Jpp='PTP' .OR. MAESTRO->JPP="PTF"
	             TipEmp=15
	             NUM=NUM4
	             AREA=" "
	          Case Maestro->Jpp='MMS'
	             TipEmp=4
	             NUM=NUM5
	             AREA=maestro->depto
	          Case Maestro->Jpp='DCC'
	             TipEmp=5
	             AREA=MAESTRO->DEPTO
	          Case Maestro->Jpp='COC'
	             TipEmp=6
	             NUM=NUM3
	             AREA=MAESTRO->DEPTO
	          OtherWise
	             TipEmp= 0
	             AREA=" "
	       EndCase
	       Select('Base')
	       APPEND BLANK 
	       Replace                              ;
	         SiRfc     With  Maestro->Rfc      ,;
	         SiNom     With  Maestro->Nombre   ,;
	         SiDep     With  Maestro->JPP+ALLTRIM(STR(Maestro->NUM)) ,;
	         SiCatg    With  " "   ,;
	         SisX      With  Maestro->Sexo     ,;
	         Cve       With  470               ,;
	         Tip_Emp   With  TipEmp            ,;
	         AREAADS   WITH  AREA              ,;
	         NUMFOLIO  WITH  NUM               ,;
	         CQNACVE   With  CveQuin           ,;
	         TIPOPAGO  WITH  1                 ,;
	         PBPSTATUS  WITH  11                ,;
	         UBICA     WITH  '3'               ,;
	         CQNAIND   With  IndNom            
	      Select('NomiNew')
	       DO While cJpp== Jpp .And. NumJpp== nNumJpp AND !EOF()
		         nClave= Clave         
		         nMonto= Monto          
		         Select('Base')
		         If nClave > 68         &&Clave de percepcion
		           cTipo= 'D'+AllTrim(Str(nClave))
		         Else
		           cTipo= 'P'+AllTrim(Str(nClave))
		         EndIf
		         &&If nClave<> 67 .Or. (nClave= 67 .And. cPenFin=='F') &&Prestamos directos
		           Replace &cTipo With &cTipo + nMonto
		         &&EndIf
		         Select('NomiNew')
		         Skip
	       EndDo
     EndIf
     Select('Maestro')
     Skip
EndDo


*/*termino 020 */

*!*	      Select('Base')
*!*	      Copy to BaseEsp


*!*	      Select('Base')            &&Copia los de la categoria mayor

*!*	      DbUseArea(.T.,,'BaseEsp')
*!*	      DbCreateIndex( 'BaseEsp', "SiNom", {||SiNom} )
*!*	      set filter to pbpstatus <> 33 .AND. PBPSTATUS <> 22

*!*	      cCorres=Space(167)
*!*	      While .T.
*!*	        @8, 4 Say 'CORRESPONDIENTE : '        Get cCorres Pict '@!S40'
*!*	        Read

*!*	        If LastKey() = 27
*!*	          If Preguntar('¨ CONTINUAR CON EL LISTADO ALFABETICO ? :')='S'
*!*	            Loop
*!*	          Else
*!*	            Exit
*!*	          EndIf
*!*	        EndIf

*!*	        If Preguntar(' ¨ COMENZAR ? ')='N'
*!*	          Loop
*!*	        EndIf

*!*	        pPan2=SaveScreen(11,5,13,75)
*!*	        Cuadro(11,5,13,75,'R')
*!*	        @12,6 Say PadC('ESPERE UN MOMENTO GENERANDO EL LISTADO',68) Color 'W*/R'

*!*	/*
*!*	        A PARTIR DE ESTAS LINEAS:
*!*	        LAS LINEAS QUE ESTAN ENTRE COMENTARIOS SE HAN DESHABILITADO PORQUE
*!*	        LA DIRECCION DE RECURSOS HUMANOS HA ORDENADO QUE DICHOS CAMPOS SON
*!*	        ELIMINADOS DE LA BASE DE DATOS Y SON ENGLOBADOS EN OTRAS CLAVES
*!*	        DE PERCEPCIONES, SOLO DEJAMOS LAS AUTORIZADAS.

*!*	        SE HAN DESABILITADO LAS CLAVES DE LAS SUMAS PORQUE NO DEBEN APARECER
*!*	        MAS EN LOS REPORTES Y LISTADOS DE NOMINAS.
*!*	*/

*!*	        Set Margin to 0
*!*	        Set Printer to 'Lstado.txt'
*!*	        Set Printer on
*!*	        Set Device to Printer
*!*	        Set Score Off
*!*	        nLin=99
*!*	        nGranTot=0

*!*	        While !Eof()

*!*	           If nLin > 57
*!*	             @ 01 , 000 Say 'SECRETARIA DE ADMINISTRACION - OFICINA DE PENSIONES'
*!*	             @ 02 , 000 Say 'PROYECTO: 4F1000DDAD101A01901'
*!*	             @ 03 , 000 Say 'LISTADO ALFABETICO DEL PERSONAL CORRESPONDIENTE ' + AllTrim(cCorres)
*!*	             @ 05 , 000 Say 'NOMBRE'
*!*	             @ 05 , 031 Say 'R.F.C.'
*!*	             @ 05 , 045 Say 'T/E'
*!*	             @ 05 , 050 Say 'CATEG.'
*!*	             @ 05 , 062 Say 'P1 '
*!*	             @ 05 , 075 Say 'P2 '
*!*	             @ 05 , 088 Say 'P4 '
*!*	             @ 05 , 101 Say 'P6 '
*!*	             @ 05 , 113 Say 'P7 '
*!*	             @ 05 , 126 Say 'P8 '
*!*	             @ 05 , 139 Say 'P9 '
*!*	             @ 05 , 151 Say 'P10'
*!*	             @ 05 , 163 Say 'P12'
*!*	             @ 05 , 175 Say 'P13'
*!*	             @ 05 , 187 Say 'P14'
*!*	             @ 05 , 199 Say 'P16'
*!*	             @ 05 , 211 Say 'P17'
*!*	             nLin = 7
*!*	           EndIf

*!*	/*           @ nLin , 000 Say substr(SiNom,1,30)
*!*	           @ nLin , 031 Say SiRfc
*!*	           @ nLin , 045 Say Tip_Emp
*!*	           @ nLin , 050 Say SiCatg
*!*	           @ nLin , 057 Say P1  Pict '9,999,999.99'
*!*	           @ nLin , 070 Say P2  Pict '9,999,999.99'
*!*	           @ nLin , 083 Say P4  Pict '9,999,999.99'
*!*	           @ nLin , 096 Say P6  Pict '9,999,999.99'
*!*	           @ nLin , 108 Say P7  Pict '9,999,999.99'
*!*	           @ nLin , 121 Say P8  Pict '9,999,999.99'
*!*	           @ nLin , 134 Say P9  Pict '9,999,999.99'
*!*	           @ nLin , 146 Say P10 Pict '9,999,999.99'
*!*	           @ nLin , 158 Say P12 Pict '9,999,999.99'
*!*	           @ nLin , 170 Say P13 Pict '9,999,999.99'
*!*	           @ nLin , 182 Say P14 Pict '9,999,999.99'
*!*	           @ nLin , 194 Say P16 Pict '9,999,999.99'
*!*	           @ nLin , 206 Say P17 Pict '9,999,999.99' */

*!*	           nLin++

*!*	           DbSkip()

*!*	        EndDo

*!*	/*        Sum P1,P2,P4,P6,P7,P8 to nP1,nP2,nP4,nP6,nP7,nP8
*!*	        Sum P9,P10,P12,P13,P14 to nP9,nP10,nP12,nP13,nP14
*!*	        Sum P16,P17 to nP16,nP17

*!*	        @ nLin , 057  Say nP1  Pict '9,999,999.99'
*!*	        @ nLin , 070  Say nP2  Pict '9,999,999.99'
*!*	        @ nLin , 083  Say nP4  Pict '9,999,999.99'
*!*	        @ nLin , 096  Say nP6  Pict '9,999,999.99'
*!*	        @ nLin , 108  Say nP7  Pict '9,999,999.99'
*!*	        @ nLin , 121  Say nP8  Pict '9,999,999.99'
*!*	        @ nLin , 134  Say nP9  Pict '9,999,999.99'
*!*	        @ nLin , 146  Say nP10 Pict '9,999,999.99'
*!*	        @ nLin , 158  Say nP12 Pict '9,999,999.99'
*!*	        @ nLin , 170  Say nP13 Pict '9,999,999.99'
*!*	        @ nLin , 182  Say nP14 Pict '9,999,999.99'
*!*	        @ nLin , 194  Say nP16 Pict '9,999,999.99'
*!*	        @ nLin , 206  Say nP17 Pict '9,999,999.99' */

*!*	        DbGoTop()

*!*	        nLin = 99

*!*	        While !Eof()

*!*	           If nLin > 57

*!*	             @ 01 , 000 Say 'SECRETARIA DE ADMINISTRACION - OFICINA DE PENSIONES'
*!*	             @ 02 , 000 Say 'PROYECTO: 4F1000DDAD101A01901'
*!*	             @ 03 , 000 Say 'LISTADO ALFABETICO DEL PERSONAL CORRESPONDIENTE ' + AllTrim(cCorres)
*!*	             @ 05 , 000 Say 'NOMBRE'
*!*	             @ 05 , 031 Say 'R.F.C.'
*!*	             @ 05 , 045 Say 'T/E CATEG.'
*!*	             @ 05 , 062 Say 'P20'
*!*	             @ 05 , 075 Say 'P21'
*!*	             @ 05 , 088 Say 'P23'
*!*	             @ 05 , 101 Say 'P25'
*!*	             @ 05 , 113 Say 'P26'
*!*	             @ 05 , 126 Say 'P27'
*!*	             @ 05 , 139 Say 'P28'
*!*	             @ 05 , 151 Say 'P29'
*!*	             @ 05 , 163 Say 'P30'
*!*	             @ 05 , 175 Say 'P32'
*!*	             @ 05 , 187 Say 'P39'
*!*	             @ 05 , 199 Say 'P40'
*!*	             nLin = 7
*!*	           EndIf

*!*	/*           @ nLin , 000 Say substr(SiNom,1,30)
*!*	           @ nLin , 031 Say SiRfc
*!*	           @ nLin , 045 Say Tip_Emp
*!*	           @ nLin , 050 Say SiCatg
*!*	           @ nLin , 057 Say P20 Pict '9,999,999.99'
*!*	           @ nLin , 070 Say P21 Pict '9,999,999.99'
*!*	           @ nLin , 083 Say P23 Pict '9,999,999.99'
*!*	           @ nLin , 096 Say P25 Pict '9,999,999.99'
*!*	           @ nLin , 108 Say P26 Pict '9,999,999.99'
*!*	           @ nLin , 121 Say P27 Pict '9,999,999.99'
*!*	           @ nLin , 134 Say P28 Pict '9,999,999.99'
*!*	           @ nLin , 146 Say P29 Pict '9,999,999.99'
*!*	           @ nLin , 158 Say P30 Pict '9,999,999.99'
*!*	           @ nLin , 170 Say P32 Pict '9,999,999.99'
*!*	           @ nLin , 182 Say P39 Pict '9,999,999.99'
*!*	           @ nLin , 194 Say P40 Pict '9,999,999.99' */

*!*	           nLin++

*!*	           DbSkip()

*!*	        EndDo

*!*	/*        Sum P20,P21,P23,P25,P26 to nP20,nP21,nP23,nP25,nP26
*!*	        Sum P27,P28,P29,P30,P32 to nP27,nP28,nP29,nP30,nP32
*!*	        Sum P39,P40 to nP39,nP40

*!*	        @ nLin , 057  Say nP20 Pict '9,999,999.99'
*!*	        @ nLin , 070  Say nP21 Pict '9,999,999.99'
*!*	        @ nLin , 083  Say nP23 Pict '9,999,999.99'
*!*	        @ nLin , 096  Say nP25 Pict '9,999,999.99'
*!*	        @ nLin , 108  Say nP26 Pict '9,999,999.99'
*!*	        @ nLin , 121  Say nP27 Pict '9,999,999.99'
*!*	        @ nLin , 134  Say nP28 Pict '9,999,999.99'
*!*	        @ nLin , 146  Say nP29 Pict '9,999,999.99'
*!*	        @ nLin , 158  Say nP30 Pict '9,999,999.99'
*!*	        @ nLin , 170  Say nP32 Pict '9,999,999.99'
*!*	        @ nLin , 182  Say nP39 Pict '9,999,999.99'
*!*	        @ nLin , 194  Say nP40 Pict '9,999,999.99' */

*!*	        DbGoTop()

*!*	        nLin = 99

*!*	        While !Eof()

*!*	           If nLin > 57

*!*	             @ 01 , 000 Say 'SECRETARIA DE ADMINISTRACION - OFICINA DE PENSIONES'
*!*	             @ 02 , 000 Say 'PROYECTO: 4F1000DDAD101A01901'
*!*	             @ 03 , 000 Say 'LISTADO ALFABETICO DEL PERSONAL CORRESPONDIENTE ' + AllTrim(cCorres)
*!*	             @ 05 , 000 Say 'NOMBRE'
*!*	             @ 05 , 031 Say 'R.F.C.'
*!*	             @ 05 , 045 Say 'T/E CATEG.'
*!*	/*             @ 05 , 062 Say 'P32'   */
*!*	             @ 05 , 062 Say 'P43'   
*!*	             @ 05 , 075 Say 'P44'
*!*	             @ 05 , 088 Say 'P45'
*!*	             @ 05 , 101 Say 'P46'
*!*	             @ 05 , 113 Say 'P47'
*!*	             @ 05 , 126 Say 'P50'
*!*	             @ 05 , 139 Say 'P51'
*!*	             @ 05 , 151 Say 'P52'
*!*	/*             @ 05 , 163 Say 'P49'   */
*!*	             @ 05 , 163 Say 'P53'
*!*	             @ 05 , 175 Say 'P54'
*!*	             @ 05 , 187 Say 'P55'
*!*	             @ 05 , 194 Say 'P109'
*!*	             @ 05 , 206 Say 'TOTAL'
*!*	             nLin = 7
*!*	           EndIf

*!*	/*           @ nLin , 000 Say substr(SiNom,1,30)
*!*	           @ nLin , 031 Say SiRfc
*!*	           @ nLin , 045 Say Tip_Emp
*!*	           @ nLin , 050 Say SiCatg
*!*	           @ nLin , 057 Say P43 Pict '9,999,999.99'
*!*	           @ nLin , 070 Say P44 Pict '9,999,999.99'  
*!*	           @ nLin , 083 Say P45 Pict '9,999,999.99'
*!*	           @ nLin , 096 Say P46 Pict '9,999,999.99'
*!*	           @ nLin , 108 Say P47 Pict '9,999,999.99'
*!*	           @ nLin , 121 Say P50 Pict '9,999,999.99'
*!*	           @ nLin , 134 Say P51 Pict '9,999,999.99'
*!*	           @ nLin , 146 Say P52 Pict '9,999,999.99'
*!*	           @ nLin , 158 Say p53 Pict '9,999,999.99'
*!*	           @ nLin , 170 Say p54 Pict '9,999,999.99'
*!*	           @ nLin , 182 Say P55 Pict '9,999,999.99'
*!*	           @ nLin , 194 Say P109 Pict '9,999,999.99'

*!*	           nTotLin = P1+P2+P4+P6+P7+P8+P9+P10+P12+P13+P14+P16+P17+P20+P21+P23+P25+;
*!*	             P26+P27+P28+P29+P30+P32+P39+P40+P43+P44+P45+P46+P47+P50+P51+P52+P53+P54+P55+P109

*!*	           @ nLin , 206 Say nTotLin Pict '9,999,999.99'

*!*	           nGranTot = nGranTot + nTotLin */

*!*	           nLin++

*!*	           DbSkip()

*!*	        EndDo

*!*	/*        Sum P43,P44,P45,P46,P47 to nP43,nP44,nP45,nP46,nP47
*!*	        Sum P50,P51,P52,p53 to nP50,nP51,nP52,np53
*!*	        Sum p54,p55,p56 to np54,np55,np56
*!*	        @ nLin , 057  Say nP43 Pict '9,999,999.99'
*!*	        @ nLin , 070  Say nP44 Pict '9,999,999.99'
*!*	        @ nLin , 083  Say nP45 Pict '9,999,999.99'
*!*	        @ nLin , 096  Say nP46 Pict '9,999,999.99'
*!*	        @ nLin , 108  Say nP47 Pict '9,999,999.99'
*!*	        @ nLin , 121  Say nP50 Pict '9,999,999.99'
*!*	        @ nLin , 134  Say nP51 Pict '9,999,999.99'
*!*	        @ nLin , 146  Say nP52 Pict '9,999,999.99'
*!*	        @ nLin , 158  Say np53 Pict '9,999,999.99'
*!*	        @ nLin , 170  Say np54 Pict '9,999,999.99'
*!*	        @ nLin , 182  Say nP55 Pict '9,999,999.99'
*!*	        @ nLin , 194  Say nP109 Pict '9,999,999.99'
*!*	        @ nLin , 206  Say nGranTot Pict '9,999,999.99'

*!*	        Count to nEmpleados
*!*	        nLin += 2
*!*	        @ nLin, 000  Say 'TOTAL DE EMPLEADOS:'+ Str(nEmpleados) */

*!*	        Eject
*!*	        Set Printer to
*!*	        Set Printer Off
*!*	        Set Device to Screen
*!*	        set filter to
*!*	        DbCloseAll()
*!*	        Dele File BaseEsp.dbf
*!*	        Dele File BaseEsp.Ntx

*!*	        RestScreen(11,5,13,75,pPan2)

*!*	        Exit
*!*	      EndDo

*!*	      Exit
*!*	    EndDo 
*!*	  EndIf


*!*	*/*
*!*	*  HASTA ESTAS LINEAS TERMINAN LAS MODIFICACIONES
*!*	*/
*!*	*!*	  While .T.
*!*	*!*	    If Preguntar('¨ DESEA COPIAR LA INFORMACION A UN DISCO ? ')=='S'
*!*	*!*	      If Preguntar('¨ EL DISCO YA ESTA EN LA UNIDAD A: ? ')== 'S'
*!*	*!*	        If !(File('Base.Dbf') .And. File('Base1.Dbf'))
*!*	*!*	          Alerta('!!! ERROR, EL LISTADO ALFABETICO NO HA SIDO GENERADO...')
*!*	*!*	          Exit
*!*	*!*	        Else
*!*	*!*	          pPan2=SaveScreen(11,5,13,75)
*!*	*!*	          Cuadro(11,5,13,75,'R')
*!*	*!*	          @12,6 Say PadC('ESPERE UN MOMENTO, COPIANDO LA INFORMACION ',68) Color 'W*/R'

*!*	*!*	          Copy File Base.Dbf To C:\Base.Dbf
*!*	*!*	/*
*!*	*!*	          Copy File Base1.Dbf To A:Base1.Dbf

*!*	*!*	          LINEA DESHABILITADA PORQUE LA INFORMACION DE MANDOS MEDIOS
*!*	*!*	          Y SUPERIORES DEBE IR EN LA MISMA BASE DE DATOS
*!*	*!*	                                                            BASE.DBF
*!*	*!*	          MODIFICO: L.I. COSME OSORIO SANTOS.
*!*	*!*	*/

*!*	*!*	          RestScreen(11,5,13,75, pPan2)
*!*	*!*	        EndIf
*!*	*!*	      Else
*!*	*!*	        Loop
*!*	*!*	      EndIf
*!*	*!*	    Else
*!*	*!*	      Exit
*!*	*!*	    EndIf
*!*	*!*	*    Exit
*!*	*!*	*  EndDo
SELECT Base
nTmp=crutades+"BASE" + RIGHT(nAnio,2) + nMes
COPY TO &nTmp TYPE Fox2x
CLOSE TABLES All
Return
