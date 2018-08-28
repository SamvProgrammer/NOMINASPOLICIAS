LPARAMETERS nNomi,nCve,nTP,nNe,cNomi
DECLARE atipos[10]
PUBLIC cTipo,cPago,cNomina
   atipos[1]= 'JUP'
   atipos[2]= 'PDP'
   atipos[3]= 'PTP'
   atipos[4]= "JUF"
   atipos[5]= "PDF"
   atipos[6]= "PTF"
   atipos[7]= "PEA"
   atipos[8]= "PEF"
   
   xtipo= atipos[nNomi]

cTipo=cNomi
cPago=nTP
cNomina=nNE


*!*	PUBLIC cTipo,cPago,cNomina
*!*	nCve=1
*!*	nTP='N'
*!*	nNE='S'
*!*	xtipo= 'JUP'
*!*	cTipo= 'JUBILADOS'
*!*	cPago='N'
*!*	cNomina='S'

cFiltro=iif(nTP = "N", "nominew.jpp = '"+xTipo+"'", ;
        "Left(nominew.jpp, 2) = '"+Left(xTipo, 2)+"'")+IIF(!EMPTY(nCve)," AND nominew.clave = "+;
        ALLTRIM(STR(nCve)),'')+" AND nominew.tipopago = '"+nTP+"' AND nominew.nomelec = '"+nNE+"' and maestro.superviven='S'"
*MESSAGEBOX('Filtro...:'+cFiltro)
*Return
SELECT nominew.jpp,nominew.clave,nominew.numjpp,nominew.secuen,catjpp.descrip as nomina,maestro.nombre as nombre,;
	perded.descri as descrip,nominew.pago4,nominew.pagot,nominew.monto FROM data\tablas\nominew ;
	LEFT JOIN data\tablas\maestro ON maestro.jpp = nominew.jpp AND maestro.num=nominew.numjpp;
	LEFT JOIN data\tablas\perded ON perded.clave = nominew.clave ;
	left join data\tablas\catjpp on catjpp.jpp = nominew.jpp ;
	WHERE &cFiltro;
	ORDER BY 1,2,3,4 ;
	INTO CURSOR salida
	Do Form Forms\Imprimir With 'Reports\Valida_Nomina.frx',""
