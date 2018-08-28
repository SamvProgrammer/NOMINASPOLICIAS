SELECT JPP,NUM,rfc,nombre,fnacimien,fching,YEAR(fching)-YEAR(fnacimien) as jubila,;
	YEAR(DATE())-YEAR(fnacimien) as edad, ANIOS, MESES, QUINCE, APORTA,;
	IIF(sexo='1','M','F') as Sexo FROM data\tablas\maestro WHERE jpp<>'PEA' ORDER BY 1,2
