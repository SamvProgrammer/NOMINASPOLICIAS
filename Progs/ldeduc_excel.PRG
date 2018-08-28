WAIT WINDOWS 'Generando Ldeduc para excel...' NoWait
SET DEFAULT TO F:\Trabajos\PENSIONES\Nomina\NOMINA
CLOSE TABLES all
CREATE TABLE temp\ldeduc FREE ;
      	(CVEDESC N(3), DESCRIP C(30), JPP C(3), NUM N(6), RFC C(13), NOMBRE C(40),;
         NUMDESC N(3), TOTDESC N(3), FOLIO N(10), IMPORTE N(9,2), TIPODESC C(1))
	USE data\tablas\perded ORDER clave IN 0
	USE data\tablas\nominew ALIAS nomina IN 0
	USE data\tablas\maestro ALIAS maestro IN 0
	SELECT maestro
    set filter to superviven="S"
	INDEX on Jpp+Str(Num,6) TO temp\maestro
	SELECT nomina
	index on Str(Clave,3)+ Jpp+ Str(NumJpp,6) to temp\DNomiCve
	set relation to STR(Clave,3) into PerDed, Jpp+Str(NumJpp,6) into  maestro
      for j= 1 to 2
         for i= 1 to 2
            select  nomina
            if (i == 1)
               if (j == 1)
                  set filter to (Jpp== 'JUP' .OR. Jpp== 'PDP' .OR. ;
                     Jpp== 'PTP') .AND. maestro .NomElec== "N" .AND. ;
                     maestro .SUPERVIVEN="S"
               else
                  set filter to (Jpp== 'JUP' .OR. Jpp== 'PDP' .OR. ;
                     Jpp== 'PTP') .AND. maestro .NomElec<> "N".AND. ;
                     maestro .SUPERVIVEN="S"
               endif
            ELSE
            	if (j == 1)
    	           set filter to (Jpp== "JUF" .OR. Jpp== "PDF" .OR. ;
        	          Jpp== "PTF") .AND. maestro .NomElec== "N" .AND. ;
            	      maestro .SUPERVIVEN="S"
	            else
         	       set filter to (Jpp== "JUF" .OR. Jpp== "PDF" .OR. ;
            	      Jpp== "PTF") .AND. maestro .NomElec<> "N" .AND. ;
                	  maestro .SUPERVIVEN="S"
               	endif
            endif
            goto top
            locate for clave >= 50
            do while (!EOF())
               select  nomina
               nclave= clave
               ntotcve= 0
               nempcve= 0
               nlin= 99
               do while (nclave == clave)
                  cjpp= jpp
                  ntotjpp= 0
                  nempjpp= 0
                  do while (cjpp == jpp .AND. nclave == clave)
                     SELECT ldeduc
		             append blank
        		     replace cvedesc WITH nomina.clave
        		     replace descrip WITH perded.descri
		             replace jpp WITH nomina.jpp
		             replace num WITH nomina.numjpp
		             replace rfc WITH maestro.rfc
        		     replace nombre with maestro.NOMBRE
        		     replace numdesc WITH nomina.pago4
        		     replace totdesc WITH nomina.pagot
        		     replace folio WITH nomina.folio
        		     replace importe WITH nomina.monto
        		     replace tipodesc WITH nomina.tipopago
					 SELECT nomina					 
                     Skip
                  enddo
               enddo
            enddo
         next
      NEXT
      SELECT ldeduc
      COPY TO temp\ldeduc TYPE xls
      CLOSE TABLES All
MESSAGEBOX('PROCESO TERMINADO...')