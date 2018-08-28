LPARAMETERS dfechaini,dfechafin,nEmp,nTipo,snLogo,sImp
local ppan, aempleado, cjpp, nnumjpp, crfc, nSobres
local cnombre, apercep, adeduc, ntotaltot, ntotalsob, asobres, aStruc, nTmp
DECLARE adeduc[50,4], apercep[50,3]
PUBLIC ARRAY amonedas[15,2]
nTmp='_Sob'+RIGHT(SYS(2015),5)

nSobres=''
*CLOSE TABLES all
aStruc= '(NOJUB   C( 9, 0), REPETIR  N( 1, 0),NOMBRE   C(40, 0), PERIODO  C( 30, 0), TPER C( 30, 0), FORMAP  C( 20, 0), CUENTABANC C( 25, 0),'+;
		' CURP    C(21, 0), RFC      C(13, 0), IMSS     C( 19, 0), MODALI  C( 40, 0),'+;
		' CLAVE1  N( 3, 0), DESCRIP1 C(25, 0), IMPORTE1 N( 12, 2),'+;
		' CLAVE2  N( 3, 0), DESCRIP2 C(30, 0), IMPORTE2 N( 12, 2), SERIE C( 15, 0), FPAGO D( 8, 0))'
Sobres=nTmp
nTmp='Temp\'+nTmp
CREATE TABLE &nTmp FREE &aStruc
*CLOSE TABLES all
*USE &nTmp ALIAS (Sobres)
IF !USED('perded')
	USE data\tablas\perded ORDER clave IN 0
ENDIF 
IF !USED('nominew')
*	USE data\tablas\nominew ORDER nomina ALIAS NomiNew IN 0
	nRut=RutaNomina+'nominew'
	USE &nRut ALIAS NomiNew IN 0
*	nTmp=nTmp+'1'

	SELECT nominew
	INDEX on jpp+STR(numjpp,6)+STR(clave,3)+STR(secuen,1) TO &nTmp
	
ENDIF 
IF !USED('MaestroNomi')
	nRut=RutaNomina+'maestro'
	USE &nRut ALIAS MaestroNomi IN 0
*	USE data\tablas\maestro order nomina IN 0
*!*		nRut=RutaNomina+'maestro'
*!*		USE nRut ALIAS maestro2 IN 0
*!*		nTmp='Temp\'+nTmp+'2'
*!*		SELECT maestro
*!*		INDEX on jpp+STR(num,6) TO $nTmp
endif

select NomiNew
set relation to STR(Clave,3) into PerDed
*!*	   dfechaini= CTOD('01/03/2016')
*!*	   dfechafin= CTOD('31/03/2016')
       DECLARE asobres[3, 5]
      asobres[1,1]=" (Jpp== 'JUP' .Or. Jpp== 'PDP' .Or. Jpp== 'PTP' ) "
      asobres[1,2]="Listados\SobJUB.Txt"
      asobres[1,3]=0
      asobres[1,4]=0
      asobres[1,5]="JUBILADOS CENTRO"

      asobres[2,1]=" (Jpp== 'JUF' .Or. Jpp== 'PDF' .Or. Jpp== 'PTF') .And. NomElec=='S' "
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
         select MaestroNomi
         IF !EMPTY(nEmp)
			 SET FILTER TO JPP=nTipo AND INLIST(NUM,&nEmp) AND superviven='S'
		 ELSE
	         SET FILTER TO &cfiltro AND superviven='S'
		 ENDIF
         goto top
       	 IF sImp=0
         	IF FIELD('F_Sobres')='F_SOBRES'
	        	IF !EMPTY(F_Sobres)
	        	   MESSAGEBOX('Este Sobre ya se imprimio el '+TTOC(F_Sobres)+'.',0+16,'Aviso...')
				   select MaestroNomi
				   use
				   select NomiNew
				   USE
				   SET DATE BRITISH
				   RETURN 
	        	ENDIF
	        ENDIF
         ENDIF

		 nTantos=0
         do while (!EOF())
         	nSobres=nSobres+CHR(13)+STR(num,10)+' '+nombre
            cjpp= jpp
            nnumjpp= num
            cnombre= nombre
            crfc= rfc
            ctipoemp1= LEFT(MaestroNomi.categ,20)
            ctipoemp2= LEFT(MaestroNomi.categ,20)
            ndeduc= 0
            npercep= 0
		    FOR xy=1 TO 50
			   adeduc[xy,1]=0
			   adeduc[xy,2]=0
			   adeduc[xy,3]=''
			   adeduc[xy,4]=''
			   apercep[xy,1]=0
			   apercep[xy,2]=0
			   apercep[xy,3]=''
		    ENDFOR 

            select NomiNew
            if !Seek(cjpp + Str(nnumjpp,6))
               select MaestroNomi
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
                     apercep[xy,1]=clave
                     apercep[xy,2]=monto
                     apercep[xy,3]=perded.descri
                  else
                  	 FOR xy=1 TO 50
                  	 	IF EMPTY(adeduc[xy,1])
                  	 		exit
						ENDIF 
                  	 ENDFOR 
                     ndeduc= ndeduc + monto
                     adeduc[xy,1]=clave
                     adeduc[xy,2]=monto
                     adeduc[xy,3]=perded.descri
                     adeduc[xy,4]=iif(pago1 == 0, "  ", Str(pago1, 3) + "->") +;
						iif(pago2 == 0, "  ", Str(pago2, 3) + "->") +;
						iif(pago3 == 0, "  ", Str(pago3, 3) + "->") +;
						iif(pago4 == 0, "  ", Str(pago4, 3) + "/") +;
						iif(pagot == 0, "  ", Str(pagot, 3))                     
                  endif
               endif
               Skip
            enddo
            nmayor=0
            FOR  i= 1 to 50
            	IF !EMPTY(apercep[i,1]) OR !EMPTY(adeduc[i,1])
	            	nmayor=i
            	endif
			ENDFOR
			nmayor=IIF(nmayor=1,2,nmayor)
			SELECT (Sobres)
			FOR xy=1 TO 2
	            for i= 1 to nmayor
	               APPEND BLANK
	               REPLACE nojub WITH MaestroNomi.jpp+STR(MaestroNomi.num,6), nombre WITH MaestroNomi.nombre
	               replace periodo WITH fnMes(month(dfechafin))+" de "+STR(year(dfechaini),4)
*!*		               replace periodo WITH IIF(DAY(dfechaini)<10,'0','')+ALLTRIM(STR(DAY(dfechaini)))+" al "+;
*!*		               						IIF(DAY(dfechafin)<10,'0','')+ALLTRIM(STR(DAY(dfechafin)))+" de "+fnMes(month(dfechafin))+" de "+STR(year(dfechaini),4)
				   replace tper WITH 'MENSUAL'
				   replace cuentabanc WITH maestronomi.cuentabanc
	               replace formap WITH IIF(MaestroNomi.nomelec='S','ELECTRONICO','EFECTIVO')
	               REPLACE curp WITH MaestroNomi.curp, rfc WITH MaestroNomi.rfc, imss WITH MaestroNomi.imss, modali WITH MaestroNomi.categ
				   replace clave1 WITH apercep[i,1], importe1 WITH apercep[i,2], descrip1 WITH apercep[i,3]
				   replace clave2 WITH adeduc[i,1],  importe2 WITH adeduc[i,2],  descrip2 WITH adeduc[i,3],SERIE WITH adeduc[i,4]
				   REPLACE FPAGO WITH dfechafin,repetir WITH xy
	            NEXT
	        NEXT
            select MaestroNomi
           	IF sImp=0
	            IF FIELD('F_Sobres')='F_SOBRES'
	            	REPLACE F_Sobres WITH DATETIME()
	            ENDIF
	        ENDIF 
            SKIP
         enddo
      next
   select MaestroNomi
   use
*   set relation to
   select NomiNew
   USE
*   set relation to
   SET DATE BRITISH
   SELECT (sobres)
   COUNT TO ImpreSobre

   IF ImpreSobre>0
	   IF snLogo=1
		   Do Form Forms\Imprimir With 'Reports\Sobres_Laser_s_logo.frx',""
	   ELSE
		   Do Form Forms\Imprimir With 'Reports\Sobres_Laser_c_logo.frx',""
	   ENDIF
	ELSE
		MESSAGEBOX('Lo siento no existe el sobre del mes indicado.',0+16,'Aviso...')
	ENDIF
   SELECT (sobres)
   use
RETURN
