********************************
*procedure IMPRIMEBAN
LPARAMETERS mes,nNomi
*local nmonto= 0, nmonto1= 0, liquido= 0, liquidot= 0, l= 0, ;
      clin, busca, nnum, nrfc, nnom, annio= 0, nmontop= 0, ;
      nmontod= 0, liquidop= 0, liquidotp= 0, empleadop= 0

local nmonto, nmonto1, liquido, liquidot, l,clin, busca, nnum, nrfc, nnom, annio, nmontop, nmontod, liquidop, liquidotp, empleadop

nmonto= 0
nmonto1= 0
liquido= 0
liquidot= 0
l= 0
annio= 0
nmontop= 0
nmontod= 0
liquidop= 0
liquidotp= 0
empleadop= 0


DO CASE 
      case nopc == 1
         tipo= "02884"
         tipo1= "JU"
         archivo= "NI0288401.PAG"
*         setprc(0, 0)
*         imprimeban()
      case nopc == 2
         tipo= "20022"
         tipo1= "PD"
         archivo= "NI2002201.PAG"
*         setprc(0, 0)
*         imprimeban()
      case nopc == 3
         tipo= "19964"
         tipo1= "PT"
         archivo= "NI1996401.PAG"
*         setprc(0, 0)
*         imprimeban()
ENDCASE 
*!*	   ppan2= SaveScreen(8, 8, 15, 40)
*!*	   cuadro(8, 8, 15, 40, "B")
*!*	   SetColor("W+/B", "W+/B")
*!*	   @ 10, 10 say "FECHA DE APLICACION " get MES picture "99999999"
*!*	   @ 12, 10 say [FORMATO "AAAAMMDD"]
*!*	   read
   select MAESTRO
   index on JPP+str(num,6) to maes
   set filter to JPP=TIPO1 .AND. NOMELEC="S" .AND. superviven="S" .AND. CUENTABANC<>0
   select MAESTRO
   goto top
   do while (!EOF())
      busca= jpp + Str(num, 6)
      select NOMINEW
      seek busca
      if (Found())
         do while (busca = jpp + Str(numjpp, 6) .AND. !EOF())
            if (clave < 61)
               nmontop= nmontop + monto
            ELSE
            	if (clave > 61)
	               	nmontod= nmontod + monto
	            endif
            endif
            skip 
         enddo
      endif
      liquidop= nmontop - nmontod
      liquidotp= liquidotp + liquidop
*      liqui= strzero(liquidotp, 16, 2)
      liqui= str(liquidotp, 16, 2)
      liqui1= SubStr(liqui, 1, 13) + SubStr(liqui, 15, 2)
      empleadop=empleadop+1
      liquidop= 0
      nmontop= 0
      nmontod= 0
      select MAESTRO
      skip 
   enddo
   b= 1
   select MAESTRO
   goto top
   set printer to (archivo)
   set device to printer
   set printer on
   clin= 1
   do while (!EOF())
      busca= jpp + Str(num, 6)
      nnum= num
      ncuentas= cuentabanc
      select NOMINEW
      seek busca
      if (Found())
         do while (busca = jpp + Str(numjpp, 6) .AND. !EOF())
            if (clave < 61)
               nmonto= nmonto + monto
            ELSE
            	if (clave > 61)
               		nmonto1= nmonto1 + monto
            	ENDIF
            endif
            skip 
         enddo
      endif
      liquido= nmonto - nmonto1
*      liqui2= strzero(liquido, 16, 2)
      liqui2= str(liquido, 16, 2)
      liqui3= SubStr(liqui2, 1, 13) + SubStr(liqui2, 15, 2)
      if (b == 1)
         @  0,  0 say "H"
         @  0, PCol() say "NE"
         @  0, PCol() say tipo
         @  0, PCol() say mes
         @  0, PCol() say "01"
*         @  0, PCol() say strzero(empleadop, 6)
         @  0, PCol() say str(empleadop, 6)
         @  0, PCol() say liqui1
         @  0, PCol() say "000000"
         @  0, PCol() say "000000000000000"
         @  0, PCol() say "000000"
         @  0, PCol() say "000000000000000"
         @  0, PCol() say "000000"
         @  0, PCol() say "0"
         @  0, PCol() say Space(77)
         b= 0
      endif
      @ clin,  0 say "D" picture "@�"
      @ clin, PCol() say mes
*      @ clin, PCol() say strzero(nnum, 10)
      @ clin, PCol() say str(nnum, 10)      
      @ clin, PCol() say Space(40)
      @ clin, PCol() say Space(40)
      @ clin, PCol() say liqui3
      @ clin, PCol() say "072"
      @ clin, PCol() say "01"
*      @ clin, PCol() say strzero(ncuentas)
      @ clin, PCol() say str(ncuentas)      
      @ clin, PCol() say "0"
      @ clin, PCol() say Space(1)
      @ clin, PCol() say "00000000"
      @ clin, PCol() say Space(18)
      liquido= 0
      nmonto= 0
      nmonto1= 0
      select MAESTRO
      clin= clin + 1
      skip 
   enddo
   set printer to 
   set printer off
   set device to screen
*   RestScreen(8, 8, 15, 40, ppan2)
