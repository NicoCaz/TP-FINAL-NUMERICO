!###########################################################################################
!CANTIDAD DE FUERZAS PUNTUALES Y DISTRIBUIDAS SE INGRESA POR TECLADO EN PROGRAMA
!FUERZAS PUNTUALES SE INGRESAN POR TECLADO EN PROGRAMA (POSICIÓN, ÁNGULO, VALOR)
!FUERZAS DISTRIBUIDAS SE INGRESAN POR TECLADO EN PROGRAMA (POSICIÓN, ÁNGULO, ALCANCE, VALOR)
!MOMENTOS SE INGRESAN POR TECLADO EN PROGRAMA (POSICIÓN, SENTIDO, VALOR)
!ÁNGULO = SENTIDO ANTIHORARIO 
!PREGUNTA DISTANCIA APOYOS
!PREGUNTA LARGO DE LA VIGA
!###########################################################################################

PROGRAM TPFinal
	
	INTEGER(8),PARAMETER:: cantidadF=1						!cantidadF=Fuerzas puntuales
	INTEGER(8),PARAMETER:: cantidadFD=1						!cantidadFD=Fuerzas distribuidas					
	INTEGER, PARAMETER :: cantEc=2
	INTEGER(8) :: i	
	REAL(8),PARAMETER:: pi=3.14159265359													
	REAL(8) :: L, opcion,ModuloE=1700000000,INERCIA=0.000520,h=0.01		    !L=Largo viga (distancias)
	REAL(8) :: M(cantidadF,3)													!M=Matriz esfuerzos (posición respecto a extremo, ángulo, valor)
	REAL(8) :: MD(cantidadFD,3)													!MD=Matriz fuerzas distribuidas (posición respecto a extremo, ángulo, alcance, valor)
	REAL(8) ::fxA,fyA,MzA,P	
	CHARACTER(13) formato
	REAL(8),DIMENSION(0:cantEc)::v,e						
	!CONDICIONES INICIALES												####NO TOCAR####
	fxA=0	!Fuerza horizontal apoyo A
	fyA=0	!Fuerza vertical apoyo A
	MzA=0	!Momento apoyo A
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	v(0)=0 !x
	v(1)=0 !y
	v(2)=0 !y prima
	!###################################################################
	!INGRESO DE ESFUERZOS POR TECLADO
	!###################################################################
	
	!CARGAS PUNTUALES
	!Esfuerzo 1
	M(1,1)=	5											    !M(i,1)=Posición esfuerzo i										
	M(1,2)=-pi/2.													!M(i,2)=Ángulo esfuerzo i en radiantes
	M(1,3)=2669.84												!M(i,3)=Valor esfuerzo i
!Esfuerzo 2
	!M(2,1)=5
	!M(2,2)=-pi/2.
	!M(2,3)=750
!Esfuerzo 3
	!M(3,1)=7
	!M(3,2)=-pi/2.
	!M(3,3)=2000
!Esfuerzo 4
	!M(4,1)=10
	!M(4,2)=PI/2.
	!M(4,3)=1500
	!PESO DE LA VIGA
	!Esfuerzo 1
		MD(1,1)=0														!MD(i,1)=Posición inicial carga distribuida i 
		MD(1,2)=5														!MD(i,2)=Posición final carga distribuida i (longitud)
		MD(1,3)=-196.2												!MD(i,3)=Valor carga distribuida i															
	WRITE(*,*) 'TRABAJO FINAL ANALISIS NUMERICO: "RESOLUCION DE UN SISTEMA ISOESTATICO DE VIGA"'
	WRITE(*,*) 'PARA CALCULO DE VIGA EMPOTRADA'
	WRITE(*,*) '-------------------------------------------------------------------------------'
	WRITE(*,*) ' '
	WRITE(*,*) ' '
	WRITE(*,*) '/|'
	WRITE(*,*) '/|'
	WRITE(*,*) '/|'
	WRITE(*,*) '/|----------------------------------------'
	WRITE(*,*) '/|'
	WRITE(*,*) '/|'
	WRITE(*,*) '/|'
	WRITE(*,*) ' '
	WRITE(*,*) '-------------------------------------------------------------------------------'
	WRITE(*,*) 'Ingrese largo de la viga (m)'
	READ(*,*)  L
	WRITE(*,*) '-------------------------------------------------------------------------------'
	WRITE(*,*) 'Seleccione las siguientes opciones'
	WRITE(*,*) '(1): Peso de la viga despreciable'
	WRITE(*,*) '(2): Peso de la viga no despreciable'												
	CALL calculoEmpotramiento(M,MD,fxA,fyA,MzA,P,L)	
	WRITE(formato,'(A1,I1,A11)') '(', cantEc+1,'(F12.6,2X))'
	OPEN(2,FILE='datos.dat',STATUS='REPLACE')
	WRITE(*,'(A/)')'Que metodo desea utilizar: '
	WRITE(*,*)'1) Euler Simple.'
	WRITE(*,*)'2) Euler Modificado.'
	WRITE(*,*)'3) Runge Kutta Merson (4to orden).'
	WRITE(*,*)'4) Runge Kutta Fehlberg (6to orden).'
	READ(*,*)metodo	
	DO WHILE (v(0)<=L)	
	CALL GrabaElastica(v,formato)			
		SELECT CASE (metodo)
			CASE(1)
				CALL EulerSimple(v,h)
			CASE(2)
				CALL EulerModificado(v,h)
			CASE(3)
				Call RungeKuttaMerson(v,h)
			CASE(4)
				CALL RungeKuttaFehlberg(v,h,e)
		END SELECT	
	END DO
	CLOSE(UNIT=2, STATUS='KEEP')
	CALL SYSTEM("gnuplot -persist script.p")
	
!#######################################################################				
CONTAINS
	SUBROUTINE EulerSimple(v, h)
		REAL(8), DIMENSION (0:cantEc) :: v
		REAL(8) ::h
		v = v+h*v_prima(v)
	END SUBROUTINE EulerSimple
!#######################################################################
	SUBROUTINE EulerModificado(v, h)
		REAL(8), DIMENSION (0:cantEc) :: v, vp
		REAL(8) h
		vp=h*v_prima(v)
		v=v+h*(v_prima(v)+v_prima(v+vp))/2.0
	END SUBROUTINE
!#######################################################################	
	SUBROUTINE RungeKuttaMerson(v, h)
		REAL(8), DIMENSION (0:cantEc) :: v, k1, k2, k3, k4
		REAL(8) h
		k1=h*v_prima(v)
		k2=h*v_prima(v+k1/2.0)
		k3=h*v_prima(v+k2/2.0)
		k4=h*v_prima(v+k3)
		v=v+(k1+2.0*k2+2.0*k3+k4)/6.0
	END SUBROUTINE RungeKuttaMerson
!#######################################################################	
	SUBROUTINE RungeKuttaFehlberg(v, h, e)
		REAL(8), DIMENSION (0:cantEc) :: v, k1, k2, k3, k4, k5, k6, e
		REAL(8) h
		k1=h*v_prima(v)
		k2=h*v_prima(v+k1/4.0)
		k3=h*v_prima(v+(3*k1/32.0)+(9*k2/32.0))
		k4=h*v_prima(v+(1932*k1/2197.0)-(7200*k2/2197.0)+(7296*k3/2197.0))
		k5=h*v_prima(v+(439*k1/216.0)-(8.0*k2)+(3680*k3/513.0)-(845*k4/4104.0))
		k6=h*v_prima(v-(8*k1/27.0)+(2.0*k2)-(3544*k3/2565.0)+(1859*k4/4104.0)-(11*k5/40.0))
		v=v+((25*k1/216.0)+(1408*k3/2565.0)+(2197*k4/4104.0)-(0.2*k5))
		e=k1/360.0-128*k3/4275.0-2197*k4/75240.0+0.02*k5+2*k6/55.0
	END SUBROUTINE RungeKuttaFehlberg

SUBROUTINE GrabaElastica(v,formato)
		REAL(8), DIMENSION(0:cantEc) :: v
		CHARACTER(13) formato
		WRITE(2,formato)v
	END SUBROUTINE GrabaElastica
	
!#######################################################################	
	FUNCTION v_prima(v)
		REAL(8),DIMENSION(0:cantEc)::v_prima
		REAL(8),DIMENSION(0:cantEc)::v
		REAL(8)::TRAMO=0
		CALL creaTramo(M,MD,v(0),TRAMO) 
		v_prima(0)=1.0
		v_prima(1)=v(2)
		v_prima(2)=((-Mza +fya*v(0)+TRAMO)/(ModuloE*INERCIA))*((1+(v(2)**2))**(3/2.)) 				  	
	END FUNCTION

!#######################################################################	
!M(i,2)=Ángulo esfuerzo i en radiantes
!M(i,1)=Posición esfuerzo i	
!M(i,3)=Valor esfuerzo i
	SUBROUTINE creaTramo(M,MD,V0,TRAMO)
			REAL(8) ::M(cantidadF,3)
			REAL(8)	::MD(cantidadFD,3)
			REAL(8) ::V0
			REAL(8) ::TRAMO	
			INTEGER(8)::I
			I=cantidadF	
			TRAMO=0	
			IF (I>0 )THEN
				DO WHILE (M(I,1)>=V0)
					I=I-1
				END DO
				DO WHILE(I>0)			
					TRAMO=TRAMO+M(I,3)*sin(M(I,2))*(V0-M(I,1))
					I=I-1
				END DO
			END IF					
			!resuelve momentos distribuidos			
			!#######################################################################
			I=cantidadFD
			IF (I>0 )THEN
				DO WHILE(MD(I,1)>=V0)
					I=I-1
				END DO
				IF (MD(I,2)>V0) THEN !V0 FUERA DE LA ZONA DE LA CARGA DISTRIBUIDA
					TRAMO=TRAMO+(V0-((V0-MD(I,1))/2.)+MD(I,1))*((V0-MD(I,1))*MD(I,3))
				ELSE!V0 DENTRO DE LA ZONA DE LA CARGA DISTRIBUIDA
					TRAMO=TRAMO+(V0-(((MD(I,2)-MD(I,1))/2.)+MD(I,1)))*((MD(I,2)-MD(I,1))*MD(I,3))
				END IF
				I=I-1
				DO WHILE(I>0)	
					TRAMO=TRAMO+MD(I,3)*(MD(I,2)-MD(I,1))*(V0-(((V0-MD(I,1))/2.)+MD(I,1)) )
					I=I-1
				END DO	
			END IF	
			!#######################################################################			
	END SUBROUTINE 
!#######################################################################	
	!MD(i,1)=Posición inicial carga distribuida i  
	!MD(i,2)=posicion final carga distribuida i (longitud)
	!MD(i,3)=Valor carga distribuida i
	
	SUBROUTINE calculoEmpotramiento(M,MD,fxA,fyA,MzA,P,L)	
		REAL(8) ::M(cantidadF,3)   
		REAL(8) ::MD(cantidadFD,3)										
		REAL(8) ::fxA,fyA,MzA,P,L   
		fyA=0	
		MzA=0
		DO i=1,cantidadF												!#Sumatoria cargas puntuales
			fxA=fxA+M(i,3)*(cos(M(i,2)))								!Reacciones en x (fuerza)							
			fyA=fyA+M(i,3)*(sin(M(i,2)))								!Reacciones en y (fuerza)                             #PONER PESO EN UNA SOLA ITERACIÓN
			MzA=MzA+M(i,3)*(sin(M(i,2)))*M(i,1)							!Reacciones en z (momento)
		END DO
		DO i=1,cantidadFD													!#Sumatoria cargas distribuidas
			fyA=fyA+MD(i,3)*(MD(i,2)-MD(i,1))							    !Reacciones en y (fuerza)
			MzA=MzA+ MD(i,3)*(MD(i,2)-MD(i,1))*((MD(i,2)+MD(i,1))/2.) 	!Reacciones en z (momento)      #FUERZA DISTRIBUIDA RECTANGULAR
		END DO			
		fxa=-fxa
		fya=-fya
		MzA=-MzA
		Write(*,*) '-------------------------------------------------------------------------------'
		WRITE(*,*) 'Reacciones en el empotramiento: fuerza en x, fuerza en y, momento en z'
		WRITE(*,*)'Fx =', fxA,'N'
		WRITE(*,*)'Fy =', fyA,'N'
		WRITE(*,*)'Mz =', MzA,'J'
	END SUBROUTINE
END PROGRAM TPFinal
