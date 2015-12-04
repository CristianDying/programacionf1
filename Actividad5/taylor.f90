program taylorprogram
  IMPLICIT NONE

  INTERFACE
     FUNCTION SerieTaylor(f_0,f_1,f_2,h)
       REAL :: SerieTaylor
       REAL, INTENT(IN) :: f_0, f_1, f_2, h
     END FUNCTION SerieTaylor
  END INTERFACE
  REAL :: x_0, x_1, x_2, h, suma, F
  INTEGER :: i, npts, icaso
  
  write(*,*) "Ingresa el numero correspondiente para el caso deseado caso seno: 1 ,coseno: 2 ,tangente: 3 ,exponencial: 4."
  read(*,*) icaso
  write(*,*) icaso

  
  npts=21
     write(*,*) "Serie de Taylor del Caso Seleccionado Anteriormente"
     h=0.1
     do i=1, npts
        x_0=float(i-1)*h
        x_1=float(i)*h
        x_2=float(i+1)*h
       suma =SerieTaylor(F(x_0,icaso), F(x_1,icaso), F(x_2,icaso), h)
       write(*,*) i, suma, F(x_1,icaso), (suma-F(x_1,icaso))
      
          
       open(unit=12, file='Error.dat')
       write(12,*)i, suma-F(x_1,icaso)
    
    
    

      
     end do
    close(12)

   END PROGRAM taylorprogram
   
   FUNCTION SerieTaylor(f_0,f_1,f_2,h)
     IMPLICIT NONE
     REAL :: SerieTaylor
     REAL, INTENT(IN) :: f_0, f_1, f_2, h

     SerieTaylor=f_1+((f_2-f_0)/(2*h))*h+((f_2-2*f_1+f_0)/(2*h*h))*h*h
     END FUNCTION SerieTaylor

     FUNCTION F(x,icaso)
       IMPLICIT NONE
       REAL :: F
       REAL, INTENT(IN) ::  x
       INTEGER, INTENT(IN) :: icaso
       if (icaso.EQ.1) then
          F=sin(x)
       end if
       
       if (icaso.EQ.2) then
          F=cos(x)
       end if

       if (icaso.EQ.3) then
          F=tan(x)
       end if

       if (icaso.EQ.4) then
          F=exp(x)
       end if
       
       
       
     END FUNCTION F
     
