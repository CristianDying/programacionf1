program sinfunct

implicit none
integer :: i, npts
real :: x, f_x,f__x,dx, sintaylor
real, parameter :: pi = 4.0 * atan (1.0)
real, parameter :: epsilon = 1.0E-6
 

  print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)
   write(*,*) 'dx= ', dx
 

  x = 0.0

  open(unit=11, file='seno.dat')

  do i = 1, npts+1, 1 

     x = dx * float(i-1)

     f_x = sin(x)

     write(11,*)  x , f_x
     if(abs(f_x) .le. epsilon) write(*,*) 'x=', x, 'es un cero de la funcion'
     
  enddo
  close(11)
  
  open(unit=11, file='cos.dat')
  
  do i = 1, npts+1, 1
     
     x = dx * float(i-1)
     
     f__x = cos(x)
     
     write(11,*) x , f__x
     if(abs(f__x) .le. epsilon) write(*,*) 'x=', x, 'es un cero de la derivada'
    
  enddo
  close(11)

 

  do  i = 1, 1, 1
     
     x = pi / 8.0
     
    
     
     sintaylor = x - (x**3.0/(3.0*2.0))
                                         
    write(*,*)  'porcentaje de error con respecto a Taylor',(1 - (sintaylor / sin(x)))*100 , '%'
           
  enddo
        
end program sinfunct
