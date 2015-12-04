program sinfunct
  implicit none
  
integer :: i, npts
real :: x,f_x,g_x,h_x,i_x,j_x,k_x,l_x,m_x,n_x,o_x,p_x,q_x,sint1_x,sint2_x,sint3_x,cost1_x,cost2_x,cost3_x,tant1_x,tant2_x,tant3_x,dx
real, parameter :: pi = 4.0 * atan (1.0)
real, parameter :: epsilon = 1.0E-6
 

  print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)
   write(*,*) 'dx= ', dx
 

  x = 0.0
  open(unit=11,file='sen2cos2.dat') 
  do i = 1, npts+1, 1 

     x = dx * float(i-1)

     f_x = sin(x)*sin(x) + cos(x)*cos(x)
     write(11,*)  x , f_x  
  enddo
  close(11)
  open(unit=11,file='sinmenosx.dat')
  do i = 1, npts+1, 1 

     x = dx * float(i-1)
     g_x = sin(-x)
     write(11,*) x , g_x 
  enddo
  close(11)
  
   open(unit=11,file='menossinx.dat')
  do i = 1, npts+1, 1 

     x = dx * float(i-1)
     
     h_x = -sin(x)
     write(11,*) x , h_x
  enddo
  close(11)
  
  open(unit=11,file='cosmenosx.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
      i_x = cos(-x)
    write(11,*) x , i_x    
  enddo
  close(11)
 open(unit=11,file='cosx.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
      j_x = cos(x)
    write(11,*) x , j_x    
 enddo
 close(11)
 open(unit=11,file='sindosx.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
     k_x = sin(2*x)
     write(11,*) x, k_x
  enddo
  close(11)
  open(unit=11,file='dossincos.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
     l_x = 2*sin(x)*cos(x)
     write(11,*) x, l_x
  enddo
  close(11)
  open(unit=11,file='dostansobreblabla.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
     m_x = (2*tan(x))/(1+(tan(x))**2)
     write(11,*) x, m_x
  enddo
  close(11)
open(unit=11,file='sintresx.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
     n_x = sin(3*x)
     write(11,*) x, n_x
  enddo
  close(11)
  open(unit=11,file='sin34blabla.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
     o_x = -4 * ((sin(x))**3) + 3 * sin(x)
     write(11,*) x, o_x
  enddo
  close(11)
  open(unit=11,file='pi.dat')
  do i = 1, npts+1, 1
     x = dx * float(i-1)
     p_x = 4.0 * atan(x)
     write(11,*) x, p_x
  enddo
  close(11)
  
  open(unit=11,file='sent1.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      sint1_x = x
      write(11,*) x, sint1_x
   enddo
   close(11)
   
   open(unit=11,file='sent2.dat')
   do i=  1, npts+1, 1
      x = dx * float(i-1)
      sint2_x = x - ((x**3)/6)
      write(11,*) x, sint2_x
   enddo
   close(11)
   
   open(unit=11,file='sent3.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      sint3_x = x - ((x**3)/6) + ((x**5)/120)
      write(11,*) x, sint3_x
   enddo
   close(11)
   
   open(unit=11,file='cost1.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      cost1_x = 1
      write(11,*) x, cost1_x
   enddo
   close(11)
   
   open(unit=11,file='cost2.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      cost2_x = 1 - ((x**2)/2)
      write(11,*) x, cost2_x
   enddo
   close(11)
   
   open(unit=11,file='cost3.dat')
   do i=  1, npts+1, 1
      x = dx * float(i-1)
      cost3_x = 1 - ((x**2)/2) + ((x**4)/24)
      write(11,*) x, cost3_x
   enddo
   close(11)
   open(unit=11,file='tant1.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      tant1_x = x
      write(11,*) x, tant1_x
   enddo
   close(11)
    open(unit=11,file='tant2.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      tant2_x = x + ((x**3)/3)
      write(11,*) x, tant2_x
   enddo
   close(11)
   open(unit=11,file='tant3.dat')
   do i = 1, npts+1, 1
      x = dx * float(i-1)
      tant3_x = x + ((x**3)/3) + 2*(x**5)/15
      write(11,*) x, tant3_x
   enddo
   close(11)
end program sinfunct
