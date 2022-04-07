program fractal

implicit none
integer, parameter :: n=250      ! einfache Größenänderung
integer :: i, j, rep, res
double precision, parameter :: delta = .01d0, x0 = 0.d0, y0 = 0.d0   ! Schrittweite, Mittelpunkt
double precision, parameter :: pi2 = .5d0*acos(-1.d0), eps = 1.d-8
double complex :: z, z1
double precision :: x, y
double precision, dimension(-n:n,-n:n) :: bild

open(unit=1,file="newton.dat")
do i=-n,n
   do j=-n,n
      z = complex(x0+i*delta,y0+j*delta)   ! Startwert
      rep = 0
      do                                   ! Endlosschleife
         z1 = .75*z+1.d0/z**3
         if (abs(z-z1) < eps) exit         ! Nullstelle gefunden
         z = z1; rep = rep+1
         if (rep > 100) exit               ! Abbruchbedingung
      end do
      if (rep <= 100) then
         res = nint(atan2(aimag(z),dble(z))/pi2)   ! "Winkel" der Nullstelle
         if (res == -2) res = 2
         write(1,*) i*delta+x0, j*delta+y0, res, rep/100.
      else
         write(1,*) i*delta+x0, j*delta+y0, 5, 1
      end if
   end do
   write(1,*)        ! Leerzeile für gnuplot
end do
      
end program
