program random_walk

implicit none
integer, parameter :: anzahl = 100000    ! Anzahl an zu rechnenden Trajektorien
integer :: i, j, k, ix, iy, iabst
integer, parameter :: tmax = 100          ! Endzeit
real(kind=8), parameter :: pi = acos(-1.d0)
real(kind=8), parameter :: lambda = 3.d0, v = 1.5d0, dt = .1d0
real(kind=8), parameter :: vergl = 1.d0-exp(-dt*v/lambda), ds = dt*v ! abgeleitete Größen
integer, parameter :: nmax = nint(v*tmax)+1, step = nint(1.d0/dt)
real(kind=8) :: x, y, phi, prob
real(kind=8), dimension(tmax) :: abstand = 0.d0
integer, dimension(tmax,-nmax:nmax) :: vert = 0.d0
integer, dimension(tmax,-nmax:nmax,-nmax:nmax) :: ort = 0.d0
character*1 :: c
! Durchlauf mit veränderten physikalischen Eigenschaften durch Änderung von
! lambda, v, dt -> alle anderen Parameter automatisch angepasst.

call random_seed()
print*, 'Soll eine individuelle Trajektorie abgespeichert werden? [j/n]'
read(*,*) c
if (c /= 'j' .and. c /= 'n') stop 'keine gueltige Option gewaehlt'

do i=1,anzahl
   x = 0.d0; y = 0.d0
   call random_number(phi)
   do j=1,tmax            ! Schleife über die Ausgabezeiten
      do k=1,step         ! Schleife zwischen Ausgabezeiten (nicht sehr effizient!)
         x = x+ds*cos(phi*2*pi); y = y+ds*sin(phi*2*pi)
         if (c == 'j' .and. i == 1) write(3,*) x, y
         call random_number(prob)
         if (prob < vergl) call random_number(phi)
      end do
      abstand(j) = abstand(j)+sqrt(x*x+y*y)      ! Berechnung mittlerer Abstand
      iabst = min(nint(sqrt(x*x+y*y)),100)*nint(x/abs(x))   ! 
      vert(j,iabst) = vert(j,iabst)+1            ! Berechnung Abstandsverteilung
      ix = nint(x); iy = nint(y)
      ort(j,ix,iy) = ort(j,ix,iy)+1              ! Berechnung Ortsverteilung
! nmax groß genug gewählt, dass ix, iy Grenzen nie überschreiten können
   end do
end do

do j=1,tmax
   write(1,*) j, abstand(j)/anzahl     ! mittlerer Abstand
   do i=-nmax,nmax
      write(4,*) i, vert(j,i), j       ! Abstandsverteilung zum Zeitpunkt j
      do k=-nmax,nmax
         write(2,*) i, k, ort(j,i,k)   ! Ortsverteilung zum Zeitpunkt j
      end do
      write(2,*)                       ! Leerzeile für gnuplot-Oberfläche
   end do
   write(2,*); write(4,*); write(4,*)  ! Leerzeilen für gnuplot-Datenblöcke
end do

end program random_walk
