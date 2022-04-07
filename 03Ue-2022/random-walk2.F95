program random_walk

implicit none
integer, parameter :: anzahl = 100000    ! Anzahl an zu rechnenden Trajektorien
integer :: i, j, k, ix, iy, iabst
integer, parameter :: tmax = 100          ! Endzeit, Ausgabezeitschritt 1
real(kind=8), parameter :: pi = acos(-1.d0)
real(kind=8), parameter :: lambda = 3.d0, v = 1.5d0
integer, parameter :: nmax = nint(v*tmax)+1
real(kind=8) :: x, y, phi, prob, s, s1, t
real(kind=8), dimension(0:tmax) :: abstand = 0.d0
integer, dimension(tmax,-nmax:nmax) :: vert = 0
integer, dimension(tmax,-nmax:nmax,-nmax:nmax) :: ort = 0
character*1 :: c
! Durchlauf mit veränderten physikalischen Eigenschaften durch Änderung von
! lambda, v, -> alle anderen Parameter automatisch angepasst.

call random_seed()
print*, 'Soll eine individuelle Trajektorie abgespeichert werden? [j/n]'
read(*,*) c
if (c /= 'j' .and. c /= 'n') stop 'keine gueltige Option gewaehlt'

do i=1,anzahl
   x = 0.d0; y = 0.d0; t = 0.d0; s1 = 0.d0
   if (c == 'j' .and. i == 1) write(3,*) x, y
   call random_number(phi)
   phi = phi*2*pi
   do
      call random_number(prob)
      s = -lambda*log(prob)
      s1 = s1+s
      if (int(s1/v) == int(t)) then
         x = x+s*cos(phi); y = y+s*sin(phi); call random_number(phi)
         phi = phi*2*pi
         if (c == 'j' .and. i == 1) write(3,*) x, y
         t = s1/v
      else
         x = x+(v*(int(t)+1-t))*cos(phi); y = y+(v*(int(t)+1-t))*sin(phi)   ! nächste Ausgabezeit
         t = int(t)+1.d0
         abstand(nint(t)) = abstand(nint(t))+sqrt(x*x+y*y)
         iabst = min(nint(sqrt(x*x+y*y)),100)*nint(x/abs(x))
         vert(nint(t),iabst) = vert(nint(t),iabst)+1                    ! Berechnung Abstandsverteilung
         ix = nint(x); iy = nint(y)
         ort(nint(t),ix,iy) = ort(nint(t),ix,iy)+1                      ! Berechnung Ortsverteilung
         do j=nint(t)+1,min(tmax,int(s1/v))
            x = x+v*cos(phi); y = y+v*sin(phi); t = t+1.d0
            abstand(nint(t)) = abstand(nint(t))+sqrt(x*x+y*y)
            iabst = min(nint(sqrt(x*x+y*y)),100)*nint(x/abs(x))
            vert(nint(t),iabst) = vert(nint(t),iabst)+1                 ! Berechnung Abstandsverteilung
            ix = nint(x); iy = nint(y)
            ort(nint(t),ix,iy) = ort(nint(t),ix,iy)+1                   ! Berechnung Ortsverteilung
         end do
         if (s1/v < tmax) then
            x = x+(s1/v-t)*v*cos(phi); y = y+(s1/v-t)*v*sin(phi); t = s1/v
            if (c == 'j' .and. i == 1) write(3,*) x, y
            call random_number(phi)
            phi = phi*2*pi
         end if
      end if
      if (s1/v > tmax) exit
   end do
end do

write(1,*) 0, abstand(0)/anzahl        ! mittlerer Abstand
do j=1,tmax
   write(1,*) j, abstand(j)/anzahl     ! mittlerer Abstand
   do i=-nmax,nmax
      write(4,*) i, vert(j,i), j       ! Abstandsverteilung zum Zeitpunkt j
      do k=-nmax,nmax
        write(2,*) i, k, ort(j,i,k)    ! Ortsverteilung zum Zeitpunkt j
      end do
      write(2,*)                       ! Leerzeile für gnuplot-Oberfläche
   end do
   write(2,*); write(4,*); write(4,*)  ! Leerzeilen für gnuplot-Datenblöcke
end do

end program random_walk
