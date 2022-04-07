program WF

implicit none
integer :: i
real :: x
real, dimension(0:1000) :: daten

open(unit=1,file='WF.dat',status='old')
do i=0,1000
   read(1,*) x, daten(i)
end do
daten = daten/sqrt(sum(daten**2)*.01)
do i=0,1000
   write(2,*) i*.01, daten(i)
end do

end program
