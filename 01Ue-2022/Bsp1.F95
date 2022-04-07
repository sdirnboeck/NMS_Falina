program faktorielle

implicit none
!integer(kind=4) :: fakt = 1, fakt_old = 1, n = 1
integer(kind=8) :: fakt = 1, fakt_old = 1, n = 1

do
   fakt = fakt*n
   if (nint(float(fakt)/fakt_old) /= n) exit
   fakt_old = fakt; n = n+1
end do

write(*,'(a14,i3,a6,i20)') 'maximales n =', n-1, ', n! =', fakt_old

end program
