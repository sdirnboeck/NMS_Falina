program euler

implicit none
integer :: i, n
real(kind=8) :: K

do i=1,25
   n = 2**i
   k = (1.d0+1.d0/n)**n
   print*, 'Anzahl der Verzinsungen', n, 'Kapitalvermehrung', k
   write(1,*) log10(float(n)), k, abs(k-exp(1.d0))/exp(1.d0)
end do

end program
