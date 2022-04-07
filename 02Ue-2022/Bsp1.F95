program arch_pi

implicit none
integer :: n = 6
double precision :: s = .5d0, pis
double precision, parameter :: pi = acos(-1.d0)

do
   pis = n*s
   n = n*2
   s = sqrt(.5d0*(1.d0-sqrt(1.d0-s*s)))
   if (n*s == pis) exit
end do

print*, n/2, pis, pi, abs(pis-pi)/pi

end program
