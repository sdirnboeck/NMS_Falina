program berg

implicit none
integer :: i, j
complex :: delta
complex, dimension(3) :: c
complex, dimension(100000,3) :: a, b

call random_seed()
a(1,1) = (0.,0.); a(1,2) = (60.,0.); a(1,3) = (35.,15.)
do i=0,7
  do j=0,4**i-1
    c(1) = .5*(a(j+1,1)+a(j+1,2))+delta(a(j+1,1),a(j+1,2))
    c(2) = .5*(a(j+1,2)+a(j+1,3))+delta(a(j+1,2),a(j+1,3))
    c(3) = .5*(a(j+1,1)+a(j+1,3))+delta(a(j+1,1),a(j+1,3))
    b(4*j+1,1) = a(j+1,1); b(4*j+1,2) = c(1); b(4*j+1,3) = c(3)
    b(4*j+2,1) = a(j+1,2); b(4*j+2,2) = c(1); b(4*j+2,3) = c(2)
    b(4*j+3,1) = a(j+1,3); b(4*j+3,2) = c(3); b(4*j+3,3) = c(2)
    b(4*j+4,1) = c(1); b(4*j+4,2) = c(3); b(4*j+4,3) = c(2)
  end do
  a = b
end do

open(unit=1,file='berg.txt')
do i=1,4**8
  write(1,*) real(a(i,1)),aimag(a(i,1))
  write(1,*) real(a(i,2)),aimag(a(i,2))
  write(1,*) real(a(i,3)),aimag(a(i,3))
end do

end program berg


complex function delta(x,y)

implicit none
real :: dist, phi
real, dimension(2) :: r
complex :: x, y

call random_number(r)
dist = .2*abs(x-y)*r(1)
phi = 6.2831853*r(2)
delta= dist*cos(phi)+(0.,1.)*dist*sin(phi)

end function delta
