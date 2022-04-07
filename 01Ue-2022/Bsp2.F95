program umfang
! Programm zur Berechnung des Umfangs von Oesterreich aus GPS-Daten

implicit none
integer :: ierr
real(kind=8), parameter :: deg_rad = acos(-1.d0)/180.d0
real(kind=8) :: Br_n, Br_o, Br_0, L_n, L_o, L_0, dist = 0.d0

open(unit=1, file='Gpsies_austria.csv', status='old')

read(1,*,iostat=ierr) Br_0, L_0            ! Einlesen des Startpunkts
Br_0 = Br_0*deg_rad; L_0 = L_0*deg_rad     ! Umrechnung Grad -> rad
Br_o = Br_0; L_o = L_0                     ! Schleifenwerte initialisieren
do
   read(1,*,iostat=ierr) Br_n, L_n         ! naechsten Punkt einlesen
   if (ierr /= 0) exit                     ! Abbruch falls keine neuen Werte
   Br_n = Br_n*deg_rad; L_n = L_n*deg_rad  ! Umrechnung Grad -> rad
   dist = dist+acos(sin(Br_n)*sin(Br_o)+cos(Br_n)*cos(Br_o)*cos(L_n-L_o)) ! Orthodrome
   Br_o = Br_n; L_o = L_n                  ! naechsten Durchlauf vorbereiten
end do
dist = dist+acos(sin(Br_0)*sin(Br_o)+cos(Br_0)*cos(Br_o)*cos(L_0-L_o)) ! Kreis schliessen!

write(*,'(a46,f6.1,a3)') 'Der Umfang von Oesterreich betraegt ungefaehr ', dist*6371, ' km'

end program
