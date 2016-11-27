program prova
  implicit none
  real :: alpha, x, y, z, depth, dip, pot1, pot2, pot3, pot4 
  real(kind=4) :: ux, uy, uz, uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret
  integer :: out_unit, in_unit

 ! print *,'pass medium constant'
 ! read *, alpha
 !print *, 'pass coordinates of observation points'
 ! read *, x, y, z
 ! print *, 'depth of source point:'
 ! read *, depth
 ! print *, 'dip angle:'
 ! read *, dip
 ! print *, 'potency for strike-slip, dip-slip, tensile and explosive'
 ! read *, pot1, pot2, pot3, pot4 

  open(unit=in_unit, file='param.txt', action='read') 
  read(in_unit, *) alpha, x, y, z, depth, dip, pot1, pot2, pot3, pot4

  call dc3d0(alpha, x, y, z, depth, dip, pot1, pot2, pot3, pot4, ux, uy, uz, &
             & uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret)

  open(unit=out_unit, file='hello.txt', action='write', status='replace')
  write(out_unit, *) uxx, ' ', uyy, ' ', uzz
  close(out_unit)
  close(in_unit)
 
end program prova
