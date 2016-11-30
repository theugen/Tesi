program prova
  implicit none
  real(kind=4), dimension(5) :: alpha, x, y, z, depth, dip, pot1, pot2, pot3, pot4 ! input values for dc3d0
  real(kind=4), dimension(5) :: ux, uy, uz, uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret ! values returned by dc3d0
  integer :: out_unit = 1, in_unit = 2 ! opening i/o devices, please don't use 5 or 6 (special meaning)
  integer :: stat ! this is for checking eof while reading file
  integer :: j=1, i ! those are counters

  open(unit=in_unit, file='param.txt', action='read') 
  
  !Reading file
  do 
    read(in_unit, *, iostat=stat) alpha(j), x(j), y(j), z(j), depth(j), dip(j), pot1(j), pot2(j), pot3(j), pot4(j)
    !if(stat /= 0) exit   checking if eof
    if(stat < 0) then
      print *, 'Reached eof'
      j = j-1
      exit
    else if(stat > 0) then
      print *, 'Error in file format'
      exit
    end if
    j = j+1
  end do
   
  
  !Calling subroutine and storing results in the i-th component of an array
  do i=1, j
    call dc3d0(alpha(i), x(i), y(i), z(i), depth(i), dip(i), pot1(i), pot2(i), pot3(i),&
               & pot4(i), ux(i), uy(i), uz(i), uxx(i), uyx(i), uzx(i), uxy(i), uyy(i) &
               &, uzy(i), uxz(i), uyz(i), uzz(i), iret(i))
  end do
 

  open(unit=out_unit, file='results.txt', action='write', status='replace')
 
  !Writing results on file 
  do i = 1, j
    write(out_unit, *) x(i), y(i), uz(i) 

  end do  

  close(out_unit)
  close(in_unit)
 
end program prova
