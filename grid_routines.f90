! Computes no of gridpoints
subroutine compute_step(gwid, step, npoints)
  real :: gwid, step
  integer :: npoints

  npoints = ceiling(gwid/step)
 
end subroutine compute_step


! generates gridpoints
subroutine grid_gen(gwid, step, npoints, x, y)
  real :: gwid, step
  real, dimension(npoints) :: x, y
  real :: inf, sup
  integer :: i! npoints, i

  sup = (gwid/2)
  inf = -sup
  print*, 'inf: ', inf, ' sup: ', sup
   
  do i = 1, npoints
    x(i) = inf + (i-1)*step
    y(i) = inf + (i-1)*step
  end do

end subroutine grid_gen

  !Reading file
  !do 
  !  read(in_unit, *, iostat=stat) alpha(j), x(j), y(j), z(j), depth(j), dip(j), pot1(j), pot2(j), pot3(j), pot4(j)
   ! if(stat < 0) then
    !  print *, 'Reached eof'
     ! j = j-1
     ! exit
    !else if(stat > 0) then
 !     print *, 'Error in file format'
  !    exit
   ! end if
    !j = j+1
  !end do
