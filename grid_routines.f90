!Module PARAMETERS
!Implicit None
!Real, Parameter :: d2r=asin(1e0)/9e1, km=1e3
!Real  :: fake
!contains
!Subroutine Start_PARAMETERS
!Implicit None
!Real :: c
!c = 8e5
!fake = dsqrt(c)
!End Subroutine
!End Module


! Computes no of gridpoints

subroutine compute_step(gwid, step, npoints)
  implicit none
  real :: gwid, step
  integer :: npoints

  npoints = ceiling(gwid/step)
 
end subroutine compute_step


! generates gridpoints
subroutine grid_gen(gwid, step, npoints, x, y)
  implicit none
  real :: gwid, step
  real, dimension(npoints) :: x, y
  real :: inf, sup
  integer ::  npoints, i

  sup = (gwid/2)
  inf = -sup
  print*, 'inf: ', inf, ' sup: ', sup
   
  do i = 1, npoints
    x(i) = inf + (i-1)*step
    y(i) = inf + (i-1)*step
  end do

end subroutine grid_gen


! Routine for rotating point of an angle equal to strike-angle
subroutine rotate_point(strike, x, y, xx, yy)
  implicit none
  real :: strike, x, y, xx, yy
   
  xx = x*cos(strike) - y*sin(strike)
  yy = x*sin(strike) + y*cos(strike)
     
end subroutine


!get dislocation from seismic moment
subroutine get_disl(moment, mu, depth, length, disl)
  implicit none
  real :: mu, depth, length, disl, moment

  disl = moment/(mu*depth*length)

end subroutine




!rotate from theta=0 to theta=strike_angle
!subroutine rotate(x_coord, y_coord, npoints, strike_angle)
!  implicit none
!  integer :: npoints, i
!  real, dimension(npoints) :: x_coord, y_coord, tempx, tempy

!  do i=1, npoints
!    tempx(i) = (x_coord(i)*cos(strike_angle)) - (y_coord(i)*sin(strike_angle))  
!    tempy(i) = (x_coord(i)*sin(strike_angle)) + (y_coord(i)*cos(strike_angle))
!  end do


!end subroutine




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
