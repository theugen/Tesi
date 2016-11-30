subroutine ask
character*99 ciao
Write(*,*) 'continue?'
Read(*,*) ciao
end subroutine


program prova
  implicit none
!  real(kind=4), dimension(5) :: alpha, x, y, z, depth, dip, pot1, pot2, pot3, pot4 ! input values for dc3d0
  real(kind=4) :: alpha, depth, dip, pot1, pot2, pot3, pot4 ! parameters of point-like fault
  real(kind=4), allocatable, dimension(:) :: x, y ! obs-point coordinates
  real(kind=4), dimension(5) :: ux, uy, uz, uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret ! values returned by dc3d0
  integer :: out_unit = 1, in_unit = 2, coords = 3 ! opening i/o devices, please don't use 5 or 6 (special meaning)
  integer :: stat ! this is for checking eof while reading file
  integer :: j=1, i ! those are counters
  real :: gwid, step ! width and step of the grid
  integer :: npoints
  

  open(unit=in_unit, file='param.txt', action='read') 
  read(in_unit, *) alpha, depth, dip, pot1, pot2, pot3, pot4 !Read parameters of the point-like fault
  
  print *, 'Which is the width of the grid?'
  read *, gwid
  print *, 'Which is the step of the grid?'
  read *, step

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
 

  !Computing number of points of the grid and generating x,y coords of the grid 
  call compute_step(gwid, step, npoints)
  allocate(x(npoints)) 
  allocate(y(npoints))
  call grid_gen(gwid, step, npoints, x, y)
  call ask  


  !Calling subroutine and storing results in the i-th component of an array
  do i=1, j
    call dc3d0(alpha, x(i), y(i), 0e0, depth, dip, pot1, pot2, pot3,&
               & pot4, ux(i), uy(i), uz(i), uxx(i), uyx(i), uzx(i), uxy(i), uyy(i) &
               &, uzy(i), uxz(i), uyz(i), uzz(i), iret(i))
  end do
 

  open(unit=out_unit, file='results.txt', action='write', status='replace')
 
  !Writing results on file 
  do i = 1, npoints 
    write(out_unit, *) x(i), y(i), uz(i) 
    write(*,'(5E15.5)') x(i),y(i),Ux(i),uy(i),uz(i)
  end do  

  close(out_unit)
  close(in_unit)
 
end program prova


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

  sup = (gwidth/2)
  inf = -sup
 
  !npoints = ceiling(gwid/step)

  !allocate(x(npoints))
  !allocate(y(npoints))

  do i = 1, npoints
    x(i) = inf + (i-1)*step
    y(i) = inf + (i-1)*step
  end do

end subroutine grid_gen
