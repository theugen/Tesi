subroutine ask
implicit none
character*99 ciao
Write(*,*) 'continue?'
Read(*,*) ciao
end subroutine

include 'grid_routines.f90'
include 'parameters.f90'

program prova
Use PARAMETERS
  implicit none
  real(kind=4) :: disl1, disl2, disl3
  real(kind=4), allocatable, dimension(:) :: x, y ! obs-point coordinates
  real(kind=4), allocatable, dimension(:,:) :: ux, uy, uz, uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret ! values returned by dc3d0
  integer :: out_unit = 1, in_unit = 2, coords = 3 ! opening i/o devices, please don't use 5 or 6 (special meaning)
  integer :: stat ! this is for checking eof while reading file
  integer :: j=1, i ! those are counters
  real :: gwid, step ! width and step of the grid
  integer :: npoints
  real(kind=4) :: xx, yy !those will be rotated versors
  real(kind=4) :: rotx, roty !rotated vectors resulting from okada


  Call Start_PARAMETERS

 
  print *, 'Which is the width of the grid?'
  read *, gwid
  print *, 'Which is the step of the grid?'
  read *, step

  !Computing dip-dislocation on the base of moment and depth
!******I NEED TO FIX GET_DISL
  call get_disl(moment, mu, depth, length, disl2)
  disl1=disl2*cos(rake)
  disl2=disl2*sin(rake)
  disl3=0e0

  !Computing number of points of the grid and generating x,y coords of the grid 
  call compute_step(gwid, step, npoints)
  allocate(x(npoints)) 
  allocate(y(npoints))
  call grid_gen(gwid, step, npoints, x, y)
  call ask  
  allocate(ux(npoints, npoints), uy(npoints, npoints), uz(npoints, npoints), uxx(npoints, npoints),&
           & uyx(npoints, npoints), uzx(npoints, npoints), uxy(npoints, npoints),&
           & uyy(npoints, npoints), uzy(npoints, npoints), uxz(npoints, npoints),&
           & uyz(npoints, npoints), uzz(npoints, npoints), iret(npoints, npoints))


  !Calling subroutine and storing results in the i-th component of an array
  do i=1, npoints 
    do j=1, npoints
!     Call GEO_TO_EPI(lon(i),colat(i),cos_lonp,sin_latp,colatp)
!     Call EPI_TO_FLAT(cos_lonp,sin_lonp,x,y)

      call rotate_point(strike,x(i),y(j),xx,yy)
      call dc3d(alpha, xx, yy, 0e0, depth, dip, al1, al2, aw1, aw2, disl1, disl2&
                 &, disl3, rotx, roty, uz(i,j), uxx(i,j), uyx(i,j)&
                 &, uzx(i,j), uxy(i,j), uyy(i,j), uzy(i,j), uxz(i,j),&
                 & uyz(i,j), uzz(i,j), iret(i,j))
      call rotate_vector(strike,rotx,roty,ux(i,j),uy(i,j))
      call rotate_point(-strike, xx, yy, x(i), y(j))
    end do
  end do
 

  open(unit=out_unit, file='results.txt', action='write', status='replace')
 
  !Writing results on file 
  do i = 1, npoints 
    do j=1, npoints
      write(out_unit, *) x(i), y(j), ux(i,j), uy(i,j), uz(i,j) 
      write(*,'(5E15.5)') x(i),y(j), ux(i,j), uy(i,j), uz(i,j)
    end do
  end do  

  close(out_unit)
!  close(in_unit)
 
end program prova


