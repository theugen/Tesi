subroutine ask
implicit none
character*99 ciao
Write(*,*) 'continue?'
Read(*,*) ciao
end subroutine

include 'grid_routines.f90'

program prova
  implicit none
  real(kind=4) :: alpha, depth, dip, al1, al2, aw1, aw2, disl1, disl2, disl3 ! parameters of rectangular fault
  real(kind=4), allocatable, dimension(:) :: x, y ! obs-point coordinates
  real(kind=4), allocatable, dimension(:,:) :: ux, uy, uz, uxx, uyx, uzx, uxy, uyy, uzy, uxz, uyz, uzz, iret ! values returned by dc3d0
  integer :: out_unit = 1, in_unit = 2, coords = 3 ! opening i/o devices, please don't use 5 or 6 (special meaning)
  integer :: stat ! this is for checking eof while reading file
  integer :: j=1, i ! those are counters
  real :: gwid, step ! width and step of the grid
  integer :: npoints
  

  open(unit=in_unit, file='rect.dat', action='read') 
  read(in_unit, *) alpha, depth, dip, al1, al2, aw1, aw2, disl1, disl2, disl3 !Read parameters of the point-like fault
  
  print *, 'Which is the width of the grid?'
  read *, gwid
  print *, 'Which is the step of the grid?'
  read *, step

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
      call dc3d(alpha, x(i), y(j), 0e0, depth, dip, al1, al2, aw1, aw2, disl1, disl2&
                 &, disl3, ux(i,j), uy(i,j), uz(i,j), uxx(i,j), uyx(i,j)&
                 &, uzx(i,j), uxy(i,j), uyy(i,j), uzy(i,j), uxz(i,j),&
                 & uyz(i,j), uzz(i,j), iret(i,j))
    end do
  end do
 

  open(unit=out_unit, file='results.txt', action='write', status='replace')
 
  !Writing results on file 
  do i = 1, npoints 
    do j=1, npoints
      write(out_unit, *) x(i), y(j), ux(i,j), uy(i,j), uz(i,j) 
      write(*,'(5E15.5)') x(i),y(j),ux(i,j),uy(i,j),uz(i,j)
    end do
  end do  

  close(out_unit)
  close(in_unit)
 
end program prova


