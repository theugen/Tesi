!Module containing the declaration of some parameters specific for every
!different fault

module PARAMETERS
  implicit none
  real, parameter :: d2r = asin(1e0)/9e1 !declaration of a constant
  !d2r is needed for converting from deg to rad
  real :: mu=40e09, moment = 5.593e22!, length, rake, strike 
! fault length, rigidity and seismic moment, needed for get_disl() subroutine 
  real(kind=4) :: al1, al2, aw1, aw2
!                  & disl2, disl3 ! parameters of rectangular fault

   real(kind=4) ::  alpha = 2e0/3e0
   real(kind=4) ::  depth = 10e0
   real(kind=4) ::  dip = 9e0
   real(kind=4) ::  length=250e0
   real(kind=4) ::  rake=78e0*d2r
   real(kind=4) ::  strike=(180e0-193e0)*d2r

   contains
   
     subroutine start_parameters
       al2 = length/2e0
       al1 = -al2
       aw1 = -150e0
       aw2 = 0e0
     end subroutine


end module
