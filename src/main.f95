
program main

  implicit none

! Data
  real(kind=8) :: th_entry,th_exit      ! Entry and exit thickness (m)
  real(kind=8) :: wr_radius           ! Work roll radius (m)

  integer :: nb_elt_thick       ! Number of elements in the thickness
  integer :: nb_elt_length      ! Number of elements along the length

  real(kind=8) :: total_length       ! Strip length (m) (roll will be in the roughly in the middle to start)

! Computed values
  integer :: nb_nodes                             ! Number of nodes in the strip
  real(kind=8), allocatable, dimension(:) :: X_node, Y_node   ! Coordinates of nodes of the strip (size : nb_nodes)
  real(kind=8), allocatable, dimension(:) :: th_node       ! Strip thickness at position Xn (size: nb_elt_length+1)

  integer :: alloc_status ! Status code for memory allocations
  integer :: i,j          ! loop controls

  ! Test data
   th_entry = 0.03
   th_exit = 0.02
   wr_radius = 0.35
   nb_elt_thick = 10
   nb_elt_length = 10
   total_length = 0.1

  ! Creation of initial mesh (undeformed radius)

  ! Allocation of tables
  nb_nodes = (nb_elt_thick+1)*(nb_elt_length+1)
  allocate(X_node(nb_nodes),Y_node(nb_nodes),th_node(nb_elt_length+1),stat=alloc_status)
  if (alloc_status/=0)then
     write(*,*) "Problem for memory allocation: X_node, Y_node, th_node"
  end if


  ! Computation of X coordinates of nodes
  do i=1,nb_elt_thick+1
    do j=1,nb_elt_length+1
        X_node((i-1)*(nb_elt_length+1)+1)=-total_length/2 + (j-1)*total_length/nb_elt_length
    end do
  end do

  write(*,*) X_node
  ! Profile along length





end
