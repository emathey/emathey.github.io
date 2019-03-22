
program main

    implicit none

    ! Data
    real(kind=8) :: th_entry,th_exit      ! Entry and exit thickness (m)
    real(kind=8) :: wr_radius           ! Work roll radius (m)

    integer :: nb_elt_thick       ! Number of elements in the thickness
    integer :: nb_elt_length      ! Number of elements along the length

    real :: nb_length_before           ! Number of bite length to be meshed before the bite
    real :: nb_length_after            ! Number of bite length to be meshed after the bite

    logical :: symmetry            ! Activates computation on half thickness if true

    ! Computed values
    integer :: nb_nodes                             ! Number of nodes in the strip

    real(kind=8) :: total_length       ! Strip length (m) (roll will be in the roughly in the middle to start)
    real(kind=8) :: bite_length        ! Length of bite along x axis

    real(kind=8), allocatable, dimension(:) :: X_node, Y_node   ! Coordinates of nodes of the strip (size : nb_nodes)
    real(kind=8), allocatable, dimension(:) :: th_node       ! Strip thickness at position Xn (size: nb_elt_length+1)

    integer :: alloc_status ! Status code for memory allocations
    integer :: i,j          ! loop controls

    ! Test data
    symmetry = .true.
    th_entry = 0.03
    th_exit = 0.02
    wr_radius = 0.35
    nb_elt_thick = 10
    nb_elt_length = 20
    nb_length_before = 2
    nb_length_after = 2

    ! Creation of initial mesh (undeformed radius)

    ! Allocation of tables
    nb_nodes = (nb_elt_thick+1)*(nb_elt_length+1)
    allocate(X_node(nb_nodes),Y_node(nb_nodes),th_node(nb_elt_length+1),stat=alloc_status)
    if (alloc_status/=0)then
        write(*,*) "Problem for memory allocation: X_node, Y_node, th_node"
    end if

    !Computation of bite length
    bite_length = sqrt(wr_radius**2 - (wr_radius + th_exit/2 - th_entry/2)**2)
    total_length = bite_length*(1+nb_length_after+nb_length_before)

    write(*,*) bite_length, total_length

    ! Computation of X coordinates of nodes
    do i=1,nb_elt_thick+1
        do j=1,nb_elt_length+1
            X_node((i-1)*(nb_elt_length+1)+j)= -bite_length*(1+nb_length_before) + (j-1)*total_length/nb_elt_length
        end do
    end do

    ! Vertical coordinates of first row
    if (symmetry) then
        Y_node(1:nb_elt_length+1) = 0
    else
        do i=1,nb_elt_length+1
            if (X_node(i) <= -bite_length) then
                Y_node(i) = -th_entry/2.
            elseif (X_node(i) <= 0) then
                Y_node(i) = -th_exit/2. -wr_radius * (1 - cos(asin(X_node(i)/wr_radius)))
            else
                Y_node(i) = -th_exit/2.
            end if
        end do
    end if

    ! Vertical coordinates of last row
    do i=(nb_elt_length+1)*nb_elt_thick+1,nb_nodes
        if (X_node(i) <= -bite_length) then
            Y_node(i) = th_entry/2.
        elseif (X_node(i) <= 0) then
            Y_node(i) = th_exit/2. + wr_radius * (1 - cos(asin(X_node(i)/wr_radius)))
        else
            Y_node(i) = th_exit/2.
        end if
    end do

    ! Vertical coordinates of middles rows
    do i=2,nb_elt_thick
        do j=1,nb_elt_length+1
            Y_node((nb_elt_length+1)*i+j) = (Y_node((nb_elt_length+1)*nb_elt_thick+j)+Y_node(j)) / nb_elt_thick * i
        end do
    end do

    do i=1,nb_nodes
        write(*,*) X_node(i), Y_node(i)
    end do

end
