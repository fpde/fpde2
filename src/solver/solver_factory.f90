! !>
! !! @file   solver_factory.f90
! !! @author Pawel Biernat <pawel.biernat@gmail.com>
! !! @date   Thu Mar 29 19:14:08 2012
! !!
! !! @brief Solver factory is used to allocate various classes of
! !! solvers, all of which should be extensions of class 'solver'
! !!
! !!
! !!
! module solver_factory

!   use constants_module
!   use class_solver
!   use class_solver_simple

! contains

!   !> Allocates memory for a solver of type given by id
!   !!
!   !! @param[in] id type of solver to allocate, it should be a stringified
!   !! version of type name
!   !!
!   !! @param[out] error FPDE_STATUS_ERROR is returned if no type
!   !! matches the given @id
!   !!
!   !! @return Allocated solver of type "id"
!   !!
!   function solver_new(id, error) result(s)
!     class(solver), pointer :: s
!     integer, optional, intent(out) :: error
!     character(len=*) :: id

!     if(present(error)) error = FPDE_STATUS_OK

!     select case(trim(id))
!     case( "solver" )
!        allocate( solver :: s )
!     case( "solver_simple" )
!        allocate( solver_simple :: s )
!     case default
!        if(present(error)) error = FPDE_STATUS_ERROR
!        nullify( s )
!     end select

!   end function solver_new

! end module solver_factory
