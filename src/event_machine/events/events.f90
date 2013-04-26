!> @file   events.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Apr 26 14:13:09 2013
!!
!! @brief  Header files for events and actions
!!
module events

  use class_action
  use class_action_hello

  use class_event
  use class_event_true
  use class_event_logical

  use class_event_machine

  ! events
  public :: event_true

  ! actions
  public :: action_hello

  ! base types
  public :: event, action, event_machine

  ! operators for events
  public :: operator(.and.), operator(.or.), operator(.not.)


end module events
