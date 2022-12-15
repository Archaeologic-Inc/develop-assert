module assert_m
  implicit none

#ifndef ASSERTIONS
# define ASSERTIONS .true.
#endif

  interface 

     pure module subroutine assert(assertion, description, diagnostic_data)
       implicit none
       logical, intent(in) :: assertion
       character(len=*), intent(in) :: description
       real, intent(in), optional :: diagnostic_data
     end subroutine

  end interface 

end module assert_m

submodule(assert_m) assert_s
  implicit none
contains

  module procedure assert
     if (ASSERTIONS) then
       if (.not. assertion) error stop "The assertion described by '" // description // "' fails."
     end if
  end procedure

end submodule assert_s
