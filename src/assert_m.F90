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
       class(*), intent(in), optional :: diagnostic_data
     end subroutine

  end interface 

end module assert_m

submodule(assert_m) assert_s
  implicit none
contains

  module procedure assert
     character(len=:), allocatable :: stop_code
     if (ASSERTIONS) then
       if (.not. assertion) then 

         if (.not. present(diagnostic_data)) then
           stop_code = "Assertion '" // description // "' fails."
         else
           select type(diagnostic_data)
             type is(integer)
               stop_code = "Assertion '" // description // "' fails with diagnostic data " // string(diagnostic_data)
             class default
               error stop "Unrecognized diagnostic data"
           end select
         end if

         error stop stop_code
       end if
     end if
  end procedure

  pure function string(i) result(string_)
    integer, intent(in) :: i
    integer :: io_status, length
    character(len=:), allocatable :: string_
    length = 1
    do 
      allocate(character(len=length) :: string_)
      write(string_,*, iostat=io_status) i
      if (io_status==0) return
      deallocate(string_)
      length = length + 1
    end do
  end function

end submodule assert_s
