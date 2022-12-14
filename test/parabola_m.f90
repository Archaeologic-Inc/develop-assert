module parabola_m
  implicit none

  private
  public :: parabola_t

  type parabola_t
    private
    real a_, b_, c_
  contains
    procedure roots 
  end type

  interface parabola_t

    pure module function construct(a, b, c) result(parabola)
      implicit none
      type(parabola_t) parabola
      real, intent(in) :: a, b, c
    end function
     
  end interface

  interface

    pure module function roots(self) result(zeros)
      class(parabola_t), intent(in) :: self
      integer, parameter :: num_roots=2
      real zeros(num_roots)
    end function

  end interface

end module parabola_m
