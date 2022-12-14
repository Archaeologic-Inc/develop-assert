module parabola_m
  use assert_m, only : assert
  implicit none

  private
  public parabola_t

  integer, parameter :: num_coefficients=3

  type parabola_t
    private
    real coefficients_(num_coefficients)
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

submodule(parabola_m) parabola_s
  implicit none

contains

  module procedure roots

    associate(a => (self%coefficients_(1)), b => (self%coefficients_(2)), c => (self%coefficients_(3)))
      associate(radical => b**2 - 4*a*c)
        call assert(radical >= 0., "roots: radical >= 0.", diagnostic_data=radical)
        zeros = [-b - sqrt(radical), -b + sqrt(radical)]/(2*a)
      end associate
      block 
        real, parameter :: tolerance = 1.E-07

        associate(residuals => a*zeros**2 + b*zeros + c)
          call assert( all(abs(residuals) < tolerance), "roots: all(abs(residuals) < tolerance)", diagnostic_data=maxval(residuals))
        end associate
      end block
    end associate

  end procedure

end submodule parabola_s

program designed_to_terminate_normally
  !! Test an assertion that is expected to succeed
  use parabola_m, only : parabola_t
  implicit none
  real, allocatable :: parabola_roots(:)

  associate(parabola => parabola_t(a=1, b=0, c=-1.))
    associate(parabola_roots => parabola%roots())
    end associate
  end associate

  sync all

  if (this_image()==1) print *, "----> All tests designed to terminate normally pass. <----"

end program
