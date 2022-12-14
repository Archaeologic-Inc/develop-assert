program designed_to_terminate_normally
  !! Test an assertion that is expected to succeed
  use assert_m, only : assert
  implicit none

  real y

  y = root(a=1, b=0, c=1.)

  sync all

  if (this_image()==1) print *, "----> All tests designed to terminate normally pass. <----"

contains

  pure function root(a,b,c) result(zeros)
    real, intent(in) :: a, b, c
    real zeros(2)

    associate(radical => b**2 - 4*a*c)
      call assert(radical>=0., "root: radical >= 0.", diagnostic_data=radical)
      root = [ -b + sqrt(radical), -b  - sqrt(radical)]/(2*a)
    end associate
     
  end function

end program designed_to_terminate_normally
