submodule(parabola_m) parabola_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    parabola%a_ = a
    parabola%b_ = b
    parabola%c_ = c
  end procedure

  module procedure roots

    associate(a => (self%a_), b => (self%b_), c => (self%c_))
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
