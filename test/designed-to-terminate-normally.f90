program designed_to_terminate_normally
  !! Test an assertion that is expected to succeed
  use parabola_m, only : parabola_t
  implicit none

  associate(parabola => parabola_t(a=1., b=0., c=-1.))
    associate(parabola_roots => parabola%roots())
    end associate
  end associate

  sync all

  if (this_image()==1) print *, "----> All tests designed to terminate normally pass. <----"

end program
