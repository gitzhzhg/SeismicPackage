c------------------------------------------------------------------
        subroutine magnitude(angles,angler,gradtau2)
c...................................................................
c    this subroutine is to determine the magnitude of the sum of
c    the two slowness vectors at the output points.
c...................................................................
        real angles, angler, gradtau2, root2
c
        root2=sqrt(2.)
        gradtau2=root2*sqrt(1.+cos(angles-angler))/2.
c       write(0,*)"gradtau2=",gradtau2
        return
        end
