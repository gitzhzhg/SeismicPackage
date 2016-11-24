c---------------------------------------------------------------------
        subroutine anglesv(deltax,deltaz,angle)
c...................................................................
c    subroutine to determine the angle of the ray from output
c    to shot or receiver from vertical.
c...................................................................
        real deltax, deltaz, angle
c
        angle=atan(deltax/deltaz)
c
        return
        end
