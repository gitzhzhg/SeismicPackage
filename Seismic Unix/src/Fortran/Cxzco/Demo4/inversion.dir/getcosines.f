c  This program is for computing reflection angles. Suppose the
c  two reflectors are horizontal.  (Zhenyue Liu, 1991)

c  eps: error control for Newton iteration.
c  c1 & c2: velocities in the first layer and the second.
c  h1 & h2: heights of the first and the second layers.
c  xs: shot position
c  x1: horizontal position of first outpoint  in  interfaces.
c  nout: number of output points
c  xd: spacing of output points 
c  out1: filename containing cosine of angle on the first interface.
c  out2: filename containing cosine of angle on the second interface.

       real eps, c1, c2, h1, h2, xs, xd, x1
       parameter( eps=0.01, c1=5000, c2=6000, h1=1500, h2=1000,
     $  nout=60, xs=1000, xd=80, x1=2000)
       real cosb(nout)

        open(1, file='out1', form='unformatted', status='new')
        open(2, file='out2', form='unformatted', status='new')
        
c       Compute cosine of angle on the first interface
        do 10 i = 1, nout
		cosb(i) = h1 / sqrt(h1**2 + (xs-x1)**2)
10       continue
        write(1) (cosb(i), i=1, nout)

c	Do Newton iterations for cosine of angle on the second interface, 
c       until tolerence met
        b1 = 0.
20      b2 = asin(c2/c1*sin(b1))
		f = h1 * tan(b1) + h2 * tan(b2) - x1 + xs
		if (abs(f) .le. h1*eps) goto 30
		df = h1 / cos(b1)**2 + h2 * cos(b1) / cos(b2)**3 * c2/c1
		b1 = b1 - f / df
        goto 20
c	End Newton iterations

30      do 40 i = 1, nout
		cosb(i)  = cos(b2)
40      continue
 
        write(2) (cosb(i), i=1, nout)
        close(1)
        close(2)

        end
