c  This program is for computing reflection angles. Suppose two reflector
c  are horizontal.
c  eps: error control for Newton iteration.
c  c1 & c2: velocities in the first layer and the second.
c  h1 & h2: heights of the first and the second layers.
c  xs: shot position
c  x1: horizontal position of first outpoint  in  interfaces.
c  nout: number of output points
c  xd: spacing of output points 

       real eps, c1, c2, h1, h2, xs, xd, x1
       parameter( eps=0.01, c1=5000, c2=6000, h1=1500, h2=1000,
     $  nout=50, xs=5000, xd=80, x1=3000)
       real cosb(nout)

        open(1,file='out1',form='unformatted',status='new')
        open(2,file='out2',form='unformatted',status='new')
        
        do 5 i = 1, nout
           x = x1 + (i-1) * xd
           cosb(i) = h1 / sqrt(h1**2 + (xs-x)**2)
5       continue
        write(1)(cosb(i),i=1,nout)

        b1 = 0.
        do 10 i=1,nout
        x = xd *(i-1) + x1
100     b2 = asin(c2/c1*sin(b1))
        f = h1 * tan(b1) + h2 * tan(b2) - x + xs
        if(abs(f).le.h1*eps) goto 15
        df = h1 / cos(b1)**2 + h2 * cos(b1) / cos(b2)**3 * c2/c1
        b1 = b1 - f / df
        goto 100
15        cosb(i) = cos(b2)
10      continue
 
        write(2)(cosb(i),i=1,nout)
        close(1)
        close(2)

        end
