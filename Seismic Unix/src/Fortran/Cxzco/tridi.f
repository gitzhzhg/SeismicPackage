c...................................................................
      subroutine tridi(n,c,d,e,b,info)
c...................................................................
c     Tridi solves the equation JX = B for X.  J is the tridiagonal
c     jacobian whose bands here are C (subdiagonal), D (diagonal),
c     and E (superdiagonal). The solution is returned as B.
c     The code is from a LINPAK listing.
c
c     REFERENCE : LINPACK USER'S GUIDE,  J.J. DONGARRA et al,
c                 SIAM, 1979.
c...................................................................
      integer    n,      info
      real       c(n),   d(n),    e(n),    b(n)
      integer    k,      kb,      kp1,     nm1,     nm2
      real       t
c
      info = 0
      c(1) = d(1)
      nm1 = n - 1
      if(nm1.lt.1) go to 40
         d(1) = e(1)
         e(1) = 0.0e0
         e(n) = 0.0e0
c
         do 30  k = 1,  nm1
            kp1 = k + 1
            if(abs(c(kp1)).lt.abs(c(k))) go to 10
               t = c(kp1)
               c(kp1) = c(k)
               c(k) = t
               t = d(kp1)
               d(kp1) = d(k)
               d(k) = t
               t = e(kp1)
               e(kp1) = e(k)
               e(k) = t
               t = b(kp1)
               b(kp1) = b(k)
               b(k) = t
10          continue
c
            if(c(k).ne.0.0e0) go to 20
               info = k
               go to 100
20          continue
c
            t = -c(kp1)/c(k)
            c(kp1) = d(kp1) + t*d(k)
            d(kp1) = e(kp1) + t*e(k)
            e(kp1) = 0.0e0
            b(kp1) = b(kp1) + t*b(k)
30       continue
40    continue
c
      if(c(n).ne.0.0e0) go to 50
         info = n
         go to 90
50    continue
c
      nm2 = n - 2
      b(n) = b(n)/c(n)
      if(n.eq.1) go to 80
         b(nm1) = (b(nm1) - d(nm1)*b(n))/c(nm1)
         if(nm2.lt.1) go to 70
            do 60  kb = 1,  nm2
               k = nm2 - kb + 1
               b(k) = (b(k) - d(k)*b(k+1) - e(k)*b(k+2))/c(k)
60          continue
70       continue
80    continue
90    continue
100   continue
      return
      end
