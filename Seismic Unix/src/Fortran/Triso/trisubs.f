*****************************************************************
*								*
*	Trisubs: Routines for TRISO				*	
*								*
*	Author: Sebastien Geoltrain				*
*								*
*	Copyrights: Center for Wave Phenomena,			*
*		    Mathematics Department,			*
*		    Colorado School of Mines,			*
*		    Golden, CO 80401.				*
*								*
*	All Rights Reserved.					*
*								*
*****************************************************************

      subroutine ccrossbound(p0,p10,p20,p30,p40,j0,amp10,amp20,
     &   iseg,ipath,p,p1,p2,p3,p4,j,amp1,amp2,error)

      complex amp10, amp20, amp1, amp2
      real p0(3), p10(3), p20(3), p30(3), p40(3), j0
      real p(3), p1(3), p2(3), p3(3), p4(3), j
      integer error, iseg, ipath

*
*   This routine continues the ray parameters from layer number
*   "inc" to layer number "scat".
*
*   INPUT VARIABLES:
*
*       p0(3)   incident ray slowness
*       p10(3)  first tube-ray incident slowness
*       p20(3)  second tube-ray incident slowness
*       p30(3)  third tube-ray incident slowness
*       p40(3)  fourth tube-ray incident slowness
*       j0      incident ray jacobian
*       amp10   incident amplitude
*       amp20   incident amplitude of QS mode if incident
*               medium is isotropic and mode is S.
*       iseg    index of incident ray segment.
*       ipath   index of raypath
*
*   OUTPUT VARIABLES:
*
*       p(3)    scattered ray slowness
*       p1(3)   first tube-ray scattered slowness
*       p2(3)   second tube-ray scattered slowness
*       p3(3)   third tube-ray scattered slowness
*       p4(3)   fourth tube-ray scattered slowness
*       j       scattered ray jacobian
*       amp1    scattered amplitude
*       amp2    scattered amplitude of QS mode if scattering
*               medium is isotropic and scattering mode is S.
*       error   0 for no error.
*               1 if postcritical scattering occurs.
*               2 if jacobian is zero (caustic)
*               3 if scattering coefficients are not computed
*
*   GLOBAL VARIABLES:
*       
*       block "model"   elastic and geometric parameters
*       block "path"    raypath parameters
*
*   EXTERNAL ROUTINES:
*
*       scatslow        real scattering slowness finder
*       groupvel        real group velocity evaluator
*       czoeppritz      complex zoeppritz system solver
*

      integer nlayer, triso(100), nsegment(200)
      integer mode(200,100), interface(200,0:100), npath
      real param(900), geom(0:99)
      common /model/ param,geom,nlayer,triso
      common /path/ mode, interface, nsegment, npath

      integer inc, scat, trans, imode, smode
      real elasti(6), elasts(6), elastt(6), axisi(3), axiss(3)
      real dot, dot0, w(3), w0(3), axist(3), normal(3)
      complex coefs(6), coefs2(6)
      integer i, iinc, iscat, isol, isol1, isol2, isol3, isol4
      integer which(2), itrans

*   initializing some variables

      error = 0
      
*   determining incident and scattering modes, incident, 
*   scattering and transmission layer indexes.
      
      imode = mode(ipath,iseg)
      smode = mode(ipath,min0(iseg+1,nsegment(ipath)))

      inc = max0(interface(ipath,iseg-1),interface(ipath,iseg))
      scat = max0(interface(ipath,iseg),interface(ipath,min0(iseg+1,
     &          nsegment(ipath))))

      if(inc .NE. scat) then
*   ...transmission
        trans = scat
      else if(interface(ipath,iseg-1) .LT. interface(ipath,iseg)) then
*   ..."v" type reflection
        trans = interface(ipath,iseg)+1
      else
*   ..."^" type reflection
        trans = interface(ipath,iseg)
      endif

*   loading in proper parameters

      iinc = (inc-1)*9
      iscat = (scat-1)*9
      itrans = (trans-1)*9


      do10 i=1,6
        elasti(i) = param(iinc+i)
        elasts(i) = param(iscat+i)
        elastt(i) = param(itrans+i)
10      continue

      do20 i=1,3
        axisi(i) = param(iinc+i+6)
        axiss(i) = param(iscat+i+6)
        axist(i) = param(itrans+i+6)
20      continue

      which(1) = smode
      normal(1) = 0.
      normal(2) = 0.

*   normal unit vector must be pointing towards incident medium

      if(inc .LT. scat) then
*   ...downward transmission
        normal(3) = -1.
        which(2) = 2
      else if(inc .GT. scat) then
*   ...upward transmission
        normal(3) = 1.
        which(2) = 2
      else if(interface(ipath,iseg-1) .LT. interface(ipath,iseg)) then
*   ..."v" type reflection
        normal(3) = -1.
        which(2) = 1
      else
*   ..."^" type reflection
        normal(3) = 1.
        which(2) = 1
      endif

*   compute the five scattered slownesses


      call scatslow(p0,normal,axiss,which,elasts,p,isol)
      call scatslow(p10,normal,axiss,which,elasts,p1,isol1)
      call scatslow(p20,normal,axiss,which,elasts,p2,isol2)
      call scatslow(p30,normal,axiss,which,elasts,p3,isol3)
      call scatslow(p40,normal,axiss,which,elasts,p4,isol4)

*   test for post-critical scattering and interrupts tracing if needed

      if((isol+isol1+isol2+isol3+isol4) .NE. 0) then
        error=1
        return
      endif

*   compute the scattered ray jacobian from its incident value


      call groupvel(p0,axisi,imode,elasti,w0)
      call groupvel(p,axiss,smode,elasts,w)

      dot0 = 0.
      dot = 0.

      do30 i=1,3
        dot0 = dot0+w0(i)*normal(i)
        dot = dot+w(i)*normal(i)
30      continue

      if(dot0.EQ.0.) then
*   ...caustic: should not happen, hence a fatal error
        error = 2
        return
      endif

      j = j0*dot/dot0
      j = abs(j)


*   compute the scattering coefficients

      call czoeppritz(p0,imode,normal,axisi,elasti,
     &                axist,elastt,coefs,error)


*   exits if any error or unsuccessful computation occured

      if(error .NE. 0) then
        error = 3
        return
      endif

      if((imode.EQ.1) .AND. (triso(inc).EQ.0)) then

*   if incident medium is isotropic and incident mode is S, then 
*   combines the SP and QS contributions together. SP scattering 
*   coefficients have already been computed. Now compute QS scattering 
*   coefficients (array coefs2(6)) and combine.

        i = 3
        call czoeppritz(p0,i,normal,axisi,elasti,axist,elastt,
     &          coefs2,error)

        if(error .NE. 0) then
                error = 3
                return
        endif

        if(inc .EQ. scat) then
                amp1 = coefs(smode)*amp10 + coefs2(smode)*amp20
                amp2 = coefs(3)*amp10 + coefs2(3)*amp20
        else
                amp1 = coefs(smode+3)*amp10 + coefs2(smode+3)*amp20
                amp2 = coefs(6)*amp10 + coefs2(6)*amp20
        endif
                
      else if(inc .EQ. scat) then

*   computes scattering amplitude for the appropriate mode, 
*   and saves QS scattering amplitude in case scattering layer 
*   is isotropic.

        amp1 = coefs(smode)*amp10
        amp2 = coefs(3)*amp10
      else
        amp1 = coefs(smode+3)*amp10
        amp2 = coefs(6)*amp10
      endif

      return
      end



      subroutine cendray(amp1,amp2,p0,mode,disp,error)

      complex amp1, amp2, disp(3)
      real p0(3)
      integer mode, error

*
*   computes the total displacement at the free elastic surface
*   of a transversely isotropic medium with arbitrary axis of symmetry. 
*
*   INPUT VARIABLES
*
*       p0(3)           incident slowness vector
*       amp1            amplitude of incident mode
*       amp2            amplitude of incident QS wave
*       mode            incident wave type (1 for S, 2 for P)
*
*   OUTPUT VARIABLES
*
*       disp(3)         total displacement vector at surface
*       error           0 if no error, 1 if wrong mode,
*       
*   GLOBAL VARIABLES
*
*       block "model"   elastic parameters
*

      integer nlayer, triso(100)
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso

      real axis(3), elast(6), normal(3)
      complex ui(3), urSP(3), urQP(3), urQS(3)
      complex coefs(3)
      integer i

      do5 i=1,6
         elast(i) = param(i)
5     continue

      do10 i=1,3
         axis(i) = param(6+i)
10    continue

      normal(1) = 0.e0
      normal(2) = 0.e0
      normal(3) = 1.e0

*     computes free surface scattering parameters for incident mode

      call freezoep(p0,mode,normal,axis,elast,coefs,error
     &              ,ui,urSP,urQP,urQS)

      if(error .NE. 0) then
         error = 1
         return
      endif         

*     assembles total free-surface displacement as sum of 
*     incident and reflected displacements

      do20 i=1,3
         disp(i) = amp1*(ui(i)+coefs(1)*urSP(i)
     &                    +coefs(2)*urQP(i)+coefs(3)*urQS(i))
20    continue

*     if top layer is isotropic and incident mode is S, then must 
*     add QS contribution (SP just computed above)

      if(triso(1) .EQ. 0 .AND. mode .EQ. 1) then

         call freezoep(p0,3,normal,axis,elast,coefs,error
     &                 ,ui,urSP,urQP,urQS)
         if(error .NE. 0) then
            error=1
            return
         endif

         do30 i=1,3
            disp(i) = disp(i)+amp2*(ui(i)+coefs(1)*urSP(i)
     &              + coefs(2)*urQP(i)+coefs(3)*urQS(i))
30       continue

      endif

      return
      end



      subroutine cgausselim(mat,vec,n)
      integer n
      complex mat(n,n), vec(n)

*
*   complex gaussian elimination routine with partial pivoting
*   and line index permutation for systems of degree less than 100.
*   
*   INPUT VARIABLES:
*
*       mat(n,n)        matrix of linear system
*       vec(n)          forcing vector
*       n               dimension of system
*
*   OUTPUT VARIABLES:
*       mat(n,n)        reduced matrix (useless)
*       vec(n)          solution of system
*
*   NOTE... 
*   ... this routine will modify the contents of both 
*   mat and vec. Moreover, mat should be declared statically
*   in the calling routine EXACTLY with the same dimension 
*   that is used when this routine is called.
*

      complex pivot, try, res(100), coef
      integer index(100), i, j, k, l, ind, jpiv

      if(n .GT. 100) then
        return
      endif

*   INITIALIZATION STEP

      do 10 i=1,n
        index(i) = i
10      continue

*   ELIMINATION LOOP

      do 20 i=1,n
        ind = index(i)
        jpiv = i
        pivot = mat(ind,i)

*       FINDING PIVOT

        do 30 j=i+1,n
                k = index(j)
                try = mat(k,i)
                if(cabs(try) .GT. cabs(pivot)) then
                        pivot = try
                        ind = k
                        jpiv = j
                endif
30         continue

*       TESTING FOR SINGULARITY AND PERMUTING INDEXES

        if(cabs(pivot) .EQ. 0.) then
                return
        else
                index(jpiv) = index(i)
                index(i) = ind
        endif

*       REACTUALIZING mat AND vec  (ELIMINATION) 

        do 40 j=i+1,n
                k = index(j)
                vec(k) = vec(k) - mat(k,i)*vec(ind)/pivot
                coef = mat(k,i)/pivot
                do 50 l=i+1,n
                        mat(k,l) = mat(k,l) - mat(ind,l)*coef
50                 continue
*               mat(k,i) = 0.
40         continue
20      continue                   

*   BACKSUBSTITUTION

      do 60 i=n,1,-1
        k = index(i)
        res(i) = 0.e0
        do 70 j=i+1,n
                res(i) = res(i) + res(j)*mat(k,j)
70         continue
        res(i) = (vec(k)-res(i))/mat(k,i)
60      continue

*   PUTS RESULT BACK IN vec

      do 80 i=1,n
        vec(i) = res(i)
80      continue

      return
      end



      subroutine cinitray(razi,rdip,mode,rg1,rg2,r,rdg,x0,x10,x20,
     &   x30,x40,p0,p10,p20,p30,p40,t0,t10,t20,t30,t40,j0,amp10,amp20)

      real razi, rdip, rg1, rg2, r, rdg, x0(3), x10(3), x20(3), x30(3)
      real x40(3), p0(3), p10(3), p20(3), p30(3), p40(3), t0, t10
      real t20, t30, t40, j0
      complex amp10, amp20
      integer mode

*
*   computes the initial parameters for a ray emanating from 
*   a point source at the free surface of a homogeneous isotropic
*   medium. The elastic parameters are in param.
*
*   INPUT VARIABLES:
*
*       razi    azimuth of point force in degrees (angle from 
*               geographic North in a horizontal plane)
*       rdip    dip of force in degrees (inclination of force
*               with respect to horizontal)
*       mode    type of excitation: 1 for S, 2 for P
*       rg1     take-off azimuth angle for the ray in degrees
*       rg2     take-off dip angle for the ray in degrees
*       r       radius of take-off sphere
*       rdg     spherical angle defining the angular aperture
*               of the ray tube in degrees
*
*   OUTPUT VARIABLES:
*
*       x0(3)   vector location of ray starting point with respect
*               to source location              
*       x10(3)  vector location of first tube ray starting point 
*               with respect to source location         
*       x20(3)  vector location of second tube ray starting point 
*               with respect to source location         
*       x30(3)  vector location of third second tube ray starting point 
*               with respect to source location         
*       x40(3)  vector location of fourth tube ray starting point 
*               with respect to source location         
*       p0(3)   initial ray slowness
*       p10(3)  initial slowness for first tube ray
*       p20(3)  initial slowness for second tube ray
*       p30(3)  initial slowness for third first tube ray
*       p40(3)  initial slowness for fourth tube ray
*       t0      initial ray traveltime
*       t10     initial traveltime for first tube ray
*       t20     initial traveltime for second tube ray
*       t30     initial traveltime for third tube ray
*       t40     initial traveltime for fourth tube ray
*       amp10   initial ray amplitude for mode
*       amp20   initial ray amplitude for QS mode if mode=1 and 
*               top layer is isotropic. 
*       j0      initial ray jacobian
*
*   GLOBAL VARIABLES:
*
*   common block "model"           contains elastic parameters 
*                               of upper layer.
*

      integer nlayer, triso(100)
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso

      real deg2rad, w0(3), w10(3), w20(3), w30(3), w40(3)
      real azi, dip, g1, g2, dg, sg1, cg1
      real elast(6), axis(3), force(3)
      complex u0(3), cu0(3), cp0(3)
      integer i

*   initializes useful variables

      deg2rad = 1.7453293e-02
      azi = razi*deg2rad
      dip = rdip*deg2rad
      g1 = rg1*deg2rad
      g2 = rg2*deg2rad
      dg = rdg*deg2rad
      cg1 = cos(g1)
      sg1 = sin(g1)

      do10 i=1,6
         elast(i) = param(i)
10    continue

      do20 i=1,3
         axis(i) = param(i+6)
20    continue

*     assemble (unit) force vector 

      force(1) = cos(azi)*cos(dip)
      force(2) = sin(azi)*cos(dip)
      force(3) = sin(dip)

*     computes initial traveltime

      if(mode .EQ. 2) then
         t0 = r*sqrt(param(6)/param(1))
      else
         t0 = r*sqrt(param(6)/param(3))
      endif

      t10 = t0
      t20 = t0
      t30 = t0
      t40 = t0

*   compute slowness direction of central ray

      p0(1) = cg1*cos(g2)
      p0(2) = sg1*cos(g2)
      p0(3) = sin(g2)

*   compute direction of four extra slownesses for ray tube
*   so that the SLOWNESS tube is square, parallel to the 
*   central slowness, and has solid angle aperture dg*dg.

      p10(1) = cg1*cos(g2+dg/2.)
      p10(2) = sg1*cos(g2+dg/2.)
      p10(3) = sin(g2+dg/2.)

      p20(1) = cg1*cos(g2-dg/2.)
      p20(2) = sg1*cos(g2-dg/2.)
      p20(3) = sin(g2-dg/2.)

      p30(1) = p0(1)+(p10(2)-p0(2))*p0(3)-(p10(3)-p0(3))*p0(2)
      p30(2) = p0(2)+(p10(3)-p0(3))*p0(1)-(p10(1)-p0(1))*p0(3)
      p30(3) = p0(3)+(p10(1)-p0(1))*p0(2)-(p10(2)-p0(2))*p0(1)

      p40(1) = p0(1)+(p20(2)-p0(2))*p0(3)-(p20(3)-p0(3))*p0(2)
      p40(2) = p0(2)+(p20(3)-p0(3))*p0(1)-(p20(1)-p0(1))*p0(3)
      p40(3) = p0(3)+(p20(1)-p0(1))*p0(2)-(p20(2)-p0(2))*p0(1)

*   compute group velocity of all rays

      call groupvel(p0,axis,mode,elast,w0)
      call groupvel(p10,axis,mode,elast,w10)
      call groupvel(p20,axis,mode,elast,w20)
      call groupvel(p30,axis,mode,elast,w30)
      call groupvel(p40,axis,mode,elast,w40)

*   compute radiated amplitude

      call freerad(force,p0,axis,elast,mode,t0,u0,x0)

      do60 i=1,3
         cp0(i) = cmplx(p0(i),0.e0)
60    continue

      call cpolar(cp0,axis,mode,elast,cu0)

      amp10 = u0(1)*cu0(1)+u0(2)*cu0(2)+u0(3)*cu0(3)

*   if top layer is isotropic and mode is S, then compute 
*   QS amplitude. 

      if(triso(1) .EQ. 0 .AND. mode .EQ. 1) then

         call freerad(force,p0,axis,elast,3,t0,u0,x0)

         do70 i=1,3
            cp0(i) = cmplx(p0(i),0.e0)
70       continue

         call cpolar(cp0,axis,3,elast,cu0)

         amp20 = u0(1)*cu0(1)+u0(2)*cu0(2)+u0(3)*cu0(3)

      else

         amp20 = cmplx(0.e0,0.e0)

      endif

*   compute slowness vector for all rays

      call slowness(p0,axis,mode,elast)
      call slowness(p10,axis,mode,elast)
      call slowness(p20,axis,mode,elast)
      call slowness(p30,axis,mode,elast)
      call slowness(p40,axis,mode,elast)

*   compute ray starting points

      do30 i=1,3
        x0(i) = w0(i)*t0
        x10(i) = w10(i)*t0
        x20(i) = w20(i)*t0
        x30(i) = w30(i)*t0
        x40(i) = w40(i)*t0
30      continue

*   compute ray jacobian

      j0 = w0(1)*( (x20(2)-x10(2))*(x40(3)-x30(3)) - 
     &   (x20(3)-x10(3))*(x40(2)-x30(2)) )
      j0 = j0 + w0(2)*( (x20(3)-x10(3))*(x40(1)-x30(1)) - 
     &   (x20(1)-x10(1))*(x40(3)-x30(3)) )
      j0 = j0 + w0(3)*( (x20(1)-x10(1))*(x40(2)-x30(2)) - 
     &   (x20(2)-x10(2))*(x40(1)-x30(1)) )
      j0 = abs(j0)

      return
      end



      subroutine cpolar(p,axis,mode,elast,u)
      real axis(3), elast(6)
      complex p(3), u(3)
      integer mode

*
*   computes displacement polarization of given mode
*
*   INPUT VARIABLES:
*
*       p(3)            slowness (complex)
*       elast(6)        elastic coefficients in order:
*                       A, C, N, L, F, rho
*       axis(3)         anisotropy axis direction
*       mode            1 for SP, 2 for QP, 3 for QS
*
*   OUTPUT VARIABLES:
*
*       u(3)            displacement polarization (complex)
*

      complex dot, pr2, pa2, pa(3), pr(3), alpha, beta, epsp
      complex epss, norm, u0(3)
      real A, C, N, L, F
      integer i


      if(mode .EQ. 1) then

*       determining the S-parallel polarization
        
        u(1) = axis(2)*p(3)-axis(3)*p(2)
        u(2) = axis(3)*p(1)-axis(1)*p(3)
        u(3) = axis(1)*p(2)-axis(2)*p(1)

        dot = cmplx(0.e0,0.e0)
      
        do 40 i=1,3
                dot = dot + u(i)*u(i)
40         continue
      
        
        if(cabs(dot) .EQ. 0.e0) then
      
*       ...undetermination case: propagation along axis
      
                u(1) = 0.e0
                u(2) = axis(3)
                u(3) = -axis(2)
                dot = sqrt(axis(2)*axis(2)+axis(3)*axis(3))
                if(cabs(dot) .NE. 0.e0) then
                        u(2) = u(2)/dot
                        u(3) = u(3)/dot
                else
                        u(1) = -axis(3)
                        u(2) = 0.e0
                        u(3) = axis(1)
                        dot = sqrt(axis(1)*axis(1)+axis(3)*axis(3))
                        if(cabs(dot) .NE. 0.e0) then
                                u(1) = u(1)/dot
                                u(3) = u(3)/dot
                        else
                                dot = sqrt(axis(2)*axis(2)+
     &                                  axis(1)*axis(1))
                                u(1) = axis(2)/dot
                                u(2) = -axis(1)/dot
                                u(3) = 0.e0
                        endif
                endif
        else
                dot = csqrt(dot)
      
                do 30 i=1,3
                        u(i) = u(i)/dot
30                 continue        
      
        endif
      
        return
      
      endif
        
*   computes axial and radial slownesses and related quantities

      A = elast(1)
      C = elast(2)
      N = elast(3)
      L = elast(4)
      F = elast(5)

      dot = cmplx(0.e0,0.e0)

      do50 i=1,3
        dot = dot+p(i)*axis(i)
50      continue
      
      do10 i=1,3
        pa(i) = dot*axis(i)
        pr(i) = p(i)-pa(i)
10      continue

      pr2 = cmplx(0.e0,0.e0)
      pa2 = cmplx(0.e0,0.e0)

      do20 i=1,3
        pr2 = pr2+pr(i)*pr(i)
        pa2 = pa2+pa(i)*pa(i)
20      continue

      alpha = (A+L)*pr2 + (C+L)*pa2
      beta = (A-L)*pr2 - (C-L)*pa2
      beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
      beta = csqrt(beta)

      if(mode .EQ. 2) then

*       computes Quasi-P polarization
      
        if(cabs(pr2) .LT. 1.e-6*cabs(pa2)) then
*       propagation is along anisotropy axis: polarization is
*       longitudinal.
                u(1) = p(1)
                u(2) = p(2)
                u(3) = p(3)
        else
                epsp = (F+L)*pr2/((alpha+beta)/2.e0-L*pr2-C*pa2)
      
                do80 i=1,3
                        u(i) = pr(i) + epsp*pa(i)
80                 continue
      
        endif
      
        norm = 0.e0
      
        do90 i=1,3
                norm = norm + u(i)*u(i)
90         continue
      
        norm = csqrt(norm)
      
        do 70 i=1,3
                u(i) = u(i)/norm
70         continue
      
        return

      endif

      if(mode .EQ. 3) then

*       computes Quasi-S polarization
      
        if(cabs(pa2) .LT. 1.e-6*cabs(pr2)) then
*       propagation within fracture planes: polarization is
*       along the axis (epss -> - infinity).
        
                u(1) = axis(1)
                u(2) = axis(2)
                u(3) = axis(3)

        else if(cabs(pr2) .LT. 1.e-6*cabs(pa2)) then
*       propagation is along axis: polarization has one degree of
*       freedom, but must be orthogonal to S-parallel so there
*       is no ambiguity after all.

                u0(1) = 0.e0
                u0(2) = axis(3)
                u0(3) = -axis(2)
                dot = sqrt(axis(2)*axis(2)+axis(3)*axis(3))
                if(dot .NE. 0.e0) then
                        u0(2) = u0(2)/dot
                        u0(3) = u0(3)/dot
                else
                        u0(1) = -axis(3)
                        u0(2) = 0.e0
                        u0(3) = axis(1)
                        dot = sqrt(axis(1)*axis(1)+
     &                          axis(3)*axis(3))
                        if(dot .NE. 0.e0) then
                                u0(1) = u0(1)/dot
                                u0(3) = u0(3)/dot
                        else
                                dot = sqrt(axis(2)*axis(2)+
     &                                  axis(1)*axis(1))
                                u0(1) = axis(2)/dot
                                u0(2) = -axis(1)/dot
                                u0(3) = 0.e0
                        endif
                endif
                
                u(1) = -p(2)*u0(3)+p(3)*u0(2)
                u(2) = -p(3)*u0(1)+p(1)*u0(3)
                u(3) = -p(1)*u0(2)+p(2)*u0(1)
                
        else
                epss = (F+L)*pr2/((alpha-beta)/2.e0-L*pr2-C*pa2)

                do110 i=1,3
                        u(i) = pr(i) + epss*pa(i)
110                continue
      
        endif   

        norm = cmplx(0.e0,0.e0)
      
        do130 i=1,3
                norm = norm + u(i)*u(i)
130        continue
      
        norm = csqrt(norm)
      
        do150 i=1,3
                u(i) = u(i)/norm
150        continue

        return

      endif

      return
      end



      subroutine cray(g1,g2,ipath,disp,t,error)

      complex disp(3)
      real g1, g2, t
      integer ipath,error

*
*   traces one raypath from source to receiver
*
*   INPUT VARIABLES:
*
*       g1              ray azimuthal take-off angle (degrees)
*       g2              ray dip take-off angle (degrees)
*       ipath           raypath index
*
*   OUTPUT VARIABLES:
*
*       disp(3)         total complex displacement at receiver
*       t               arrival time at receiver
*       error           0 if no error,
*                       1 if receiver is not illuminated etc...
*
*   GLOBAL VARIABLES:
*
*       block "model"   elastic and geometric model parameters
*       block "array"   source-receiver configuration
*       block "path"    raypath parameters
*       block "output"  graphics/verbose parameters
*       block "io"      input/output logical unit numbers
*
*   EXTERNAL ROUTINES:
*
*       shootmap        creates kinematic interpolation map
*       cinitray        initializes ray parameters at the source
*       ctracr          traces a ray between two interfaces
*       ccrossbound        continues ray parameters through interfaces
*       cendray         incorporates free surface effects to the 
*                       field at the receiver.
*


*      ray color
      parameter (icolor=2)


      real param(900), geom(0:99), dt, scale, xmin, ymin
      real xs, ys, cazi, cdip, cg1, dx, cr, cdg, ail, acl, xr, yr
      integer nlayer, nsegment(200), mode(200,100), tsamp, trace
      integer interface(200,0:100), triso(100), ng, npath, verbose
      integer stdin, stderr, stdout, temp1, temp2, temp3
      common /model/ param, geom, nlayer, triso
      common /path/ mode, interface, nsegment, npath
      common /array/ xs,ys,cazi,cdip,cr,cdg,xr,yr,cg1,
     &               ail,acl,dx,ng,tsamp,dt
      common /output/ verbose, trace, scale, xmin, ymin
      common /io/ stdin, stdout, stderr, temp1, temp2, temp3
      
      real p0(3), p10(3), p20(3), p30(3), p40(3), x0(3), x10(3)
      real x20(3), x30(3), x40(3), t0, t10, t20, t30, t40
      real p(3), p1(3), p2(3), p3(3), p4(3), x(3), x1(3)
      real x2(3), x3(3), x4(3), t1, t2, t3, t4
      real j0, j, xp, yp, zp
      complex amp10, amp20, amp1, amp2
      integer i, iseg


      error = 0

      if(trace .EQ. 1) then
        write(stdout,*) nsegment(ipath)+1, icolor
      endif

      call cinitray(cazi,cdip,mode(ipath,1),g1,g2,cr,cdg,x0,x10,x20,
     &   x30,x40,p0,p10,p20,p30,p40,t0,t10,t20,t30,t40,j0,amp10,amp20)

      if(trace .EQ. 1) then
        xp = xs-xmin
	yp = ys-ymin
	zp = 0. 
        write(stdout,*) xp,yp,zp
      endif

      do10 iseg=1,nsegment(ipath)-1

        call ctracr(p0,p10,p20,p30,p40,x0,x10,x20,x30,x40,t0,t10,
     &          t20,t30,t40,j0,amp10,amp20,interface(ipath,iseg-1),
     &          interface(ipath,iseg),mode(ipath,iseg),x,x1,x2,x3,x4,t,
     &          t1,t2,t3,t4,j,amp1,amp2)

      if(trace .EQ. 1) then
        xp = x(1)+xs-xmin
	yp = x(2)+ys-ymin
        zp = x(3)
        write(stdout,*) xp,yp,zp
      endif

        call ccrossbound(p0,p10,p20,p30,p40,j,amp1,amp2,iseg,ipath,
     &          p,p1,p2,p3,p4,j0,amp10,amp20,error)

        if(error .NE. 0) then
                return
        endif

        t0 = t
        t10 = t1
        t20 = t2
        t30 = t3
        t40 = t4

        do20 i=1,3
                x0(i) = x(i)
                x10(i) = x1(i)
                x20(i) = x2(i)
                x30(i) = x3(i)
                x40(i) = x4(i)
                p0(i) = p(i)
                p10(i) = p1(i)
                p20(i) = p2(i)
                p30(i) = p3(i)
                p40(i) = p4(i)
20         continue
      
10      continue

      call ctracr(p0,p10,p20,p30,p40,x0,x10,x20,x30,x40,t0,t10,t20,
     &   t30,t40,j0,amp10,amp20,interface(ipath,nsegment(ipath)-1),
     &   interface(ipath,nsegment(ipath)),mode(ipath,nsegment(ipath)),
     &   x,x1,x2,x3,x4,t,t1,t2,t3,t4,j,amp1,amp2)

      if(trace .EQ. 1) then
        xp = x(1)+xs-xmin
	yp = x(2)+ys-ymin
        zp = x(3)
        write(stdout,*) xp,yp,zp
      endif

      call cendray(amp1,amp2,p0,mode(ipath,nsegment(ipath)),disp,error)

      return
      end



      subroutine croskin(p0,inc,scat,smode,p,error)

      real p0(3), p(3)
      integer inc, scat, smode, error

*
*   This routine continues the ray parameters through a boundary
*
*   INPUT VARIABLES:
*
*       p0(3)   incident ray slowness
*       inc     incident layer index
*       scat    scattering layer index
*       smode   scattered wave type: 1 for SP, 2 for QP, 3 for QS
*
*   OUTPUT VARIABLES:
*
*       p(3)    scattered ray slowness
*       error   0 for no error. 1 if postcritical scattering occurs.
*
*   GLOBAL VARIABLES:
*
*       block "model"   geometric and elastic model parameters.
*
*   EXTERNAL ROUTINES:
*
*       scatslow        real scattering slowness finder
*

      integer nlayer, triso(100)
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso

      real elasti(6), elasts(6), axisi(3), axiss(3), normal(3)
      integer i,ii, is, which(2)

*   initializes some useful variables

      error = 0
      ii = (inc-1)*9
      is = (scat-1)*9

*   load in proper parameters

      do10 i=1,6
        elasti(i) = param(ii+i)
        elasts(i) = param(is+i)
10      continue

      do20 i=1,3
        axisi(i) = param(ii+i+6)
        axiss(i) = param(is+i+6)
20      continue

      which(1) = smode
      normal(1) = 0.
      normal(2) = 0.

*   normal unit vector must be pointing towards incident medium

      if(inc .LT. scat) then
        normal(3) = -1.
        which(2) = 2
      else if(inc .GT. scat) then
        normal(3) = 1.
        which(2) = 2
      else if(p0(3) .GE. 0.) then
        normal(3) = -1.
        which(2) = 1
      else
        normal(3) = 1.
        which(2) = 1
      endif

*   compute the scattered slowness

      call scatslow(p0,normal,axiss,which,elasts,p,error)

      return
      end



      subroutine cscatslow(pinc,iscat,normal,axis,elast,
     &                     pscat1,pscat2,pscat3,error)
      real pinc(3), normal(3), axis(3), elast(6)
      complex pscat1(3), pscat2(3), pscat3(3)
      integer error

*
*   computes the complex scattering slowness of the given mode.
*
*   INPUT VARIABLES:
*   
*       pinc(3)         incident slowness (real)
*       iscat           0 for reflection, 1 for transmission
*       normal(3)       normal unit vector to interface
*                       pointing towards incident medium.
*       axis(3)         anisotropy direction in scattering medium
*       elast(6)        array of elastic coefficients in scattering
*                       medium: A, C, N, L, F, rho
*
*   OUTPUT VARIABLES:
*
*       pscat1(3)       scattered slowness SP mode (complex)
*       pscat2(3)       scattered slowness QP mode (complex)
*       pscat3(3)       scattered slowness QS mode (complex)
*       error           0 if algorithm converged
*                       1 if root finding algorithm did not converge
*                       2 if QP and QS roots could not be separated
*                       3 if triplication occured
*
*   EXTERNAL ROUTINES:
*
*       roots           root finder
*       slowness        real slowness evaluator
*

      real t(3), ptan, ptan2, vtan(3)
      complex del, del1, del2, z1, z2, z3, z4, poly(5), pnorm
      integer iscat, sign, isol, ip, is, i, ireal
      complex pa2, pr2, alpha, beta, zp1, zs1, zp2, zs2, z(4), zz
      real AL, CL, LAF, ca, dot1, dot2, p1(3), p2(3), w1(3), w2(3)
      real A, C, N, L, F, rho, dot, pinc1(3), pinc2(3)
      real pinc3(3), na, ta, na1, ta1, na2, ta2, c2, c1, c0

      error = 0


*   transfers data to more explicit variables

      A = elast(1)
      C = elast(2)
      N = elast(3)
      L = elast(4)
      F = elast(5)
      rho = elast(6)

*   computes tangential slowness and tangent direction

      dot = 0.e0

      do10 i=1,3
        dot = dot + pinc(i)*normal(i)
10      continue

      ptan2 = 0.e0

      do20 i=1,3
        vtan(i) = pinc(i) - dot*normal(i)
        ptan2 = ptan2 + vtan(i)*vtan(i)
20      continue

      dot = pinc(1)*pinc(1)+pinc(2)*pinc(2)+pinc(3)*pinc(3)     
      ptan = sqrt(ptan2)

*       gets rid of trivial case (normal incidence)

      if(ptan2/dot .EQ. 0.e0) then

        if(iscat .EQ. 0) then
                sign = 1
        else
                sign = -1
        endif

        do40 i=1,3
                pinc1(i) = sign*normal(i)
                pinc2(i) = sign*normal(i)
                pinc3(i) = sign*normal(i)
40         continue

        call slowness(pinc1,axis,1,elast)
        call slowness(pinc2,axis,2,elast)
        call slowness(pinc3,axis,3,elast)

        do50 i=1,3
                pscat1(i) = cmplx(pinc1(i),0.e0)
                pscat2(i) = cmplx(pinc2(i),0.e0)
                pscat3(i) = cmplx(pinc3(i),0.e0)
50         continue

        return

      endif

      do30 i=1,3
        t(i) = vtan(i)/ptan
30      continue

*   computes some usefull quantities for later use

      na = 0.e0

      do60 i=1,3
        na = na + normal(i)*axis(i)
60      continue

      ta = 0.e0

      do70 i=1,3
        ta = ta + t(i)*axis(i)
70      continue

      na2 = na*na
      ta2 = ta*ta

      na1 = 1.e0 - na2
      ta1 = 1.e0 - ta2

*   SP case (solved analytically)

        c2 = L*na2 + N*na1
        c1 = 2.e0*ptan*ta*na*(L-N)
        c0 = ptan2*(L*ta2+N*ta1)-rho

        del = csqrt(cmplx(c1*c1-4.e0*c2*c0,0.e0))
        z1 = (-c1+del)/c2/2.e0
        z2 = (-c1-del)/c2/2.e0

*       determines appropriate solution

        ca = cabs(z1)

        if(ca .EQ. 0.0) then

                pnorm = cmplx(0.,0.)

        else if(abs(aimag(z1))/ca .LE. 1.e-6) then

*       scattering is real: test for scattering direction

                do888 i=1,3
                        p1(i) = vtan(i)+real(z1)*normal(i)
                        p2(i) = vtan(i)+real(z2)*normal(i)
888                continue
                call groupvel(p1,axis,1,elast,w1)
                call groupvel(p2,axis,1,elast,w2)
                dot1 = 0.e0
                dot2 = 0.e0
                do999 i=1,3
                        dot1 = dot1 + w1(i)*normal(i)
                        dot2 = dot2 + w2(i)*normal(i)
999                continue

                if(iscat .EQ. 0) then
                        if(dot1 .GT. 0.e0) then
                                pnorm = z1
                        else
                                pnorm = z2
                        endif
                else
                        if(dot1 .GT. 0.e0) then
                                pnorm = z2
                        else
                                pnorm = z1
                        endif
                endif

        else

*       scattering is complex: test for decay

                if(iscat .EQ. 0) then
                        if(aimag(z1) .GT. 0.e0) then
                                pnorm = z1
                        else
                                pnorm = z2
                        endif
                else
                        if(aimag(z1) .GT. 0.e0) then
                                pnorm = z2
                        else
                                pnorm = z1
                        endif
                endif

        endif

      do100 i=1,3
        pscat1(i) = vtan(i)+pnorm*normal(i)
100      continue


*   QP and QS cases (numerical solution)

*   computes four scattered slownesses (roots of eikonal determinant)

      AL = A*L
      CL = C*L
      LAF = L*L+A*C-(F+L)*(F+L)

      poly(5) = AL*na1*na1 + CL*na2*na2 + LAF*na2*na1
      poly(4) = 2.e0*ptan*na*ta*(LAF*(1.e0-2.e0*na2)
     &          +2.e0*(CL*na2-AL*na1))
      poly(3) = 6.e0*CL*na2*ta2+2.e0*AL*(ta1*na1+2.e0*ta2*na2)
      poly(3) = poly(3)+LAF*(na2+ta2-6.e0*na2*ta2)
      poly(3) = poly(3)*ptan2-rho*((C+L)*na2+(A+L)*na1)
      poly(2) = 2.e0*(CL*ta2-AL*ta1)+LAF*(1.e0-2.e0*ta2)
      poly(2) = poly(2)*ptan2+rho*(A-C)
      poly(2) = poly(2)*2.e0*ptan*na*ta
      poly(1) = CL*ta2*ta2+AL*ta1*ta1+LAF*ta2*ta1
      poly(1) = poly(1)*ptan2-rho*((L+C)*ta2+(L+A)*ta1)
      poly(1) = poly(1)*ptan2+rho*rho

      call roots(poly,z1,z2,z3,z4,isol)

      z(1) = z1
      z(2) = z2
      z(3) = z3
      z(4) = z4

      if(isol .EQ. 0) then
        error = 1
        return
      endif

*   determines number of real roots and reshuffles them 
*   so that if there are any real roots, they are at the 
*   first ranks in the array.

      ireal = 0

      do111 i=1,4
        ca = cabs(z(i))
        if(ca .EQ. 0.e0) then
                ca = 1.e0
        endif
        if(abs(aimag(z(i)))/ca .LE. 1.e-4) then
                zz = z(1)
                z(1) = z(i)
                z(i) = zz
                ireal = ireal + 1
                goto 222
        endif
111      continue

222   do333 i=2,4
        ca = cabs(z(i))
        if(ca .EQ. 0.e0) then
                ca = 1.e0
        endif
        if(abs(aimag(z(i)))/ca .LE. 1.e-4) then
                zz = z(2)
                z(2) = z(i)
                z(i) = zz
                ireal = ireal + 1
        endif
333      continue          

      if(ireal .EQ. 1 .OR. ireal .EQ. 3) then

*   An odd number of real roots, something is wrong.
        error = 2
        return
      endif

      if(ireal .EQ. 4) then

*   There is real QP and QS scattering

      ip = 0
      is = 0

      pa2 = 2.e0*ptan*na*ta*real(z(1))
      pr2 = pa2
      pa2 = pa2+ptan2*ta2+real(z(1))*real(z(1))*na2
      pr2 = ptan2*ta1-pr2+real(z(1))*real(z(1))*na1

      alpha = (A+L)*pr2+(C+L)*pa2
      beta = (A-L)*pr2-(C-L)*pa2
      beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
      beta = csqrt(beta)

      del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
      del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

      if(cabs(del1) .LE. cabs(del2)) then
        zp1 = real(z(1))
        ip = ip+1               
      else
        zs1 = real(z(1))
        is = is+1
      endif

      pa2 = 2.e0*ptan*na*ta*real(z(2))
      pr2 = pa2
      pa2 = pa2+ptan2*ta2+real(z(2))*real(z(2))*na2
      pr2 = ptan2*ta1-pr2+real(z(2))*real(z(2))*na1

      alpha = (A+L)*pr2+(C+L)*pa2
      beta = (A-L)*pr2-(C-L)*pa2
      beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
      beta = csqrt(beta)

      del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
      del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

      if(cabs(del1) .LE. cabs(del2)) then
        if(ip .EQ. 1) then
                zp2 = real(z(2))
        else
                zp1 = real(z(2))
        endif
        ip = ip+1
      else
        if(is .EQ. 1) then
                zs2 = real(z(2))
        else
                zs1 = real(z(2))
        endif
        is = is+1
      endif

      if(ip .EQ. 2) then
        zs1 = real(z(3))
        zs2 = real(z(4))
      else if(is .EQ. 2) then
        zp1 = real(z(3))
        zp2 = real(z(4))
      else if(is .EQ. 1 .AND. ip .EQ. 1) then
        pa2 = 2.e0*ptan*na*ta*real(z(3))
        pr2 = pa2
        pa2 = pa2+ptan2*ta2+real(z(3))*real(z(3))*na2
        pr2 = ptan2*ta1-pr2+real(z(3))*real(z(3))*na1

        alpha = (A+L)*pr2+(C+L)*pa2
        beta = (A-L)*pr2-(C-L)*pa2
        beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
        beta = csqrt(beta)

        del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
        del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

        if(cabs(del1) .LE. cabs(del2)) then
                zp2 = real(z(3))
                ip = ip+1
        else
                zs2 = real(z(3))
                is = is+1
        endif

        if(ip .EQ. 2) then
                zs2 = real(z(4))
        else
                zp2 = real(z(4))
        endif
      else
        error = 2
        return
      endif
      
*   finds the correct root according to mode and scattering type

*   QP slowness is real: test for scattering direction

        do444 i=1,3
                p1(i) = vtan(i)+real(zp1)*normal(i)
                p2(i) = vtan(i)+real(zp2)*normal(i)
444        continue
        call groupvel(p1,axis,2,elast,w1)
        call groupvel(p2,axis,2,elast,w2)
        dot1 = 0.e0
        dot2 = 0.e0
        do555 i=1,3
                dot1 = dot1 + w1(i)*normal(i)
                dot2 = dot2 + w2(i)*normal(i)
555        continue

        if(iscat .EQ. 0) then
                if(dot1 .GT. 0.e0) then
                        pnorm = zp1
                else if(dot2 .GT. 0.e0) then
                        pnorm = zp2
                else
*               no reflected wave
                        error = 3
                        return
                endif
        else
                if(dot1 .GT. 0.e0) then
                        pnorm = zp2
                else if(dot2 .GT. 0.e0) then
                        pnorm = zp1
                else
*               no transmitted wave
                        error = 3
                        return
                endif
        endif

      do200 i=1,3
        pscat2(i) = vtan(i)+pnorm*normal(i)
200   continue

*   QS slowness is real: test for scattering direction

        do666 i=1,3
                p1(i) = vtan(i)+real(zs1)*normal(i)
                p2(i) = vtan(i)+real(zs2)*normal(i)
666        continue

        call groupvel(p1,axis,3,elast,w1)
        call groupvel(p2,axis,3,elast,w2)

        dot1 = 0.e0
        dot2 = 0.e0

        do777 i=1,3
                dot1 = dot1 + w1(i)*normal(i)
                dot2 = dot2 + w2(i)*normal(i)
777        continue

        if(iscat .EQ. 0) then
                if(dot1 .GT. 0.e0) then
                        pnorm = zs1
                else if(dot2 .GT. 0.e0) then
                        pnorm = zs2
                else
*               no reflected wave
                        error = 3
                        return
                endif
        else
                if(dot1 .GT. 0.e0) then
                        pnorm = zs2
                else if(dot2 .GT. 0.e0) then
                        pnorm = zs1
                else
*               no transmitted wave
                        error = 3
                        return
                endif
        endif

        do300 i=1,3
          pscat3(i) = vtan(i)+pnorm*normal(i)
300     continue

        return

      endif

      if(ireal .EQ. 2) then

*   check that the real roots are associated with the QS wave

        ip = 0
        is = 0

        pa2 = 2.e0*ptan*na*ta*real(z(1))
        pr2 = pa2
        pa2 = pa2+ptan2*ta2+real(z(1))*real(z(1))*na2
        pr2 = ptan2*ta1-pr2+real(z(1))*real(z(1))*na1

        alpha = (A+L)*pr2+(C+L)*pa2
        beta = (A-L)*pr2-(C-L)*pa2
        beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
        beta = csqrt(beta)

        del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
        del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

        if(cabs(del1) .LE. cabs(del2)) then
          zp1 = real(z(1))
          ip = ip+1               
        else
          zs1 = real(z(1))
          is = is+1
        endif

        pa2 = 2.e0*ptan*na*ta*real(z(2))
        pr2 = pa2
        pa2 = pa2+ptan2*ta2+real(z(2))*real(z(2))*na2
        pr2 = ptan2*ta1-pr2+real(z(2))*real(z(2))*na1

        alpha = (A+L)*pr2+(C+L)*pa2
        beta = (A-L)*pr2-(C-L)*pa2
        beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
        beta = csqrt(beta)

          del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
          del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

        if(cabs(del1) .LE. cabs(del2)) then
          if(ip .EQ. 1) then
            zp2 = real(z(2))
          else
            zp1 = real(z(2))
          endif
          ip = ip+1
        else
          if(is .EQ. 1) then
            zs2 = real(z(2))
          else
            zs1 = real(z(2))
          endif
          is = is+1
        endif

        if(is .NE. 2) then

          error=2
          return

        else

*   QS slowness is real: test for scattering direction

          do1666 i=1,3
            p1(i) = vtan(i)+real(zs1)*normal(i)
            p2(i) = vtan(i)+real(zs2)*normal(i)
1666      continue

          call groupvel(p1,axis,3,elast,w1)
          call groupvel(p2,axis,3,elast,w2)

          dot1 = 0.e0
          dot2 = 0.e0

          do1777 i=1,3
            dot1 = dot1 + w1(i)*normal(i)
            dot2 = dot2 + w2(i)*normal(i)
1777      continue

        if(iscat .EQ. 0) then
          if(dot1 .GT. 0.e0) then
            pnorm = zs1
          else if(dot2 .GT. 0.e0) then
            pnorm = zs2
          else
*           no reflected wave
            error = 3
            return
          endif
        else
          if(dot1 .GT. 0.e0) then
            pnorm = zs2
          else if(dot2 .GT. 0.e0) then
            pnorm = zs1
          else
*           no transmitted wave
            error = 3
            return
          endif
        endif
        
      endif

      do1300 i=1,3
        pscat3(i) = vtan(i)+pnorm*normal(i)
1300  continue

*   QP scattering is complex: test for decay
*   bearing in mind that we are considering positive
*   frequencies(!).

        if(iscat .EQ. 0) then
                if(aimag(z(3)) .GT. 0.) then
                        pnorm = z(3)
                else
                        pnorm = z(4)
                endif
        else
                if(aimag(z(3)) .GT. 0.) then
                        pnorm = z(4)
                else
                        pnorm = z(3)
                endif
        endif

        do1200 i=1,3
          pscat2(i) = vtan(i)+pnorm*normal(i)
1200    continue

        return
 
      endif

      if(ireal .EQ. 0) then

*   QP and QS modes are both complex: reshuffle the roots 
*   so that the two with positive imaginary parts are in front ranks

        do1111 i=1,4
          if(aimag(z(i)) .GT. 0.e0) then
            zz = z(1)
            z(1) = z(i)
            z(i) = zz
            goto 1222
          endif
1111    continue

1222    do1333 i=2,4
           if(aimag(z(i)) .GT. 0.e0) then
             zz = z(2)
             z(2) = z(i)
             z(i) = zz
           endif
1333    continue          

        if(iscat .EQ. 0.e0) then

*   The evanescent slownesses have positive imaginary part.
*   Now discriminate between QP and QS:

        
          pa2 = 2.e0*ptan*na*ta*z(1)
          pr2 = pa2
          pa2 = pa2+ptan2*ta2+z(1)*z(1)*na2
          pr2 = ptan2*ta1-pr2+z(1)*z(1)*na1
    
          alpha = (A+L)*pr2+(C+L)*pa2
          beta = (A-L)*pr2-(C-L)*pa2
          beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
          beta = csqrt(beta)

          del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
          del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

          if(cabs(del1) .LE. cabs(del2)) then
            zp1 = z(1)
            zs1 = z(2)
          else
            zp1 = z(2)
            zs1 = z(1)
          endif

          do2300 i=1,3
            pscat3(i) = vtan(i)+zs1*normal(i)
2300      continue

          do2200 i=1,3
              pscat2(i) = vtan(i)+zp1*normal(i)
2200      continue

          return

        else

*   The evanescent slownesses have negative imaginary part.
*   Now discriminate between QP and QS:

        
          pa2 = 2.e0*ptan*na*ta*z(3)
          pr2 = pa2
          pa2 = pa2+ptan2*ta2+z(3)*z(3)*na2
          pr2 = ptan2*ta1-pr2+z(3)*z(3)*na1
    
          alpha = (A+L)*pr2+(C+L)*pa2
          beta = (A-L)*pr2-(C-L)*pa2
          beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
          beta = csqrt(beta)

          del1 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha+beta)
          del2 = cmplx(1.e0,0.e0) - 2.e0*rho/(alpha-beta)

          if(cabs(del1) .LE. cabs(del2)) then
            zp1 = z(3)
            zs1 = z(4)
          else
            zp1 = z(4)
            zs1 = z(3)
          endif

          do3300 i=1,3
            pscat3(i) = vtan(i)+zs1*normal(i)
3300      continue

          do3200 i=1,3
              pscat2(i) = vtan(i)+zp1*normal(i)
3200      continue

          return

        endif

      endif

      return
 
      end



      subroutine cshotprof()

*
*   generates a shotprofile over a tabular transversely 
*   isotropic medium.
*
*   GLOBAL VARIABLES:
*
*       block "model"   elastic and geometric model parameters
*       block "array"   source-receiver configuration
*       block "path"    raypath parameters
*       block "output"  graphics/verbose parameters
*       block "io"      input/output logical unit numbers
*
*   OUTPUT FILES:
*
*       uh1.dat         first horizontal displacement
*       uh2.dat         second horizontal displacement
*       uv.dat          vertical displacement
*

*     initializing a few parameters:
*        maxtrace, maxsample: max numbers of traces and time samples
*                              on a shot record
*        maxrad, maxang: maxrad*(maxang-2) is the maximum number of 
*                        points in the interpolation map. maxang must 
*                        be more than 3 and less than 100000.
*        difdip: initial increment for take-off dip angle


      integer maxtrace, maxsample
      integer maxrad, maxang
      real difdip

      parameter(maxtrace=200, maxsample=2048)
      parameter(maxang=43, maxrad=1000)
      parameter(difdip=1.e0)

*      layer boundary color
      parameter(lcolor=4)

*     variable declaration

      real param(900), geom(0:99), dt, scale, xmin, ymin
      real xs, ys, cazi, cdip, cg1, dx, cr, cdg, ail, acl, xr, yr
      real wave(255), hilwav(255), fl, fh
      integer nlayer, nsegment(200), mode(200,100), tsamp, trace
      integer interface(200,0:100), triso(100), ng, npath, verbose
      integer nsweep, ntaper, leng
      integer stdin, stderr, stdout, temp1, temp2, temp3
      common /model/ param, geom, nlayer, triso
      common /path/ mode, interface, nsegment, npath
      common /array/ xs,ys,cazi,cdip,cr,cdg,xr,yr,cg1,
     &               ail,acl,dx,ng,tsamp,dt
      common /output/ verbose, trace, scale, xmin, ymin
      common /wavelet/ nsweep, ntaper, leng, fl, fh, wave, hilwav
      common /io/ stdin, stdout, stderr, temp1, temp2, temp3

      complex disp(3), tempil, tempcl
      real uil(maxtrace,maxsample), ucl(maxtrace,maxsample)
      real uv(maxtrace,maxsample), sg2(maxrad), dg2
      real deg2rad, dxx, dyy, cail, sail, cacl, sacl, xp, yp, zp
      real x, y, g1, g2, t, xx(maxang,maxrad), yy(maxang,maxrad)
      real xmax, ymax, sg1(maxang), ddip
      integer ipath, ig, error, time, j, imax, imin, ii, i, ng2
      integer maxa, maxr

      call getoutput()
      call getmodel()
      call getarray()
      call getpath()

*     checks record parameters against max array bounds

      write(stderr,"(/,' *** RUNNING ...',/)")

      if(ng .GT. maxtrace) then
	    stop ' chshotprof: ng too large'
      endif

      if(tsamp .GT. maxsample) then
	    stop ' chshotprof: tsamp too large'
      endif

      deg2rad = 1.7453292e-2

      dxx = dx*cos(cg1*deg2rad)
      dyy = dx*sin(cg1*deg2rad)
      cail = cos(ail*deg2rad)
      sail = sin(ail*deg2rad)
      cacl = cos(acl*deg2rad)
      sacl = sin(acl*deg2rad)

      if(trace .EQ. 1) then

*        compute some graphics parameters
         xmin = amin1(xr,xr+(ng-1)*dxx,xs)
         ymin = amin1(yr,yr+(ng-1)*dyy,ys)
         xmax = amax1(xr,xr+(ng-1)*dxx,xs)
         ymax = amax1(yr,yr+(ng-1)*dyy,ys)

*        trace interfaces

         do1000 j=1,nlayer

            write(stdout,*) 5, lcolor

            xp = 0.
            yp = 0.
            zp = geom(j-1)
            write(stdout,*) xp,yp,zp

            xp = xmax-xmin
            write(stdout,*) xp,yp,zp

            yp = ymax-ymin
            write(stdout,*) xp,yp,zp

            xp = 0.
            write(stdout,*) xp,yp,zp

            yp = 0.
            write(stdout,*) xp,yp,zp

1000     continue

      endif

      do10 ipath=1,npath

         if(verbose .EQ. 1) 
     &      write(stderr,"(/,' *** RAYPATH #',i3,':',/)") ipath

         if(verbose .EQ. 1) write(stderr,"('   map started...')")

         maxr = maxrad
         maxa = maxang
         ddip = difdip

         call shootmap(xx,yy,sg1,sg2,ipath,ng2,maxr,maxa,ddip)

         if(verbose .EQ. 1) 
     &      write(stderr,"('              ...map completed',/)")

         x = xr-xs
         y = yr-ys

         do20 ig=1,ng

            if(verbose .EQ. 1) write(stderr,"('    receiver #',i3)") ig

            call search(xx,yy,x,y,g1,g2,dg2,sg1,sg2,error,
     &                    ng2,maxa)

*           updates raytube angular aperture

            cdg = amin1(ddip/10.e0,dg2)

            if(error .NE. 0) then
               if(verbose .EQ. 1 .AND. error .EQ. 1) then
                  write(stderr,"(' ...receiver #',i3,': offset
     &            too large (out of map).',/)") ig
                  goto 101
               else if(verbose .EQ. 1 .AND. error .EQ. 2) then
                  write(stderr,"(' ...receiver #',i3,':
     &            no real ray to that receiver.',/)") ig
                  goto 101
               endif
            endif

            call cray(g1,g2,ipath,disp,t,error)

            if(error .NE. 0 .AND. verbose .EQ. 1) then
               write(stderr,"(' ...receiver #',i3,
     &         ': undefined scattering.',/)") ig
            endif

            time = t/dt

            tempil = disp(1)*cail+disp(2)*sail
            tempcl = disp(1)*cacl+disp(2)*sacl

            imin = max0(1,time-leng/2)
            imax = min0(tsamp,time+leng/2)


            do100 i=imin,imax

               ii = i-time+(leng+1)/2

               uil(ig,i) = uil(ig,i)+real(tempil)
     &             *wave(ii)+aimag(tempil)*hilwav(ii)

               ucl(ig,i) = ucl(ig,i)+real(tempcl)
     &             *wave(ii)+aimag(tempcl)*hilwav(ii)

               uv(ig,i) = uv(ig,i)+real(disp(3))
     &             *wave(ii) +aimag(disp(3))*hilwav(ii)

100         continue

101         x = x+dxx
            y = y+dyy

20       continue

10    continue


      if(verbose .EQ. 1) 
     &  write(stderr,"(/,'    writing output to files...')")

      open(unit=temp1,file='uh1.dat',status='unknown',
     &     form='unformatted')
      rewind(temp1)
      open(unit=temp2,file='uh2.dat',status='unknown',
     &     form='unformatted')
      rewind(temp2)
      open(unit=temp3,file='uv.dat',status='unknown',
     &     form='unformatted')
      rewind(temp3)

      do30 ig=1,ng
         write(temp1) (uil(ig,j),j=1,tsamp)
         write(temp2) (ucl(ig,j),j=1,tsamp)
         write(temp3) (uv(ig,j),j=1,tsamp)
30    continue

      close(temp1)
      close(temp2)
      close(temp3)

      if(verbose .EQ. 1) write(stderr,"(27x,'...done',/)")
      if(verbose .EQ. 1) 
     &  write(stderr,"('             *** END OF JOB ***',/)")

      return
      end



      subroutine ctracr(p0,p10,p20,p30,p40,x0,x10,x20,x30,x40,
     &   t0,t10,t20,t30,t40,j0,amp10,amp20,start,end,mode,
     &   x,x1,x2,x3,x4,t,t1,t2,t3,t4,j,amp1,amp2)

      real p0(3), p10(3), p20(3), p30(3), p40(3), x0(3), x10(3)
      real x20(3), x30(3), x40(3), t0, t10, t20, t30, t40
      real j0, x(3), x1(3), x2(3), x3(3), x4(3), t, t1, t2, t3, t4
      real w0(3), w10(3), w20(3), w30(3), w40(3), j
      complex amp10, amp20, amp1, amp2
      integer mode, start, end

*
*   This routine traces a ray from one interface to the next.
*
*   INPUT VARIABLES:
*
*       p0(3)   ray slowness vector
*       p10(3)  first tube-ray slowness
*       p20(3)  second   "    "    "
*       p30(3)  third    "    "    "
*       p40(3)  fourth   "    "    "
*       x0(3)   ray starting point
*       x10(3)  first tube-ray starting point
*       x20(3)  second    "    "    "   "
*       x30(3)  third     "    "    "   "
*       x40(3)  fourth    "    "    "   "
*       t0      ray initial traveltime
*       t10     first tube-ray initial traveltime
*       t20     second    "    "    "   "
*       t30     third     "    "    "   "
*       t40     fourth    "    "    "   "
*       j0      initial ray jacobian
*       amp10   initial amplitude
*       amp20   initial QS amplitude if medium is isotropic
*               and mode is S (mode=1).
*       start   index of starting interface
*       end     index of destination interface
*       mode    type of wave: 1 for SP, 2 for QP, 3 for QS
*
*   OUTPUT VARIABLES:
*
*       x(3)    ray destination point
*       x1(3)   first tube ray destination point
*       x2(3)   second    "    "    "    " 
*       x3(3)   third     "    "    "    "
*       x4(3)   fourth    "    "    "    "
*       t       ray final traveltime
*       t1      first tube ray final traveltime
*       t2      second    "    "    "    "
*       t3      third     "    "    "    "
*       t4      fourth    "    "    "    "
*       j       final ray jacobian
*       amp1    final amplitude
*       amp2    final QS amplitude if medium is isotropic
*               and mode is S (mode=1).
*   
*   GLOBAL VARIABLES:
*
*       block "model"   geometric and elastic model parameters
*
*   EXTERNAL ROUTINES:
*
*       groupvel        group velocity calculator
*

      integer nlayer, triso(100)
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso

      real axis(3), elast(6), spread
      integer i,ii,layer

      layer = max0(start,end)
      ii = (layer-1)*9

      do10 i=1,6
        elast(i) = param(ii+i)
10      continue

      do20 i=1,3
        axis(i) = param(ii+i+6)
20      continue

*   compute the five ray directions

      call groupvel(p0,axis,mode,elast,w0)
      call groupvel(p10,axis,mode,elast,w10)
      call groupvel(p20,axis,mode,elast,w20)
      call groupvel(p30,axis,mode,elast,w30)
      call groupvel(p40,axis,mode,elast,w40)

*   compute the arrival point for the central ray

      x(3) = geom(end)
      t = t0 + (x(3)-x0(3))/w0(3)
      x(1) = x0(1)+w0(1)*(t-t0)
      x(2) = x0(2)+w0(2)*(t-t0)

*   compute the jacobian and amplitude at the arrival point

*   trace the four extra rays to traveltime t

      do30 i=1,3
        x1(i) = x10(i)+w10(i)*(t-t10)
        x2(i) = x20(i)+w20(i)*(t-t20)
        x3(i) = x30(i)+w30(i)*(t-t30)
        x4(i) = x40(i)+w40(i)*(t-t40)
30      continue

*   compute the jacobian itself from four tube-rays
      
      j = w0(1)*((x2(2)-x1(2))*(x4(3)-x3(3))
     &    -(x2(3)-x1(3))*(x4(2)-x3(2)))
      j = j-w0(2)*((x2(1)-x1(1))*(x4(3)-x3(3))
     &    -(x2(3)-x1(3))*(x4(1)-x3(1)))
      j = j+w0(3)*((x2(1)-x1(1))*(x4(2)-x3(2))
     &    -(x2(2)-x1(2))*(x4(1)-x3(1)))
      j = abs(j)

*   compute the amplitude

      if(j .EQ. 0.) then
        return
      endif

      spread = sqrt(abs(j0/j))
      amp1 = amp10*spread
      amp2 = amp20*spread


*      continue the four extra rays all the way to the interface

      x1(3) = geom(end)
      x2(3) = geom(end)
      x3(3) = geom(end)
      x4(3) = geom(end)

      t1 = t10+(x1(3)-x10(3))/w10(3)
      t2 = t20+(x2(3)-x20(3))/w20(3)
      t3 = t30+(x3(3)-x30(3))/w30(3)
      t4 = t40+(x4(3)-x40(3))/w40(3)

      x1(1) = x10(1)+w10(1)*(t1-t10)
      x1(2) = x10(2)+w10(2)*(t1-t10)
      x2(1) = x20(1)+w20(1)*(t2-t20)
      x2(2) = x20(2)+w20(2)*(t2-t20)
      x3(1) = x30(1)+w30(1)*(t3-t30)
      x3(2) = x30(2)+w30(2)*(t3-t30)
      x4(1) = x40(1)+w40(1)*(t4-t40)
      x4(2) = x40(2)+w40(2)*(t4-t40)

      return
      end



      subroutine ctraction(p,normal,u,axis,elast,traction)
      real normal(3), axis(3), elast(6)
      complex p(3), u(3), traction(3)

*
*   computes the traction traction(3) on the surface element 
*   specified by the normal unit vector normal(3).
*
*   INPUT VARIABLES:
*   
*       p(3)            slowness vector (complex)
*       normal(3)       unit vector normal to traction surface
*       u(3)            displacement vector (complex)
*       elast(6)        elastic parameters in this order:
*                       A, C, N, L, F, rho
*       axis(3)         anisotropy axis unit vector
*
*   OUTPUT VARIABLES:
*
*       traction(3)     traction vector (complex)
*

      real A, C, N, L, F
      real r1(3), r2(3), nprinc(3), dot
      complex epsilon(3,3), sigma(3,3), uprinc(3), pprinc(3)
      complex fprinc(3)
      integer i,j

      A = elast(1) 
      C = elast(2) 
      N = elast(3) 
      L = elast(4) 
      F = elast(5) 

*   Computation of principal axes of anisotropy (i.e. 2 unit vectors 
*    r1(3) and r2(3) to complement axis(3) into a cartesian basis)

      r1(1) = 0.e0
      r1(2) = -axis(3)
      r1(3) = axis(2)
      dot = 0.e0
      
      do 10 i=1,3
        dot = dot+r1(i)*r1(i)
10      continue

      if(dot .NE. 0.e0) then
        dot = sqrt(dot)

        do 20 i=1,3
                r1(i) = r1(i)/dot
20         continue

      else
        r1(1) = -axis(3)
        r1(2) = 0.e0
        r1(3) = axis(1)
        dot = 0.e0
        
        do 30 i=1,3
                dot = dot+r1(i)*r1(i)
30         continue

        if(dot .NE. 0.e0) then
                dot = sqrt(dot)
        
                do 40 i=1,3
                        r1(i) = r1(i)/dot
40                 continue

        else
                r1(1) = axis(2)
                r1(2) = -axis(1)
                r1(3) = 0.e0
                dot = 0.e0

                do 50 i=1,3
                        dot = dot+r1(i)*r1(i)
50                 continue
                
                dot = sqrt(dot)

                do 60 i=1,3
                        r1(i) = r1(i)/dot
60                 continue

        endif
      endif

      r2(1) = axis(2)*r1(3) - axis(3)*r1(2)
      r2(2) = axis(3)*r1(1) - axis(1)*r1(3)
      r2(3) = axis(1)*r1(2) - axis(2)*r1(1)
      dot = 0.e0

      do 70 i=1,3
        dot = dot+r2(i)*r2(i)
70      continue

      dot = sqrt(dot)

      do 80 i=1,3
        r2(i) = r2(i)/dot
80      continue


*   project displacement, unit normal, and slowness onto 
*   principal directions        

      uprinc(1) = u(1)*axis(1) + u(2)*axis(2) + u(3)*axis(3)
      uprinc(2) = u(1)*r1(1) + u(2)*r1(2) + u(3)*r1(3)
      uprinc(3) = u(1)*r2(1) + u(2)*r2(2) + u(3)*r2(3)

      pprinc(1) = p(1)*axis(1) + p(2)*axis(2) + p(3)*axis(3)
      pprinc(2) = p(1)*r1(1) + p(2)*r1(2) + p(3)*r1(3)
      pprinc(3) = p(1)*r2(1) + p(2)*r2(2) + p(3)*r2(3)

      nprinc(1) = normal(1)*axis(1) + normal(2)*axis(2) 
     &            + normal(3)*axis(3)
      nprinc(2) = normal(1)*r1(1) + normal(2)*r1(2) + normal(3)*r1(3)
      nprinc(3) = normal(1)*r2(1) + normal(2)*r2(2) + normal(3)*r2(3)

*   compute deformation tensor epsilon in principal axes 
*   (scaled down by i*omega)

      do 90 i=1,3
        do 100 j=1,3
        epsilon(i,j) = (uprinc(i)*pprinc(j)+uprinc(j)*pprinc(i))/2.
100        continue
90      continue

*   computes stress tensor in principal directions

      sigma(1,1) = C*epsilon(1,1) + F*epsilon(2,2) + F*epsilon(3,3)     
      sigma(2,2) = F*epsilon(1,1) + A*epsilon(2,2) 
     &             + (A-2.e0*N)*epsilon(3,3)    
      sigma(3,3) = F*epsilon(1,1) + (A-2.e0*N)*epsilon(2,2) 
     &             + A*epsilon(3,3)    
      sigma(1,2) = 2.e0*L*epsilon(1,2)
      sigma(1,3) = 2.e0*L*epsilon(1,3)
      sigma(2,3) = 2.e0*N*epsilon(2,3)
      sigma(2,1) = sigma(1,2)
      sigma(3,1) = sigma(1,3)
      sigma(3,2) = sigma(2,3)

*   computes traction in principal directions

      do 110 i=1,3
        fprinc(i) = 0.e0
        do 120 j=1,3
                fprinc(i) = fprinc(i) + sigma(i,j)*nprinc(j)
120        continue
110      continue

*   expresses traction back in reference coordinates

      do 130 i=1,3
        traction(i) = fprinc(1)*axis(i) + fprinc(2)*r1(i) 
     &                + fprinc(3)*r2(i)
130      continue

      return
      end



      subroutine czoeppritz(pi,modei,normal,axisi,elasti,axist,
     &   elastt,coefs,error)
      real pi(3), normal(3), axisi(3), elasti(6), axist(3), elastt(6)
      complex coefs(6)
      integer error, modei

*
*   computes complex scattering coefficients.
*
*   INPUT VARIABLES:
*
*       pi(3)           incident slowness vector (real)
*       modei           incident mode
*       normal(3)       unit normal to interface
*       axisi(3)        anisotropy axis in incident medium
*       elasti(6)       A, C, N, L, F, rho in incident medium
*       axist(3)        anisotropy axis in transmission medium  
*       elastt(6)       A, C, N, L, F, rho in transmission medium
*
*   OUTPUT VARIABLES:
*
*       coefs(6)        complex scattering coefficient in order:
*                       R-SP, R-QP, R-QS, T-SP, T-QP, T-QS
*       error           0 if solutions were found
*                       1 if algorithm did not converge
*
*   EXTERNAL ROUTINES:
*
*       cscatslow       computes complex scattering slownesses
*       cpolar          computes complex polarizations
*       ctraction       computes complex tractions
*       cgausselim      complex linear system solver
*

      integer i
      complex prSP(3), prQP(3), prQS(3), ptSP(3), ptQP(3), ptQS(3)
      complex urSP(3), urQP(3), urQS(3), utSP(3), utQP(3), utQS(3)
      complex frSP(3), frQP(3), frQS(3), ftSP(3), ftQP(3), ftQS(3)
      complex mat(6,6), vec(6), ui(3), fi(3), cpi(3)

      error = 0

*   computation of scattered slownesses from incident slowness

      call cscatslow(pi,0,normal,axisi,elasti,prSP,prQP,prQS,error)

      if(error .NE. 0) then
        return
      endif

      call cscatslow(pi,1,normal,axist,elastt,ptSP,ptQP,ptQS,error)

      if(error .NE. 0) then
        return
      endif

*   compute displacement unit vectors for each wave

      do40 i=1,3
        cpi(i) = cmplx(pi(i),0.e0)
40      continue
      call cpolar(cpi,axisi,modei,elasti,ui)
      call cpolar(prSP,axisi,1,elasti,urSP)
      call cpolar(prQP,axisi,2,elasti,urQP)
      call cpolar(prQS,axisi,3,elasti,urQS)
      call cpolar(ptSP,axist,1,elastt,utSP)
      call cpolar(ptQP,axist,2,elastt,utQP)
      call cpolar(ptQS,axist,3,elastt,utQS)

*   compute traction at interface for each wave

      call ctraction(cpi,normal,ui,axisi,elasti,fi)
      call ctraction(prSP,normal,urSP,axisi,elasti,frSP)
      call ctraction(prQP,normal,urQP,axisi,elasti,frQP)
      call ctraction(prQS,normal,urQS,axisi,elasti,frQS)
      call ctraction(ptSP,normal,utSP,axist,elastt,ftSP)
      call ctraction(ptQP,normal,utQP,axist,elastt,ftQP)
      call ctraction(ptQS,normal,utQS,axist,elastt,ftQS)

*   fill in zoeppritz matrix

      do 10 i=1,3
        mat(i,1) = urSP(i)
        mat(i,2) = urQP(i)
        mat(i,3) = urQS(i)
        mat(i,4) = -utSP(i)
        mat(i,5) = -utQP(i)
        mat(i,6) = -utQS(i)
        mat(i+3,1) = frSP(i)            
        mat(i+3,2) = frQP(i)            
        mat(i+3,3) = frQS(i)            
        mat(i+3,4) = -ftSP(i)
        mat(i+3,5) = -ftQP(i)
        mat(i+3,6) = -ftQS(i)
10      continue

*   fill in forcing term

      do 20 i=1,3
        vec(i) = -ui(i)
        vec(i+3) = -fi(i)
20      continue

*   solve the zoeppritz system (if not singular) using gaussian
*   elimination with partial pivoting. (vec is input and output)

      call cgausselim(mat,vec,6)

*   transfer solution to output

      do 30 i=1,6
        coefs(i) = vec(i)
30      continue

      return
      end



	subroutine freerad(f,p,a,elast,mode,t,u,x)

	real f(3), p(3), a(3), elast(6), t, x(3)
	complex u(3)
	integer mode

*
*	Computes the radiated displacement of a point force at the
*	free surface of a homogeneous, unbounded, transversely 
*	isotropic medium using the reciprocity principle. 
*	Radiation pattern is evaluated on a surface of constant 
*	traveltime (i.e. a snap shot of amplitudes on the wave 
*	surface). The x3 direction points towards the elastic 
*	half space.
*
*	INPUT VARIABLES:
*
*		f(3)		force vector.
*		p(3)		unit vector in the propagation (slowness)
*				direction 
*		a(3)		unit vector along anisotropy axis.
*		elast(6)	elastic parameters in the following order:
*				A, C, N, L, F, rho.
*		mode		radiated wavetype: 1 for SP, 2 for QP, 
*				3 for QS.
*		t		traveltime on wave surface.
*	
*	OUTPUT VARIABLES:
*
*		u(3)		radiated displacement.
*		x(3)		location of observation point
*				on the wave surface.
*
*	EXTERNAL ROUTINES:
*
*		phasvel		(real function) phase velocity evaluator
*		cpolar		displacement polarization evaluator
*

	real pa, phas, twopi
	real phasvel
	real pi(3), normal(3), prev(3)
	integer i, error, j
	complex r(3), coefs(3), amp, ui(3)
	complex uSP(3), uQP(3), uQS(3), utotal(3)

	twopi = 6.283185308e0

* compute the full-space far-field radiation vector 

        do2 i=1,3
           prev(i) = -p(i)
2       continue

	call rad(r,prev,a,elast,mode,t,x)

* compute the incident and reflected displacement 
* polarizations at the free surface as well as 
* the three reflection coefficients by solving 
* the 3x3 traction-free zoeppritz system. 

	pa = 0.e0

	do5 i=1,3
		pa = pa + prev(i)*a(i)
5	continue

	phas = phasvel(mode,pa,elast)

	do10 i=1,3
		pi(i) = prev(i)/phas
10	continue

	normal(1) = 0.e0
	normal(2) = 0.e0
	normal(3) = 1.e0
	
	call freezoep(pi,mode,normal,a,elast,coefs,error,
     &  ui,uSP,uQP,uQS)

* assemble the total free surface displacement (from which 
* the Green's dyadic can be build, if needed).

	do20 j=1,3
		utotal(j) = ui(j) + coefs(1)*uSP(j)
     &	       + coefs(2)*uQP(j) + coefs(3)*uQS(j)
20	continue	

* assembles total displacement at free surface in direction f

	amp = cmplx(0.e0,0.e0)

	do30 j=1,3
		amp = amp + utotal(j)*f(j)
30	continue

	do40 j=1,3
		u(j) = amp*r(j)
40	continue

	return
	end



      subroutine freezoep(pi,mode,normal,axis,elast,
     &   coefs,error,ui,urSP,urQP,urQS)

      real pi(3), normal(3), axis(3), elast(6)
      complex coefs(3), ui(3), urSP(3), urQP(3), urQS(3)
      integer error, mode

*
*   computes complex reflection coefficients for an incident 
*   wave at the traction-free surface of a homogeneous elastic 
*   transversely isotropic half space.
*
*   INPUT VARIABLES:
*
*       pi(3)           incident slowness vector (real)
*       mode            incident mode
*       normal(3)       unit normal to interface
*       axis(3)         anisotropy axis of half space
*       elast(6)        A, C, N, L, F, rho of half space
*
*   OUTPUT VARIABLES:
*
*       coefs(3)        complex reflection coefficient in order:
*                       R-SP, R-QP, R-QS
*	ui(3)		incident displacement polarization
*	urSP(3)		reflected SP displacement polarization
*	urQP(3)		reflected QP displacement polarization
*	urQS(3)		reflected QS displacement polarization
*       error           0 if solutions were found
*                       1 if algorithm did not converge
*
*   EXTERNAL ROUTINES:
*
*       cscatslow       computes complex scattering slownesses
*       cpolar          computes complex polarizations
*       ctraction       computes complex tractions
*       cgausselim      complex linear system solver
*

      integer i
      complex prSP(3), prQP(3), prQS(3)
      complex frSP(3), frQP(3), frQS(3)
      complex mat(3,3), vec(3), fi(3), cpi(3)

      error = 0

*   computation of scattered slownesses from incident slowness

      call cscatslow(pi,0,normal,axis,elast,prSP,prQP,prQS,error)

      if(error .NE. 0) then
        return
      endif

*   compute displacement unit vectors for each wave

      do40 i=1,3
        cpi(i) = cmplx(pi(i),0.e0)
40    continue

      call cpolar(cpi,axis,mode,elast,ui)
      call cpolar(prSP,axis,1,elast,urSP)
      call cpolar(prQP,axis,2,elast,urQP)
      call cpolar(prQS,axis,3,elast,urQS)

*   compute traction at interface for each wave

      call ctraction(cpi,normal,ui,axis,elast,fi)
      call ctraction(prSP,normal,urSP,axis,elast,frSP)
      call ctraction(prQP,normal,urQP,axis,elast,frQP)
      call ctraction(prQS,normal,urQS,axis,elast,frQS)

*   fill in zoeppritz matrix

      do 10 i=1,3
        mat(i,1) = frSP(i)            
        mat(i,2) = frQP(i)            
        mat(i,3) = frQS(i)            
10      continue

*   fill in forcing term

      do 20 i=1,3
        vec(i) = -fi(i)
20      continue

*   solve the zoeppritz system (if not singular) using gaussian
*   elimination with partial pivoting. (vec is input and output)

      call cgausselim(mat,vec,3)

*   transfer solution to output

      do 30 i=1,3
        coefs(i) = vec(i)
30      continue

      return
      end



      subroutine groupvel(p,axis,mode,elast,w)
      real elast(6), axis(3), p(3), w(3)
      integer mode

*
*   computes group velocity of the given mode
*
*   INPUT VARIABLES:
*
*       p(3)            slowness vector or its direction
*       axis(3)         anisotropy axis unit vector
*       mode            1 for SP, 2 for QP, 3 for QS
*       elast(6)        elastic parameters in following
*                       order: A, C, N, L, F, rho
*
*   OUTPUT VARIABLES:
*
*       w(3)            group velocity vector
*

      integer i
      real pax(3), prad(3), dot, den, s2, c2, num, wax, wrad, punit(3)
      real A, C, N, L, F, RHO
      real alpha, beta

      A = elast(1)
      C = elast(2)
      N = elast(3)
      L = elast(4)
      F = elast(5)
      RHO = elast(6)

      dot = 0.e0

      do 10 i=1,3
        dot = dot + p(i)*p(i)
10      continue

      dot = sqrt(dot) 

      do 20 i=1,3
        punit(i) = p(i)/dot
20      continue

      dot = 0.e0

      do 30 i=1,3
        dot = dot + punit(i)*axis(i)
30      continue   

      c2 = dot*dot
      s2 = 1.e0 - c2

      do 40 i=1,3
        pax(i) = dot*axis(i)
        prad(i) = punit(i) - pax(i)
40      continue

      if(mode .EQ. 1) then
        den = sqrt(RHO*(N*s2+L*c2))
        wax = L/den
        wrad = N/den
      else
        alpha = (A+L)*s2 + (C+L)*c2
        beta = (A-L)*s2 - (C-L)*c2
        num = beta
        beta = beta*beta + 4.e0*s2*c2*(L+F)*(L+F)
        beta = sqrt(beta)
        if(mode .EQ. 2) then
                den = beta*sqrt(2.e0*RHO*(alpha+beta))
                wax = beta*(C+L)+2.e0*s2*(L+F)*(L+F)-(C-L)*num
                wax = wax/den
                wrad = beta*(A+L)+2.e0*c2*(F+L)*(F+L)+(A-L)*num
                wrad = wrad/den
        else if(mode .EQ. 3) then
                den = beta*sqrt(2.e0*RHO*(alpha-beta))
                wax = beta*(C+L)-2.e0*s2*(L+F)*(L+F)+(C-L)*num
                wax = wax/den
                wrad = beta*(A+L)-2.e0*c2*(F+L)*(F+L)-(A-L)*num
                wrad = wrad/den
        else 
*               write(stderr,*) 'ERROR: illegal mode. routine:groupvel'
        endif   
      endif

      do 50 i=1,3
        w(i) = wax*pax(i) + wrad*prad(i)
50      continue
      
      return
      end



      subroutine initkin(mode,rg1,rg2,r,x0,p0,t0)

      real rg1, rg2, r, x0(3), p0(3), t0
      integer mode

*
*   computes the initial parameters for a ray emanating from 
*   a point source at the free surface of a homogeneous isotropic
*   medium. Amplitudes are discarded for this application.
*
*   INPUT VARIABLES:
*
*       mode    type of excitation: 1 for S, 2 for P
*       rg1     take-off azimuth angle for the ray in degrees
*       rg2     take-off dip angle for the ray in degrees
*       r       average radius of take-off sphere
*
*   OUTPUT VARIABLES:
*
*       x0(3)   vector location of ray starting point with respect
*               to source location              
*       p0(3)   initial ray slowness
*       t0      initial ray traveltime
*
*   GLOBAL VARIABLES:
*
*       block "model"   geometric and elastic model parameters.
*

      integer nlayer, triso(100)
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso

      real pi, twopi, deg2rad, axis(3), elast(6), g1, g2
      real w0(3)
      integer i

*   compute P and S velocities and initializes useful variables

      pi = 3.141592654
      twopi = 6.2831856
      deg2rad = 1.7453292e-02
      g1 = rg1*deg2rad
      g2 = rg2*deg2rad

      do2 i=1,6
         elast(i) = param(i)
2     continue

      do5 i=1,3
         axis(i) = param(i+6)
5     continue

*   computes initial traveltime as ratio of the take-off radius 
*   to the fastest velocity for the mode. 

      if(mode .EQ. 2) then

        t0 = r*sqrt(param(6)/param(1))

      else

        t0 = r*sqrt(param(6)/param(3))

      endif

*   compute initial location and slowness

      p0(1) = cos(g1)*cos(g2)
      p0(2) = sin(g1)*cos(g2)
      p0(3) = sin(g2)

      call groupvel(p0,axis,mode,elast,w0)

      call slowness(p0,axis,mode,elast)

      do10 i=1,3
        x0(i) = w0(i)*t0
10    continue

      return
      end



      subroutine laguerre(pol,n,x,eps,isol)
      integer n, isol
      complex pol(n+1), x
      real eps

*   searches for one root of a complex polynomial
*   using Laguerre's technique.
*   Adapted from a C program from Numerical Recipes in C
*
*   INPUT VARIABLES:
*
*       n               degree of polynomial.
*       pol(n+1)        array of polynomial coefficients.
*       eps             desired relative accuracy on result.
*       x               initial guess for solution.
*
*   OUTPUT VARIABLES:
*
*       x               root of polynomial.
*       isol            0 if no solution was found after 100
*                       iterations.
*                       1 if solution found.
*
      integer i, maxiter, iter
      complex p, d, a, f, g, h, den, sq
      real err, xab, ca
      complex den1, den2

      maxiter = 100
      isol = 0

*   iteration loop

      do10 iter=1,maxiter

        xab = cabs(x)
        p = pol(n+1)
        err = cabs(p)
        d = cmplx(0.,0.)
        f = cmplx(0.,0.)

*   evaluates polynomial and its first two derivatives at x

        do20 i=n,1,-1
                f = f*x+d
                d = d*x+p
                p = p*x+pol(i)
                err = err*xab+cabs(p)
20         continue
      

        err = err*1.e-6

        if(cabs(p) .LE. err) then
                isol = 1
                return
        else
                g = d/p
                h = g*g-2.0*f/p
                sq = csqrt((n-1)*(n*h-g*g))
                den1 = g+sq
                den2 = g-sq

                if(cabs(den1) .GT. cabs(den2)) then
                        den = den1
                else
                        den = den2
                endif

*               if(iter .GT. maxiter-1) then
*                       a = cmplx(50.e0,50.e0)
*                       iter = 1
*               else if(cabs(den) .LT. 1.e-8) then
*                       a = cmplx(100.e0,100.e0)
*               else
                        a = float(n)/den
*               endif


*   increment step

                x = x-a
                ca = cabs(a)

                if(ca .LE. cabs(x)*eps) then
                        isol = 1
                        return
                endif

        endif

10      continue

      return
      end



      real function phasvel(mode,cosine,elast)

      integer mode
      real cosine, elast(6)

*
*   ARGUMENTS:
*
*       mode            1 for SP, 2 for QP, 3 for QS.
*       cosine          cosine of angle between slowness and
*                       anisotropy axis.
*       elast(6)        elastic parameters of medium in 
*                       following order: A, C, N, L, F, rho.
*
*   RETURNED VALUE:
*
*       phasvel         scalar phase velocity of desired mode.
*

      real s2, c2, A, C, L, N, F, RHO, alpha, beta

      c2 = cosine*cosine
      s2 = 1.e0 - c2
      A = elast(1)
      C = elast(2)
      N = elast(3)
      L = elast(4)
      F = elast(5)
      RHO = elast(6)

*   COMPUTES PHASE VELOCITY ACCORDING TO MODE

      if(mode .EQ. 1) then
        phasvel = sqrt((N*s2+L*c2)/RHO)
      else
        alpha = (A+L)*s2 + (C+L)*c2
        beta = (A-L)*s2 - (C-L)*c2
        beta = beta*beta + 4.e0*s2*c2*(L+F)*(L+F)
        beta = sqrt(beta)       
        if(mode .EQ. 2) then
                phasvel = sqrt((alpha+beta)/RHO/2.)
        else if(mode .EQ. 3) then
                phasvel = sqrt((alpha-beta)/RHO/2.)
        else
                phasvel = 1.e0
        endif
      endif

      return
      end



      subroutine poldiv(a,n,b,m,q,r)

*   computes the synthetic division of polynomial a by b
*   in the sense:  a = q*b + r  where deg(r)<deg(b).
*
*   INPUT VARIABLES:
*
*       n               degree of polynomial to be divided.
*       m               degree of divider polynomial.
*       a(n+1)          array of coefficients of polynomial
*                       of degree n to be divided.
*       b(m+1)          array of coefficients of dividing 
*                       polynomial of degree m.
*   OUTPUT VARIABLES:
*
*       q(n)            array of coefficients of quotient 
*                       polynomial.
*       r(n)            array of coefficients of remainder 
*                       polynomial.

      integer n,m,n1,m1,i,j
      complex a(n+1), b(m+1), q(n+1), r(n+1)

      n1 = n+1
      m1 = m+1

      do10 i=1,n1
        r(i) = a(i)
        q(i) = cmplx(0.,0.)
10      continue

      do20 i=n1-m,1,-1
        q(i) = r(i+m)/b(m1)
        
        do30 j=m+i-1,i,-1
                r(j) = r(j)-q(i)*b(j-i+1)
30         continue

20      continue

      return
      end



	subroutine rad(r,p,a,elast,mode,t,x)

	real p(3), a(3), elast(6), t, x(3)
	complex r(3)
	integer mode

*
*	Computes the radiation vector of a point force in a 
*	homogeneous, unbounded, transversely isotropic medium. 
*	To obtain the radiated amplitude corresponding to a given 
*	force direction, just take the dot product between the 
*	radiation vector r(3) and the force.
*	The radiated amplitude is evaluated on the wave surface 
*	at traveltime t (wavefront).
*
*	INPUT VARIABLES:
*
*		p(3)		unit vector in the propagation (slowness)
*				direction.
*		a(3)		unit vector along anisotropy axis.
*		elast(6)	elastic parameters in the following order:
*				A, C, N, L, F, rho.
*		mode		radiated wavetype: 1 for SP, 2 for QP, 
*				3 for QS.
*		t		traveltime on wave surface.
*	
*	OUTPUT VARIABLES:
*
*		r(3)		radiation vector.
*		x(3)		location of observation point
*				on the wave surface.
*
*	EXTERNAL ROUTINES:
*
*		phasvel		(real function) phase velocity evaluator
*		cpolar		displacement polarization evaluator
*

	real pa, mod, phas, twopi, fourpi, dphi, sine
	real first, second, slow, slowplus, slowmin, w(3)
	real first2, slow2, k, sina, group, phasvel, dot, rho
	integer i,j
	complex cp(3), cu(3), amp

	twopi = 6.283185308e0
	fourpi = 2.e0*twopi
	rho = elast(6)

* compute phase velocity for use in cpolar

	pa = 0.e0

	do5 i=1,3
		pa = pa + p(i)*a(i)
5	continue

	sine = sqrt(1.e0-pa*pa)

	phas = phasvel(mode,pa,elast)

	do10 i=1,3
		cp(i) = cmplx(p(i)/phas,0.e0)
10	continue
	
* evaluate first and second derivatives of slowness in the 
* longitudinal plane by finite difference.

	dphi = twopi/100.e0

	slow = 1.e0/phas
	slowplus = 1.e0/phasvel(mode,pa-dphi*sine,elast)
	slowmin = 1.e0/phasvel(mode,pa+dphi*sine,elast)

	first = (slowplus-slowmin)/dphi/2.e0
	second = (slowplus-slow-slow+slowmin)/dphi/dphi/2.e0

* (accurately) compute group velocity and observation point

	call groupvel(p,a,mode,elast,w)

	do30 i=1,3
		x(i) = w(i)*t
30	continue

* compute the dimensionless curvature term

	first2 = first*first
	slow2 = slow*slow

	k = slow*second-first2-first2-slow2
	k = k/slow/sqrt(slow2+first2)

* compute proper phase shift (0 if k<0, pi/2 if k>0, in which 
* case output is on a triplication cusp)

	if(k .LE. 0.e0) then
		amp = cmplx(1.e0,0.e0)
	else
		amp = cmplx(0.e0,1.e0)
	endif

	k = abs(k)

* test for singularity (vanishing curvature <=> cuspidal edge
* associated with triplication: ray theory fails there anyway)

	if(k .LE. 1.e-4) then
                r(1) = cmplx(0.e0,0.e0)
                r(2) = cmplx(0.e0,0.e0)
                r(3) = cmplx(0.e0,0.e0)
                return
	endif

* compute the radiation amplitude

	call cpolar(cp,a,mode,elast,cu)

* test for propagation along axis in which case the ratio sine/sina
* is finite in the limit although sine and sina vanish there.
* the ratio may be computed from small argument expansions:

	mod = 0.e0

	do35 i=1,3
		mod = mod+w(i)*w(i)
35	continue

	group = sqrt(mod)

	if(abs(sine) .LE. 1.e-3) then
		sine = 1.e0
		if(mode .EQ. 1) then
			sina = elast(3)/elast(4)
		else if(mode .EQ. 2) then
			sina = elast(4)+elast(5)
			sina = sina*sina
			sina = sina/(elast(2)-elast(4))
			sina = (elast(1)-sina)/elast(4)
		else if(mode .EQ. 3) then
			sina = elast(4)+elast(5)
			sina = sina*sina
			sina = sina/(elast(2)-elast(4))
			sina = (elast(4)+sina)/elast(2)
		endif

		sina = abs(2.e0-sina)

	else

		dot = 0.e0

		do40 i=1,3
			dot = dot + w(i)*a(i)
40		continue

		sina = sqrt(1.e0-dot*dot/mod)

	endif

* compute final amplitude

	do45 j=1,3
		r(j) = amp*slow2*cu(j)*sqrt(sine/sina/k)
		r(j) = r(j)/fourpi/rho/group/t
45	continue

	return
	end



      subroutine roots(a,z1,z2,z3,z4,isol)

      complex a(5), z1, z2, z3, z4
      integer isol

*
*   finds the 4 complex roots of a real coefficient polynomial
*   by Laguerre's technique combined with polynomial deflation.
*
*   INPUT VARIABLES:
*
*       a(5)            polynomial coefficients in increasing power
*                       NOTE: although a(5) is declared complex, the
*                       coefficients must have zero imaginary parts.
*
*   OUTPUT VARIABLES:
*
*       z1,...,z4       four complex roots of polynomial
*       isol            0 if no solution was found, 1 either
*
*   EXTERNAL ROUTINES:
*
*       laguerre        single root finder
*       poldiv          polynomial divider
*

      complex a1(4), b(3), q(5), r(5), x, del, del2, c4, q1, q3
      real eps, cz1
      integer i

*   searches for one root of polynomial

      eps = 1.e-6
      c4 = cmplx(4.e0,0.e0)
      isol = 0
      x = cmplx(0.,0.)

      if(cabs(a(5)) .EQ. 0.e0) then
        isol = 0
        return
      endif

      call laguerre(a,4,x,eps,isol)

      if(isol .EQ. 0) then
        return
      endif

      z1 = x

      cz1 = cabs(z1)

      if(cz1 .EQ. 0.) then
        cz1 = 1.e0
      endif

      if( abs(aimag(z1))/cz1 .LE. 1.e-4 ) then

*       deflates by one real monome

        b(2) = cmplx(1.,0.)
        b(1) = -1.e0*z1

        call poldiv(a,4,b,1,q,r)

*       searches for root of deflated polynomial

        do10 i=1,4
                a1(i) = q(i)
10         continue

        x = cmplx(0.,0.)

        call laguerre(a1,3,x,eps,isol)
        
        if(isol .EQ. 0) then
                return
        endif

        z2 = x

*       deflates by second monome

        b(2) = cmplx(1.,0.)
        b(1) = -1.e0*z2

        call poldiv(a1,3,b,1,q,r)

      else

*       deflates by two conjugate roots

        z2 = conjg(x)
        b(3) = cmplx(1.,0.)
        b(2) = -1.e0*(z1+z2)
        b(1) = z1*z2


        call poldiv(a,4,b,2,q,r)
      endif

*   computes the last two roots by traditionnal method

      q1 = q(1)
      q3 = q(3)
      del2 = q3*q1
      del2 = -c4*del2
      del2 = del2+q(2)*q(2)
      del = csqrt(del2)
      z3 = (del-q(2))/q(3)/2.
      z4 = -1.e0*(del+q(2))/q(3)/2.

      isol = 1

      return
      end



      subroutine scatslow(pinc,normal,axis,which,elast,pscat,isol)
      real pinc(3), normal(3), axis(3), elast(6), pscat(3)
      integer which(2),isol

*
*   computes the scattering slowness of the given mode, if real.
*
*   INPUT VARIABLES:
*   
*       pinc(3)         incident slowness (real)
*       normal(3)       normal unit vector to interface
*                       pointing towards incident medium.
*       axis(3)         anisotropy direction in scattering medium
*       which(2)        which(1) is the scattering mode: 
*                       1 for SP, 2 for QP, 3 for QS
*                       which(2) = 1 for reflection, 2 for transmission
*       elast(6)        array of elastic coefficients in scattering
*                       medium: A, C, N, L, F, rho
*
*   OUTPUT VARIABLES:
*
*       pscat(3)        scattered slowness
*       isol            0 if real scattering occured 
*                       1 if scattering is complex
*
*   EXTERNAL ROUTINES:
*
*       roots           root finder
*       slowness        real slowness evaluator
*

      real t(3), ptan, ptan2, vtan(3)
      real del, del1, del2, pnorm, x1, x2, x3, x4
      complex z1, z2, z3, z4, poly(5), z(4), zz
      integer iscat, sign, is, ip, iss, i, ireal, mode
      real pa2, pr2, alpha, beta, zp1, zs1, zp2, zs2
      real AL, CL, LAF, ca, dot1, dot2, p1(3), p2(3), w1(3), w2(3)
      real A, C, N, L, F, rho, dot, pinc1(3)
      real na, ta, na1, ta1, na2, ta2, c2, c1, c0

      iscat = which(2)-1
      mode = which(1)
      isol = 0


*   transfers data to more explicit variables

      A = elast(1)
      C = elast(2)
      N = elast(3)
      L = elast(4)
      F = elast(5)
      rho = elast(6)

*   computes tangential slowness and tangent direction

      dot = 0.e0

      do10 i=1,3
        dot = dot + pinc(i)*normal(i)
10      continue

      ptan2 = 0.e0

      do20 i=1,3
        vtan(i) = pinc(i) - dot*normal(i)
        ptan2 = ptan2 + vtan(i)*vtan(i)
20      continue

      dot = pinc(1)*pinc(1)+pinc(2)*pinc(2)+pinc(3)*pinc(3)     
      ptan = sqrt(ptan2)

*       gets rid of trivial case (normal incidence)

      if(ptan2/dot .EQ. 0.e0) then

        if(iscat .EQ. 0) then
                sign = 1
        else
                sign = -1
        endif

        do40 i=1,3
                pinc1(i) = sign*normal(i)
40         continue

        call slowness(pinc1,axis,mode,elast)

        do50 i=1,3
                pscat(i) = pinc1(i)
50         continue

        return

      endif

      do30 i=1,3
        t(i) = vtan(i)/ptan
30      continue

*   computes some usefull quantities for later use

      na = 0.e0

      do60 i=1,3
        na = na + normal(i)*axis(i)
60      continue

      ta = 0.e0

      do70 i=1,3
        ta = ta + t(i)*axis(i)
70      continue

      na2 = na*na
      ta2 = ta*ta

      na1 = 1.e0 - na2
      ta1 = 1.e0 - ta2

      if(mode .EQ. 1) then

*   SP case (solved analytically)

        c2 = L*na2 + N*na1
        c1 = 2.e0*ptan*ta*na*(L-N)
        c0 = ptan2*(L*ta2+N*ta1)-rho
        del = c1*c1-4.e0*c2*c0

        if(del .LT. 0.e0) then
*       scattering is complex: exit
                isol = 1
                return
        else
*       scattering is real
                del = sqrt(del)
        endif

        x1 = (-c1+del)/c2/2.e0
        x2 = (-c1-del)/c2/2.e0

*       determines appropriate solution

        do888 i=1,3
                p1(i) = vtan(i)+x1*normal(i)
                p2(i) = vtan(i)+x2*normal(i)
888        continue
        call groupvel(p1,axis,1,elast,w1)
        call groupvel(p2,axis,1,elast,w2)
        dot1 = 0.e0
        dot2 = 0.e0
        do999 i=1,3
                dot1 = dot1 + w1(i)*normal(i)
                dot2 = dot2 + w2(i)*normal(i)
999        continue

        if(iscat .EQ. 0) then
                if(dot1 .GT. 0.e0) then
                        pnorm = x1
                else
                        pnorm = x2
                endif
        else
                if(dot1 .GT. 0.e0) then
                        pnorm = x2
                else
                        pnorm = x1
                endif
        endif

        do100 i=1,3
                pscat(i) = vtan(i)+pnorm*normal(i)
100        continue

        return

      endif

*   QP and QS cases (numerical solution)

*   computes four scattered slownesses (roots of eikonal determinant)

      AL = A*L
      CL = C*L
      LAF = L*L+A*C-(F+L)*(F+L)

      poly(5) = AL*na1*na1 + CL*na2*na2 + LAF*na2*na1
      poly(4) = 2.e0*ptan*na*ta*(LAF*(1.e0-2.e0*na2)
     &          +2.e0*(CL*na2-AL*na1))
      poly(3) = 6.e0*CL*na2*ta2+2.e0*AL*(ta1*na1+2.e0*ta2*na2)
      poly(3) = poly(3)+LAF*(na2+ta2-6.e0*na2*ta2)
      poly(3) = poly(3)*ptan2-rho*((C+L)*na2+(A+L)*na1)
      poly(2) = 2.e0*(CL*ta2-AL*ta1)+LAF*(1.e0-2.e0*ta2)
      poly(2) = poly(2)*ptan2+rho*(A-C)
      poly(2) = poly(2)*2.e0*ptan*na*ta
      poly(1) = CL*ta2*ta2+AL*ta1*ta1+LAF*ta2*ta1
      poly(1) = poly(1)*ptan2-rho*((L+C)*ta2+(L+A)*ta1)
      poly(1) = poly(1)*ptan2+rho*rho

      call roots(poly,z1,z2,z3,z4,iss)

      z(1) = z1
      z(2) = z2
      z(3) = z3
      z(4) = z4

      if(iss .EQ. 0) then
        isol = 1
        return
      endif

*   determines which mode is associated with each root

      ireal = 0

      do111 i=1,4
        ca = cabs(z(i))
        if(ca .EQ. 0.e0) then
                ca = 1.e0
        endif
        if(abs(aimag(z(i)))/ca .LE. 1.e-4) then
                zz = z(1)
                z(1) = z(i)
                z(i) = zz
                ireal = ireal + 1
                goto 222
        endif
111      continue

222   do333 i=2,4
        ca = cabs(z(i))
        if(ca .EQ. 0.e0) then
                ca = 1.e0
        endif
        if(abs(aimag(z(i)))/ca .LE. 1.e-4) then
                zz = z(2)
                z(2) = z(i)
                z(i) = zz
                ireal = ireal + 1
        endif
333      continue          

      if(ireal .LT. 2) then
*   both QP and QS modes are evanescent: return
        isol = 1
        return
      endif

*   at least the first two roots in z(4) are real: determine the
*   associated modes.

      ip = 0
      is = 0

      x1 = real(z(1))
      pa2 = 2.e0*ptan*na*ta*x1
      pr2 = pa2
      pa2 = pa2+ptan2*ta2+x1*x1*na2
      pr2 = ptan2*ta1-pr2+x1*x1*na1

      alpha = (A+L)*pr2+(C+L)*pa2
      beta = (A-L)*pr2-(C-L)*pa2
      beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
      beta = sqrt(beta)

      del1 = 1.e0 - 2.e0*rho/(alpha+beta)
      del2 = 1.e0 - 2.e0*rho/(alpha-beta)

      if(abs(del1) .LE. abs(del2)) then
        zp1 = x1
        ip = ip+1               
      else
        zs1 = x1
        is = is+1
      endif

      x2 = real(z(2))
      pa2 = 2.e0*ptan*na*ta*x2
      pr2 = pa2
      pa2 = pa2+ptan2*ta2+x2*x2*na2
      pr2 = ptan2*ta1-pr2+x2*x2*na1

      alpha = (A+L)*pr2+(C+L)*pa2
      beta = (A-L)*pr2-(C-L)*pa2
      beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
      beta = sqrt(beta)

      del1 = 1.e0 - 2.e0*rho/(alpha+beta)
      del2 = 1.e0 - 2.e0*rho/(alpha-beta)

      if(abs(del1) .LE. abs(del2)) then
        if(ip .EQ. 1) then
                zp2 = x2
        else
                zp1 = x2
        endif
        ip = ip+1
      else
        if(is .EQ. 1) then
                zs2 = x2
        else
                zs1 = x2
        endif
        is = is+1
      endif

      x3 = real(z(3))
      x4 = real(z(4))

      if(ip .EQ. 2) then
        if(mode .EQ. 2 .OR. (mode .EQ. 3 .AND. ireal .EQ. 4)) then
                zs1 = x3
                zs2 = x4
        else
*       desired mode (QS) is evanescent
                isol = 1
                return
        endif
      else if(is .EQ. 2) then
        if(mode .EQ. 3 .OR. (mode .EQ. 2 .AND. ireal .EQ. 4)) then
                zp1 = x3
                zp2 = x4
        else
*       desired mode (QP) is evanescent
                isol = 1
                return
        endif
      else if(is .EQ. 1 .AND. ip .EQ. 1) then
*   both QP and QS modes are real

        pa2 = 2.e0*ptan*na*ta*x3
        pr2 = pa2
        pa2 = pa2+ptan2*ta2+x3*x3*na2
        pr2 = ptan2*ta1-pr2+x3*x3*na1

        alpha = (A+L)*pr2+(C+L)*pa2
        beta = (A-L)*pr2-(C-L)*pa2
        beta = beta*beta + 4.e0*pr2*pa2*(L+F)*(L+F)
        beta = sqrt(beta)

        del1 = 1.e0 - 2.e0*rho/(alpha+beta)
        del2 = 1.e0 - 2.e0*rho/(alpha-beta)

        if(abs(del1) .LE. abs(del2)) then
                zp2 = x3
                ip = ip+1
        else
                zs2 = x3
                is = is+1
        endif

        if(ip .EQ. 2) then
                zs2 = x4
        else
                zp2 = x4
        endif
      else
        isol = 1
        return
      endif
      
      if(mode .EQ. 2) then
*   choose QP root according to scattering medium

        do444 i=1,3
                p1(i) = vtan(i)+zp1*normal(i)
                p2(i) = vtan(i)+zp2*normal(i)
444        continue
        call groupvel(p1,axis,2,elast,w1)
        call groupvel(p2,axis,2,elast,w2)
        dot1 = 0.e0
        dot2 = 0.e0
        do555 i=1,3
                dot1 = dot1 + w1(i)*normal(i)
                dot2 = dot2 + w2(i)*normal(i)
555        continue

        if(iscat .EQ. 0) then
                if(dot1 .GT. 0.e0) then
                        pnorm = zp1
                else if(dot2 .GT. 0.e0) then
                        pnorm = zp2
                else
                        isol = 1
                        return
                endif
        else
                if(dot1 .GT. 0.e0) then
                        pnorm = zp2
                else if(dot2 .GT. 0.e0) then
                        pnorm = zp1
                else
                        isol = 1
                        return
                endif
        endif

        do200 i=1,3
                pscat(i) = vtan(i)+pnorm*normal(i)
200        continue

        return
      endif     

      if(mode .EQ. 3) then
*   choose QS root according to scattering medium


        do666 i=1,3
                p1(i) = vtan(i)+zs1*normal(i)
                p2(i) = vtan(i)+zs2*normal(i)
666        continue
        call groupvel(p1,axis,3,elast,w1)
        call groupvel(p2,axis,3,elast,w2)
        dot1 = 0.e0
        dot2 = 0.e0
        do777 i=1,3
                dot1 = dot1 + w1(i)*normal(i)
                dot2 = dot2 + w2(i)*normal(i)
777        continue

        if(iscat .EQ. 0) then
                if(dot1 .GT. 0.e0) then
                        pnorm = zs1
                else if(dot2 .GT. 0.e0) then
                        pnorm = zs2
                else
                        isol = 1
                        return
                endif
        else
                if(dot1 .GT. 0.e0) then
                        pnorm = zs2
                else if(dot2 .GT. 0.e0) then
                        pnorm = zs1
                else
                        isol = 1
                        return
                endif
        endif

        do300 i=1,3
                pscat(i) = vtan(i)+pnorm*normal(i)
300        continue

        return

      endif
      end



      subroutine search(xx,yy,x,y,g1,g2,dg2,sg1,sg2,error,
     &                  ng2,maxa)

      integer error, ng2, maxa
      real xx(maxa,1), yy(maxa,1), x, y
      real dg2, g1, g2, sg1(maxa), sg2(1)

*
*   determines correct ray take off angle for given receiver
*
*   INPUT VARIABLES:
*
*       xx(maxa,maxr)   x-coordinate interpolation map
*       yy(maxa,maxr)   y-coordinate interpolation map
*       x               x-coordinate of receiver
*       y               y-coordinate of receiver
*       ng2             number of dip angles in the map
*       maxr            max number of increments in the take-off 
*                       dip angle.
*       maxa            number of increments in the take-off azimuth
*                       angle plus two.
*
*   OUTPUT VARIABLES:
*
*       g1              take off azimuth at source point
*       g2              take off dip at source point    
*       dg2             reasonable angular aperture for the 
*                       ray tube around (g1,g2).
*       error           0 if successful search
*                       1 if receiver out of map reach
*                       2 if there is no body wave for the event 
*                       considered that reaches the receiver.
*
*   GLOBAL VARIABLES:
*
*       block "array"   source-receiver parameters
*

      real xs, ys, cazi, cdip, cr, cdg, xr, yr, cg1, ail, acl, dx, dt
      integer ng, tsamp
      common /array/ xs,ys,cazi,cdip,cr,cdg,xr,yr,cg1,
     &               ail,acl,dx,ng,tsamp,dt

      real infinity, x0, y0, g10, g20, small1, small2, small3
      real x1, x2, y1, y2, g11, g21, g12, g22, normal(3)
      real twopi, sign1, sign2, sign3, x3, y3
      integer i, j, i0, j0, i1, j1

      error = 0
      twopi =  6.283185308e0

      infinity = 1000000.*dx

*   determines in which grid triangle the receiver lies

      do10 j=1,ng2-1
        do20 i=1,maxa-1

*    searches first triangle type

                x1 = xx(i,j)
                x2 = xx(i,j+1)
                x3 = xx(i+1,j+1)
                y1 = yy(i,j)
                y2 = yy(i,j+1)
                y3 = yy(i+1,j+1)

                if(x1 .EQ. infinity .OR. x2 .EQ. infinity 
     &                  .OR. x3 .EQ. infinity) then
                        error = 2
                        goto 20
                endif

                sign1 = (x-x1)*(y2-y1)-(y-y1)*(x2-x1)
                sign2 = (x-x2)*(y3-y2)-(y-y2)*(x3-x2)
                sign3 = (x-x3)*(y1-y3)-(y-y3)*(x1-x3)

*   if sine of angle between (x-xi) and (xj-xi) is less than 1.e-4 
*   then the vectors are considered colinear. 
*   Theorem: two quantities are identical if the computer can't 
*   make the difference.

                small1 = (x-x1)*(x-x1)+(y-y1)*(y-y1)
		small1 = small1*((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
		small1 = small1*1.e-8
                small2 = (x-x2)*(x-x2)+(y-y2)*(y-y2)
		small2 = small2*((x2-x3)*(x2-x3)+(y2-y3)*(y2-y3))
		small2 = small2*1.e-8
                small3 = (x-x3)*(x-x3)+(y-y3)*(y-y3)
		small3 = small3*((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1))
		small3 = small3*1.e-8

                if(sign1*sign1 .LE. small1 
     &             .AND. sign2*sign3 .GE. 0.e0) then
                        i0 = i
                        j0 = j+1
                        i1 = i+1
                        j1 = j
                        goto 100
                endif
                if(sign2*sign2 .LE. small2 
     &             .AND. sign3*sign1 .GE. 0.e0) then
                        i0 = i
                        j0 = j+1
                        i1 = i+1
                        j1 = j
                        goto 100
                endif
                if(sign3*sign3 .LE. small3 
     &             .AND. sign1*sign2 .GE. 0.e0) then
                        i0 = i
                        j0 = j+1
                        i1 = i+1
                        j1 = j
                        goto 100
                endif
                if(sign1*sign2 .GT. 0.e0 .AND. sign2*sign3
     &                  .GT. 0.e0) then
                        i0 = i
                        j0 = j+1
                        i1 = i+1
                        j1 = j
                        goto 100
                endif

*   searches second triangle type (only j > 1)

        if(j .NE. 1) then

                x2 = xx(i+1,j)
                y2 = yy(i+1,j)

                if(x2 .EQ. infinity) then
                        error = 2
                        goto 20
                endif

                sign1 = (x-x1)*(y2-y1)-(y-y1)*(x2-x1)
                sign2 = (x-x2)*(y3-y2)-(y-y2)*(x3-x2)
                sign3 = (x-x3)*(y1-y3)-(y-y3)*(x1-x3)

                small1 = (x-x1)*(x-x1)+(y-y1)*(y-y1)
		small1 = small1*((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
		small1 = small1*1.e-8
                small2 = (x-x2)*(x-x2)+(y-y2)*(y-y2)
		small2 = small2*((x2-x3)*(x2-x3)+(y2-y3)*(y2-y3))
		small2 = small2*1.e-8
                small3 = (x-x3)*(x-x3)+(y-y3)*(y-y3)
		small3 = small3*((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1))
		small3 = small3*1.e-8

                if(sign1*sign1 .LE. small1 
     &             .AND. sign2*sign3 .GE. 0.e0) then
                        i0 = i+1
                        j0 = j
                        i1 = i
                        j1 = j+1
                        goto 100
                endif
                if(sign2*sign2 .LE. small2 
     &             .AND. sign3*sign1 .GE. 0.e0) then
                        i0 = i+1
                        j0 = j
                        i1 = i
                        j1 = j+1
                        goto 100
                endif
                if(sign3*sign3 .LE. small3 
     &             .AND. sign1*sign2 .GE. 0.e0) then
                        i0 = i+1
                        j0 = j
                        i1 = i
                        j1 = j+1
                        goto 100
                endif
                if(sign1*sign2 .GT. 0.e0 .AND. sign2*sign3
     &                  .GT. 0.e0) then
                        i0 = i+1
                        j0 = j
                        i1 = i
                        j1 = j+1
                        goto 100
                endif

        endif

20         continue
10      continue

      error = 1
      return

100   x0 = xx(i0,j0)
      y0 = yy(i0,j0)
      g10 = sg1(i0)
      g20 = sg2(j0)

      x1 = xx(i0,j1)
      y1 = yy(i0,j1)
      g11 = sg1(i0)
      g21 = sg2(j1)

      x2 = xx(i1,j0)
      y2 = yy(i1,j0)
      g12 = sg1(i1)
      g22 = sg2(j0)

*   interpolates within the grid triangle in which 
*   the receiver lies

      normal(1) = (y1-y0)*(g12-g10)-(g11-g10)*(y2-y0)
      normal(2) = (g11-g10)*(x2-x0)-(x1-x0)*(g12-g10)
      normal(3) = (x1-x0)*(y2-y0)-(y1-y0)*(x2-x0)

      g1 = g10 - ((x-x0)*normal(1)+(y-y0)*normal(2))/normal(3)

      normal(1) = (y1-y0)*(g22-g20)-(g21-g20)*(y2-y0)
      normal(2) = (g21-g20)*(x2-x0)-(x1-x0)*(g22-g20)
      normal(3) = (x1-x0)*(y2-y0)-(y1-y0)*(x2-x0)

      g2 = g20 - ((x-x0)*normal(1)+(y-y0)*normal(2))/normal(3)
      dg2 = abs(sg2(j1)-sg2(j0))/10.e0

      return
      end



      subroutine shootmap(xx,yy,sg1,sg2,ipath,ng2,maxr,maxa,ddip)

      integer ipath, ng2, maxa, maxr
      real xx(maxa,maxr), yy(maxa,maxr)
      real ddip, sg1(maxa), sg2(maxr)

*
*   kinematic shooting routine to create interpolation map
*   to be used in generating shot profiles.
*
*   INPUT VARIABLES:
*
*       ipath           raypath index
*
*   OUTPUT VARIABLES:
*
*       maxa            number of azimuthal divisions of the map
*                       plus two (# take-off azimuth + 2)
*       maxr            maximum number of radial subdivisions of 
*                       the map (max # of take-off dips)
*       xx(maxa,maxr)   x coordinate of ray emergence point.
*                       First index represents azimuth of 
*                       slowness at the source, starting at -dang
*                       and ending at (360+dang) by steps of dang
*                       Second index represents dip of slowness at
*                       the source, starting at 90 degrees.
*                       (dang=360/(maxa-2) degrees)
*       yy(maxa,maxr)   y coordinate ...   "    "    "    "
*       ng2             number of take-off dip angles actually 
*                       used in computing the map.
*       ddip            initial dip increment from 90 degrees.
*
*   GLOBAL VARIABLES:
*
*       block "model"   geometric and elastic model parameters
*       block "path"    raypath parameters
*       block "array"   source-receiver parameters
*
*   EXTERNAL ROUTINES:
*
*       initkin         kinematic ray initializer
*       tracekin        kinematic ray tracer
*       croskin         kinematic continuator at boundaries
*
      

      real param(900), geom(0:99), dt
      real xs, ys, cazi, cdip, cg1, dx, cr, cdg, ail, acl, xr, yr
      integer nlayer, nsegment(200), mode(200,100), tsamp
      integer interface(200,0:100), triso(100), ng, npath
      common /model/ param, geom, nlayer, triso
      common /path/ mode, interface, nsegment, npath
      common /array/ xs,ys,cazi,cdip,cr,cdg,xr,yr,cg1,
     &               ail,acl,dx,ng,tsamp,dt

      real p(3), p0(3), x(3), x0(3), rmin(100000)
      real t, t0, g2, g1, infinity, xxx, deg2rad, maxdist
      real rmax(100000), xend, yend, dang
      integer iseg, inc, scat, i, error, k, l, ndip
      integer kmax, minus

*   initialize xx and yy for future recognition of postcritical boundary

      infinity = 1000000.*dx

      do1 k=1,maxa
         do2 l=1,maxr
            xx(k,l) = infinity
            yy(k,l) = infinity
2        continue
1     continue

*   computing maximum offset and azimuth increment, 
*   and initializing number of dips

      ndip = maxr
      ng2 = ndip
      deg2rad = 1.72453293e-2
      dang = 360.e0/(maxa-2)
      xend = xr+(ng-1)*dx*cos(cg1*deg2rad)
      yend = yr+(ng-1)*dx*sin(cg1*deg2rad)

*   kinematic ray tracing
      
      g1 = -2.e0*dang

      do40 k=1,maxa
        g1 = g1+dang
        sg1(k) = g1
40      continue

      sg2(1) = 90.e0

      g2 = ddip

      do10 l=1,ndip

        rmin(l) = infinity*infinity
        rmax(l) = 0.e0

        minus = 0

      do5 k=1,maxa

        call initkin(mode(ipath,1),sg1(k),sg2(l),cr,x0,p0,t0)

        if(x0(3) .le. 0.e0) then
*       rejects rays that do not go downward
                goto 5
        endif

        do20 iseg=1,nsegment(ipath)-1
                

                call tracekin(p0,x0,t0, interface(ipath,iseg-1),
     &                  interface(ipath,iseg),mode(ipath,iseg),x,t)

                inc = max0(interface(ipath,iseg-1),
     &                  interface(ipath,iseg))

                scat = max0(interface(ipath,iseg),
     &                  interface(ipath,iseg+1))
      
                call croskin(p0,inc,scat,
     &                  mode(ipath,iseg+1),p,error)


                if(error .NE. 0) then
                        goto 1000
                endif

                t0 = t
                
                do30 i=1,3
                        p0(i) = p(i)
                        x0(i) = x(i)
30                 continue

20         continue

        call tracekin(p0,x0,t0,interface(ipath,
     &          nsegment(ipath)-1),interface(ipath,nsegment(ipath)),
     &          mode(ipath,nsegment(ipath)),x,t)

        xx(k,l) = x(1)
        yy(k,l) = x(2)

        xxx = (x(1)-xs)*(x(1)-xs)+(x(2)-ys)*(x(2)-ys)

        if(xxx .LT. rmin(l)) then
           rmin(l) = xxx
        endif

        if(xxx .GT. rmax(l)) then
           rmax(l) = xxx
           kmax = k
        endif

5       continue

        if(l .GT. 1) then
           xxx = (xx(kmax,l)-xx(kmax,l-1))
           xxx = xxx*xxx
           xxx = xxx + (yy(kmax,l)-yy(kmax,l-1))
     &           *(yy(kmax,l)-yy(kmax,l-1))
              if(xxx .GT. 4.e0*dx*dx) then
                  g2 = g2/2.e0
              endif
        else 
           maxdist = (xend-xx(1,1))*(xend-xx(1,1))
           maxdist = maxdist+(yend-yy(1,1))*(yend-yy(1,1))
           xxx = (xr-xx(1,1))*(xr-xx(1,1))
           xxx = xxx+(yr-yy(1,1))*(yr-yy(1,1))
           maxdist = amax1(maxdist,xxx)
        endif

        sg2(l+1) = sg2(l)-g2

        if(rmin(l) .GT. maxdist .AND. l .GT. 1) then
           ng2 = l
           goto 2000
        endif 

        if(rmin(l) .EQ. infinity) then
           ng2 = l
           minus = 1
           goto 2000
        endif 

1000    if(error .NE. 0) then
           ng2 = l
           minus = 1
           goto 2000
        endif 

10      continue

        if(l .EQ. ndip+1) then
           minus=1
        endif

2000    continue

        ng2 = ng2-minus

*    insure that emergence point is identical for all azimuths at zero dip.

      do70 k=2,maxa
        xx(k,1) = xx(1,1)
        yy(k,1) = yy(1,1)
70    continue

      return
      end



      subroutine slowness(p,axis,mode,elast)
      integer mode
      real p(3), axis(3), elast(6)
*
*   computes slowness from its direction.
*
*   INPUT VARIABLES:
*
*       p(3)            direction of slowness (any modulus)
*       axis(3)         anisotropy axis unit vector
*       mode            1 for SP, 2 for QP, 3 for QS
*       elast(6)        elastic parameters in following order:
*                       A, C, N, L, F, rho.
*
*   OUTPUT VARIABLES:
*
*       p(3)            slowness vector
*

      integer i
      real s2, c2, A, C, L, N, F, RHO, alpha, beta, cosine, slow

      cosine = 0.

*   COMPUTES COSINE OF ANGLE BETWEEN SLOWNESS AND ANISOTROPY AXIS

      do 10 i=1,3
        cosine = cosine + p(i)*axis(i)
10      continue

      c2 = cosine*cosine
      s2 = 1.e0 - c2
      A = elast(1)
      C = elast(2)
      N = elast(3)
      L = elast(4)
      F = elast(5)
      RHO = elast(6)

*   COMPUTES MODULUS OF SLOWNESS ACCORDING TO MODE

      if(mode .EQ. 1) then
        slow = sqrt(RHO/(N*s2+L*c2))
      else
        alpha = (A+L)*s2 + (C+L)*c2
        beta = (A-L)*s2 - (C-L)*c2
        beta = beta*beta + 4.e0*s2*c2*(L+F)*(L+F)
        beta = sqrt(beta)
        if(mode .EQ. 2) then
                slow = sqrt(RHO*2./(alpha+beta))
        else if(mode .EQ. 3) then
                slow = sqrt(RHO*2./(alpha-beta))
        else
                slow = 1.e0
        endif
      endif

*   MULTIPLIES SLOWNESS UNIT VECTOR BY ITS MODULUS

      do 20 i=1,3
        p(i) = p(i)*slow
20      continue

      return
      end



      subroutine tracekin(p0,x0,t0 ,start,end,mode,x,t)

      real p0(3), x0(3), t0, x(3), t
      integer mode, start, end

*
*   This routine traces a ray between interfaces
*
*   INPUT VARIABLES:
*
*       p0(3)   ray slowness vector
*       x0(3)   ray starting point
*       t0      ray initial traveltime
*       start   index of starting interface
*       end     index of destination interface
*       mode    type of wave: 1 for SP, 2 for QP, 3 for QS
*
*   OUTPUT VARIABLES:
*
*       x(3)    ray destination point
*       t       ray final traveltime
*   
*   GLOBAL VARIABLES:
*
*       block "model"   geometric and elastic model parameters.
*

      integer nlayer, triso(100)
      real param(900), geom(0:99)
      common /model/ param, geom, nlayer, triso

      real axis(3), elast(6), w0(3)
      integer i,ii,layer

      layer = max0(start,end)
      ii = (layer-1)*9

      do10 i=1,6
        elast(i) = param(ii+i)
10      continue

      do20 i=1,3
        axis(i) = param(ii+i+6)
20      continue

*   compute the ray direction

      call groupvel(p0,axis,mode,elast,w0)

*   compute the traveltime and arrival point for the ray

      x(3) = geom(end)
      t = t0 + (x(3)-x0(3))/w0(3)
      x(1) = x0(1)+w0(1)*(t-t0)
      x(2) = x0(2)+w0(2)*(t-t0)

      return
      end
