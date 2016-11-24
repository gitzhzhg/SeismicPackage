% The CREWES Project Raytrace toolbox
%
% raytrace - Raytracing in a v(z) world. The toolbox contains functions for
%            shooting fans of rays and functions for tracing rays with specific
%            start and end points. General multi-bounce rays with any number
%            of mode conversions can be traced. Automatic routines for 
%            P-P and P-S primaries in shot-gather geometry are provided for 
%            convenience. Any recording geometry can be used but most functions
%            vectorize (e.g. are most efficient) for shot-gather geometry. Only
%	     traveltimes and rayparameters are detemined, not amplitudes. In
%            these routines, the velocity models should be considered as
%            "background" models. This means that the depths at which reflections
%            and mode conversions occur are independent of the depths given
%            as layer boundaries in the velocity models. Velocity models are
%            specified by a vector of velocities and a same-size vector of depths.
%            The j th velocity is in effect from the j th to the j+1 th depth. 
%            The bottom of the model is assumed at infinite depth. Thus v=5000
%            and z=0 specifies a constant velocity half space.
%            Once the appropriate ray parameters have been determined by one of the
%            traceray functions, they can be drawn using drawray, or otherwise
%            analyzed using shootray. For example, the coordinates of reflection
%            or transmission points can be determined. To see how this might be
%            done, examine the code at the end of one of the traceray functions
%            where the raypaths are drawn after the ray parameters have been
%	     determined.
% 
% raytrace_demo --- run a demo of ray tracing abilities.
% drawray       --- draw raypaths. (Note blocky velocity functions can be drawn
%                   with drawvint from the velocity toolbox.)
% rayfan        --- compute a fan of rays given their ray parameters.
% rayfan_a      --- compute a fan of  rays given their takeoff angles.
% shootray      --- fast version of rayfan.
% traceray      --- general raytracing. Handles any number of bounces and/or
%                   mode conversions.
% traceray_pp   --- trace primary rays for a p-p source gather.
% traceray_ps   --- trace primary rays for a p-s source gather.
% 
% (C) The CREWES Project, 1996  
% NOTE: It is illegal for you to use this software for a purpose other
% than non-profit education or research UNLESS you are employed by a CREWES
% Project sponsor. By using this software, you are agreeing to the terms
% detailed in this software's Matlab source file.
 
% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by 
% its author (identified above) and the CREWES Project.  The CREWES 
% project may be contacted via email at:  crewesinfo@crewes.org
% 
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) Use of this SOFTWARE by any for-profit commercial organization is
%    expressly forbidden unless said organization is a CREWES Project
%    Sponsor.
%
% 2) A CREWES Project sponsor may use this SOFTWARE under the terms of the 
%    CREWES Project Sponsorship agreement.
%
% 3) A student or employee of a non-profit educational institution may 
%    use this SOFTWARE subject to the following terms and conditions:
%    - this SOFTWARE is for teaching or research purposes only.
%    - this SOFTWARE may be distributed to other students or researchers 
%      provided that these license terms are included.
%    - reselling the SOFTWARE, or including it or any portion of it, in any
%      software that will be resold is expressly forbidden.
%    - transfering the SOFTWARE in any form to a commercial firm or any 
%      other for-profit organization is expressly forbidden.
%
% END TERMS OF USE LICENSE
