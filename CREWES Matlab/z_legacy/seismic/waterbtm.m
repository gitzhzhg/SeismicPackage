function mtrain=waterbtm(twater,t,rc,flag)
% mtrain=waterbtm(twater,t,rc,flag)
% mtrain=waterbtm(twater,t,rc)
% mtrain=waterbtm(twater,t)
%
% WATERBTM computes the impulse response for a zero offset water
%  bottom multiple.
%
% twater= vertical traveltime thickness of the water (2-way)
% t= time coordinate vector. This will determine the sample rate
%    and length of mtrain
% rc= reflection coefficient of the water bottom
% ********** default =.2 *************
% flag= 0 ... produce the combinde response (source and receiver
%             multiples combined)
%       1 ... only the source multiples are desired
% ********** default = 0 ******************
%
% by G.F. Margrave, July 1991
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
 if nargin<4, flag=0; end
 if nargin<3, rc=.2; end
% generate the one way response
 time=twater;
 r=1;
 mtrain=impulse(t,1);
 while time<=max(t)
  tw=near(t,time);
  r=r*(-rc);
  mtrain(tw)=r;
  time=time+twater;
 end
 if flag==0,
   mtrain=conv(mtrain,mtrain);
   mtrain=mtrain(1:length(t));
 end
  
  
 