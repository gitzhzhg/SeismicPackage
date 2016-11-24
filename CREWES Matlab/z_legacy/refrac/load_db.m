% Load the fbpick and offset file.
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

fprintf(1,'Loading fbpicks\n');
load fbpicks.txt;
fprintf(1,'Loading offset\n');
load offset.txt;
fprintf(1,'Done loading files\n');
% Make three vectors: trace#, offset, and fbpick
trace_vec = fbpicks(:,1)';
offset_vec = offset(:,2)';
fbpick_vec = fbpicks(:,2)';
% Geometry is 189 shots of 200 receivers
recs = 200;
shots = 189;
% Allocate a matrix of shots x receivers
shotoffset = zeros(shots, recs);
shotpick = zeros(shots, recs);
%display fbbreak in function of line location
load sinx.txt;
load siny.txt;
sin=sinx(:,1);
x=sinx(:,2)-sinx(189,2);
y=siny(:,2)-siny(189,2);
r=sqrt(x.^2+y.^2);
load uphole.txt;
u=uphole(:,2);
for i=1:shots
   first = (i-1)*recs+1;
   last = i*recs;
   fprintf(1,'Getting shot %d from traces %d to %d\n',i,first,last);
   shotoffset(i,1:recs) = offset_vec( first:last );
   shotpick(i,1:recs) = fbpick_vec( first:last );
   pickuphole(i,1:recs) = fbpick_vec( first:last) + u(i);
   reclocation(i,1:recs) = offset_vec( first:last ) + r(i);
end