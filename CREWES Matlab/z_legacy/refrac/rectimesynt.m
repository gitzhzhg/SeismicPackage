% nshots = number of shots 
% nrecs = number of receivers 
% fbcoord = receiver location matrix (nshots,nrecs)
% fbtime = fb pick matrix (nshots, nrecs)
% shotcoord = shot location vector (nshots)
%
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

% Output: diffmat = matrix of reciprocal shot fb pick differences
function diffmat = rectimesynt(nshots, nrecs, fbcoord, fbtime, shotcoord)
diffmat(nshots-1, nshots) = NaN;
for i=1:nshots-1;       % i is the first shot (t1)
  for j=i+1:nshots;    % j is the 2nd shot (t2)
    t1 = NaN;
    t2 = NaN;
    if( (shotcoord(j)>min(fbcoord(i,:))) & (shotcoord(j)<max(fbcoord(i,:))) )
       t1=interp1(fbcoord(i,:),fbtime(i,:),shotcoord(j),'linear');
    end
    if( shotcoord(i) > min(fbcoord(j,:)) & shotcoord(i) < max(fbcoord(j,:)) )
       t2=interp1(fbcoord(j,:),fbtime(j,:),shotcoord(i),'linear');
    end
    diffmat(i,j) = t2-t1;
  end
end