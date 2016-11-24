function v1rec=calcvel(fbtime,fbcoord,shotcoord,cvpavg,nshots,recelev)
% Calculation of the first layer velocity based on a polyfit of the direct arrivals
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

% The cros over point averages are used to determine the range of the direct arrivals 
v1=NaN.*ones(nshots,2);
avgv1=NaN.*ones(nshots,1);
[a b]=size(recelev);
v1rec=NaN.*ones(b,1);
% Polyfit of the direct arrivals for each shot
for n=1:nshots
 validt = find(~isnan(fbtime(n,:)));
  % Left side calculation, direct arrivals from the left cross over point average 
  % to the shot location
  if (isnan(cvpavg(n,1)) ~=1)
	validfbc=find(fbcoord(n,validt)>cvpavg(n,1) & fbcoord(n,validt)<shotcoord(n)); 
	[a b]=size(validfbc);
     if (b>2)
	p=polyfit(fbcoord(n,validt(validfbc)),fbtime(n,validt(validfbc)),1);
	v1(n,1)=abs(1/p(1,1));
     end
  end
  % Rigth side calculation, direct arrivals from the rigth cross over point average 
  % to the shot location
  if (isnan(cvpavg(n,2)) ~=1)
	validfbc=find(fbcoord(n,validt)<cvpavg(n,2) & fbcoord(n,validt)>shotcoord(n));
	[a b]=size(validfbc);
     if (b>2) 
	p=polyfit(fbcoord(n,validt(validfbc)),fbtime(n,validt(validfbc)),1);
	v1(n,2)=abs(1/p(1,1));
     end
  end
  % Average of both side velocities
  goodv1=find(~isnan(v1(n,:)));
  if (size(goodv1)>0)
	avgv1(n)=mean(v1(n,goodv1));
  end
end
% interpolation of the first layer velocity for the nonvalid shot
okavg = find(~isnan(avgv1(:,1)));
nanavg = find(isnan(avgv1(:,1)));
firstokavg = okavg(1);                % This is the first good shot
lastokavg = okavg(length(okavg));     % This is the last good shot
% Build a new v1 average array, with only the points between
% the first and last good shots.  This will be interpolated.
v1int = avgv1(firstokavg:lastokavg,1);
shotint = firstokavg:lastokavg;
% Now we need to find the NaN's in the v1 average array that
% will be interpolated (v1int)
okavg = find(~isnan(v1int));
nanavg = find(isnan(v1int));
% Interpolate over any NaN's in the middle v1 array. 
fixed = interp1(shotint(okavg), v1int(okavg), shotint(nanavg));
% Now, put the interpolated v1 average values back into the middle array
v1int(nanavg) = fixed;
% Now put the middle array in the original array
avgv1(firstokavg:lastokavg,1) = v1int;
% find and average v1 at both end of the good array 
v1firstext=mean(avgv1(firstokavg:firstokavg + 3,1));
v1lastext=mean(avgv1(lastokavg-3:lastokavg,1));
% extrapolation for the v1 average outside the first and the last good shot
okavg = find(~isnan(avgv1(:,1)));
nanavg = find(isnan(avgv1(:,1)));
firstokavg = okavg(1);                % This is the first good shot
lastokavg = okavg(length(okavg));     % This is the last good shot
% replacement of the outside array by the corresponding end v1 average 
indstart=find(nanavg<firstokavg); 
indend=find(nanavg>lastokavg);
avgv1(nanavg(indstart),1)=v1firstext*ones(length(indstart),1);
avgv1(nanavg(indend),1)=v1lastext*ones(length(indend),1); 
% interpolation to each receiver station
v1rec=interpextrap(shotcoord,avgv1,recelev(1,:));
% replacement of the extrapolated receiver velocity by a velocity average
minloc=min(shotcoord);
maxloc=max(shotcoord);
intindex=find(recelev(1,:)>minloc & recelev(1,:)<maxloc);
extfind=find(recelev(1,:)<minloc);
extlind=find(recelev(1,:)>maxloc);
v1fext=mean(v1rec(1,intindex(1:4)));
v1lext=mean(v1rec(1,intindex(length(intindex)-4:length(intindex))));
v1rec(1,extfind)=v1fext*ones(1,length(extfind));
v1rec(1,extlind)=v1lext*ones(1,length(extlind));