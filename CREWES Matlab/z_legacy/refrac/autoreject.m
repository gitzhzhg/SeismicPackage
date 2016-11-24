function [cvpi,cvpj]=autoreject(cvpi,cvpj,nshots,dev,standard)
% Function used to reject some of the cross over point based on the standard
% deviation or a constant difference from the cross over point average for
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

% each shot
si=NaN.*ones(1,nshots);
sj=NaN.*ones(1,nshots);
mi=NaN.*ones(1,nshots);
mj=NaN.*ones(1,nshots);
% Call the function calculating the average cross over points 
[cvpavg cvpstd,cvpfold] = avgcvp(cvpi, cvpj,nshots);
% Cross over point average for each shot (cvpi=left side; cvpj=right side)
meani = cvpavg(:,1);
meanj = cvpavg(:,2);
devi = cvpstd(:,1);
devj = cvpstd(:,2);
% Determination of the bad cross over point based on the standard deviation 
% or a constant limit
for n=1:nshots
	% Average the cross over points for 'cvpi' (left side)
	validcvpi = find(~isnan(cvpi(n,:)));
	x=cvpi(n,validcvpi);
	d = abs(x - meani(n));
    if (standard==1)
	f = dev * devi(n); % rejection based on the standard deviation
    else
	f = dev; % rejection based on a constant limit
    end
        badcvpi = find(d>f);
	[a b]=size(badcvpi);
	if (b ~=0)
	   x(badcvpi)=NaN*badcvpi;
	   cvpi(n,validcvpi) = x;
	end
	% Average the cross over points for 'cvpj' (right side)
	validcvpj = find(~isnan(cvpj(:,n)));
	x=cvpj(validcvpj,n);
	d = abs(x - meanj(n));
    if (standard==1)
	f = dev * devj(n);
    else
	f = dev;
    end
        badcvpj = find(d>f);
	[a b]=size(badcvpj);
	if (b ~=0)
	   x(badcvpj)=NaN*badcvpj;
	   cvpj(validcvpj,n) = x;
	end
end