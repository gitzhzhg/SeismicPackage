function [cvpavg,cvpstd,cvpfold]=avgcvp(cvpi,cvpj,nshots)
% Calculation of the average cross over points for each shot
% The left side cross over point (cvpi) and the rigth side cross over point
% (cvpj) are averaged independently and store in two collom
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

% The standard deviation and the fold are found for each of them
 
cvpavg=NaN.*ones(nshots,2);
cvpstd=NaN.*ones(nshots,2);
cvpfold=NaN.*ones(nshots,2);
% Averaging loop for each shot
for n=1:nshots
    % Left cross over points averaging
	validcvpi = find(~isnan(cvpi(n,:)));
	[a b]= size(validcvpi);
    if( b ~=0 )
	avgcvpi(n)=mean(cvpi(n,validcvpi));
	cvpavg(n,1)=avgcvpi(n);
        cvpstd(n,1)=std(cvpi(n,validcvpi));
	cvpfold(n,1)=length(validcvpi);
    end
    % Rigth cross over points averaging
	validcvpj = find(~isnan(cvpj(:,n)));
	[a b]= size(validcvpj);
    if( b ~=0 )
	avgcvpj(n)=mean(cvpj(validcvpj,n));
	cvpavg(n,2)=avgcvpj(n);
        cvpstd(n,2)=std(cvpj(validcvpj,n));
	cvpfold(n,2)=length(validcvpj);
    end
end