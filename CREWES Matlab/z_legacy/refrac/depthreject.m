function [hg1]=depthreject(hg1,dev,standard)
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

[coord, alldepth, depth, fold] = avgdepth(fbcoord,hg1);
for n=1:nshots
	% Average the crossover points for 'i' (left side)
	validcvpi = find(~isnan(cvpi(n,:)));
	x=cvpi(n,validcvpi);
	d = abs(x - meani(n));
    if (standard==1)
	f = dev * devi(n);
    else
	f = dev;
    end
        badcvpi = find(d>f);
	[a b]=size(badcvpi);
	if (b ~=0)
	   x(badcvpi)=NaN*badcvpi;
	   cvpi(n,validcvpi) = x;
	end
	% Average the crossover points for 'j' (right side)
	validcvpj = find(~isnan(cvpj(n,:)));
	x=cvpj(n,validcvpj);
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
	   cvpj(n,validcvpj) = x;
	end
end