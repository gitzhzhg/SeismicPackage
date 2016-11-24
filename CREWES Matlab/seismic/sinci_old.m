function trout=sinci_old(trin,t,tout,sizetable)
% SINCI: sinc function interpolation for time series without nan's
%
%  trout=sinci(trin,t,tout,sizetable)
%
% SINCI performs 8 point sinc function interpolation using a 
% design for the approximate sinc function due to Dave Hale.
%
% trin= input trace
% t= time coordinate vector for trin. Trin must be regularly
%    sampled. SINCI uses only the first two points in t.
% tout= vector of times for which interpolated amplitudes are
%       desired
% trout= output trace. Contains the length(tout) interpolated 
%        amplitudes.
% sizetable= size of the sinc function table: [npts,nfuncs]
%     where npts = number of points on the sinc function and
%     nfuncs = number of uniquely optimized sinc functions. If dt is
%	  the input sample interval, then there will be a unique, optimized
%	  sinc function designed for interpolation every nfuncs'th of 
%	  dt.
%   ************* default = [8 25] *********
%
% by G.F. Margrave, November 1991
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

%time=clock;
trflag=0;

if nargin<4, sizetable=[8,25];end

global SINC_TABLE

% see if table needs to be made
maketable=0;
[lsinc,ntable]=size(SINC_TABLE);
if( lsinc*ntable==0)
	maketable=1;
elseif( lsinc~=sizetable(1) || ntable~=sizetable(2) )
	maketable=1;
end
	

%convert to row vector
[rr,cc]=size(trin);
if(rr>1)
		trin=trin';
		t=t';
		tout=tout';
		trflag=1;
end

% initialize trout
trout=zeros(size(tout));

if(maketable)
    % Make the sinc function table
     lsinc=sizetable(1);
     ntable=sizetable(2);
    % lsinc should be an even integer
     frac=(0:ntable-1)/ntable;
     SINC_TABLE=zeros(lsinc,ntable);
     jmax=fix(ntable/2)+1;
    % the first half of the table is computed by least squares
    % while the second half is derived from the first by symmetry
    for j=1:jmax
            fmax=min([.066+.265*log(lsinc),1.0]);
            a=sinque(fmax*(0:lsinc-1));
            b=fmax*((lsinc/2-1:-1:-lsinc/2)+frac(j)*ones(1,lsinc));
            c=sinque(b);
            SINC_TABLE(:,j)=toeplitz(a',a)\c';
    end
    point=lsinc/2;
    jtable=ntable;ktable=2;
    while SINC_TABLE(point,jtable)==0.0
                SINC_TABLE(:,jtable)=flipud(SINC_TABLE(:,ktable));
                jtable=jtable-1;ktable=ktable+1;
    end
end

% now interpolate with the tabulated coefficients
% first extrapolate with constant end values
% for beginning:
  ii=find(tout<=t(1));
  if ~isempty(ii)
    trout(ii)=trin(1)*ones(1,length(ii));
  end
% for end:
  ii=find(tout>=t(length(t)));
  if ~isempty(ii)
    trout(ii)=trin(length(trin))*ones(1,length(ii));
  end
% intermediate samples
  dtin=t(2)-t(1);
  ii=find((tout>t(1))&(tout<t(length(t))));
  if(~isempty(ii))
	% changed the following on 8 October 93
	% pdata=(tout(ii)-tout(1))/dtin+1;
      pdata=(tout(ii)-t(1))/dtin+1;
      del=pdata-fix(pdata);
	% del now contains the fractional sample increment
	% for each interpolation site
	% compute row number in interpolation table
      ptable=1+round(ntable*del); 
	% compute pointer to input  data
      pdata=fix(pdata)+lsinc/2-1;
	% pad input data with end values
      trin=[trin(1)*ones(1,lsinc/2-1) trin trin(length(trin))*ones(1,lsinc/2)];
      ij=find(ptable==ntable+1);
      ptable(ij)=1*ones(1,length(ij));
      pdata(ij)=pdata(ij)+1;
	% finally interpolate by a vector dot product
	%  trout(ii)=trin(pdata-lsinc/2+1:pdata+lsinc/2)*SINC_TABLE(:,ptable); % why doesn't % this work just as well as the following loop????
	
      for k=1:length(ii)
		trout(ii(k))=trin(pdata(k)-lsinc/2+1:pdata(k)+lsinc/2)*SINC_TABLE(:,ptable(k));
      end
  end

 if(trflag)
		trout=trout';
 end
%etime(clock,time)