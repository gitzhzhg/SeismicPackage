function [fbtime, fbcoord, shotcoord, nshots, nrecs, uphole, recelev, shotelev] = loadPM
% Load files exported from ProMax
% using database ASCII save
% Data is loaded from ten files:
%  fbpick:    (trace#) refracted arrival times
%  sinx:      (shot#)  shot x-coordinate
%  siny:      (shot#)  shot y-coordinate
%  recx:      (rec#)   receiver x-coordinate
%  recy:      (rec#)   receiver y-coordinate
%  uphole:    (shot#)  uphole time
%  sine:      (shot#)  shot elevation
%  rece:      (rec#)   receiver elevation
%  trcrec:    (trace#) trace vs surface station (rec)
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

%  trcsin;    (trace#) trace vs shot number
more off
[fbpickname fbpath] = uigetfile('*.a_db', 'Select the First Break Pick file');
% If they cancel at this point, then cancel loading
if( strcmp(fbpickname,'') | fbpickname==0 )
   return;
else
   [sinename epath] = ...
       uigetfile('*.a_db', 'Select the SIN - elevation file');
   [recename epath] = ...
       uigetfile('*.a_db', 'Select the REC (SRF)- elevation file');
   [sinxname xpath] = ...
       uigetfile('*.a_db', 'Select the SIN - X Coordinate file');
   [sinyname ypath] = ...
       uigetfile('*.a_db', 'Select the SIN - Y Coordinate file');
   [recxname xpath] = ...
       uigetfile('*.a_db', 'Select the REC (SRF)- X Coordinate file');
   [recyname ypath] = ...
       uigetfile('*.a_db', 'Select the REC (SRF)- Y Coordinate file');
   [upholename upholepath] = ...
       uigetfile('*.a_db', 'Select the Uphole Time file');
   [trcrecname trcpath] = ...
       uigetfile('*.a_db', 'Select the REC (SRF) - Trace number file');
   [trcsinname trcpath] = ...
       uigetfile('*.a_db', 'Select the SIN - Trace number file');
end
% If the user does not want to read an uphole file, make sure that
% both 'cancel' and 'done' (with no file) work.
if( strcmp(upholename,'') | upholename == 0 )
   upholefile = 0;
   upholepath = 0;
end
fprintf(1,'Starting to read DB files\n');
% Define a filename from the combination of the path and the file
fbpickfile = [fbpath fbpickname];
sinefile = [epath sinename];
recefile = [epath recename];
sinxfile = [xpath sinxname];
sinyfile = [ypath sinyname];
recxfile = [xpath recxname];
recyfile = [ypath recyname];
upholefile = [upholepath upholename];
trcrecfile = [trcpath trcrecname];
trcsinfile = [trcpath trcsinname];
% Call the reading function
[trace fbpick] = readPMdb(fbpickfile);
[nsin sine] = readPMdb(sinefile);
[nrec rece] = readPMdb(recefile);
[nsin sinx] = readPMdb(sinxfile);
[nsin siny] = readPMdb(sinyfile);
[nrec recx] = readPMdb(recxfile);
[nrec recy] = readPMdb(recyfile);
[trace srf] = readPMdb(trcrecfile);
[trace sin] = readPMdb(trcsinfile);
% If a trace is marked as 'dead' in ProMAX, its pick time will
% be a VERY LARGE number.  Check for this.
verylarge = 1000000000;
dtr = find(fbpick > verylarge);
fbpick(dtr) = NaN * ones(size(dtr));
% Allow variable values from the loaded file 
[m n] = size(nsin);
nshots=m;   % number of shots
[a b] = size(nrec);
nrecs=a;    % number of receivers
% Call the reading function for the uphole time file only
% a file has been selected
if( upholefile == 0 )
   uphole = zeros(nshots,1);
else
   [nsin uphole] = readPMdb(upholefile);
end
% Integration of X and Y coordinates in a single coordinate array
% Find the minimum coordinate among the shot (sin) and receiver (rec)
% coordinates.  This will be the origin of our coordinate system.
xo = min( min(sinx), min(recx) );
ir = find(recx == xo);
is = find(sinx == xo);
if( ir ~= 0 )
   xo = recx(ir);
   yo = recy(ir);
end
if( is ~= 0 )
   xo = sinx(is);
   yo = siny(is);
end
% If the coordinates have all the same X values (synthetic data, for eg.)
% then the 'xo' and 'yo' values will be vectors of (the same) number.
% Just pick the first one.
if( length(xo) > 1 )
   xo = xo(1);
end
if( length(yo) > 1 )
   yo = yo(1);
end
xs=sinx(:)-xo;
ys=siny(:)-yo;
s=sqrt(xs.^2+ys.^2);
shotcoord=s;
xr=recx(:)-xo;
yr=recy(:)-yo;
r=sqrt(xr.^2+yr.^2);
reccoord=r;
l=length(reccoord);
recelev=NaN*ones(2,l);
recelev(1,:)=reccoord';
recelev(2,:)=rece';
shotelev=sine';
trc=trace';
sin=sin';
srf=srf';
fbpick=fbpick';
nsrf=nrec';
srfcoord=reccoord';
trcsrf=NaN.*ones(nshots,nrecs);
fbcoord=NaN.*ones(nshots,nrecs);
fbtime=NaN.*ones(nshots,nrecs);
% Search for zeros in the fbpick matrix.  These come from dead traces,
% and must be replaced by NaNs.
dtr = find(fbpick == 0);
ndtr = size(dtr);
fbpick(dtr) = NaN * ones(ndtr);
fprintf(1,'Found %d zeros in the first break times\n', ndtr(1));
% Fill the fbtime  matrix with the refracted arrival times 
% Fill the fbcoord matrix with corresponding receiver coordinates
% (rows=shot number: colums=consecutive trace number) 
for i=1:nshots
   ind=find(sin(1,:)==i);
   trcsrf(i,1:length(ind)) = trc(1,ind);
   fbtime(i,1:length(ind)) = fbpick(1,ind) + uphole(i);
   nn=1;
   for n=ind
     indsrf=find(nsrf==srf(1,n));
     fbcoord(i,nn)=srfcoord(1,indsrf);
     nn=nn+1;
   end
end
fprintf(1,'Done reading DB files\n');
return;