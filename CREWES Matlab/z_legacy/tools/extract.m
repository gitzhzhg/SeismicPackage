function randobject = extract(extractfile,extractinc)
%
% randobject = extract(extractfile,extractinc)
%
%   where randobject is a random object which
%   can be read by slicetool, and
%   extractfile is an ascii card file output
%   in ZMAP format by extract_raster, for
%   example, extractfile='denis.dat';
%
% lineid     sp       utmx        utmy      t1     t2       t3    t4    t5    t6    index
%wcb85115    200     1245655.25  385304.34  862.0 -111.9                               129
%wcb85115    197     1246045.38  384917.06  865.4 -125.0                               130
%wcb85115    195     1246435.50  384529.81  868.8 -128.8                               131
%wcb85115    192     1246825.63  384142.56  871.8 -128.1                               132
%wcb85115    190     1247215.75  383755.31  874.6 -127.1                               133
%wcb85115    187     1247606.00  383368.06  877.3 -106.9                               134
%wcb85115    185     1247996.13  382980.81  879.8  -91.3                               135
%
% and extractinc is the trace increment used in extract raster, for example,
% if every trace was taken, extractinc=1, every other trace, extractinc=2, etc.
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
% READ IN DATA 
%
% The global coordinates are in x and y. 
%
% read in ascii seis file (from seisline, originally)
[lineno,tr,xtr,ytr,z1,z2,z3,z4,z5,z6,index,com,ncom]=...
      readseisfile(extractfile);
% The global coordinates are in xtr and ytr. 
%
% MAKE LOCAL COORDINATES
%
% xtrr and ytrr contain local trace coordinates. 
%
xmin=min(xtr);
ymin=min(ytr);
xtrr = (xtr-xmin)/1000.;
ytrr = (ytr-ymin)/1000.;
dtrr = sqrt( (xtrr-xtrr(1)).^2 + (ytrr-ytrr(1)).^2 );
len = length(xtrr);
dinc = sqrt((xtrr(2:len)-xtrr(1:len-1)).^2 + ...
            (ytrr(2:len)-ytrr(1:len-1)).^2 );
dinc = [0 dinc];
dtrr = cumsum(dinc);
if(extractinc>0)
  index = index*extractinc;
end
%
% MAKE SEISMIC LINE (RAND) OBJECT
fprintf('Now making matlab object');
[lmin,lmax,nlines] = split(lineno);
% make line objects
fileobj=contobj('file','prvt');
fileobj=objset(fileobj,'filename','slicemaster');
fileobj=objset(fileobj,'pathname',' ');
for i = 1:nlines
  linen=randobj( lineno(lmin(i),:) ,tr(lmin(i):lmax(i)),...
   'shotpt',xtrr(lmin(i):lmax(i)),ytrr(lmin(i):lmax(i)),'line');
  linen=objset(linen,'inline dist',dtrr(lmin(i):lmax(i)));
  linen=objset(linen,'index',index(lmin(i):lmax(i)));
  if(ncom < 1)
    fprintf(' warning: you have no horizons in the file!');
  end 
  linen=objset(linen,com(1,:),z1(lmin(i):lmax(i)));
  if(ncom >= 2)
    linen=objset(linen,com(2,:),z2(lmin(i):lmax(i)));
  end
  if(ncom >= 3)
    linen=objset(linen,com(3,:),z3(lmin(i):lmax(i)));
  end
  if(ncom >= 4)
    linen=objset(linen,com(4,:),z4(lmin(i):lmax(i)));
  end
  if(ncom >= 5)
    linen=objset(linen,com(5,:),z5(lmin(i):lmax(i)));
  end
  if(ncom >= 6)
    linen=objset(linen,com(6,:),z6(lmin(i):lmax(i)));
  end
  linebox=contobj(lineno(lmin(i),:),'slce');
  linebox=objset(linebox,'randobj',linen);
  linebox=objset(linebox,'file',fileobj);
% make the container objset
	if( i==1 )
		randobject=contobj('All lines',linebox,...
            lineno(lmin(1),:));
        else
          randobject = objset(randobject,lineno(lmin(i),:),...
             linebox);
        end
end
size(randobject)