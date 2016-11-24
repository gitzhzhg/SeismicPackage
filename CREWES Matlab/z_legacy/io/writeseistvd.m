function code=writeseistvd(filename,x,y,z,zmd,utmx,utmy,uwid,tz,zt,flag)
% code=writeseistvd(filename,x,y,z,zmd,utmx,utmy,uwid,tz,zt,flag)
%
% WRITESEISTVD writes a SEISLINE readable ascii file containing
% the well deviation geometry in UTM coordinates
%
% filename ... full filename of the file to write to
% x,y,z ... 3 column vectors giving the borehole geometry
% zmd ... vector of same length as z giving the measured depths
% utmx ... utm x coordinate of well at z==0
% utmy ... utm y coordinate of well at z==0
% uwid ... unique well id (ugly)
% tz,zt ... time-depth function for well
% flag ... 1 -> zt (depths of time-depth function) are measured depths
%	along the borehole and must be converted to tvd's
%      ... 0 -> zt is already in tvd
%  ***** default for flag is 1 ****
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
if (nargin < 11)
	flag=1;
end
%open the file
fid=fopen(filename,'w');
if(fid==-1)
	code=-1;
	return;
end
% compute times to go with the depths
if(flag)
	t=interpextrap(zt,tz,zmd);
else
	t=interpextrap(zt,tz,z);
end
% compute the pretty well id
pid=prettywid(uwid);
%write first line
rec='CH: WELL NAME       SM    KB FLG CL DIST AZM SEIS_CUBE';
rec=[rec '  CHEVNO STK  CMP#   SURF_X   SURF_Y     TD'];
fprintf(fid,'%s\n',rec);
%second line
fmt='WH: %s  0     0   1  1  500   0    ***      ******   *    ** ';
fmt=[fmt '%8.0f %8.0f   %5.0f'];
rec=sprintf(fmt,pid,utmx,utmy,z(length(z)));
fprintf(fid,'%s\n',rec);
%third line
rec='CP: WELL NAME              WX       WY   XOFF   YOFF XLAB';
rec=[rec ' YLAB   TVD  TIME  WELL_ID'];
fprintf(fid,'%s\n',rec);
%now the rest
fmt='WP: %s   %8.0f %8.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f';
zip=0.;
uno=1.;
for k=1:length(z)
 ux=utmx+x(k);
 uy=utmy+y(k);
	rec=sprintf(fmt,pid,ux,uy,x(k),y(k),zip,zip,z(k),1000*t(k),uno);
	fprintf(fid,'%s\n',rec);
end
	
fclose(fid)