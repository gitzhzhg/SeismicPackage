function expvel
% export the receiver coordinates, the first and the second layer velocities 
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

% at the receiver location 
recelev = refdata('get','recelev');
v1rec = 1000 * refdata('get','v1rec');
v2rec = 1000 * refdata('get','v2rec');
reccoord = recelev(1,:);
recnum = 1:length(reccoord);
% Ask for filename
[filename,path] = myuifile(gcf, '*.vel', 'Export Velocity', 'put');
if( filename == 0 )
   return;
end
ind = findstr(filename,'.vel');
if(strcmp(computer,'MAC2'))
%   fullfilename=filename(1:ind(1)-1);
   fullfilename=filename;
else
%   fullfilename=[path filename(1:ind(1)-1)];
   fullfilename=[path filename];
end
fid = fopen(fullfilename, 'w');
if( fid ~= -1 )
   fprintf(fid, '#Rec.   Coord.   V1(m/s)   V2(m/s)\n');
   for i=1:length(reccoord)
      count = fprintf(fid, '%5d %11.5f %11.5f %11.5f\n', recnum(i), ...
                      reccoord(i), v1rec(i), v2rec(i) );
   end
   fclose(fid);
else
   str = sprintf('Could not open file: %s',fullfilename);
   error(str);
end