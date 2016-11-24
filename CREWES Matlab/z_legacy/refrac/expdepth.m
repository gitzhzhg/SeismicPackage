function expdepth
% Export the receiver coordinates, their surface elevation, the elevation of 
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

% the layer1-layer2 interface and the thickness of the first layer 
depth = refdata('get','depth');
recelev = refdata('get','recelev');
depthelev = recelev(2,:)-depth(2,:);
reccoord = recelev(1,:);
recnum = 1:length(reccoord);
% Ask for filename
[filename,path] = myuifile(gcf, '*.dpt', 'Export Depth', 'put');
if( filename == 0 )
   return;
end
ind = findstr(filename,'.dpt');
if(strcmp(computer,'MAC2'))
%   fullfilename=filename(1:ind(1)-1);
   fullfilename=filename;
else
%   fullfilename=[path filename(1:ind(1)-1)];
   fullfilename=[path filename];
end
fid = fopen(fullfilename, 'w');
if( fid ~= -1 )
   fprintf(fid, ' #Rec.   Coord.    Surf.     Interf.    Thick.\n');
   for i=1:length(reccoord)
      count = fprintf(fid, '%5d %9.2f %9.2f %9.2f %9.2f\n', recnum(i), ...
                      reccoord(i), recelev(2,i), depthelev(i), depth(2,i) );
   end
   fclose(fid);
else
   str = sprintf('Could not open file: %s',fullfilename);
   error(str);
end