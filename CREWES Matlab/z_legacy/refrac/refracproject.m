% Saves or loads a Refraction Statics project
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

function refracproject(action)
if( nargin < 1 )
   error('refracproject: needs an action');
end
% Ask for filename
[filename,path] = myuifile(gcf, '*.mat', 'Save/Load project', 'put');
if( ~isempty(filename) )
	if( filename == 0 )
  	 return;
	end
end
ind = findstr(filename,'.mat');
if(strcmp(computer,'MAC2'))
%   fullfilename=filename(1:ind(1)-1);
   fullfilename=filename;
else
%   fullfilename=[path filename(1:ind(1)-1)];
   fullfilename=[path filename];
end
% The current project is saved using a 'container object' which is
% one big matrix holding all the data saved in 'refdata'.
if( strcmp(action, 'save') )
   if(~isempty(ind))
      % Trim off the '.mat' at the end of the file.
      filename = filename(1:ind-1);
      fullfilename = [path filename];
   end
   % Build an array holding all of the useful single-value parameters
   pa(1) = refdata('get','rtrange');
   pa(2) = refdata('get','rtpair1');
   pa(3) = refdata('get','rtpair2');
   pa(4) = refdata('get','mint');
   pa(5) = refdata('get','aprange');
   pa(6) = refdata('get','appair1');
   pa(7) = refdata('get','appair2');
   pa(8) = refdata('get','apsub');
   pa(9) = refdata('get','rtmax');
   pa(10) = refdata('get','window');
   pa(11) = refdata('get','nshots');
   pa(12) = refdata('get','nrecs');
   pa(13) = refdata('get','dev');
   pa(14) = refdata('get','standard');
   pa(15) = refdata('get','plustreject');
   pa(16) = refdata('get','datum');
   pa(17) = refdata('get','repvel');
   pa(18) = refdata('get','nd');
   pa(19) = refdata('get','shotgap');
   pa(20) = refdata('get','shotlength');
   pa(21) = refdata('get','flatslope');
   pa(22) = refdata('get','windmn');
   pa(23) = refdata('get','offsetrange1');
   pa(24) = refdata('get','offsetrange2');
   pa(25) = refdata('get','offsetpt');   
   % Build a container object to hold all this stuff
   % This little bit of code is optimized to minimize memory requirments.
   box = contobj('Refrac-proj','list');
   box = objset(box, 'parmlist', pa, 'rvec');
   fbtime = refdata('get','fbtime');
   box = objset(box, 'fbtime', fbtime);
   disp('Adding to box...');
   clear fbtime;
   
   fbcoord = refdata('get','fbcoord');   
   box = objset(box, 'fbcoord', fbcoord);
   disp('Adding to box...');
   clear fbcoord;
   
   shotcoord = refdata('get','shotcoord');   
   box = objset(box, 'shotcoord', shotcoord);
   disp('Adding to box...');
   clear shotcoord;
   
   diffmat = refdata('get','diffmat');
   box = objset(box, 'diffmat', diffmat);
   disp('Adding to box...');
   clear diffmat;
   cvpi = refdata('get','cvpi');
   box = objset(box, 'cvpi', cvpi);
   disp('Adding to box...');
   clear cvpi;
   cvpj = refdata('get','cvpj');   
   box = objset(box, 'cvpj', cvpj);
   disp('Adding to box...');
   clear cvpj;
   cvpavg = refdata('get','cvpavg');   
   box = objset(box, 'cvpavg', cvpavg);
   disp('Adding to box...');
   clear cvpavg;
   v1rec = refdata('get','v1rec');
   box = objset(box, 'v1rec', v1rec);
   disp('Adding to box...');
   clear v1rec;
   v2rec = refdata('get','v2rec');
   box = objset(box, 'v2rec', v2rec);
   disp('Adding to box...');
   clear v2rec;
   depth = refdata('get','depth');
   box = objset(box, 'depth', depth);
   disp('Adding to box...');
   clear depth;
   td1 = refdata('get','td1');
   box = objset(box, 'td1', td1);
   disp('Adding to box...');
   clear td1;
   plust = refdata('get','plust');
   box = objset(box, 'plust', plust);
   disp('Adding to box...');
   clear plust;
   shotelev = refdata('get','shotelev');
   box = objset(box, 'shotelev', shotelev);
   disp('Adding to box...');
   clear shotelev;
   recelev = refdata('get','recelev');
   box = objset(box, 'recelev', recelev);
   disp('Adding to box...');
   clear recelev;
   uphole = refdata('get','uphole');
   box = objset(box, 'uphole', uphole);
   disp('Adding to box...');
   clear uphole;
   shotstat = refdata('get','shotstat');
   box = objset(box, 'shotstat', shotstat);
   disp('Adding to box...');
   clear shotstat;
   recstat = refdata('get','recstat');
   box = objset(box, 'recstat', recstat);
   disp('Adding to box...');
   clear recstat;
   refracprojectobj = box;
   disp('Saving box to disk...');
   eval([ 'save ' fullfilename ' refracprojectobj'; ]);
   disp('Done saving box...');
end
if( strcmp(action, 'load') )
   if( isempty(filename) | filename == 0 )
      error('refracprojct: No filename given for load');
   end
   ind=findstr(filename, '.mat');
   if( length(ind)>0 )
      filename=filename(1:ind-1);
   else
      disp('Selected file must be a previously saved project');
      disp('and it must have a .mat extension on the file name');
      error(' invalid file extension');
   end
   fullfilename = [path filename];
   clear refracprojectobj;
   eval([ 'load ' fullfilename ] );
   box = refracprojectobj;
   if( ~isearthobj(box) )
      str = sprintf('refracproject: Invalid project file: %s', filename );
      error(str);
   end
   % Default all global parameters 
   refdata('clear');
   pa = objget(box, 'parmlist');
   fbtime = objget(box, 'fbtime');
   fbcoord = objget(box, 'fbcoord');
   shotcoord = objget(box, 'shotcoord');
   diffmat = objget(box, 'diffmat');
   cvpi = objget(box, 'cvpi');
   cvpj = objget(box, 'cvpj');
   cvpavg = objget(box, 'cvpavg');
   v1rec = objget(box, 'v1rec');
   v2rec = objget(box, 'v2rec');
   depth = objget(box, 'depth');
   td1 = objget(box, 'td1');
   plust = objget(box, 'plust');
   shotelev = objget(box, 'shotelev');
   recelev = objget(box, 'recelev');
   uphole = objget(box, 'uphole');
   shotstat = objget(box, 'shotstat');
   recstat = objget(box, 'recstat');
   if( length(pa) == 25 )
      refdata('set', 'rtrange', pa(1) );
      refdata('set', 'rtpair1', pa(2) );
      refdata('set', 'rtpair2', pa(3) );
      refdata('set', 'mint', pa(4) );
      refdata('set', 'aprange',pa(5));
      refdata('set', 'appair1', pa(6) );
      refdata('set', 'appair2', pa(7) );
      refdata('set', 'apsub', pa(8) );
      refdata('set', 'rtmax', pa(9) );
      refdata('set', 'window', pa(10) );
      refdata('set', 'nshots', pa(11) );
      refdata('set', 'nrecs', pa(12) );
      refdata('set', 'dev', pa(13) );
      refdata('set', 'standard', pa(14) );
      refdata('set', 'plustreject', pa(15) );
      refdata('set', 'datum', pa(16) );
      refdata('set', 'repvel', pa(17) );
      refdata('set', 'nd', pa(18) );
      refdata('set', 'shotgap', pa(19) );
      refdata('set', 'shotlength', pa(20) );
      refdata('set', 'flatslope', pa(21) );
      refdata('set', 'windmn', pa(22) );
      refdata('set', 'offsetrange1', pa(23) );
      refdata('set', 'offsetrange2', pa(24) );
      refdata('set', 'offsetpt', pa(25) );
   else
      fprintf(1,'WARNING: old project file loaded. Processing parameters set to default values..\n');
      nshots = length(shotcoord);
      nrecs = length(recelev);
      refdata('set', 'nshots', nshots );
      refdata('set', 'nrecs', nrecs );
   end
   refdata('set', 'fbtime', fbtime);
   refdata('set', 'fbcoord', fbcoord);
   refdata('set', 'shotcoord', shotcoord);
   refdata('set', 'diffmat', diffmat);
   refdata('set', 'cvpi', cvpi);
   refdata('set', 'cvpj', cvpj);
   refdata('set', 'cvpavg', cvpavg);
   refdata('set', 'v1rec', v1rec);
   refdata('set', 'v2rec', v2rec);
   refdata('set', 'td1', td1);
   refdata('set', 'plust',plust);
   refdata('set', 'depth', depth);
   refdata('set', 'shotelev', shotelev);
   refdata('set', 'recelev', recelev);
   refdata('set', 'uphole', uphole);
   refdata('set', 'shotstat', shotstat);
   refdata('set', 'recstat', recstat);
   % Enable all the menus
   PMTsetmenus;
end