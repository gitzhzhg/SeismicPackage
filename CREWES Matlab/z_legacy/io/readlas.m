function [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
		kb,tops,ztops,lasheader]=readlas(varargin)
% READLAS: read LAS well log files
%
% [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
%       kb,tops,ztops,lasheader]=readlas()
% OR
%                               =readlas(filein)
% OR
%                               =readlas(filein, errflag)
%
% READLAS reads well logs from a disk file in LAS (Log ASCII Standard) format
%
% filein  (optional)... string with the file name (and full path if desired)
% errflag ... flag indicating error behavior. if errflag == 0, then if an
% error occurs during reading, this function will abort calling MATLABS
% ERROR function. If errflag==1 and an error occurs, the function will
% return normally with logmat set to -1 and all other values [].
% *************** default = 0 *********************%
%
% logmat ... matrix containing the logs in the file. Each log is in a column 
% and the first column contains the depths. Thus, if logmat has n columns,
% then there are n-1 logs
% mnem ...matrix of standard 4 letter mnemonics of the n columns of logmat
% desc ... matrix of string descriptors of the n columns of the logmat
% name ... string with the well name
% id ... the unique well id
% loc ... string containg the well location
% null ... the null value used in the logs
% units ... string indicating the depth units of the log
% kb ... elevation of the kelly bushing
% tops ... names of tops found in the file
% ztops ... depths of tops
% lash ... matlab string matrix containing the entire las header
%
% G.F. Margrave, May 1994
% KWH, Dec 2012
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

% set errflag to default value of zero
errflag=0;

% suppress all warning messages from las()
%warning('off','all');

% check input arguments
switch nargin
    case 1 %readlas(filein)
        lasobj = las(varargin{1});
    case 2 %readlas(filein, errflag)
        lasobj = las(varargin{1});
        errflag = varargin{2};
    otherwise %readlas() or more than two args
        lasobj = las();
end

% get the information expected as output from readlas
[logmat,logmnem,logdesc,wellname,...
   wellid,loc,nullval,dpthunits,...
   kb,tops,ztops,lasheader,fname] = lasobj.readlas();

% deal with errflag
if isequal(logmat,-1)
    switch errflag
        case 0
            % stop execution with an error message
            warning('crewes:las:readlas',...
                ['An error occurred while reading file: ''' char(fname) '''']);
        otherwise
            % shouldn't have to do anything
    end
end

end