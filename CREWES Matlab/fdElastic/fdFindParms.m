function [oldFile] = fdFindParms(modelDir)
%Find the user's chosen geological model on disc
%Identical to fdLoadPrior except for extension
%
% P.M. Manning, Oct 2008
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

%Look up .parm files in model directory
parmFileList = dir([modelDir,'\*.parm']);
nFiles = length(parmFileList);
%disp(nFiles)
%oldFile = '';
if nFiles<1
    error('No .parm files found')
end
parmString = '''Choose parameter file''';
cquote = ',''';
quote = ' ''';
for nF=1:nFiles
    %gfdString = [gfdString,',''',gfdFile(nF).name'''];
    parmString = cat(2,parmString,cquote,parmFileList(nF).name,quote);
end
eval(['kOptn = menu(',parmString,');']);
%disp(kOptn)
%oldFile = parmFile(kOptn).name;
oldFile = [modelDir,'\',parmFileList(kOptn).name];