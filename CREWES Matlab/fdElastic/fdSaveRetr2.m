function fdSaveRetr2(modelDir,SorR,permName)
%fdSaveRetr(SorR,permName)
%Save trace files to longer term names, or retrieve
%
%SorR is a switch to indicate whether to save or retrieve trace data
%The valid values are 's' or 'r'
%permName will become the root name of the (permenant) trace files
%         or the permenant name of the trace data to retrieve
%
% P.M. Manning, Dec 2011
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
if SorR == 'r'
    disp('Retrieve')
else
    disp('Save')
end
% pref = 'Name';
% source(1,:) = 'wellFileX.mat'; dest(1,:) = 'wFX.fdt';
% source(2,:) = 'wellFileZ.mat'; dest(2,:) = 'wFZ.fdt';
% source(3,:) = 'surfFileX.mat'; dest(3,:) = 'sFX.fdt';
% source(4,:) = 'surfFileZ.mat'; dest(4,:) = 'sFZ.fdt';
% source(5,:) = 'headrFile.mat'; dest(5,:) = 'hed.fdt';
source(1,:) = [modelDir,'\wellFileX.mat'];
disp(source(1,:))
source(2,:) = [modelDir,'\wellFileZ.mat'];
source(3,:) = [modelDir,'\surfFileX.mat'];
source(4,:) = [modelDir,'\surfFileZ.mat'];
source(5,:) = [modelDir,'\headrFile.mat'];
% dest(1,:) = [modelDir,'\',permName,'_wFX.fdt'];
% disp(dest(1,:))
% dest(2,:) = [modelDir,'\',permName,'_wFZ.fdt'];
% dest(3,:) = [modelDir,'\',permName,'_sFX.fdt'];
% dest(4,:) = [modelDir,'\',permName,'_sFZ.fdt'];
% dest(5,:) = [modelDir,'\',permName,'_hed.fdt'];
dest(1,:) = [permName,'_wFX.fdt'];
disp(dest(1,:))
dest(2,:) = [permName,'_wFZ.fdt'];
dest(3,:) = [permName,'_sFX.fdt'];
dest(4,:) = [permName,'_sFZ.fdt'];
dest(5,:) = [permName,'_hed.fdt'];

for ii=1:5
    %%destA = [pref,dest(ii,:)];
    %destA = [permName,dest(ii,:)];
    %disp(destA)
    if(strcmpi('r',SorR))
        copyfile(dest(ii,:),source(ii,:));
    else
        copyfile(source(ii,:),dest(ii,:));
    end
end