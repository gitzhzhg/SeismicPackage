function traces = getrLoad(modelDir,dir,comp,exten)
% function traces = getrLoad(dir,comp,exten)
%The input parameters are
%   dir ..... The line direction of the data required (X or Z)
%   comp .... The component of the displacement required (X or Z)
%   exten ... A file extension used when combining two trace data sets
%The output parameters are
%traces .. The output array of trace data
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
disp(exten)
if dir=='x'
    alph1 = 'surfFile';
    Valph1 = 'srfU';
    %spMin = xMin;
else
    alph1 = 'wellFile';
    Valph1 = 'wllU';
    %spMin = zMin;
end

if comp=='x'
    alph2 = 'X';
    Valph2 = 'x';
else
    alph2 = 'Z';
    Valph2 = 'z';
end
%alFile = [alph1 alph2 exten];
alFile = [alph1 alph2];
    % searchStr = [alFile '*.mat'];
    % searchStrQ = ['''' searchStr ''''];
    % %codeStr = ['trFiles = dir(' searchStrQ ')'];
    % codeStr = ['dir(' searchStrQ ')'];
    % disp(codeStr)
    % trFiles = eval(codeStr);
    % disp(trFiles)
    % %parmFile = dir('*.parm');
    % nFiles = length(trFiles);
    % if nFiles<1
    %     error('Can''t find trace file')
    % end
    % disp(trFiles(1))
    % load(trFiles(1))
for iRun = 1:2
    if (iRun==2)
        if(isempty(exten))
            break
        else
            tracesP = traces;
            alFile = [alph1 alph2 exten];
        end
    end
    dirLoad = [modelDir,'\',alFile];
    %disp(dirLoad)
    load(dirLoad)

    %load(alFile)                    %Load
    Valph = [Valph1 Valph2];
    for ind1 = 1:9999               %Find Valph of any number
        alVar = [Valph num2str(ind1)];
        if exist(alVar,'var')==1
            break
        end
    end
    if ~exist(alVar,'var')==1       %Confirm find
        error(['Cant find ' alVar])
    end
    %disp(['Found ' alVar])
    for ind = ind1:9999             %Find last piece
        alVx = [Valph num2str(ind)];
        if ~exist(alVx,'var')==1
            break
        end
    end
    last = ind-1;
    %disp(['Last piece is ' num2str(last)])
    st = ['chunk = ' alVar ';'];
    eval(st)
    [nSp,nT] = size(chunk);
    %disp([nSp,nT])
    traces = zeros(nSp,nT*last);
    traces(:,1:nT) = chunk;
    %nch = nT+1;
    nch = 1;
    %for ind = ind1+1:last+ind1-1
    for ind = ind1:last
        alVar = [Valph num2str(ind)];
        st = ['chunk = ' alVar ';'];
        eval(st)
        [nSp,nT] = size(chunk);
        %disp([nSp,nT])
        %disp(size(traces));disp(size(chunk));
        traces(:,nch:nch+nT-1) = chunk;
        nch = nch+nT;
    end
end
if ~isempty(exten)
    traces = (traces+tracesP)*0.5;
end