function dataout=nodefind(arg1,arg2,arg3,arg4);
%****************
%*** NODEFIND ***
%****************
%
% dataout=nodelack(trc,midpt,lockto);
% dataout=nodelack(trc,midpt,lockto,nhood);
%
% NODELOCK enables users to lock to specific nodes on a set of data that
% has been inputted.  The choices are peaks, troughs, zerocrossings
%
% trc: The single column array.  The trace will be normalized
%
% midpt: The point from which NODELOCK will search
%
% lockto: Choices are 'peak', 'trough', 'zerocross+', 'zerocross-'
%
% nhood: The number of samples away from the midpoint that will be searched
% for the lockto argument. (default == 10 samples)
%
% Christopher Harrison, 2004
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

dataout=[];
trc=arg1;
midpt=arg2;
lockto=arg3;
if(nargin<3)
    stringinfo=['Not enough arguments for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(nargin>4)
    stringinfo=['To many arguments for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(nargin==3)
    nhood=10;
else
    nhood=arg4;
end
% Checking arguments
checklockto='peak trough zerocross+ zerocross- inflection';
if(~isnumeric(trc))
    stringinfo=['Trace has to be numeric for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(~isnumeric(midpt))
    stringinfo=['Midpoint must be numeric for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(midpt~=abs(midpt))|(midpt~=round(midpt))
    stringinfo=['Midpoint must a positive interger NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(midpt>length(trc))
    stringinfo=['Midpoint must be on trace for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(~ischar(lockto))
    stringinfo=['Lock to must be a string for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(~findstr(checklockto,deblank(lower(lockto))))
    stringinfo=['Lock to must be "peak", "trough", or "zerocross" for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
elseif(~isnumeric(nhood)|abs(nhood)~=nhood)
    stringinfo=['NHOOD must numeric and positive for NODEFIND'];
    helpdlg(stringinfo,'Check NODEFIND help file');
    return
else
    lockto=deblank(lower(lockto));
    % All Files have been checked an all is well.
    normtrc=trc-mean(trc);
    normtrc=normtrc/(max(normtrc));
    difftrc=diff(trc);  
    % differentiating trace to break the samples into positive values for
    % positive slopes, negative values for negative slopes, and zero values
    % for flat responses.
    switch lockto(1:4)
        case 'peak'
            % peaks occure when positves go to negatives in difftrc
            trcfind=find(difftrc>0)+1;   % sample # on trc (+1 makes it so)
        case 'trou'
            % troughs occur when negatives go to positives in difftrc
            trcfind=find(difftrc<0)+1;   % sample # on trc (+1 makes it so)
        case 'zero'
            trcfind=find(normtrc==0);
            if(isempty(trcfind)) trcfind=[]; end
            holdtrc=normtrc;
            holdtrc(find(holdtrc>0))=1;
            holdtrc(find(holdtrc<0))=-1;
            if(strcmp(lockto,'zerocross'))
                % all cross over points 
                trcfind2=find(abs(diff(holdtrc))==2)+1;
            elseif(strcmp(lockto,'zerocross+'))
                % only positive cross over points
                trcfind2=find(diff(holdtrc)==2)+1;
            elseif(strcmp(lockto,'zerocross-'))
                % only negative cross over points
                trcfind2=find(diff(holdtrc)==-2)+1;
            end
            if(isempty(trcfind2)) trcfind2=[]; end
            trcfind=sort(cell2mat({trcfind trcfind2}));
        case 'infl'
            % there should me more choices, but right now we are not going
            % there.  
            
    end
    % peaks or troughs occure when numbers in the trcfind array are not
    % concurent, finding none concurrent samples
    trcfind=trcfind(find(diff(trcfind)~=1));   % plus one is need to line up with trc properly
    % trcfind number correspond to peaks on trc
    % finding if they occure in nhood area of wvltmid
    trcfind=trcfind(find(trcfind>=midpt-nhood));
    trcfind=trcfind(find(trcfind<=midpt+nhood));
    % trcfind is now a neighboorhood of peaks / troughs / zerocross
    dataout=trcfind(find(abs(trcfind-midpt)==min(abs(trcfind-midpt))));
    if(length(dataout)>=2)
        % sometimes two peaks are equidistance from one another, taking the
        % first value.
        dataout=dataout(1);
    end
end