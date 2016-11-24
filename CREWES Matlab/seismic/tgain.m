function seisg=tgain(seis,t,n)
% tgain ... gain seismic gather by t^n
%
% seisg=tgain(seis,t,n)
%
% seis ... seismic trace or gather. If a single trace it can be either a row or
%           column vector. If a matrix, then the traces should be columns.
% t ... time coordinate for seis. If seis is a vector then t must be the same
%           size vector. If seis is a matrix, then length(t) must equal size(seis,1).
% n ... exponent for time gain. (Try something in the range [1.5,2.5].)
%
% If seis is a single trace, then the gain is accomplished by seisg=seis.*t^n; 
% For a gather the same thing is accomplised on a trace-by-trace basis.
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

%

[nr,nc]=size(seis);
transpose=0;
if((nr-1)*(nc-1)==0)
    %ok, we have a single trace
    if(nr==1) % a row vector
        seis=seis(:);
        t=t(:);
        transpose=1;
    else
        t=t(:);
    end
end
[nr,nc]=size(seis);
nt=size(t,1);
if(nt~=nr)
    if(length(t)==nr)
        nt=nr;
        t=t';
    else
        error('sizes of t and seis are not compatible');
    end
end

tt=t(:,ones(1,nc));

seisg=seis.*tt.^n;

if(transpose==1)
    seisg=seisg.';
end