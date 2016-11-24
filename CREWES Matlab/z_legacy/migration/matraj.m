function traj=matraj(amat,ksamps)
% traj=matraj(amat,ksamps)
%
% MATRAJ evaluates a matrix along a columnwise trajectory.
% That is, ksamps contains one entry per column of amat giving
% a desired sample number. Traj is a row vector, one entry
% per column of amat, containing those samples desired.
% If ksamps contains the entry 0, then zero will be returned
% in traj. No checking is done to ensure ksamps points to
% values between 1 and nrows. This must be done externally. 
%
% G.F. Margrave, University of Calgary, 1996
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
[nr,nc]=size(amat);
nsamps=nr*nc;
%compute indicies of first sample in each column
i1=1:nr:nr*nc-1;
%indicies of desired samples
isamp= i1+ksamps-1;
%initialize trajectory
traj=zeros(1,nc);
ind=find(ksamps~=0);
traj(ind)= amat(isamp(ind));