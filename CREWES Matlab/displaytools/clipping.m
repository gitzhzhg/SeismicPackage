function C=clipping(seis,n)
% CLIPPING: automatically determin clipping levels for imagesc
%
% C=clipping(seis,n)
%
% seis ... seismic matrix to be shown with imagesc
% n ... number of standard deviations from the mean at which clipping
%       should occur.
% ******* default n=2 ********
%
% Usage: Let seis be a seismic matrix sith time coordinate t and space
% coordinate x. Then, to display it with imagesc using clipping at 2*sigma
% [sigma=std(seis(:))], use
% figure;imagesc(x,t,seis,clipping(seis));
% To display with clipping at n*sigma, use
% figure;imagesc(x,t,seis,clipping(seis,n));
%
%
% by G.F. Margrave, 2015
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

if(nargin<2)
    n=2;
end

m=mean(seis(:));
sig=std(seis(:));
C1=m-n*sig;
C2=m+n*sig;

C=[C1 C2];