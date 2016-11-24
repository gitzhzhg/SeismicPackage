function [x,tx,a,ta,w,tw]=ensemble(ntraces,nsamps,dt,fdom)
% [x,tx,a,ta,w,tw]=ensemble(ntraces,nsamps)
%
% ENSEMBLE generates an ensemble (profile) of synthetic traces
% for use with ensemble averaging experiments
%
% ntraces= number of rows in the matricies x and a
% nsamps= number of columns in x, keep this a power of 2!! 
% dt= time sample rate desired on output
% fdom= dominant frequency of the waveform 
% x= an ntraces by nsamps matrix of gaussian random noise where
%    each row has been convolved with the waveform w.
% tx= time coordinate vector for x
% a= the two-sided autocorrelation matrix of x, ntraces by 2*nsamps
%    (There is an extra sample added to keep things a power of two)
% ta= time coordinate vector for a
% w= minimum phase waveform with a dominant frequency of fdom
% tw= time coordinate vector for w
%
% by G.F. Margrave, July, 1991
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
 if nargin<4, fdom=15;end
 if nargin<3, dt=.002;end
 if nargin<2, nsamps=256;end
 if nargin<1, ntraces=20;end
% generate the random matrix
 mtx=randn(ntraces,nsamps);
 tx=xcoord(0.,dt,mtx(1,:));
% generate the wavelet
 nw=nsamps;
 [w,tw]=wavemin(dt,fdom,(nw-1)*dt);
% convolve them to get x
 x=convm(mtx,w);
% autocorrelate x
 a=auto2(x);
 ta=xcoord(-max(tx),dt,a(1,:));