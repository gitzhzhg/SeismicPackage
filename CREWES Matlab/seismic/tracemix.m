function seismix=tracemix(seis,weights)
% TRACEMIX ... weighted trace mixing on a seismic gather or section
%
% seismix=tracemix(seis,weights)
%
% seis ... input seismic section. SHould be a 2D matrix with one trace per
%           column
% weights ... mixing weights. Each output trace will be a linear
%           combination of length(weights) input traces. For best results
%           make length(weights) an odd number. 
% seismix ... output seismic section
%
%  For example, weights=[1 1 1] means that each output trace will be the
%  sum of the corresponding input trace together with the neighboring trace
%  on either side. Each trace in the sum will get equal weight and the
%  summed trace is divided by sum(weights)=3. weights=[.2 .5 1 .5 .2] means
%  each output trace is the sum of 5 input traces. In this case the two
%  traces on each side get the weights .5 and .2 and the output trace is
%  divided by sum(weights) =  2.4. For those with signal knowledge, this
%  mixing is just a spatial convolution of the input section with the weights
%  vector. In the wavenumber domain it is a k filter whose shape is the
%  transform of weights.
%
% For more understanding, run this:
% a=[1 2 2 2 10 2 2 2 1];
% a=ones(5,1)*a
% b=[1 1 1]
% c=tracemix(a,b)
% You should be able to explain the output given the input.
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


weights=weights(:).';% ensures that weights is a row vector

seismix=conv2(seis,weights,'same')/sum(weights);