function seismic=s_attribute(seismic,action,attributes) %#ok 
% Function computes functionals of the seismic traces and stores them in like-named 
% header(s); if no output data set is provided, information about these functionals
% printed via the 'list' (short list) option of S_HEADER.
%
% Function is obsolete: use "s_attributes" instead.
%
% Written by: E. Rietsch: April 11, 2000
% Last updated: September 18, 2005: Add L2-norm calculation
%
%              seismic=s_attribute(seismic,action,attributes)            
% INPUT
% seismic      Seismic structure;
% 
% action       Defines action to take. Possible values are:
%              'add'      Add header with mnemonic "type". Gives error message if 
%                         header already exists 
%              'add_ne'   Add header with mnemonic "type". Replaces it if it already exists.
%              'Default: 'add_ne'
%             
% attributes   Cell array with one or more character strings describing the 
%              functional(s) to compute. Possible values are:
%              'amax'     Compute the maximum absolute value of each trace and store 
%                         it in header amax
%              'amean'    Compute the mean of the absolute value of each trace and store it 
%              'amedian'  Compute the median of the absolute value of each trace and store 
%                         it in header amedian
%              'amin'     Compute the minimum absolute value of each trace and store 
%                         it in header amin
%                         in header amean (same as 'aaa').
%              'l2norm'   Compute the L2 norm of each trace, i.e. 
%                         SQRT(sum of squares of the samples of each trace)
%              'max'      Compute the maximum value of each trace and store it in header max
%              'mean'     Compute the mean value of each trace and stores it in header mean
%              'median'   Compute the median value of each trace and store it in 
%                         header median
%              'min'      Compute the minimum value of each trace and store it in 
%                         header min
%              'minabs'   Compute the absolute value of the minimum of each trace and store 
%                         it in header minabs
%              'rms'      Compute the rms value of each trace and store it in header rms
%              'trend'    Compute the trend (robust estimate of the average gradient)
%
%              Default: all functionals except trend
%
% OUTPUT
% seismic      "Updated" seismic structure
%

alert('Function is obsolete: use "s_attributes" instead.')

error('Abnormal termination.')

