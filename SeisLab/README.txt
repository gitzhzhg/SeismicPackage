SeisLab 3.01

This folder contains a folder, S4M, with Matlab functions for seismic and 
well-log analysis, in the following collectively called SeisLab.

Folder "Other" contains support functions written by others and available
from the Matlab Central Program Exchange.

A manual in PDF format explains the philosophy behind the design of the
functions in SeisLab and the way they can be used. For good measure, I have 
also added a few sample scripts (verba docent, exempla trahunt).

To run SeisLab functions, copy folder S4M to your computer and add 
it and its subfolders to your Matlab path (please note that
folders that start with "@" are class directories and need not be included 
explicitly in the Matlab path; Matlab will find them anyway; the same 
is true for "private" folders). 

The folder "HelpFiles" need not be included in the Matlab path either 
as it contains only text files that provide instructions for the use of 
certain figures that expect user input.

I recommended that you run the sample scripts to ascertain that the 
installation was successful.

Please note that I do not promise any kind of support for these functions. 
However, should I happen to hear about a problem that illustrates an obvious 
bug (not a "feature") or that tickles my fancy I might do something about 
it and include the fix (or improvement) in the next release.

Also, from time to time, I intend to upload  updated or otherwise enhanced
versions of this software collection. Each such version has a unique 
Distribution ID. To find the Distribution ID of a SeisLab package you may 
already have, type "ddid" at the Matlab prompt. If you get the error message
"Undefined function or variable 'ddid'." you have the very first release I 
uploaded and should download a newer one. Otherwise, compare the 
Distribution ID with that of this release. If it is lower then this release 
is newer.

However, please note that some functions in this release of SeisLab may require
Matlab version R2007a or higher. An earlier release, that should run on Matlab
R12 and higher, is still available from ---
http://www.mathworks.com/matlabcentral/fileexchange/8827
--- but is not supported at all.

Also, function s_volume_browser, a viewer for 3-D data volumes, requires 
the volume browser package available on the Matlab file exchange 
http://www.mathworks.com/matlabcentral/fileexchange/13526

Eike Rietsch
