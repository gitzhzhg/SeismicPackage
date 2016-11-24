%ææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææ            
%                                                                               
%                  FUNCTION AJ0                                                 
%                                                                               
%ææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææ          
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
function [aj0, aj1] = aj01(z)
%
%ææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææ
%                                                                               
%   AJ01 CALCULATES THE VALUE  OF A BESSEL FUNCTION OF THE FIRST KIND OF         
%   ORDERS 0 AND 1 FOR REAL ARGUEMENTS.        
%                                                                               
      a=[0.0002100,-0.0039444,0.0444479,-0.3163866,...                         
            1.2656208,-2.2499997];                                              
      b=[0.00001109,-0.00031761,0.00443319,-0.03954289,...                     
            0.21093573,-0.56249985];                                          
%
%ææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææææ
%
      if ( abs(z) < 3.0) 
         aj0 = 0.0;
         aj1 = 0.0; 
         x2 = (z/3.0)^2;                                                            
         for i = 1 : 6                                                                
            aj0 = ( aj0 + a(i) )*x2;
            aj1 = ( aj1 + b(i) )*x2;
         end                                                                
         aj0 = aj0 + 1.0;
         aj1 = ( aj1 + 0.5 )*z;
         return
      else                                                                   
         sqrx = sqrt(2.0 / (pi * z));
         aj0 = sqrx * cos(z - pi * 0.25);
         aj1 = sqrx * cos(z - pi * 0.75);
      end                                                                                                                                    
end