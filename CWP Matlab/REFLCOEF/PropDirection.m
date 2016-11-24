%===================================================================
% Compute the direction of propagation giving inc. angle and azimuth;
% azim from <-90,90>*pi/180 only !!!
%===================================================================

function n_dir = PropDirection(ang, azim)

%========================================
if ang == 0
 n_dir = [0 0 1];
else
 if abs(azim) > pi/4
  n(2) = sign(azim);
  n(1) = cot(abs(azim));
 else
  n(1) = 1; 
  n(2) = tan(azim);
 end

 nh = sqrt(n(1)^2 + n(2)^2);
 n(3) = nh/tan(ang);

 n_dir = n/norm(n,2);
end