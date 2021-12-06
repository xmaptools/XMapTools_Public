function [b] = XimrotateX(A,phi)
   
if phi == 0
    b = A;
elseif phi == 90
    b = rot90(A,1);
elseif phi == 180
    b = rot90(A,2);
elseif phi == 270
    b = rot90(A,3);
elseif phi == -90
    b = rot90(A,-1);
elseif phi == -180
    b = rot90(A,-2);
elseif phi == -270
    b = rot90(A,-3);
end
   
return

