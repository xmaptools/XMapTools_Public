function BW = Xpoly2maskX(x,y,M,N)
%POLY2MASK-like function for VER_XMAPTOOLS_750 that 
%   Convert region-of-interest polygon to mask.


%narginchk(4,4);

% This function narginchk to validate the number of arguments is not
% compatible with the old version of Matlab. 
% Removed 1.6.5 (We don't need to check this here). 


validateattributes(x,{'double'},{},mfilename,'X',1);
validateattributes(y,{'double'},{},mfilename,'Y',2);
if length(x) ~= length(y)
    error(message('images:poly2mask:vectorSizeMismatch'));
end
if isempty(x)
    BW = false(M,N);
    return;
end
validateattributes(x,{'double'},{'real','vector','finite'},mfilename,'X',1);
validateattributes(y,{'double'},{'real','vector','finite'},mfilename,'Y',2);
validateattributes(M,{'double'},{'real','integer','nonnegative'},mfilename,'M',3);
validateattributes(N,{'double'},{'real','integer','nonnegative'},mfilename,'N',4);

if (x(end) ~= x(1)) || (y(end) ~= y(1))
    x(end+1) = x(1);
    y(end+1) = y(1);
end

[xe,ye] = Xpoly2edgelistX(x,y);
BW = Xedgelist2maskX(M,N,xe,ye);

return

function [xe, ye] = Xpoly2edgelistX(x,y,scale)

if nargin < 3
    scale = 5;
end

% Scale and quantize (x,y) locations to the higher resolution grid.
x = round(scale*(x - 0.5) + 1);
y = round(scale*(y - 0.5) + 1);

num_segments = length(x) - 1;
x_segments = cell(num_segments,1);
y_segments = cell(num_segments,1);
for k = 1:num_segments
    [x_segments{k},y_segments{k}] = XintlineX(x(k),x(k+1),y(k),y(k+1));
end

% Concatenate segment vertices.
x = cat(1,x_segments{:});
y = cat(1,y_segments{:});

% Horizontal edges are located where the x-value changes.
d = diff(x);
edge_indices = find(d);
xe = x(edge_indices);

% Wherever the diff is negative, the x-coordinate should be x-1 instead of
% x.
shift = find(d(edge_indices) < 0);
xe(shift) = xe(shift) - 1;

% In order for the result to be the same no matter which direction we are
% tracing the polynomial, the y-value for a diagonal transition has to be
% biased the same way no matter what.  We'll always chooser the smaller
% y-value associated with diagonal transitions.
ye = min(y(edge_indices), y(edge_indices+1));


return

function BW = Xedgelist2maskX(M,N,xe,ye,scale)

if nargin < 5
    scale = 5;
end

shift = (scale - 1)/2;

% Scale x values, throwing away edgelist points that aren't on a pixel's
% center column. 
xe = (xe+shift)/5;
idx = xe == floor(xe);
xe = xe(idx);
ye = ye(idx);

% Scale y values.
ye = ceil((ye + shift)/scale);

% Throw away horizontal edges that are too far left, too far right, or below the image.
bad_indices = find((xe < 1) | (xe > N) | (ye > M));
xe(bad_indices) = [];
ye(bad_indices) = [];

% Treat horizontal edges above the top of the image as they are along the
% upper edge.
ye = max(1,ye);

% Insert the edge list locations into a sparse matrix, taking
% advantage of the accumulation behavior of the SPARSE function.
S = sparse(ye,xe,1,M,N);

% We reduce the memory consumption of edgelist2mask by processing only a
% group of columns at a time (g274577); this does not compromise speed.
BW = false(size(S));
numCols = size(S,2);
columnChunk = 50;
for k = 1:columnChunk:numCols
  firstColumn = k;
  lastColumn = min(k + columnChunk - 1, numCols);
  columns = full(S(:, firstColumn:lastColumn));
  BW(:, firstColumn:lastColumn) = parityscan(columns); 
end

function [BW] = parityscan(F)
% F is a two-dimensional matrix containing nonnegative integers

nR = size(F,1);
nC = size(F,2);

BW = false(nR,nC);

for c=1:nC    
    
    somme = 0;
    for r=1:nR
        somme = somme+F(r,c);
        if mod(somme,2) == 1
            
            BW(r,c) = 1;
        end
    end
    
    
    %if length(find(F(:,c))) > 0
    %    keyboard
    %end
    
end

 
return

function [x,y] = XintlineX(x1, x2, y1, y2)
dx = abs(x2 - x1);
dy = abs(y2 - y1);

% Check for degenerate case.
if ((dx == 0) && (dy == 0))
  x = x1;
  y = y1;
  return;
end

flip = 0;
if (dx >= dy)
  if (x1 > x2)
    % Always "draw" from left to right.
    t = x1; x1 = x2; x2 = t;
    t = y1; y1 = y2; y2 = t;
    flip = 1;
  end
  m = (y2 - y1)/(x2 - x1);
  x = (x1:x2).';
  y = round(y1 + m*(x - x1));
else
  if (y1 > y2)
    % Always "draw" from bottom to top.
    t = x1; x1 = x2; x2 = t;
    t = y1; y1 = y2; y2 = t;
    flip = 1;
  end
  m = (x2 - x1)/(y2 - y1);
  y = (y1:y2).';
  x = round(x1 + m*(y - y1));
end
  
if (flip)
  x = flipud(x);
  y = flipud(y);
end

return