function [J, grad] = costFunction(theta, X, y)
%COSTFUNCTION Compute cost and gradient for logistic regression
%   J = COSTFUNCTION(theta, X, y) computes the cost of using theta as the
%   parameter for logistic regression and the gradient of the cost
%   w.r.t. to the parameters.

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
%J = 1/2/size(X,1).*sum((sum((theta'.*X),2)-y).^2);
h = sigmoid(theta'.*X);
J = 1/2/size(X,1).*sum((-y).*log(h) - (1-y).*log(1-h));
grad = 1/size(X,1).*sum((h-y).*X);

end
