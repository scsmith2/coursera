function [J, grad] = lrCostFunction(theta, X, y, lambda)
%LRCOSTFUNCTION Compute cost and gradient for logistic regression with 
%regularization
%   J = LRCOSTFUNCTION(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
%m = length(y); % number of training examples

% You need to return the following variables correctly 
m = size(X,1);
h = sigmoid(sum(X*theta,2));
J = 1/m.*sum((-y).*log(h) - (1-y).*log(1-h)) + lambda/2/m.*sum(theta(2:size(theta,1),:).^2);
% grad_mult is used to only activate regularization for theta 1:m (not theta 0)
grad_mult = zeros(size(theta,1),1); grad_mult(2:size(grad_mult,1)) = 1;
grad = 1/m*X'*(h-y) + lambda/m*grad_mult.*theta;

end
