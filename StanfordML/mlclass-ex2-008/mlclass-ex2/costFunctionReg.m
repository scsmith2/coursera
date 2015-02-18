function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
%m = length(y); % number of training examples

% You need to return the following variables correctly 
%J = 0;
%grad = zeros(size(theta));
h = sigmoid(sum(theta'.*X,2));
J = 1/size(X,1).*sum((-y).*log(h) - (1-y).*log(1-h)) + lambda/2/size(X,1).*sum(theta(2:size(theta,1),:).^2);
grad_mult = zeros(size(theta,1),1); grad_mult(2:size(grad_mult,1)) = 1;
grad = (1/size(X,1).*sum((h-y).*X))' + lambda/size(X,1).*grad_mult.*theta;

end
