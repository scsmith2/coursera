function p = predict(Theta1, Theta2, X)
%PREDICT Predict the label of an input given a trained neural network
%   p = PREDICT(Theta1, Theta2, X) outputs the predicted label of X given the
%   trained weights of a neural network (Theta1, Theta2)

% Useful values
m = size(X, 1);
X = [ones(m,1) X];
A = sigmoid(X*Theta1');
A = [ones(size(A,1),1) A];
B = sigmoid(A*Theta2');
[mx ix] = max(B,[],2);
p = ix;
end
