% Quiz II - Linear Regression with One Variable
%% II-1 Number of Samples
    dat = [3 4;
           2 1;
           4 3;
           0 1];
    nrows = size(dat,1)
    sprintf(strcat('II1:nrows=',num2str(nrows)))
    
 %% II-2 Cost Function for J(0,1)
 % J(theta0,theta1) = (1/2m)sum((h(x)-y)^2)
 theta0 = 0; theta1 = 1;   
 h_x = dat(:,1).*theta1 + theta0;
 J = 1/2/nrows*sum((h_x-dat(:,2))^2)
 
%% IV-1
X = [1 89 7921;
    1 72 5184;
    1 94 8836;
    1 69 4761]
y = [96 74 87 78]'
x2mean = mean(X(:,3))
x2range = max(X(:,3))-min(X(:,3))
x2_3 = (X(4,3)-x2mean) / x2range


