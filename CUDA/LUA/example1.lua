require 'torch'

torch.manualSeed(1234)

-- choose a dimension
N = 5

-- create a random NxN matrix
A = torch.rand(N, N)
print(A)

-- make it symmetric positive
A = A*A:t()
print(A)

-- make it definite
A:add(0.001,torch.eye(N))
print(A)

-- add a linear term
b = torch.rand(N)
print(b)

-- create the quadratic form
function J(x)
	return 0.5*x:dot(A*x)-b:dot(x)
end

print(J(torch.rand(N)))

xs = torch.inverse(A)*b
print(string.format('J(x^*) = %g',J(xs)))

function dJ(x)
	return A*x-b
end

x = torch.rand(N)
lr = 0.01
for i=1,20000 do
	x = x - dJ(x)*lr
	-- we print the value of the objective function at each iteration
	print(string.format('at iter %d J(x) = %f', i, J(x)))
end
