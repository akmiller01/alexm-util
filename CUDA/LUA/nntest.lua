require 'torch'
require 'nn'

-- Command line parameters
cmd = torch.CmdLine()
cmd:text()
cmd:text('Options for my NN')
cmd:option('-units',10,'units in the hidden layer')
cmd:option('-learningRate',0.01,'learning rate')
cmd:option('-csv',"eth_dat.csv",'csv file')
cmd:option('-header',true,'csv has header')
-- etc...
cmd:text()
opt = cmd:parse(arg)


-- Fully connected feed-forward network container
mlp = nn.Sequential()

-- Data requirement: lua table with method size()
-- Method to read CSV
function string:splitAtCommas()
    local sep, values = ",", {}
    local pattern = string.format("([^%s]+)", sep)
    self:gsub(pattern, function(c) values[#values+1] = c end)
    return values
end

function loadData(dataFile,header)
    local dataset = {}
    local length = 0
    local i = 1
    for line in io.lines(dataFile) do
        if header == true then
            header = false
        else
            local values = line:splitAtCommas()
            local y = torch.Tensor(1)
            y[1] = values[#values] + 1 -- the target class is the last number in the line
            values[#values] = nil
            length = #values
            local x = torch.Tensor(values) -- the input data is all the other numbers
            dataset[i] = {x, y}
            i = i + 1
        end
    end
    function dataset:size() return (i - 1) end -- the requirement mentioned
    function dataset:length() return length end
    return dataset
end

dataset = loadData(opt.csv,opt.header)

-- Using tanh as transfer function for non-linearlity
inputSize = dataset:length()
hiddenLayer1Size = opt.units
hiddenLayer2Size = opt.units

mlp:add(nn.Linear(inputSize,hiddenLayer1Size))
mlp:add(nn.Tanh())
mlp:add(nn.Linear(hiddenLayer1Size,hiddenLayer2Size))
mlp:add(nn.Tanh())

-- outputs
nclasses = 2

mlp:add(nn.Linear(hiddenLayer2Size,nclasses))
mlp:add(nn.LogSoftMax())

-- Print
-- print(mlp)
-- out = mlp:forward(torch.randn(1,10))
-- print(out)

-- Training with SGD and negative log-likelihood criterion

criterion = nn.ClassNLLCriterion()
trainer = nn.StochasticGradient(mlp,criterion)
trainer.learningRate = opt.learningRate

-- Training!
trainer:train(dataset)

-- Prediction
-- x = torch.randn(43)
-- y = mlp:forward(x)
-- print(y)

-- Accuracy
function argmax(v)
  local maxvalue = torch.max(v)
  for i=1,v:size(1) do
    if v[i] == maxvalue then
      return i
    end
  end
end

tot = 0
pos = 0
local header = opt.header
for line in io.lines(opt.csv) do
    if header == true then
        header = false
    else
        values = line:splitAtCommas()
        local y = torch.Tensor(1)
        y[1] = values[#values] + 1
        values[#values] = nil
        local x = torch.Tensor(values)
        local prediction = argmax(mlp:forward(x))
        if math.floor(prediction) == math.floor(y[1]) then
            pos = pos + 1
        end
        tot = tot + 1
    end
end
print("Accuracy(%) is " .. pos/tot*100)

-- Save for later
torch.save('model.th', mlp)
-- mlp2 = torch.load('model.th')
