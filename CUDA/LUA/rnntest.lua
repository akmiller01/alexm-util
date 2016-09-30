require 'torch'
require 'rnn'

-- Command line parameters
cmd = torch.CmdLine()
cmd:text()
cmd:text('Options for my NN')
cmd:option('-batchSize',10,'batch size')
cmd:option('-hiddenSize',64,'units in the hidden layer')
cmd:option('-rho',5,'rho')
cmd:option('-nIndex',10000,'nIndex')
cmd:option('-csv',"eth_dat.csv",'csv file')
cmd:option('-header',true,'csv has header')
-- etc...
cmd:text()
opt = cmd:parse(arg)

batchSize = opt.batchSize
rho = opt.rho
hiddenSize = opt.hiddenSize
nIndex = opt.nIndex

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

function gradientUpgrade(model, x, y, criterion, learningRate, i)
    local prediction = model:forward(x)
    local err = criterion:forward(prediction, y)
    if i % 100 == 0 then
        print('error for iteration ' .. i  .. ' is ' .. err/rho)
    end
	local gradOutputs = criterion:backward(prediction, y)
	model:backward(x, gradOutputs)
	model:updateParameters(learningRate)
    model:zeroGradParameters()
end

-- Model
model = nn.Sequential()
model:add(nn.Sequencer(nn.LookupTable(nIndex, hiddenSize)))
model:add(nn.Sequencer(nn.FastLSTM(hiddenSize, hiddenSize, rho)))
model:add(nn.Sequencer(nn.Linear(hiddenSize, nIndex)))
model:add(nn.Sequencer(nn.LogSoftMax()))

criterion = nn.SequencerCriterion(nn.ClassNLLCriterion())


-- dummy dataset (task predict the next item)
dataset = torch.randperm(nIndex)
print(dataset)
--dataset = loadData(opt.csv,opt.header)
-- this dataset represent a random permutation of a sequence between 1 and nIndex

-- define the index of the batch elements
offsets = {}
for i= 1, batchSize do
   table.insert(offsets, math.ceil(math.random() * batchSize))
end
offsets = torch.LongTensor(offsets)

lr = 0.1
for i = 1, 10e4 do
    local inputs, targets = {}, {}
    for step = 1, rho do
        --get a batch of inputs
        table.insert(inputs, dataset:index(1, offsets))
        -- shift of one batch indexes
        offsets:add(1)
        for j=1,batchSize do
            if offsets[j] > nIndex then
                offsets[j] = 1
            end
        end
        -- a batch of targets
        table.insert(targets, dataset:index(1, offsets))
    end

    gradientUpgrade(model, inputs, targets, criterion, lr, i)
end