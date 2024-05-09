from functions import *

file = input('Write the file name:\n') # default = input.xlsx
data = read_file(file)
model = setModel(data,time_limit=60,mipgap=0,solver='gurobi',status=True)
plot(model)
save(model)
