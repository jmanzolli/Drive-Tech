from functions import *

file = input('Write the file name:\n') # default = input.xlsx
data = read_file(file)
model = setModel(data,time_limit=None,mipgap=None,solver='glpk',status=True)
plot(model)
save(model)
