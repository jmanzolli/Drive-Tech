from inst.miscellaneous.functions import *
data = read_file()
model = setModel(data,time_limit=60,mipgap=None,solver='gurobi',status=True)
plot(model)
save(model)
