import random
import numpy as np
import pandas as pd
import math
import torch
from torch import nn
from torch.nn import functional as F
from torch.utils.data import TensorDataset, DataLoader
    
def set_seed(seed = 1234):
    """ Sets the seed of the entire notebook so results are the same every time we run.
    This is for reproducibility. """
    np.random.seed(seed)
    random.seed(seed)
    torch.manual_seed(seed)

def lstm_data_transform(x_data, y_data, seq_sz=5):
    """ Changes data to the format for LSTM training for sliding window approach. """
    # Prepare the list for the transformed data
    X, y = list(), list()
    # Loop of the entire data set
    for i in range(x_data.shape[0]):
        # compute a new (sliding window) index
        end_ix = i + seq_sz
        # if index is larger than the size of the dataset, we stop
        if end_ix >= x_data.shape[0]:
            break
        # Get a sequence of data for x
        seq_X = x_data[i:(end_ix+1)]
        # Get only the last element of the sequency for y
        seq_y = y_data[end_ix]
        # Append the list with sequencies
        X.append(seq_X)
        y.append(seq_y)
    # Make final arrays
    x_array = np.array(X)
    y_array = np.array(y)
    return x_array, y_array

class MarkovRegimeSwitchingLSTM(nn.Module):
    def __init__(self, input_sz: int, hidden_sz: int, output_sz: int, regime_nb: int, init_prob: float):
        super().__init__()
        self.input_size = input_sz
        self.hidden_size = hidden_sz
        self.output_size = output_sz
        self.regime_nb = regime_nb
        
        # psi
        psi = torch.ones(regime_nb, regime_nb) * (1 - init_prob) / (regime_nb - 1)
        psi.fill_diagonal_(init_prob)
        self.psi = nn.Parameter(psi)
        
        # output layer
        self.output_layer = nn.Parameter(torch.Tensor(hidden_sz, output_sz))
        
        # i, f, c, o
        self.U = nn.Parameter(torch.Tensor(regime_nb, input_sz, hidden_sz * 4))
        self.V = nn.Parameter(torch.Tensor(regime_nb, hidden_sz, hidden_sz * 4))
        self.b = nn.Parameter(torch.Tensor(regime_nb, hidden_sz * 4))
        
        self.init_weights()
        
    def init_weights(self):
        """ Initialize network weights. """
        iter_params = iter(self.parameters())
        next(iter_params)
        out_layer = next(iter_params)
        torch.nn.init.normal_(out_layer)
        hs = self.hidden_size
        for weight in iter_params:
            if len(weight.size()) > 2:
                for k in range(self.regime_nb):
                    for j in range(0, 4):
                        torch.nn.init.xavier_normal_(weight[k, :, (j*hs):((j+1)*hs)])
            else:
                for k in range(self.regime_nb):
                    for j in range(0, 4):
                        torch.nn.init.normal_(weight[k, (j*hs):((j+1)*hs)])
                    
    def forward(self, x, states, alpha_t, likelihoods=None):
        """ Assumes x.shape represents (batch_size, sequence_size, input_size). """
        
        batch_sz, seq_sz, _ = x.size()
        regime_nb = self.regime_nb
        (h_t, c_t) = states

        if likelihoods is not None:
            # Check likelihoods for zeros
            if torch.all(likelihoods < 0.0001):
                likelihoods = torch.ones(regime_nb, 1)
            # Update alpha_t
            alpha_t = torch.mul(likelihoods, torch.mm(torch.t(self.psi), alpha_t))
            alpha_t = torch.div(alpha_t, torch.sum(alpha_t))
        
        hs = self.hidden_size
        for t in range(seq_sz):
            x_t = x[:, t, :]
            c_t_new = []; h_t_new = []; y_tk = []
            for k in range(regime_nb):
                gates = x_t @ self.U[k,:,:] + h_t @ self.V[k,:,:] + self.b[k,:]
                i_t, f_t, g_t, o_t = (
                    torch.sigmoid(gates[:, :hs]),
                    torch.sigmoid(gates[:, hs:hs*2]),
                    torch.tanh(gates[:, hs*2:hs*3]),
                    torch.sigmoid(gates[:, hs*3:])
                )
                c_tk_current = f_t * c_t + i_t * g_t
                h_tk_current = o_t * torch.tanh(c_tk_current)
                c_t_new.append(alpha_t[k, 0] * c_tk_current)
                h_t_new.append(alpha_t[k, 0] * h_tk_current)
                if t == (seq_sz - 1):
                    y_tk_current = h_tk_current @ self.output_layer
                    y_tk_current = F.softplus(y_tk_current, beta = 5, threshold = 5)
                    y_tk.append(y_tk_current.unsqueeze(0))
            
            c_t = torch.stack(c_t_new, dim = 0).sum(dim = 0)
            h_t = torch.stack(h_t_new, dim = 0).sum(dim = 0)
        
        y_tk = torch.cat(y_tk, dim = 0)
        y_tk = y_tk.contiguous()
        y_tk = y_tk.view(-1, self.output_size)
        
        out = h_t @ self.output_layer
        out = F.softplus(out, beta = 5, threshold = 5)
        
        return out, (h_t, c_t), y_tk, alpha_t

def test_epoch(data_loader, model, criterion, batch_sz, hidden_sz, regime_nb, beta, device):
    
    # Initialize states, alpha, likelihoods and R_t
    states, alpha_t, likelihoods, R_t = initialize(batch_sz, hidden_sz, regime_nb, device)
    
    model.eval()
    test_loss = 0; test_mae = 0
    prediction = []; true = []; regime_probs = []
    
    for i, (x, y) in enumerate(data_loader):

        # Predict validation data
        pred, states, y_tk, alpha_t = model(x, states, alpha_t, likelihoods)

        # Compute loss
        loss = criterion(pred, y)
        test_loss += loss.item()
        
        # Compute MAE
        test_mae += torch.abs(pred - y).item()
        
        # Compute errors
        e_t = torch.sub(y, y_tk)
        e_t = e_t.data
        
        # Compute likelihoods
        lh = []
        for k in range(regime_nb):
            r_t = R_t[k, 0]
            a = 1 / np.sqrt(2 * math.pi * r_t)
            b = np.exp((-1 / 2) * (e_t[k, 0]**2) / r_t)
            lh.append(a * b)
        likelihoods = torch.FloatTensor(lh).reshape(-1, 1)

        # Update R_t
        R_t = (1 - beta) * R_t  + beta * torch.mul(e_t, e_t)
        
        prediction.append(pred.item())
        true.append(y.item())
        regime_probs.append(alpha_t.detach().numpy())

    test_loss /= (i+1)
    test_mae /= (i+1)
    
    return test_loss, test_mae, prediction, true, regime_probs

def test(test_loader, batch_sz, input_sz, output_sz, vehicle, device, checkpoint, test_output_dir):
    
    hidden_sz = 128
    learning_rate = 0.0001
    beta = 0.1
    regime_nb = 4
    init_prob = 0.9

    # Build model
    model = MarkovRegimeSwitchingLSTM(input_sz, hidden_sz, output_sz, regime_nb, init_prob).to(device)

    # Create criterion and optimizer
    criterion = nn.MSELoss()
    model.load_state_dict(checkpoint['model_state_dict'])
    model.eval()

    # Test phase
    test_loss, test_mae, pred, true, regime_probs = test_epoch(test_loader, model, criterion, 
                                                         batch_sz, hidden_sz, regime_nb, beta, device)
    true = np.array(true).reshape(-1,1)
    pred = np.array(pred).reshape(-1,1)
    test_mae_org = np.mean(np.abs(true - pred))
    regime_probs = np.array(regime_probs).reshape(-1,regime_nb)
    regime_probs = pd.DataFrame(regime_probs, columns = ["regime" + str(i) for i in range(1,regime_nb+1)])
    out_df = pd.DataFrame(data={'true': true.flatten(), 'pred': pred.flatten()})
    out_df = pd.concat([out_df, regime_probs], axis=1)
    out_df.to_csv(test_output_dir + '/' + vehicle + '_test_output.csv')   
    
    return test_loss, test_mae, true, pred

def initialize(batch_sz, hidden_sz, regime_nb, device):
    
    # Initialize R_t
    R_t = torch.ones(regime_nb, 1)

    # Initializing hidden and cell states for first input using method defined below
    states = (
        torch.zeros(batch_sz, hidden_sz).to(device),
        torch.zeros(batch_sz, hidden_sz).to(device)
    )

    # Initialize likelihoods
    likelihoods = None

    # Initialize alpha_t
    alpha_t = torch.mul(1/regime_nb, torch.ones(regime_nb, 1))
    
    return states, alpha_t, likelihoods, R_t