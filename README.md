# CNN
Convolutional Neural Network in R  
The framework is tailored for experimentation not production environements.  
Netowrk assumes a default architecture as follows:  
(CONV-CONV-POOL)x3 - FCx3  
Customized architectures can be input by filling the arch table  
The framework assumes a padding non overlapping of size 2  
Filter sizes and steps can be set manually  
Here we describe API notations
# 1. Layers: 
There are L+1 layer including final layer (Input data is denoted as layer 1 
eventhough there are no operations related to it). We consider 4 types of layers
1.1 CONV-ACTIVATION: Convolution followed by an activation such as ReLU
1.2 POOL layer: max pooling, average, L1 pooling
1.3 final layer: fully connected layer
1.4 Input layer: only kept for convenience, there are no operations related to it.

# 2. Weights: Weights are a list denoted W, length=L+1
L+1 layers of weights such that W_1=NULL and W_l=NULL if it's pooling layer
each weight is a list composed of coefficients W and biad term b

# 3. Outputs: layer output are a list denoted Y, length=L+1
such that Y_1=X (no operation)
if l=2...(L+1): Y_l=X_(l+1), layer l output=layer l+1 input

# 4. deltas (dE/dZ): list of length L+1, deltas_1=NULL
deltas contain partial derivatives of cost by Z_l
deltas_l=dE/dZ_l

# 5.grads (dE/dW): weights gradients

Please send all your remarks to azzouz.marouen@gmail.com
