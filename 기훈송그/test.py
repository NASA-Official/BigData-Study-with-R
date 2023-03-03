from tensorflow.keras.models import Sequential
from tensorflow.keras.optimizers import Adam
from tensorflow.keras import layers
import numpy as np

model = Sequential([layers.Input((3, 1)),
                    layers.LSTM(64),
                    layers.Dense(32, activation='relu'),
                    layers.Dense(32, activation='relu'),
                    layers.Dense(1)])

model.compile(loss='mse',
              optimizer=Adam(learning_rate=0.001),
              metrics=['mean_absolute_error'])


X_train = np.random.rand(100, 3)
y_train = np.random.rand(100)
X_val = np.random.rand(100, 3)
y_val = np.random.rand(100)
model.fit(X_train, y_train, validation_data=(X_val, y_val), epochs=100)
