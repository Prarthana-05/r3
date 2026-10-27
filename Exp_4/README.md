# Project Title: Image Classification using Keras and EBImage

**Project Objective:**
To design and implement an end-to-end data science project using R, from data processing to model evaluation, and maintain it using Git version control.

**Brief Description of the Problem:**
This project builds a deep learning model to process, resize, and classify local image files into specific categories using a sequential neural network.

**Dataset Information:**
The dataset consists of 12 local image files (e.g., `P1.jpeg` to `C6.jpeg`). The images are programmatically imported, resized to 28x28 pixels, and reshaped into arrays for training and testing. 

**R Packages/Libraries Used:**
* `EBImage`: For image reading, resizing, and manipulation.
* `keras`: For building, compiling, and training the deep learning sequential model.

**Major Operations Performed:**
* Data Import and Preprocessing: Reading images, resizing (28x28), and reshaping arrays.
* Data Splitting: Segregating images into training (`trainx`) and testing (`testx`) sets.
* Categorical Encoding: One-hot encoding the target labels.
* Model Construction: Building a sequential model with dense layers using `relu` and `softmax` activations.
* Compilation and Training: Compiling with `binary_crossentropy` loss and fitting over 30 epochs.
* Evaluation: Predicting probabilities, extracting the highest probability class, and generating a confusion matrix.

**Instructions to Execute the Project:**
1. Clone this repository.
2. Ensure R and RStudio are installed, along with the `EBImage` (via BiocManager) and `keras` packages.
3. Run `install_keras()` in the R console if the Python backend is not configured.
4. Update the `setwd()` path in the R script to match your local directory containing the images.
5. Execute the R script sequentially.

**Important Results/Output:**
* Model training history plot (Accuracy and Loss vs. Epochs).
* Confusion matrix displaying Predicted vs. Actual classifications.

**Screenshots:**
*(Replace this text with screenshots of your RStudio environment, the generated training history plot, and the final console output demonstrating successful execution)*