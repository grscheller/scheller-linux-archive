#!/usr/bin/python

model_performance = {
    'Experiment 1': {
        'Model A': 0.85, 'Model B': 0.9, 'Model C': 0.88, 'Model D': 0.92, 'Model E': 0.87
    },
    'Experiment 2': {
        'Model A': 0.91, 'Model B': 0.89, 'Model C': 0.93, 'Model D': 0.94, 'Model E': 0.86
    },
    'Experiment 3': {
        'Model A': 0.87, 'Model B': 0.9, 'Model C': 0.86, 'Model D': 0.95, 'Model E': 0.84
    },
    'Experiment 4': {
        'Model A': 0.88, 'Model B': 0.85, 'Model C': 0.89, 'Model D': 0.93, 'Model E': 0.87
    },
    'Experiment 5': {
        'Model A': 0.89, 'Model B': 0.88, 'Model C': 0.91, 'Model D': 0.92, 'Model E': 0.85
    },
    'Experiment 6': {
        'Model A': 0.9, 'Model B': 0.87, 'Model C': 0.92, 'Model D': 0.91, 'Model E': 0.88
    },
    'Experiment 7': {
        'Model A': 0.86, 'Model B': 0.89, 'Model C': 0.85, 'Model D': 0.94, 'Model E': 0.89
    },
    'Experiment 8': {
        'Model A': 0.91, 'Model B': 0.92, 'Model C': 0.88, 'Model D': 0.93, 'Model E': 0.86
    },
    'Experiment 9': {
        'Model A': 0.92, 'Model B': 0.87, 'Model C': 0.89, 'Model D': 0.95, 'Model E': 0.87
    },
    'Experiment 10': {
        'Model A': 0.89, 'Model B': 0.9, 'Model C': 0.87, 'Model D': 0.94, 'Model E': 0.88
    }
}

# Create a dictionary to store the count of models meeting the performance threshold
performance_count_dict: dict[str, float] = {}
threshold = 0.9

# Iterate through the model performance dictionary
for experiment, models in model_performance.items():
    for model, performance in models.items():
        if performance >= threshold:
            if model in performance_count_dict:
                performance_count_dict[model] += 1
            else:
                performance_count_dict[model] = 1

# Print the resulting dictionary
print(performance_count_dict)

# Create a dictionary to store the count of models exceeding different thresholds
thresholds = [0.85, 0.9, 0.95]
threshold_count_dict: dict[float, dict[float, model]] = {threshold: {} for threshold in thresholds}

# Iterate through the model performance dictionary
for experiment, models in model_performance.items():
    for model, performance in models.items():
        for threshold in thresholds:
            if performance >= threshold:
                if model in threshold_count_dict[threshold]:
                    threshold_count_dict[threshold][model] += 1
                else:
                    threshold_count_dict[threshold][model] = 1

# Print the resulting dictionary
print(threshold_count_dict)

# Assuming model_performance dictionary is already created from the previous exercise

# Create a dictionary to store the total performance and count of evaluations for each model
total_performance = {}
count_evaluations = {}

# Iterate through the model performance dictionary
for experiment, models in model_performance.items():
    for model, performance in models.items():
        if model in total_performance:
            total_performance[model] += performance
            count_evaluations[model] += 1
        else:
            total_performance[model] = performance
            count_evaluations[model] = 1

# Calculate the average performance for each model
average_performance = {model: total_performance[model] / count_evaluations[model] for model in total_performance}

# Find the maximum average performance
max_average_performance = max(average_performance.values())

# Create a list of models with the maximum average performance
best_performing_models = [model for model, performance in average_performance.items() if performance == max_average_performance]

# Print the resulting list
print(best_performing_models)
