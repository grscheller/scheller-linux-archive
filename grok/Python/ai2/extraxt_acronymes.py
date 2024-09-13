#!/usr/bin/python

models = ["Logistic Regression", "Decision Tree", "Random Forest", "Support Vector Machine", "Naive Bayes"]
model_acronyms = [''.join([name[0] for name in model.split()]) for model in models]

### Notebook grading
correct_answer = ['LR', 'DT', 'RF', 'SVM', 'NB']
if model_acronyms == correct_answer:
    print("Good job!")
else:
    print("Not quite! Did you create the acronyms correctly?")


initial_lr = 0.1
decay_factor = 0.1
learning_rates: list[float|str] = [initial_lr*(decay_factor**i) for i in range(0, 6)]
learning_rates = [format(lr, '.6f') for lr in learning_rates]
print(learning_rates)

### Notebook grading
correct_answer = ['0.100000', '0.010000', '0.001000', '0.000100', '0.000010', '0.000001']
if learning_rates == correct_answer:
    print("Good job!")
else:
    print("Not quite! Are you sure the decay factor is applied correctly for each step?")

model_performances = {
    "Logistic Regression": 90,
    "Decision Tree": 75,
    "Random Forest": 92,
    "Support Vector Machine": 80,
    "Naive Bayes": 88
}

passed_models = [model for model, perf in model_performances.items() if perf >= 85]

### Notebook grading
correct_answer = ['Logistic Regression', 'Random Forest', 'Naive Bayes']
if passed_models == correct_answer:
    print("Good job!")
else:
    print("Not quite! Did you use the corresponding performance to filter each model?")
